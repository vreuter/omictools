package omictools
package pathway


import com.typesafe.scalalogging.LazyLogging


/**
 * Tools and abstractions related to gene set analysis.
 *
 * @author Vince Reuter
 */
object GeneSetAnalysisTools extends LazyLogging {
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.show._         // For .show
  import eu.timepit.refined
  import refined.refineV
  import refined.api.Refined
  import refined.numeric.Positive
  import omictools.{ ClustergramPartitioner, DifferentialIntervals => DiffIntv, FileTools => FT, OrganismTools }
  import OrganismTools.Organism
  import omictools.generic.{ QuantityTools, WrappedCommand }
  import QuantityTools._
  import SetRankParamTools._
  import experic.Program._
  import experic.cluster.ClusterTools._

  /** Expand name for better readability. */
  type NonzeroProportion = QuantityTools.GoodProp

  /**
   * From records in which gene and group/cluster label are stored, create gene query sets for use with {@code SetRank}.
   *
   * @tparam X The type of value to wrap in a {@code Left} when at least one failure arises during the parsing of the records.
   * @param records Collection of data from which to parse gene and group/cluster label.
   * @param outdir Folder in which to place each gene set file.
   * @param labelToFilename How to create filename from gene group/cluster label.
   * @param partitioner Entity with which to validate each record and parse gene and label.
   * @param handleErrors How to handle record parsing error(s).
   * @return Either a {@code Left} containing a {@code X} representing record parsing errors, or a {@code Right} 
   *    in which the first component is a collection (hopefully empty) of pairs of group/cluster name and file writing error, 
   *    and the second component is a binding between group/cluster label and path to its gene list file.
   */
  def manifestInputFiles[X](records: Iterator[Seq[String]], outdir: File, labelToFilename: String => String)(
    implicit partitioner: ClustergramPartitioner, handleErrors: NEL[(String, Seq[String])] => X): Either[X, (List[(String, Throwable)], Map[String, File])] = {
    DiffIntv.genesFromClustergramData(records).fold(
      problems => Left(handleErrors(problems)),
      genesByGroup => {
        outdir.mkdirs()
        Right( genesByGroup.foldLeft(List.empty[(String, Throwable)] -> Map.empty[String, File]){ case(acc, groupAndGenes) => {
          val (errs, files) = acc
          val (group, genes) = groupAndGenes
          val outfile = new File(outdir, labelToFilename(group))
          logger.info("Writing genes for group '{}': {}", group, outfile.getPath)
          val out = new BW(new FW(outfile))
          try {
            genes foreach { g => { out.write(g); out.newLine() } }
            (errs, files + (group -> outfile))
          }
          catch { case e: Throwable => ((group -> e) :: errs, files) }
          finally { out.close() }
        } } )
      }
    )
  }

  /**
   * From records in which gene and group/cluster label are stored, create gene query sets for use with {@code SetRank}.
   *
   * @tparam X The type of value to wrap in a {@code Left} when at least one failure arises during the parsing of the records.
   * @param dataFile Path to the file in which each line encodes hea
   * @param outdir Folder in which to place each gene set file.
   * @param labelToFilename How to create filename from gene group/cluster label.
   * @param partitioner Entity with which to validate each record and parse gene and label.
   * @param handleErrors How to handle record parsing error(s).
   * @return Either a {@code Left} containing a {@code X} representing record parsing errors, or a {@code Right} 
   *    in which the first component is a collection (hopefully empty) of pairs of group/cluster name and file writing error, 
   *    and the second component is a binding between group/cluster label and path to its gene list file.
   */
  def manifestInputFiles[X](dataFile: File, header: Boolean, outdir: File, labelToFilename: String => String)(
    implicit partitioner: ClustergramPartitioner, handleErrors: NEL[(String, Seq[String])] => X): Either[X, (List[(String, Throwable)], Map[String, File])] = {
    require(dataFile.isFile, s"Alleged data path isn't a file: ${dataFile.getPath}")
    import scala.io.Source
    val records = Source.fromFile(dataFile).getLines.map(_.split("\t").toSeq)
    if (header) records.next()
    manifestInputFiles(records, outdir, labelToFilename)
  }

  /**
   * Write a file declaring for each gene group/cluster the path to the file containing its list of genes.
   *
   * @param specPath Path to the file to create
   * @param files Colllection of pairs in which first component is group/cluster name and second component is path to genes list file
   */
  def makeFileSpec(specPath: File, files: Seq[(String, File)]): Unit = {
    Option(specPath.getParentFile) map { d => if (!List("", ".").contains(d.getName)) d.mkdirs() }
    val out = new BW(new FW(specPath))
    try { files foreach { case(group, f) => { out.write(s"${group}\t${f.getPath}"); out.newLine() } } }
    finally { out.close() }
  }

  /**
   * Thin wrapper around command with which to run gene set / pathway enrichment analysis program.
   *
   * @constructor
   * @param command The comand with which to run the program
   * @author Vince Reuter
   */
  final case class GeneEnrichmentProg(command: String) extends WrappedCommand
  object GeneEnrichmentProg

  /**
   * Make the command with which to run {@code R} package {@code SetRank}'s gene set / pathway enrichment analysis program.
   *
   * @param dataFile Path to the file in which genes are labeled with group/cluster ID. Genes and rows are 1:1, and the first field in each row is a gene identifier.
   *    The last field in each row stores a gene's group/cluster ID.
   * @param dataHasHeader Whether the {@code dataFile}'s first line is non-data, i.e. comprised of column names
   * @param outdir Path to folder in which to place output and intermediate files
   * @param inputFormat The format of the individual gene identifiers in {@code dataFile}.
   * @param organism Specification of the organism for which the data being processed are relevant / from which they were generated.
   * @param msigdbCollection (Possible empty) collection of MSigDB categories denoting gene sets to use in the SetRank analysis
   * @param rCores Number of CPUs to allocate for use in execution, i.e. {@code options("mc.cores" = <rCores>)}.
   * @return Either a {@code Left} containing a file(s) to which errors have been written on a per-group basis, or a {@code Right} containing 
   *    a successfully created command.
   */
  def createCommand(
    dataFile: File, dataHasHeader: Boolean, outdir: File, inputFormat: GeneFormatType, organism: Organism, rCores: Refined[Int, Positive], 
    msigdbCollection: Option[MsigdbCollection] = None)(implicit params: GeneSetRankParams, cgPart: ClustergramPartitioner): Either[NEL[File], String] = {
    
    import java.nio.file.Paths
    
    logger.info("Starting gene set enrichment based on data file: {}", dataFile.getPath)
    
    implicit val handleErrors: NEL[(String, Seq[String])] => NEL[File] = Preamble.getErrorHandler(outdir)
    
    // Gene set filepaths manifest
    val manFile = new File(outdir, params.manifestFilename)
    
    // If and only if the creation of the gene set file succeeded for every group, write the manifest file that 
    // declares for each group the path to its gene set file.
    val maybeProceed: Either[NEL[File], File] = manifestInputFiles(dataFile, dataHasHeader, outdir, params.labelToFilename)(cgPart, handleErrors) map { case(fileWriteErrors, fileByGroup) =>
      fileWriteErrors match {
        // TODO: allow "partial" submission, i.e. of any groups that did succeed?
        case Nil => {
          logger.info("Writing gene sets manifest: {}", manFile.getPath)
          val man = new BW(new FW(manFile))
          try {
            fileByGroup foreach { case(g, f) => man.write(s"$g\t${f.getPath}"); man.newLine() }
            manFile
          }
          finally { man.close() }
        }
        case _ => throw new Exception(s"Failed groups:\n${(fileWriteErrors map { case(g, err) => s"$g: ${err.getMessage}" }).mkString("\n")}")
      }
    }
    
    val codePath = Option(System.getenv(params.codeHome)).getOrElse(throw new Exception(s"Missing ${params.codeHome} environment variable"))
    val findProgPath: String => String = code => Paths.get(code, "ScalaGenomeUtils", "src", "main", "R", "run_gene_set_rank.R").toString
    maybeProceed map { manifest => {
      val progFile = new File(findProgPath(codePath))
      if (!progFile.isFile) params.handleMissingProgram(progFile)
      val organismText = s"${organism.genus}.${organism.species}"
      val base = s"Rscript ${progFile.getPath} -O ${outdir.getPath} --fileSpec ${manFile.getPath} --inputFormat ${inputFormat.show} -G $organismText --msigdbSpecies $organismText --cores ${params.cores.value}"
      val intermediate = if (params.keepGeneFormat) s"$base --keepGeneFormat" else base
      msigdbCollection.fold(intermediate)(c => s"$intermediate --msigdbCollection ${c.show}")
    } }
  }

  /**
   * Create a script with which to submit analysis of enrichment of pathways or other gene sets.
   *
   * @param script Path to script file to create.
   * @param dataFile Path to file storing a label for each of a number of genes.
   * @param outdir Path to the folder in which to place output
   * @param inputFormat The format of the gene identifier, as used in {@code AnnotationDbi} package in {@code R}
   * @param organism Organism specification.
   * @param resources Bundle of resources for LSF job submission.
   * @param logfile Path to file to use for job logging.
   * @param rCores Number of CPUs to allocate for use in execution, i.e. {@code options("mc.cores" = <rCores>)}.
   * @param msigdbCollection (Possible empty) singleton collection of MSigDB category denoting gene sets to use in the SetRank analysis.
   * @return Either a {@code Left} containing a nonempty collection of paths to files 
   */
  def createSubmissionScriptLSF(
    script: File, dataFile: File, dataHasHeader: Boolean, outdir: File, inputFormat: GeneFormatType, organism: Organism, 
    resources: ResourceSpecTimedHours, logfile: File, rCores: Refined[Int, Positive], msigdbCollection: Option[MsigdbCollection] = None)(
    implicit params: GeneSetRankParams, cgPart: ClustergramPartitioner): Either[NEL[File], File] = {
    import omictools.generic.implicits.RefinedImplicits._
    require(resources.cores >= rCores, s"More R cores than job resource cores: ${rCores.value} > ${resources.cores.value}")
    createCommand(dataFile, dataHasHeader, outdir, inputFormat, organism, rCores, msigdbCollection) map { case rawCmd => 
      val program = GeneEnrichmentProg(rawCmd)
      val cmd = LSFContext.createCommand(program, resources, logfile)
      logger.info("Writing submission script: {}", script.getPath)
      val out = new BW(new FW(script))
      try {
        out.write("#!/bin/bash"); out.newLine()
        out.write(cmd); out.newLine()
        script
      } finally { out.close() }
    }
  }

}
