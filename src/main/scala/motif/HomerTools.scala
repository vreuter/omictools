package omictools
package motif


/**
 * Tools for working with {@code HOMER} for motif analysis.
 *
 * @author Vince Reuter
 */
object HomerTools {
  import java.io.File
  import java.nio.file.{ Files, Paths }
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.show._
  import mouse.boolean._
  import eu.timepit.refined
  import refined.{ refineV }
  import refined.api.{ Refined, Validate }
  import refined.numeric.{ Positive }
  import omictools.generic.{ RefinedBuilder, WrappedCommand }
  import omictools.AssemblyTools._

  /** Simple wrapper of a command designated as a {@code findMotifsGenome.pl} program. */
  case class FindMotifsGenomeProg(command: String) extends WrappedCommand

  /**
   * Create a command with which to determine the subset of genomic regions that intersect at least one particular motif.
   *
   * @param regionsFile Path to the HOMER peak file or BED file containing the collection of query regions
   * @param genome The genome assembly to be used
   * @param motifFiles Collection of filepaths, each to a motif file
   * @param outfile Path to output file to write; this is used as an argument to the {@code -mbed} option of {@code annotatePeaks.pl}.
   * @param skipGenomicAnnotation Whether to perform general genomic annotation
   * @param skipGenicAnnotation Whether to perform genic annotation
   * @param size Optionally, the argument to the {@code -size} option provided by {@code annotatePeaks.pl}.
   * @param homerBaseDir Optionally, a path to the folder in which {@code HOMER} is installed
   * @return Either a {@code Left} containing an explanation for a failure to create the command, or a {@code Right} 
   *    containing a successfully constructed command.
   */
  def getFindSpecificMotifsCommand(
    regionsFile: File, genome: GenomeAssembly, motifFiles: NEL[File], outfile: File, 
    skipGenomicAnnotation: Boolean = true, skipGenicAnnotation: Boolean = true, 
    size: Option[Int] = None, homerBaseDir: Option[File] = None): Either[String, String] = {
    val maybeBaseCmd: Either[String, String] = for {
      progFpPair <- validateHomerPathAndRegions("annotatePeaks.pl", regionsFile, homerBaseDir)
      motifPaths <- motifFiles.filterNot(_.isFile) match {
        case Nil => Right(motifFiles.map(_.getPath).toList.mkString(" "))
        case missing => Left(s"${missing.size} missing motif file(s): ${missing.map(_.getPath).toList.mkString(", ")}")
      }
      sizeText <- size.fold[Either[String, String]](Right("given")){ x => (x > 0).either(s"Negative size: $x", x.toString) }
    } yield s"${progFpPair._1} ${progFpPair._2} ${genome.show} -size $sizeText -m $motifPaths -mbed ${outfile.getPath}"
    maybeBaseCmd.map(c => {
      Option(outfile.getParentFile) map { d => if (d.getPath != "" && d.getPath != "." && d.getPath != "./") d.mkdirs() }
      skipGenomicAnnotation.fold(c ++ " -noann", c)
    }).map(c => skipGenicAnnotation.fold(c ++ " -nogene", c))
  }

  /**
   * Create command with which to run the {@code findMotifsGenome.pl} program from {@code HOMER}.
   *
   * @param regionsFile Path to file in which the query regions for the motif finding are contained.
   * @param genome Genome assembly with which to run the motif finding.
   * @param outdir Path to output folder
   * @param size Specification of argument to {@code HOMER}'s {@code -size} option
   * @param maskRepeats Whether to exclude repetitive regions from the analysis
   * @param homerBaseDir Optionally, a path to the main {@code HOMER} installation folder, in which
   * @return Either a {@code Left} containing a command creation failure explanation, or a {@code Right} 
   *    containing a successfully generated command.
   */
  def getFindMotifsGenomeCommand(
    regionsFile: File, genome: GenomeAssembly, outdir: File, size: HomerSizeSpec, 
    maskRepeats: Boolean = true, homerBaseDir: Option[File] = None): Either[String, String] = {
    validateHomerPathAndRegions("findMotifsGenome.pl", regionsFile, homerBaseDir) map { case(prog, reg) => {
      if ( ! outdir.isDirectory && List(".", "./").contains(outdir.getName) ) outdir.mkdirs()
      val base = s"$prog ${regionsFile.getPath} ${genome.show} ${outdir.getPath} -size ${size.show}"
      maskRepeats.fold(s"$base -mask", base)
    } }
  }

  /**
   * Check that regions path is a file, and--if a {@code HOMER} installation path is provided--that there's a path to the requested program.
   *
   * @param progName Name of the {@code HOMER} program for which executability is being validated.
   * @param regionsFile Path to file containing regions to use as queries/targets to a {@code HOMER} program.
   * @param homerBaseDir Optionally, path to {@code HOMER} installation folder.
   * @return Either a {@code Left} with explanation of validation failure, or a {@code Right} with pair of program path and text version 
   *    of regions filepath.
   */
  def validateHomerPathAndRegions(progName: String, regionsFile: File, homerBaseDir: Option[File] = None): Either[String, (String, String)] = {
    import scala.sys.process._
    import cats.instances.int._
    import cats.syntax.eq._
    for {
      fp <- regionsFile.isFile.either(s"Missing regions file: ${regionsFile.getPath}", regionsFile.getPath)
      progPath <- homerBaseDir match {
        case None => { (0 === s"which $progName".!).either(s"Program isn't path: $progName", progName) }
        case Some(d) => {
          val pp = Paths.get(d.getPath, "bin", progName)
          Files.isExecutable(pp).either(s"Program path isn't an executable file: ${pp.toString}", pp.toString)
        }
      }
    } yield (progPath, fp)
  }
  
}
