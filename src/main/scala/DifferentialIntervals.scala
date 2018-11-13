package omictools


/**
 * Tools for working with genomic regions in which counts of sequencing reads vary substantially between sample conditions.
 *
 * @author Vince Reuter
 */
object DifferentialIntervals {
  
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import scala.io.Source
  import cats.data.{ NonEmptyList => NEL }
  import mouse.boolean._
  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric.NonNegative
  import TypedFiles._
  import experic.KeyedStreamTools.{ switchFields, writeKeyed }

  /** Wrapper for a file representing a particular kind of data. */
  abstract class TypedFile(get: File) extends FileTypeLike { def path: File = get }
  
  /**
   * Path to file storing tabular form of regularized differential counts, clustered and labeled.
   *
   * The regularization is typically performed in {@code R} with the {@code DESeq2} package, and 
   * the clustering and labeling are typically done with {@code kmeans}.
   *
   * @constructor
   */
  case class RegularizedDataFile(get: File) extends TypedFile(get)
  
  /**
   * Path to file representing {@code featureCounts} result, modified for use within {@code R}.
   * @constructor
   */
  case class CountsDataFile(get: File) extends TypedFile(get)
  
  /** Conveniences for working with {@code TypedFile} instances. */
  object TypedFile {
    /** Provide automatic unboxing of a {@code TypedFile} instance to ordinary {@code File} */
    implicit def toFile: TypedFile => File = _.path
  }

  // The first column of the header of a regularized data file or a counts file must be populated by this value.
  private val goodKeyCols = Set("Peak", "Gene", "peak", "gene")

  /**
   * Pair up with original counts data the differential genes/peaks and their labels from a text file of regularized data.
   *
   * @param regDataFile Path to the file from which to read peak/gene names and labels; name is first column and label is last column.
   * @param countsFile Path to file with the original counts data.
   * @param outfile Path to file to create
   * @param sep Delimiter between data fields on each line
   * @param labelColumn Name of the column that contains the label for each row observation
   * @return {@code Either} a {@code Left} containing a fatal exception, or an empty {@code Right}, which suggests write success.
   */
  def labeledNormToCounts(regDataFile: RegularizedDataFile, countsFile: CountsDataFile, 
    outfile: File, sep: String = "\t", labelColumn: String = "label"): Either[Throwable, Unit] = {
    import cats.instances.string._    // For Show[String]
    import cats.syntax.eq._
    
    require(regDataFile.isFile, s"Alleged regularized data path doesn't point to a file: ${regDataFile.getPath}")
    require(countsFile.isFile, s"Alleged counts data path doesn't point to a file: ${countsFile.getPath}")
    val lines = Source.fromFile(regDataFile).getLines
    val headerFields = Option(lines.next()).fold(throw new Exception(s"No lines: ${regDataFile.getPath}"))(_.split(sep).toSeq)
    require(goodKeyCols.contains(headerFields(0)), 
      s"Invalid first column (${headerFields(0)}) in regularized data file: ${regDataFile.getPath}; expected one of: ${goodKeyCols.mkString(", ")}")
    require(headerFields.last === labelColumn, s"Expected label column isn't the last header field; expected ${labelColumn} but got ${headerFields.last}")
    
    val (countsLines, indexBySample) = readCountsData(countsFile, sep, labelColumn)
    
    val maybeIndexOrder: Either[Throwable, Seq[Int]] = headerFields.tail.init.foldLeft(List.empty[String], Seq.empty[Int]){
      case(accs, s) => indexBySample.get(s).fold((s :: accs._1, accs._2))(i => (accs._1, accs._2 :+ i)) } match {
      case (Nil, indices) => Right(indices)
      // Any missing sample is a fatal exception, as the collection of samples used for differential analysis must be a 
      // subset of the full array of samples for which raw counts are available on a per-peak/-gene basis.
      case (missingSamples, _) => Left(new Exception(s"Samples missing from counts header: ${missingSamples.mkString(", ")}"))
    }
    
    maybeIndexOrder map { indexOrder => {
      val countData: Map[String, Seq[String]] = (countsLines map {
        l => { val fields = l.split(sep); fields(0) -> indexOrder.map(i => fields(i)) } } ).toMap
      val out = new BW(new FW(outfile))
      try {
        out.write(headerFields.mkString(sep)); out.newLine()
        lines foreach { l => {
          val fields = l.split(sep)
          val outdata = ((fields(0) +: countData(fields(0))) :+ fields.last)
          out.write(outdata.mkString(sep)); out.newLine()
        } }
        Right(())
      }
      catch { case e: Throwable => Left(e) }
      finally { out.close() }
    } }
  }

  /**
   * From records encoding--at minimum--gene and a group/cluster label, gather genes by label.
   *
   * @param ev Instance with which to validate each record and parse label and gene.
   * @return If the validation or parse attempts failed for any line, a {@code Left} containing the (nonempty) collection 
   *    of pairs of error message and failed record; otherwise, a {@code Right} containing the binding between 
   *    group/cluster label and the associated collection of genes.
   */
  def genesFromClustergramData(records: Iterator[Seq[String]])(implicit ev: ClustergramPartitioner): Either[NEL[(String, Seq[String])], Map[String, Seq[String]]] = {
    // TODO: this feels like a good candidate test/trial case for fs2.
    import scala.collection.mutable.{ ListBuffer, Map => MutMap }
    import cats.syntax.list._
    val m = MutMap.empty[String, Seq[String]]
    val bads = ListBuffer.empty[(String, Seq[String])]
    while (records.hasNext) {
      val r = records.next()
      // Process each record by first validating it and then attempting to parse the necessary data.
      // Failure of the validation or of any data parsing results in an update to the accumulating errors collection 
      // while successful validation and parsing updates the binding between group/cluster label and genes collection.
      ev.validate(r).fold(
        msg => bads += (msg -> r), 
        _ => try {
          val g = ev.parseGene(r)
          val l = ev.parseLabel(r)
          m.put(l, m.getOrElse(l, Seq.empty[String]) :+ g)
        } catch { case e: Throwable => bads += (e.getMessage -> r) }
      )
    }
    Either.cond(bads.isEmpty, m.toMap, bads.toList.reverse.toNel.get)
  }

  /* Counts file header validation and construction of binding between sample name and column index.
  This is used to inform how to parse the counts file in to preserve sample (column) order in regularized file, 
  while simultaneously accounting for the fact that the differential analysis may have been conducted over a proper 
  subset of the samples in the counts table. This is because the counts table is a data entity "upstream" of the 
  regularized data file, and thus just a subset of the full array of samples available may be of interest for analysis. */
  private def readCountsData(f: File, sep: String = "\t", labelColumn: String = "label"): (Iterator[String], Map[String, Int]) = {
    require(f.isFile, s"Alleged counts path isn't a file: ${f.getPath}")
    val allLines = Source.fromFile(f).getLines
    val headerFields = Option(allLines.next()).fold(throw new Exception(s"No lines: ${f.getPath}"))(_.split(sep).toSeq)
    require(goodKeyCols.contains(headerFields(0)), 
      s"Invalid first column (${headerFields(0)}) in counts data file: ${f.getPath}; expected one of: ${goodKeyCols.mkString(", ")}")
    (allLines, headerFields.zipWithIndex.tail.toMap)
  }

}
