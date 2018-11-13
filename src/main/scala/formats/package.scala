package omictools

/**
 * Tools for working with file formats
 *
 * @author Vince Reuter
 */
package object formats {
  import scala.io.Source
  import java.io.File
  import cats.Show
  import cats.data.{ NonEmptyList => NEL }
  import cats.instances.either._
  import cats.syntax.bifunctor._
  import cats.syntax.list._
  import cats.syntax.show._
  import mouse.boolean._
  import omictools.generic.{ RefinedBuilder, Znn, Zpos }
  import eu.timepit.refined.numeric._
  import omictools.FileTools.ExtantFile
  import ExtantFile._

  /**
   * Exception-like encountered while attempting parse of an alleged {@code SAF} record.
   *
   * @tparam T The type of value specific to this parse error subtype.
   * @author Vince Reuter
   */
  sealed protected trait ParseErrorSAF[+T] {
    /** The line being parsed when the error occurred. */
    def line: String
    /** The context-specific value for this error. */
    def get: T
  } 
  
  /** When the alleged {@code SAF} line has an unexpected number of fields. */
  final protected case class WrongFieldsCount(line: String, get: Int) extends ParseErrorSAF[Int]
  
  /** When the alleged {@code SAF} line has a chromosome field value that cannot be parsed as a chromosome. */
  final protected case class InvalidChromosome(line: String, get: String) extends ParseErrorSAF[String]
  
  /** When one or both coordinates of the alleged {@code SAF} line cannot be parsed as {@code Int}. */
  final protected case class NonIntegralCoordinates(line: String, get: (String, String)) extends ParseErrorSAF[(String, String)]
  
  /** When the alleged {@code SAF} line's end coordinate is not strictly greater than its start coordinate. */
  final protected case class IllegalCoordinates(line: String, get: (Int, Int)) extends ParseErrorSAF[(Int, Int)]
  
  /** When the alleged {@code SAF} line's strand field cannot be parsed as strand. */
  final protected case class IllegalStrand(line: String, get: String) extends ParseErrorSAF[String]
  
  /**
   * Implicits for working with SAF record parsing errors.
   *
   * @author Vince Reuter
   */
  object ParseErrorSAF {
    implicit def showParseError[T]: Show[ParseErrorSAF[T]] = new Show[ParseErrorSAF[T]] {
      def show(e: ParseErrorSAF[T]): String = e match {
        case WrongFieldsCount(l, n) => s"Expected 5 fields for SAF record but got $n in '$l'"
        case InvalidChromosome(l, c) => s"Could not parse chromosome '$c' in '$l'"
        case NonIntegralCoordinates(l, coords) => s"Non integral coordinate(s) (${coords._1}, ${coords._2}) in '$l'"
        case IllegalCoordinates(l, coords) => s"Illegal coordinates (${coords._1}, ${coords._2}) in '$l'"
        case IllegalStrand(l, strand) => s"Illegal strand ($strand) in '$l'"
      }
    }
  }

  private def parseStrand: String => Either[String, Option[Strand]] = s =>
    Strand.apply(s).fold(str => (str == ".").either(s, Option.empty[Strand]), str => Right(Some(str)))
  
  private def readLineSAF: String => Either[(String, String), SAFRecord] = l => {
    import ParseErrorSAF._
    l.split("\t").toList match {
      case n :: cRaw :: sRaw :: eRaw :: strandRaw :: Nil => {
        val maybeData: Either[ParseErrorSAF[Any], Tuple5[String, Chromosome, Znn, Zpos, Option[Strand]]] = for {
          c <- Chromosome(cRaw).left.map(_ => InvalidChromosome(l, cRaw))
          s <- RefinedBuilder.tryTextIntRefine[NonNegative](sRaw).left.map(_ => NonIntegralCoordinates(l, sRaw -> eRaw))
          e <- RefinedBuilder.tryTextIntRefine[Positive](eRaw).left.map(_ => NonIntegralCoordinates(l, sRaw -> eRaw))
          maybeStrand <- parseStrand(strandRaw).left.map(_ => IllegalStrand(l, strandRaw))
        } yield (n, c, s, e, maybeStrand)
        maybeData.fold(e => Left(l -> e.show), data => SAFRecord.buildSafe(data).bimap(e => l -> e, r => r))
      }
      case fields => { val err: ParseErrorSAF[Int] = WrongFieldsCount(l, fields.size); Left(l -> err.show) }
    }
  }

  /**
   * Parse records from a {@code SAF} file.
   *
   * @param f Path to the file to parse
   * @return An iterator in which each element is either a {@code Left} wrapping a pair of line and 
   *    error message, or a {@code Right} wrapping a valid {@code SAF} record.
   */
  def parseSAF(f: ExtantFile): Iterator[Either[(String, String), SAFRecord]] = {
    val lines = Source.fromFile(f).getLines
    lines.next()
    lines.map(readLineSAF)
  }

  private[this] def makeSAF2BEDName: String => Either[String, String] = fn => {
    val fields = fn.split("\\.").toSeq
    (fields.size > 1).either(s"Could not transform filename for SAF to BED: $fn", (fields.init :+ "bed").mkString("."))
  }

  /**
   * Write {@code BED3} from {@code SAF} file.
   *
   * @param f Path to {@code SAF} file
   * @param makeName How to map SAF filename to BED filename
   * @return Either a {@code Left} containing pairs of faulty line and error message, or a {@code Right} 
   *    wrapping path to output file created.
   */
  def saf2BED3(f: ExtantFile, makeName: String => Either[String, String] = makeSAF2BEDName): Either[NEL[(String, String)], File] = {
    import java.io.{ BufferedWriter => BW, FileWriter => FW }
    import omictools.Chromosome._
    import omictools.Strand._
    import omictools.generic.implicits.RefinedImplicits._
    // TODO: add Score type with a Show[Score] and Show[Option[Score]]
    val getBed: SAFRecord => String = r => Seq(r.chr.show, r.start.show, r.end.show, r.name, "0", r.strand.show).mkString("\t")
    val outname = makeSAF2BEDName(f.getName).fold(msg => throw new Exception(msg), fn => fn)
    val outfile = Option(f.getParentFile).fold(new File(outname))(d => new File(d, outname))
    val out = new BW(new FW(outfile))
    try {
      (parseSAF(f).foldLeft(Seq.empty[(String, String)]){ case (badAcc, errOrRec) => 
        errOrRec.fold(e => badAcc :+ e, r => { out.write(getBed(r)); out.newLine(); badAcc }) }).toList.toNel.toLeft(outfile)
    } finally { out.close() }
  }

}
