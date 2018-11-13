package omictools
package peaks


/**
 * Tools for getting statistics about peak call sets.
 *
 * @author Vince Reuter
 */
object PeakStats {
  
  import scala.io.Source
  import java.io.File
  
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.list._
  import mouse.boolean._
  
  import eu.timepit.refined
  import refined.refineV
  import refined.api.Refined
  import refined.numeric._
  
  import omictools.FileTools._
  import omictools.generic.Znn

  // TODO: consider default sorting behavior.

  /**
   * Parse size of each called peak in the given file.
   *
   * The input file can be of any format, so long as each line is interpretable as a peak 
   * record, with that interpretability defined by compliance with a couple constraints:
   * 1. Each line has tab-delimited fields
   * 2. The first three fields of each line may be regarded as BED3
   *
   * @param f Path to called peaks file
   * @return Pair in which first element is collection of faulty lines and the second 
   *    is a collection of nonnegative peak sizes.
   */
  def parsePeakSizes(f: ExtantFile): (Seq[String], Seq[Znn]) = {
    Source.fromFile(f.value).getLines.foldLeft((Seq.empty[String], Seq.empty[Znn])){ case(acc, l) => 
      val fields = l.split("\t")
      try { refineV[NonNegative](fields(2).toInt - fields(1).toInt - 1).fold(_ => (acc._1 :+ l, acc._2), z => (acc._1, acc._2 :+ z)) }
      catch { case _ @ (_: ArrayIndexOutOfBoundsException | _: NumberFormatException) => (acc._1 :+ l, acc._2) }
    }
  }

  /**
   * Gather in a single collection the peak sizes for multiple peak call files; good for, e.g. pooling replicates.
   *
   * @param files Collection of paths to peak call files
   * @return Either a {@code Left} containing pairs in which first component is filepath and second is 
   *    collection of problematic lines; or a {@code Right} containing the full collection of peak sizes.
   */
  def collectPeakSizes(files: NEL[ExtantFile]): Either[NEL[(ExtantFile, Seq[String])], NEL[Znn]] = 
    peakSizesByFile(files).map(_.toList.flatMap(_._2).toList.toNel.get)

  /**
   * Gather peak sizes on a per-file basis.
   *
   * @param files Collection of paths to peak call files
   * @return Either a {@code Left} containing pairs in which first component is filepath and second is 
   *    collection of problematic lines; or a {@code Right} containing a collection of pairs in which 
   *    first component is filepath and second is collection of peak sizes.
   */
  def peakSizesByFile(files: NEL[ExtantFile]): Either[NEL[(ExtantFile, Seq[String])], NEL[(ExtantFile, Seq[Znn])]] = {
    val (bads, goods) = files.foldLeft((Seq.empty[(ExtantFile, Seq[String])], Seq.empty[(ExtantFile, Seq[Znn])])){ case(acc, f) => 
      val (badLines, peakSizes) = parsePeakSizes(f)
      badLines.isEmpty.fold((acc._1, acc._2 :+ (f -> peakSizes)), (acc._1 :+ (f -> badLines), acc._2))
    }
    bads.toList.toNel.toLeft(goods.toList.toNel.get)
  }
  
}
