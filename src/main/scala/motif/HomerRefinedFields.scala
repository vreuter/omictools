package omictools
package motif


import eu.timepit.refined
import refined.api.{ Refined, Validate }
import omictools.generic.RefinedBuilder


/**
 * Collection of text fields valid as {@code HOMER} input data format.
 *
 * @tparam P The preficate with which to ensure that the text fields collection complies.
 * @author Vince Reuter
 */
sealed class HomerRefinedFields[P](implicit ev: Validate[Seq[String], P]) extends HomerQueryLike[Refined[Seq[String], P]] {
  import HomerRefinedFields._
  def makeLine(fields: Refined[Seq[String], P]): String = fields2String(fields)
  def makeLineUnsafe(fields: Seq[String]): String = makeLine(fieldsBuilder.buildUnsafe(fields))
  private lazy val fieldsBuilder: RefinedBuilder[Seq[String], P] = RefinedBuilder.apply[Seq[String], P]
}

/**
 * Conveniences for working with objects representing text sequences intended for use as {@code HOMER}-accepted format fields.
 *
 * @author Vince Reuter
 */
object HomerRefinedFields {
  import eu.timepit.refined
  import refined.W
  import refined.collection.Size
  import refined.numeric.Greater
  
  /* Collection of at least 5 elements */
  type Min5Fields = Size[Greater[W.`4`.T]]
  /* Collection of at least 6 elements */
  type Min6Fields = Size[Greater[W.`5`.T]]

  /**
   * {@code HOMER} accepts "HOMER peak" files [[http://homer.ucsd.edu/homer/ngs/annotation.html descibed here]] in the '''Acceptable Input Files''' section.
   *
   * @author Vince Reuter
   */
  implicit object HomerPeakData extends HomerRefinedFields[Min5Fields]
  
  /**
   * {@code HOMER} accepts {@code BED6} files [[http://homer.ucsd.edu/homer/ngs/annotation.html as descibed here]] in the '''Acceptable Input Files''' section.
   *
   * @author Vince Reuter
   */
  implicit object HomerBedData extends HomerRefinedFields[Min6Fields]
  
  protected def fields2String: Refined[Seq[String], _] => String = _.value.mkString("\t")

}
