package omictools
package generic

import eu.timepit.refined.refineV
import eu.timepit.refined.api.{ Refined, Validate }


/**
 * An entity capable of refining instances of a particular type according to a predicate.
 *
 * @tparam P The predicate with which to attempt refinement of any given {@code I}.
 * @param ev How to validate that an {@code I} complies with predicate {@code P}.
 * @author Vince Reuter
 */
class RefinedBuilder[I, P](implicit ev: Validate[I, P]) extends SafeUnsafeBuilder[I, String, Refined[I, P]] {
  def buildSafe(i: I): Either[String, Refined[I, P]] = refineV[P](i)
  protected def buildError(msg: String): Exception = new Exception(msg)
}


/**
 * Convenience functionality for working with {@code RefinedBuilder}.
 *
 * @author Vince Reuter
 */
object RefinedBuilder {
  
  /** Create a new {@code RefinedBuilder} that accepts input of type {@code I}, and construction attempt is based on predicate {@code P}. */
  def apply[I, P](implicit ev: Validate[I, P]): RefinedBuilder[I, P] = new RefinedBuilder[I, P]
  
  /**
   * Attempt to parse text as integer and refine it according to predicate {@code P}.
   *
   * @tparam P Predicate to use for validation
   * @param s The string of text to attempt to parse as {@code Int} and validate.
   * @return Either a {@code Left} containing an error message or a {@code Right} containing the refined integer.
   */
  def tryTextIntRefine[P](s: String)(implicit ev: Validate[Int, P]): Either[String, Refined[Int, P]] =  
    try { refineV[P](s.toInt) } catch { case e: NumberFormatException => Left(e.getMessage) }

}
