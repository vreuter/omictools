package omictools
package motif

import eu.timepit.refined.refineV
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive


/**
 * Specification of argument to {@code -size} option in {@code HOMER} programs.
 *
 * @author Vince Reuter
 */
trait HomerSizeSpec


/**
 * Specification of a fixed region size for a {@code HOMER} program.
 *
 * @author Vince Reuter
 */
case class FixedRegionSize(get: Refined[Int, Positive]) extends HomerSizeSpec {
  def value: Int = get.value
}


/**
 * Variable sized region specification for {@code HOMER}.
 *
 * @author Vince Reuter
 */
case object VariableRegionSize extends HomerSizeSpec


/**
 * Convenience for working with {@code HOMER} size specifications.
 *
 * @author Vince Reuter
 */
object HomerSizeSpec {
  import cats.Show
  /** Handle, e.g., CLI opt argument specification for a size specification provided. */
  implicit val showSizeSpec: Show[HomerSizeSpec] = new Show[HomerSizeSpec] {
    def show(s: HomerSizeSpec): String = s match {
      case VariableRegionSize => "given"
      case s: FixedRegionSize => s.value.toString
    }
  }
  /** Attempt to create a validated ({@code Refined}) size specification from the given value. */
  def apply(s: Int): Either[String, FixedRegionSize] = refineV[Positive](s).map(ref => FixedRegionSize(ref))
}
