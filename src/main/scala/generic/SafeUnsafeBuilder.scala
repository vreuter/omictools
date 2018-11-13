package omictools
package generic


import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined, Validate }


/**
 * An entity offering a safe "construction" operation and--by implementation of an error construction function--an unsafe version.
 *
 * @tparam I The type of argument with which to attempt to build a {@code R}
 * @tparam L The type of object generated when the attempt to build a {@code R} from an {@code I} fails.
 * @tparam R The type of object to attempt to build from an {@code I}.
 * @author Vince Reuter
 */
trait SafeUnsafeBuilder[I, L, +R] {
  /** How to attempt construction of a {@code R} from an {@code I}. */
  def buildSafe(i: I): Either[L, R]
  /** From the {@code Left} side of a failed attempt to build a {@code R} from an {@code I}, create an error. */
  protected def buildError(l: L): Throwable
  /** Attempt to build an {@code R} from an {@code I}, throwing an exception if construction fails.  */
  def buildUnsafe(i: I): R = buildSafe(i).fold(l => throw buildError(l), r => r)
}
