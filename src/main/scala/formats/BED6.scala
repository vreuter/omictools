package omictools
package formats


import eu.timepit.refined
import refined.{ refineV, W }
import refined.api.Refined
import refined.numeric.Interval


/**
 * Behavior to extract from a {@code T} the data to constitute at least a {@code BED6} format record
 *
 * @tparam R The type of entity from which the {@code BED6} data may be readily extracted by this behavior set.
 * @author Vince Reuter
 */
trait BED6Data[-R] extends BED3Data[R] {
  import BED6Data._
  /** Extract or otherwise provide name for a region. */
  def name(r: R): String
  /** Extract or otherwise provide score for a region, e.g. for track shading. */
  def score(r: R): ScoreRange
  /** Extract or otherwise provide strand for a region. */
  def strand(r: R): Strand
}


/**
 * Types and helpers for working with {@code BED6}-like data.
 *
 * @author Vince Reuter
 */
object BED6Data {
  /** The valid {@code BED} score range. */
  type ScoreRange = Interval.Closed[W.`0`.T, W.`1000`.T]
  /** Integral type restricted to the {@code BED} format score range. */
  type BEDScore = Refined[Int, ScoreRange]
  /** Conveniences for working with {@code BED} scores. */
  object BEDScore {
    import refined.refineMV
    def empty: BEDScore = refineMV[ScoreRange](0)
  }
}
