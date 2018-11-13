package omictools
package motif


/**
 * Data instance sufficient for provision of a query line to a {@code HOMER} program.
 *
 * @tparam D The type of instance that admits the data required to constitute a fully formed query record.
 * @author Vince Reuter
 */
trait HomerQueryLike[-D] {
  /** Create a query record line from a given data instance. */
  def makeLine(d: D): String
}
