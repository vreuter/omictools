package omictools
package formats


import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import omictools.generic.SafeUnsafeBuilder


/**
 * Minimal entity capable of constituting a record from a {@code HOMER} motif file.
 *
 * @author Vince Reuter
 */
trait MinHomerMotifLike {
  /** The actual motif sequence */
  def sequence: String
  /** An (perhaps arbitrary) name for the motif; this can be empty. */
  def name: String
  /** log */
  def logOdds: Double
}


/**
 * Convenience functionality for working with minimal constitutition of a {@code HOMER} motif file record.
 *
 * @author Vince Reuter
 */
object MinHomerMotifLike extends {
  def fromLine: String => MinHomerMotifLike = ???
}
