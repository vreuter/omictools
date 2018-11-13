package omictools
package generic
package implicits


/**
 * Implicits for convenience working with {@code Refined} types.
 *
 * @author Vince Reuter
 */
object RefinedImplicits {

  import cats.{ Eq, Order, Show }
  import cats.instances.int._
  import cats.syntax.order._
  import eu.timepit.refined
  import refined.refineV
  import refined.api.Refined
  import refined.numeric.{ NonNegative, Positive }
  import omictools.generic.{ Znn, Zpos, ZRef }

  // Positive ints constitute subset of nonnegative ints.
  implicit def toNonNegative: Zpos => Znn = z => refineV[NonNegative](z.value).fold(
    e => throw new Exception(s"Positive-to-nonnegative conversion failed (${z.value}) -- $e"), nn => nn)

  // Order refined values by raw value.
  implicit def ordRefInf[P]: Order[ZRef[P]] = Order.by(_.value)

  // Derive Eq from Order.
  implicit def eqRefInt[P]: Eq[Refined[Int, P]] = new Eq[ZRef[P]] { def eqv(a: ZRef[P], b: ZRef[P]): Boolean = 0 === (a compare b) }

  /** "Widen" a refined integer to an ordinary integer. */
  implicit def widenRefInt[P]: ZRef[P] => Int = _.value

  /** For an arbitrary integral refinement type, provide implicit {@code .show} behavior. */
  implicit def showRefInt[P]: Show[ZRef[P]] = Show.show(_.value.toString)

}
