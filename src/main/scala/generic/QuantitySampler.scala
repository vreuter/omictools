package omictools
package generic


/**
 * Tools related, broadly, to working with quantities.
 *
 * @author Vince Reuter
 */
object QuantityTools {
  import cats.Order
  import cats.syntax.order._
  import eu.timepit.refined
  import refined.refineV
  import refined.api.{ Refined, Validate }
  import refined.numeric.{ NonNegative, Positive }

  /** Dummy case class with which to represent a floating-point value successfully validated as in (0, 1.0] */
  protected[omictools] case class ValidatedProportion()
  // Put in scope that Validate instance for a proportion, so that calls like refineV[ValidatedProportion] can work
  implicit val samplePropValidate: Validate.Plain[Double, ValidatedProportion] = 
    Validate.fromPredicate(p => p > 0 && p <= 1, p => s"($p is in (0, 1])", ValidatedProportion())

  /** Alias for type of value in (0, 1], representing a nontrivial proprtion of some quantity. */
  type GoodProp = Refined[Double, ValidatedProportion]
  /** Syntactic convenience for working with refined proportion types */
  object GoodProp {
    def apply(p: Double): GoodProp = refineV[ValidatedProportion](p).fold(msg => throw new Exception(msg), refP => refP)
  }
  
  /** Alias for result of attempt to refine an integral value as nonnegative. */
  type SampleResultOr[E] = Either[E, Refined[Int, NonNegative]]

  /** Automatic access to a value refined as a valid proportion. */
  implicit def toRawProp: GoodProp => Double = _.value

  /** Automatic access to value refined as a positive integer. */
  implicit def getPositiveInt: Refined[Int, Positive] => Int = _.value

  /** Entity capable of comparing an arbitrary {@code T} to bound {@code b}. */
  sealed abstract class UpperBoundCheck[T : Order](b: T) { def isValid(t: T): Boolean }
  
  /** Entity capable of checking whether an arbitrary {@code T} is strictly less than upper bound {@code b}. */
  case class UpperBoundExcl[T : Order](get: T) extends UpperBoundCheck[T](get) { def isValid(t: T): Boolean = (t compare get) < 0 }
  
  /** Entity capable of checking whether an arbitrary {@code T} is loosely less than upper bound {@code b}. */
  case class UpperBoundIncl[T : Order](get: T) extends UpperBoundCheck[T](get) { def isValid(t: T): Boolean = (t compare get) <= 0 }

  /** Entity representing successful validation of some {@code T} as complicant with (upper) {@code bound}. */
  case class ValidatedFiniteNonnegative[B[_] <: UpperBoundCheck[_], T](bound: B[T])

  /** Entity that takes some proportion of a positive quantity. */
  sealed trait QuantitySampler extends SafeUnsafeBuilder[Refined[Int, Positive], String, Refined[Int, NonNegative]] {
    final def takeSafe(x: Refined[Int, Positive]): SampleResultOr[String] = buildSafe(x)
    final def takeUnsafe(x: Refined[Int, Positive]): Refined[Int, NonNegative] = buildUnsafe(x)
  }

  /** Entity that takes some proportion of a positive quantity by storing the fixed proportion, which may be construed as a sort of "sampling rate". */
  case class TakeProp(prop: GoodProp)(implicit bias: Bias) extends QuantitySampler {
    /** Implementation of the core sampling notion/operation, returning from the argument the instance's fixed proportion of the quantity. */
    def buildSafe(x: Refined[Int, Positive]): SampleResultOr[String] = {
      val rawRes = (prop * x)
      val rounded = bias match {
        case BiasLow => rawRes.toInt         // Here, simple truncation is fine since the input is refined tom be positive.
        case BiasHigh => (rawRes + { if (rawRes.toInt == Int.MaxValue) 0 else 1 }).toInt
      }
      refineV[NonNegative](rounded)
    }
    /** Construct an exception from a message. */
    protected def buildError(msg: String): Exception = new Exception(msg)
  }

  /** Direction to bias conversion of decimals to integers */
  sealed trait Bias
  /** Favor downward conversion of decimals to integers */
  final case object BiasLow extends Bias
  /** Favor upward conversion of decimals to integers */
  final case object BiasHigh extends Bias

}

