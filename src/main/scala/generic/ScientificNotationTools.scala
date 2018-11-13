package omictools
package generic


object ScientificNotationTools {

  import eu.timepit.refined
  import refined.{ refineV, W }
  import refined.api.Refined
  import refined.numeric.Interval
  
  /** For valid scientific notation, the base should be in [1, 10). */
  private type SciBaseRange = Interval.ClosedOpen[W.`1.0`.T, W.`10.0`.T]
  type ScientificNotationBase = Refined[Double, SciBaseRange]
  object ScientificNotationBase extends RefinedBuilder[Double, SciBaseRange]

  /**
   * Number in scientific notation.
   *
   * @constructor
   * @param base The base value being raised to a power
   * @param exponent The power to which to raise the base
   * @author Vince Reuter
   */
  case class ScientificNumber(base: ScientificNotationBase, exponent: Int) {
    def text: String = s"${base}e${exponent}"
  }

  /**
   * Implicits for working with scientific (notation) numbers.
   *
   * @author Vince Reuter
   */
  object ScientificNumber {
    import cats.{ Order, Show }
    // TODO: implement
    implicit val ordSciNum: Order[ScientificNumber] = ???
    implicit val showSciNum: Show[ScientificNumber] = Show.show(_.text)
  }

}
