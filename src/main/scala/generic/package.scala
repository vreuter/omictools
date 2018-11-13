package omictools

/**
 * Tools and types for working with (relatively) generic values.
 *
 * @author Vince Reuter
 */
package object generic {
  
  import cats.Show
  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric._

  /* Integral type alii */  
  private[omictools] type ZRef[P] = Refined[Int, P]

  /** Positive integer */
  protected[omictools] type Zpos = Refined[Int, Positive]
  object Zpos {
    def apply(x: Int): Either[String, Zpos] = ZRef.apply[Positive](x)
  }

  /** Nonnegative integer */ 
  protected[omictools] type Znn = Refined[Int, NonNegative]
  object Znn {
    def apply(x: Int): Either[String, Znn] = ZRef.apply[NonNegative](x)
  }

  /** Working with refined integers */
  object ZRef {
    
    import refined.refineV
    import refined.api.Validate
    
    /** Get an integer refinement type builder for the implied validation. */
    implicit def getBuilder[P](implicit ev: Validate[Int, P]): RefinedBuilder[Int, P] = 
      RefinedBuilder.apply[Int, P]
    
    /** Attempt to validate a given integer as in compliance with the implied validation scheme. */
    def apply[P](x: Int)(implicit ev: Validate[Int, P]): Either[String, ZRef[P]] = 
      getBuilder[P](ev).buildSafe(x)
  }

}
