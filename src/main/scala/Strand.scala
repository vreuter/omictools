package omictools


/**
 * Genomic reference sequence strand.
 *
 * @author Vince Reuter
 */
sealed trait Strand

/**
 * The forward/plus reference strand.
 *
 * @author Vince Reuter
 */
final case object PlusStrand extends Strand {
  override def toString: String = "+"
}

/**
 * The bottom/minus reference strand.
 *
 * @author Vince Reuter
 */
final case object MinusStrand extends Strand {
  override def toString: String = "-"
}


/**
 * Conveniences for working with {@code Strand}.
 *
 * @author Vince Reuter
 */
object Strand {

  import cats.Show
  import cats.syntax.show._

  /** Attempt {@code Strand} parse from given text. */  
  def apply(s: String): Either[String, Strand] = s match {
    case "+" => Right(PlusStrand)
    case "-" => Right(MinusStrand)
    case _ => Left(s)
  }
  
  /** Implicit {@code Show[Strand]} */
  implicit val showStrand: Show[Strand] = Show.fromToString[Strand]
  
  /** Use {@code .} to {@code .show} an empty {@code Option[Strand]}; otherwise, use the value's {@code Show}. */
  implicit val showMaybeStrand: Show[Option[Strand]] = Show.show(_.fold(".")(_.show))

}
