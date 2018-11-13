package omictools


/**
 * Extensible representation of a chromosome, requiring only a validated name.
 *
 * @constructor
 * @param name The validated chromosome name
 * @author Vince Reuter
 */
case class Chromosome(name: Chromosome.ChrName) {
  override def toString: String = name.value
}


/**
 * Implicits, types, and convenience functions for working with chromosomes.
 *
 * @author Vince Reuter
 */
object Chromosome {
  import cats.{ Order, Show }
  import cats.instances.order._         // For Contravariant[Order[_]]
  import cats.syntax.contravariant._    // For .contramap
  import eu.timepit.refined
  import refined.{ refineV, W }
  import refined.api.Refined
  import refined.string.StartsWith
  
  /** Validated chromosome name, bearing the restricted prefix. */
  type ChrName = Refined[String, StartsWith[W.`"chr"`.T]]

  /** Tools for working with validated chromosome name. */
  object ChrName {
    import cats.instances.string._

    /** Attempt creation of a validated chromosome name from raw text. */
    def apply(n: String): Either[String, ChrName] = refineV[StartsWith[W.`"chr"`.T]](n)

    /** Show a chromosome name by accessing the value. */
    implicit val showChrName: Show[ChrName] = new Show[ChrName] {
      def show(n: ChrName): String = n.value
    }
    
    /** Order chromosome names by first respecting numeric order, then respecting lexicographical order. */
    implicit val ordChrName: Order[ChrName] = new Order[ChrName] {
      def compare(n1: ChrName, n2: ChrName): Int = (extractNumeric(n1), extractNumeric(n2)) match {
        case (Right(x1), Right(x2)) => x1 - x2
        case (Left(a), Left(b)) => implicitly[Order[String]].compare(a, b)
        case (Right(_), Left(_)) => -1
        case (Left(_), Right(_)) => 1
      }
    }

  }
  
  /** Attempt to create a chromosome from raw text, which must bear the 'chr' prefix in order to be considered valid. */
  def apply(chr: String): Either[String, Chromosome] = ChrName(chr).map(n => Chromosome(n))
  
  /** Extract from a validated chromosome name the raw text value. */
  implicit def chrNameValue: ChrName => String = _.value
  
  /** Order chromosomes according to name. */
  implicit val orderChromosome: Order[Chromosome] = ChrName.ordChrName.contramap(_.name)

  /** Display a chromosome by showing its name. */
  implicit val showChromosome: Show[Chromosome] = Show.fromToString[Chromosome]
  
  // Attempt to parse the numeric value associated with a chromosome, as embedded in its name, e.g. 22 from "chr22"
  private[this] def extractNumeric: ChrName => Either[String, Int] = n => {
    val raw = n.value.stripPrefix("chr"); try { Right(raw.toInt) } catch { case _: NumberFormatException => Left(raw) }
  }
}
