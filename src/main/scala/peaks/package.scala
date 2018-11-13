package omictools

/**
 * Tools and types for working with calls of peaks in sequencing read density.
 *
 * @author Vince Reuter
 */
package object peaks {

  import cats.Show
  import cats.syntax.show._
  import mouse.boolean._
  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric._
  import omictools.Chromosome
  import Chromosome._
  import omictools.generic.{ Znn, Zpos }
  
  /** Peak call type for {@code MACS} / {@code MACS2} */
  sealed trait MacsPeakType {
    /** Get the file extension typically associated with the particular type of peak call. */
    def extension: String = {
      val s = this.toString
      // The MACS extensions are typically camel cased with leading lower.
      Character.toLowerCase(s.charAt(0)) + s.substring(1)
    }
  }
  
  /** {@code narrowPeak}, e.g. for TF ChIP-seq or perhaps ATAC-seq */
  final case object NarrowPeak extends MacsPeakType
  
  /** {@code broadPeak}, e.g. for a dispersed histone PTM, like H3K27me3 */
  final case object BroadPeak extends MacsPeakType
  
  /** Convenience for working with {@code MacsPeakType} */
  object MacsPeakType {
    import cats.Show
    implicit def showMacsPeakType[T <: MacsPeakType]: Show[T] = Show.fromToString[T]
  }

  /* Refined subsets of integers */
  private implicit def showRefZ[P]: Show[Refined[Int, P]] = Show.show(_.value.toString)

  /** Convert genome location data to text, checking that end exceeds start by at least 1. */
  def loc2BedNameSafe(data: (Chromosome, Znn, Zpos), sep: String = "_"): Either[String, String] = { 
    data match { case (c, s, e) => (e.value > s.value).either(
      s"End (${e.value}) not greater than start (${s.value})", List(c.show, s.show, e.show).mkString(sep)) }
  }

  /** Convert genome location data to text, checking that end exceeds start by at least 1. */
  def loc2BedNameUnsafe: Tuple3[Chromosome, Znn, Zpos] => String = 
    loc2BedNameSafe(_).fold(e => throw new IllegalStateException(e), n => n)

  /**
   * Entity capable of producing the data from a {@code T} that {@code HOMER} needs for {@code BED} format.
   *
   * @tparam T Type of value from which the critical data will be obtained.
   * @author Vince Reuter
   */
  trait HomerBedData[-T] {
    import omictools.{ Strand, PlusStrand, MinusStrand }
    import Strand._

    /** Get chromosome */
    def getChr: T => Chromosome
    
    /** Get start coordinate. */
    def getStart: T => Znn
    
    /** Get end coordinate */
    def getEnd: T => Zpos
    
    /** Get record/feature name (4th field) */
    def getName: T => String = t => loc2BedNameUnsafe((getChr(t), getStart(t), getEnd(t)))
    
    /** Get record/feature label (inserted in 5th field unusede by {@code HOMER}) */
    def getLabel: T => String
    
    /** Get strand */
    def getStrand: T => Option[Strand] = _ => None
    
    /** Create {@code BED} line for {@code HOMER}. */
    def makeLine: T => String = t => 
      List(getChr(t).show, getStart(t).show, getStart(t).show, getName(t), getLabel(t), getStrand(t)).mkString("\t")
  
  }

  /** Support easy creation of a {@code BED} line for {@code HOMER}. */
  implicit class HomerBedOps[-T](t: T)(implicit ev: HomerBedData[T]) {
    /** Create {@code BED} line in format used by {@code HOMER}. */
    def makeLine: String = ev.makeLine(t)
  }

  /**
   * Type that attempts to make a {@code BED} line for {@code HOMER} from a {@code T}.
   *
   * @tparam T Type from which to attempt to make line.
   * @author Vince Reuter
   */
  trait HomerBedParse[T] extends Function1[T, Either[(T, String), String]]

  /**
   * Convenience for working in {@code BED} context with {@code HOMER}.
   *
   * @author Vince Reuter
   */
  object HomerBedable {
    import omictools.generic.RefinedBuilder.tryTextIntRefine
    implicit def attemptparseLabeledPeakLocCoded[V : Show]: HomerBedParse[(String, V)] = 
      new HomerBedParse[(String, V)] {
        def apply(peakLabelPair: (String, V)): Either[((String, V), String), String] = {
          val (peak, label) = peakLabelPair
          peak.split("_").toList match {
            case chrText :: startText :: endText :: Nil => (for {
              c <- Chromosome(chrText)
              s <- tryTextIntRefine[NonNegative](startText)
              e <- tryTextIntRefine[Positive](endText)
            } yield List(c.show, s.show, e.show, loc2BedNameUnsafe((c, s, e)), label.show, ".").mkString("\t")).left.map(err => peakLabelPair -> err)
            case badParse => Left(peakLabelPair -> s"Expected 3 fields in alleged peak name but found ${badParse.size} in $peak")
          }
        }
      }
  }

}
