package omictools
package formats


import cats.syntax.show._
import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import refined.numeric.{ NonNegative, Positive }
import omictools.Chromosome
import omictools.generic._
import omictools.generic.implicits.RefinedImplicits._


/**
 * Entity that can extract at least the minimal {@code BED3} data, which also implies interval/region size.
 *
 * @tparam R The type of entity from which the {@code BED3} data may be extracted according to this behavior set.
 * @author Vince Reuter
 */
trait BED3Data[-R] {
  /** Determine the entity's associated {@code Chromosome}. */
  def chr(r: R): Chromosome
  /** Determine the entity's genomic start coordinate. */
  def start(r: R): Znn
  /** Determine the entity's genomic end coordinate. */
  def end(r: R): Zpos
  /** Determine the region's size in number of bases of sequence. */
  final def size(r: R): Zpos = (start(r), end(r)) match { case (s, e) => 
    refineV[Positive](e - s).getOrElse(
      throw new IllegalStateException(s"Invalid region coordinates: (${s.show}, ${e.show})"))
  }
}


/**
 * Basic container for the core {@code BED3} data.
 *
 * @constructor
 * @param chr Chromosome
 * @param start Start coordinate, 0-based inclusive in accordance with {@code BED}.
 * @param end End coordinate, 0-based exclusive in accordance with {@code BED}.
 * @author Vince Reuter
 */
class BED3 private(chr: Chromosome, start: Znn, end: Zpos)


/**
 * Implicits and syntactic convenience for working with genomic regions represented in {@code BED3}-like form.
 *
 * @author Vince Reuter
 */
object BED3 {
  import mouse.boolean._
  import BED6Data.BEDScore
  import Chromosome._
  import Strand._

  /** Unpacking to attain {@code case class} behavior on {@code match} syntax. */
  def unapply(r: BED3): Option[(Chromosome, Znn, Zpos)] = Some((r.chr, r.start, r.end))

  /** The result of an attempt in which failure is possible to create a new {@code BED3} instance, providing failure reason in a {@code Left}. */
  type TryBed = Either[String, BED3]

  /* Convenience constructors */
  def apply(chr: String, start: Int, end: Int): TryBed = for {
    c <- Chromosome(chr)
    s <- refineV[NonNegative](start)
    e <- refineV[Positive](end)
    _ <- (s.value <= e.value).either(s"Start past end: ${s.value} > ${e.value}", ())
    r = new BED3(c, s, e)
  } yield r

  /** Create a new {@code BED3} instance according to given chromosome, start position, and region/interval size. */
  def apply(chr: Chromosome, start: Znn, size: Znn): BED3 = new BED3(chr, start, unsafeGetEnd(start, size))

  /** When an appropriate typeclass instance for a {@code R} is available, facilitate access to the relevant {@code BED3} data. */
  implicit class BED3Ops[-R](r: R)(implicit ev: BED3Data[R]) {
    def chr: Chromosome = ev.chr(r)
    def start: Znn = ev.start(r)
    def end: Zpos = ev.end(r)
    def size: Znn = ev.size(r)
  }
  
  /** From the basic {@code BED3} case class representation, provide a more general {@code BED3} data instance.*/
  implicit val asData: BED3Data[BED3] = new BED3Data[BED3] {
    def chr(r: BED3): Chromosome = r.chr
    def start(r: BED3): Znn = r.start
    def end(r: BED3): Zpos = r.end
  }

  // From a region's start coordinate and its size/length, deduce its end coordinate.
  private[omictools] def unsafeGetEnd(start: Znn, size: Znn): Zpos = 
    refineV[Positive](start.value + size.value + 1).fold(
      msg => throw new IllegalStateException(s"Genome region with non-positive end coordinate (start=${start.value}, size=${size.value})"), x => x)

  /**
   * Lift {@code BED3} data up into {@code BED6}-like fields space, by providing hooks for provision of the additional fields, but using dummy values if no legitimate ones are provided.
   *
   * @param name Name for the region; if unspecified, this will be created from the {@code BED3} positional data, as {@code <chr>_<start>_<end>}.
   * @param score Score for the interval/region, e.g. for use in track-based visualization.
   * @param strand Optionally, a specification of which reference strand the feature is on. This could be useful for strand-specific analyses>
   * @return A sequence of fields that constitute the data with which to write a {@code BED6}-formatted line.
   */
  def toBed6Fields(name: String = "", score: BEDScore = BEDScore.empty, strand: Option[Strand] = None): BED3 => Seq[String] = r => {
    val chr = r.chr.show
    Seq(chr, r.start.value.toString, r.end.value.toString, s"${chr}_${r.start.value}_${r.end.value}", score.value.toString, strand.fold(".")(_.show))
  }

}
