package omictools
package formats

import cats.syntax.show._
import omictools.generic.{ SafeUnsafeBuilder, Znn, Zpos }
import omictools.generic.implicits.RefinedImplicits._
import omictools.Strand
import omictools.Chromosome._


/**
 * Typeclass for obtaining {@code SAF} record data from an instance of a particular type.
 *
 * @tparam R The type of which an instance will provide the necessary data to constitute a {@code SAF} record
 * @author Vince Reuter
 */
trait SAFLike[-R] extends BED3Data[R] {
  /** Obtain name (first field) for a record */
  def name(r: R): String
  /** Obtain chromosome for a record. */
  def chr(r: R): Chromosome
  /** Obtain region start coordinate for a record. */
  def start(r: R): Znn
  /** Obtain region end coordinate for a record. */
  def end(r: R): Zpos
  /** Obtain, if available, region's declared strand/orientation. */
  def strand(r: R): Option[Strand]
}


/**
 * Implicits and convenience functions for working in {@code SAF} context.
 *
 * @author Vince Reuter
 */
object SAFLike {

  /**
   * Derive a {@code SAFLike} instance from a {@code BED3Data} instance for an arbitrary type.
   *
   * @tparam R The type of value for which a {@code BED3Data} instance is available in scope.
   * @param ev The {@code BED3Data[R]} instance available in scope.
   * @retunr A {@code SAFLike[R]} instance
   */
  def from[R](implicit ev: BED3Data[R]): SAFLike[R] = new SAFLike[R] {
    def name(r: R): String = List(chr(r).show, start(r).show, end(r).show).mkString("_")
    def chr(r: R): Chromosome = ev.chr(r)
    def start(r: R): Znn = ev.start(r)
    def end(r: R): Zpos = ev.end(r)
    def strand(r: R): Option[Strand] = None
  }

}


/**
 * A type in which instances contain the fields that consitute a {@code SAF} record.
 *
 * @constructor
 * @param name The record's name
 * @param chr The record's chromosome
 * @param start The record's region's genomic start coordinate, inclusive
 * @param end The record's region's genomic end coordinate, exclusive
 * @param strand The record's region's declared strand, optionally
 * @author Vince Reuter
 */
case class SAFRecord private(name: String, chr: Chromosome, start: Znn, end: Zpos, strand: Option[Strand])


/**
 * Syntactic convenience for working with {@code SAFRecord}.
 *
 * @author Vince Reuter
 */
object SAFRecord extends SafeUnsafeBuilder[Tuple5[String, Chromosome, Znn, Zpos, Option[Strand]], String, SAFRecord] {
  import mouse.boolean._
  
  /** Attempt creation of a {@code SAFRecord}, validating the region coordindates with respect to one another. */
  def buildSafe(data: Tuple5[String, Chromosome, Znn, Zpos, Option[Strand]]): Either[String, SAFRecord] = 
    data match { case (n, c, s, e, strand) => (s < e).either(
      s"Start coordinate (${s.show}) not less than end coordinate (${e.show})", new SAFRecord(n, c, s, e, strand) ) }
  
  protected def buildError(msg: String): Throwable = new Throwable(msg)
  
  /** {@code SAFLike} from {@code SAFRecord} */
  implicit object RecordSAFLike extends SAFLike[SAFRecord] {
    def name(r: SAFRecord): String = r.name
    def chr(r: SAFRecord): Chromosome = r.chr
    def start(r: SAFRecord): Znn = r.start
    def end(r: SAFRecord): Zpos = r.end
    def strand(r: SAFRecord): Option[Strand] = r.strand
  }
}
