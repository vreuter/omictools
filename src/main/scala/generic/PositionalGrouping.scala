package omictools
package generic


import cats.data.{ NonEmptyList => NEL }
import mouse.boolean._
import eu.timepit.refined
import refined.{ refineV, W }
import refined.api.{ Refined }
import refined.collection._
import refined.numeric._


/**
 * Entity capable of grouping sequence elements according to position.
 *
 * @author Vince Reuter
 */
trait PositionalGrouping {
  /**
   * Attempt to group the given items by position.
   *
   * @tparam A The type of element stored in the given sequence.
   * @param items Collection of items to attempt to group
   */
  def apply[A](items: Seq[A]): Either[String, NEL[NEL[A]]]
}


/**
 * Entity that groups, by position, sequence elements, and enforces strict size equality constraint on any collection to be grouped.
 *
 * @constructor
 * @param size The number of elements any sequence to group must contain exactly.
 * @param indexGroups Groups of indices, each specifying positions of elements to grab from an input sequence in order to constitute a group.
 * @author Vince Reuter
 */
class StrictlySizedPositionalGrouping private(val size: Refined[Int, Positive], val indexGroups: NEL[NEL[Int]]) {
  import cats.instances.int._
  import cats.syntax.eq._
  import StrictlySizedPositionalGrouping._
  
  // Validate indices.
  indexGroups.toList.map(_.toList).flatten.filter(i => i < 0 || i >= size) match {
    case Nil => ()
    case invalid => throw new IllegalArgumentException(s"Invalid index(es) into sequences of size ${size.value}: ${invalid.mkString(", ")}")
  }
  
  // Group elements of a sequence of items.
  def apply[A](items: Seq[A]): Either[String, NEL[NEL[A]]] = 
    (items.size === size).either(s"Expected ${size.value} items but got ${items.size}", indexGroups.map(_.map(i => items(i)))
  )

}


/**
 * Implicits and convenience for working with sized positional groupings.
 *
 * @author Vince Reuter
 */
object StrictlySizedPositionalGrouping {
  import cats.Eq
  import cats.instances.int._
  import cats.instances.list._
  import cats.syntax.eq._
  import cats.syntax.list._

  implicit def sizeToValue: Refined[Int, Positive] => Int = _.value

  /** A pair of positional groupings is equivalent iff each grouping expects the same sized input and groups elements identically. */
  implicit val eqGrouping: Eq[StrictlySizedPositionalGrouping] = new Eq[StrictlySizedPositionalGrouping] {
    def eqv(g1: StrictlySizedPositionalGrouping, g2: StrictlySizedPositionalGrouping): Boolean = 
      (g1.size.value === g2.size.value) && (g1.indexGroups === g2.indexGroups)
  }

  private def validateIndices: (Int, List[Int]) => Either[String, (NEL[Int], NEL[Int])] = (n, indices) => indices.toList.filter(_ >= n) match {
    case Nil => for {
      a <- indices.nonEmpty.either("No indices", indices.toNel.get)
      b <- List.range(0, n).filterNot(i => indices.contains(i)).toNel.fold[Either[String, NEL[Int]]](
        Left(s"No indices left for collection of size $n given indices: ${indices.mkString(", ")}"))(other => Right(other))
    } yield (a, b)
    case bads => Left(s"Invalid indices into collection of size $n: ${bads.mkString(", ")}")
  }

  /**
   * Create a bifurcation-like grouping of a collection of a particular size.
   *
   * @param n The exact size required of each collection to bifurcate.
   * @param aIndices The collection of indices to constitute the first of the 2 groups.
   * @return 
   */
  def bifurcate(n: Int, aIndices: Seq[Int]): Either[String, StrictlySizedPositionalGrouping] = for {
    refSize <- refineV[Positive](n)
    thisAndThat <- validateIndices(n, aIndices.toList)
  } yield new StrictlySizedPositionalGrouping(refSize, NEL(thisAndThat._1, List(thisAndThat._2)))

}
