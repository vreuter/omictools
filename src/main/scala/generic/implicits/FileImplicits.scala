package omictools
package generic
package implicits


/**
 * Generic implicits.
 *
 * @author Vince Reuter
 */
object FileImplicits {
  
  import java.io.File
  import cats.Eq
  import cats.instances.string._
  import cats.syntax.eq._

  /** Regard two files as equivalent if and only if their paths are equivalent. */
  implicit val fileEq: Eq[File] = new Eq[File] { def eqv(a: File, b: File): Boolean = a.getPath === b.getPath }

}
