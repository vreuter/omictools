package omictools


/**
 * Working with type-specific files
 *
 * @author Vince Reuter
 */
object TypedFiles {
  
  import java.io.File
  import cats.Eq
  
  /** Entity wrapping a filepath, intended to be extended to represent path to 'particular' type/format of file. */
  trait FileTypeLike { def path: File }
  
  /** Use types and paths on a pair of typed files to test for equality. */
  implicit def eqTypedFiles[F  <: FileTypeLike]: Eq[F] = new Eq[F] {
    import cats.instances.string._
    import cats.syntax.eq._
    def eqv(a: F, b: F): Boolean = a.path.getPath === b.path.getPath
  }

}
