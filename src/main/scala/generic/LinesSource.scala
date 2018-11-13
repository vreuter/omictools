package omictools
package generic


/**
 * A source of lines of text.
 *
 * @tparam S The underlying type that supports the {@code Iterator[String]}-like operations.
 * @author Vince Reuter
 */
trait LinesSource[-S] {
  /** Determine whether the source is exhausted. */
  def hasNext(s: S): Boolean
  /** Fetch the next text element available. */
  def next(s: S): String
}


/**
 * Implicits for working with source of lines to text.
 *
 * @author Vince Reuter
 */
object LinesSource {
  import scala.io.Source
  import scala.language.implicitConversions
  import java.io.File
  import cats.syntax.eq._
  import implicits.FileImplicits._
  
  /** A true {@code Iterator[String]} provides a natural view as a source of lines. */
  implicit object LinesFromIterator extends LinesSource[Iterator[String]] {
    def hasNext(s: Iterator[String]): Boolean = s.hasNext
    def next(s: Iterator[String]): String = s.next()
  }
  
  /** Enable automatic interpretation of a file as supportive of the lines source operations. */
  implicit def fromFile: File => LinesSource[File] = original => {
    require(original.isFile, s"Alleged lines source isn't a file: ${original.getPath}")
    new LinesSource[File] {
      private[this] val origFile = original
      private[this] val lines = Source.fromFile(original).getLines
      private[this] def validate(f: File): Unit = 
        if (f =!= origFile) throw new Exception(s"Operation on different file: ${f.getPath} differs from ${origFile}")
      def hasNext(f: File): Boolean = { validate(f); lines.hasNext }
      def next(f: File): String = { validate(f); lines.next() }
    }
  }

  /** When a {@code LinesSource} instance is in scope for {@code S}, provide syntactic convenience. */
  implicit class LinesSourceOps[-S](s: S)(implicit ev: LinesSource[S]) {
    def hasNext: Boolean = ev.hasNext(s)
    def next(): String = ev.next(s)
  }

}
