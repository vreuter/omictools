package omictools
package pathway


/** Type of gene format used to identify each gene in a dataset */
sealed trait GeneFormatType

/**
 * Tools for working with specification of gene format, in accordance with {@code AnnotationDbi} R package's {@code keytypes}
 *
 * @author Vince Reuter
 */
object GeneFormatType {
  import cats.Show
  import cats.Eq
  import cats.syntax.show._

  /* Oft-used GeneFormatType instances */
  final case object ENTREZID extends GeneFormatType
  final case object HGNC extends GeneFormatType
  final case object HUGO extends GeneFormatType
  
  /** Generally, show gene format simply by its name, but account for corner cases. */
  implicit val showGeneFormatType: Show[GeneFormatType] = new Show[GeneFormatType] {
    def show(fmt: GeneFormatType): String = fmt match {
      case (HGNC | HUGO) => "SYMBOL"
      case _ => fmt.toString
    }
  }
  
  /** In the spririt of usage model (string passed to R), two gene format types are equivalent if and only if they display the same way. */
  implicit val eqGeneFormatType: Eq[GeneFormatType] = new Eq[GeneFormatType] {
    def eqv(a: GeneFormatType, b: GeneFormatType): Boolean = a.show == b.show
  }
}
