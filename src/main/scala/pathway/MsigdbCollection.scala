package omictools
package pathway

/**
 * One of the fixed MSigDb categories
 *
 * @author Vince Reuter
 */
sealed trait MsigdbCollection


/**
 * Instances and implicits for MSigDB collection specification
 *
 * @author Vince Reuter
 */
object MsigdbCollection {
  import cats.Show
  
  /** MSigDB hallmark gene sets */
  final case object Hallmark extends MsigdbCollection
  
  /** MSigDB positional gene sets */
  final case object C1 extends MsigdbCollection
  
  /** MSigDB curated gene sets */
  final case object C2 extends MsigdbCollection
  
  /** MSigDB motif gene sets */
  final case object C3 extends MsigdbCollection
  
  /** MSigDB computational gene sets */
  final case object C4 extends MsigdbCollection
  
  /** MSigDB GO gene sets */
  final case object C5 extends MsigdbCollection
  
  /** MSigDB oncogenic gene sets */
  final case object C6 extends MsigdbCollection
  
  /** MSigDB immunologic gene sets */
  final case object C7 extends MsigdbCollection

  /** Generally, {@code .show} a collection by its name, but abbreview hallmark differently; behavior accords with MSigDB site. */
  implicit val showCollection: Show[MsigdbCollection] = new Show[MsigdbCollection] {
    def show(c: MsigdbCollection): String = c match {
      case Hallmark => "H"
      case _ => c.toString
    }
  }
}
