package omictools
package pathway


/**
 * Tools for analyzing gene clusters vis-a-vis enrichment/depletion of gene sets.
 *
 * @author Vince Reuter
 */
object GeneSetAnalysisR {
  //import org.ddahl.rscala.RClient
  /*
  implicit def toValue: Refined[Int, NonNegative] => Int = _.value

  /**
   * 
   */
  sealed trait GeneSymbol[T] { def symbol: T }
  final case class EntrezId(get: Refined[Int, NonNegative]) extends GeneSymbol[Int] {
    def symbol: Int = get.value
  }
  object EntrezId {
    def apply(id: Int): Either[String, EntrezId] = refineV[NonNegative](id).map(i => new EntrezId(i))
  }
  final case class EnsemblId(get: String) extends GeneSymbol[String] {
    def symbol: String = get
  }

  // TODO: seal the types of conversions, and parameterize conversion in terms of types (pairs of types?) of symbols

  def ensembleToEntrez(rc: RClient, genes: Iterable[String]): Seq[Int] {

    val converter = rc.evalObject()
  }
  */

}
