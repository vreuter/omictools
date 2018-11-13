package omictools
package pathway


/**
 * Tools for defining parameters for running gene set enrichment analysis in {@code R} with package {@code SetRank}.
 *
 * @author Vince Reuter
 */
object SetRankParamTools {

  import java.io.File
  import cats.data.{ NonEmptyList => NEL }
  import mouse.boolean._
  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric.Positive
  import omictools.ClustergramPartitioner

  private[this] type PosInt = Refined[Int, Positive]

  /** Environment variable pointing to where code is stored, used to find the enrichment program path. */
  val defaultCodeVarname = "CODE"

  /**
   * Collection of parameters used to run the gene set enrichment analysis program with {@code SetRank}.
   *
   * @author Vince Reuter
   */
  trait GeneSetRankParams {
    /** Number of CPUs to allocation, used to set {@code R} option {@code mc.cores}. */
    def cores: PosInt
    /** How to handle the case in which the path to the enrichment program isn't accessible as an executable file. */
    def handleMissingProgram: File => Unit = p => throw new Exception(s"Missing program: ${p.getPath}")
    /** How to infer from gene group/cluster label the name for the file in which to list the identifiers of the constituent genes. */
    def labelToFilename: String => String
    /** Name of the environment variable on the execution machine(s) where code repositories are stored. */
    def codeHome: String = defaultCodeVarname
    /** Name for the "manifest" file, specifying for each gene query group the path to the file that lists gene identifiers. */
    def manifestFilename: String = "gene_sets.txt"
    /** Whether to--in outputs--preserve the input gene format type rather than converge on Entrez ID. */
    def keepGeneFormat: Boolean
  }

  /** Enrichment program parameterization that preserves gene ID format used for input. */
  case class FormatPreservingSetRankParams(
    cores: PosInt, msigdbCollection: Option[MsigdbCollection], 
    override val handleMissingProgram: File => Unit, labelToFilename: String => String, 
    override val codeHome: String = defaultCodeVarname) extends GeneSetRankParams {
    def keepGeneFormat: Boolean = true
  }

  /** Enrichment program parameterization that leaves gene IDs converted to Entrez ID. */
  case class EntrezSetRankParams(
    cores: PosInt, msigdbCollection: Option[MsigdbCollection], 
    override  val handleMissingProgram: File => Unit, labelToFilename: String => String, 
    override val codeHome: String = defaultCodeVarname) extends GeneSetRankParams {
    def keepGeneFormat: Boolean = false
  }

  /** Convenience for working with enrichment program parameterization. */
  object GeneSetRankParams {
    def apply(cores: PosInt, msigdbCollection: Option[MsigdbCollection], 
      handleMissingProgram: File => Unit, labelToFilename: String => String, codeHome: String, keepGeneFormat: Boolean): GeneSetRankParams = 
      keepGeneFormat.fold(
        new FormatPreservingSetRankParams(cores, msigdbCollection, handleMissingProgram, labelToFilename, codeHome), 
        new EntrezSetRankParams(cores, msigdbCollection, handleMissingProgram, labelToFilename, codeHome))
    private implicit def lift: MsigdbCollection => Option[MsigdbCollection] = Some(_)
  }

  /**
   * Entity specifying creation of commands for gene set enrichment analysis with {@code SetRank} over multiple MSigDB collections.
   *
   * @tparam T Type of specification of resources and collections
   * @author Vince Reuter
   */
  trait MultiMsigdbCollectionSpec[-T] { def enumerate: T => MsigdbCollectionSpec.ResourceSpecs }

  /**
   * Implicit instances of multi-collection specification of {@code SetRank} parameters.
   *
   * @author Vince Reuter
   */
  object MsigdbCollectionSpec {
    import cats.syntax.show._
    import cats.syntax.order._
    import omictools.generic.implicits.RefinedImplicits._
    import experic.cluster.ClusterTools._
    
    /** Specification of job resources, number of cores for {@code SetRank}, and a MSigDB gene set collection. */
    protected[SetRankParamTools] type ResourceSpecs = NEL[(ResourceSpecTimedHours, PosInt, MsigdbCollection)]
    
    /** Fixed job resources and {@code SetRank} cores across each gene set collection. */
    implicit object FixedResourcesMultiMsigdbSpec extends MultiMsigdbCollectionSpec[(ResourceSpecTimedHours, PosInt, NEL[MsigdbCollection])] {
      def enumerate: Tuple3[ResourceSpecTimedHours, PosInt, NEL[MsigdbCollection]] => ResourceSpecs = resAndCoresAndColls => {
        val (jobRes, rCores, colls) = resAndCoresAndColls
        require(jobRes.cores >= rCores, s"More R cores than job resource cores: ${rCores.value} > ${jobRes.cores.value}")
        colls.map(c => (jobRes, rCores, c))
      }
    }
    
    /** Fixed job resources but variable {@code SetRank} cores across each gene set collection. */
    implicit object HalfDynamicResourcesMultiMsigdbSpec extends MultiMsigdbCollectionSpec[(ResourceSpecTimedHours, NEL[(PosInt, MsigdbCollection)])] {
      def enumerate: Tuple2[ResourceSpecTimedHours, NEL[(PosInt, MsigdbCollection)]] => ResourceSpecs = resAndCoresCollPairs => {
        val (jobRes, coreCollPairs) = resAndCoresCollPairs
        FullyDynamicResourcesMultiMsigdbSpec.enumerate(coreCollPairs map { case(cores, coll) => (jobRes, cores, coll) })
      }
    }
    
    /** Variable job resources and {@code SetRank} cores for each gene set collection. */
    implicit object FullyDynamicResourcesMultiMsigdbSpec extends MultiMsigdbCollectionSpec[ResourceSpecs] {
      def enumerate: ResourceSpecs => ResourceSpecs = specs => {
        val bads = specs filter { case(jobRes, cores, _) => cores > jobRes.cores }
        if (bads.nonEmpty) {
          val errTexts = bads map { case(jobRes, cores, coll) => s"${coll.show} (${cores.value} > ${jobRes.cores.value})" }
          val msg = s"Core count requests in excess of whole job's core count: ${errTexts.mkString("; ")}"
          throw new Exception(msg)
        }
        specs
      }
    }
  }

}
