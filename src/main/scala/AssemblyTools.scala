package omictools


/**
 * The genome assembly specification to use with a {@code HOMER} program
 *
 * @author Vince Reuter
 */
object AssemblyTools {

  /** Paricular assembly version of an organism's genome. */
  sealed trait GenomeAssembly
  
  /** mm9 assembly */
  final case object Mm9 extends GenomeAssembly
  
  /** mm10 assembly */
  final case object Mm10 extends GenomeAssembly
  
  /** hg19 assembly */
  final case object Hg19 extends GenomeAssembly
  
  /** hg38 assembly */
  final case object Hg38 extends GenomeAssembly
  
  /** Convenience/helpers for working with a genome assembly specification. */
  object GenomeAssembly {
    import cats.Show
    implicit val showGenomeAssembly: Show[GenomeAssembly] = 
      new Show[GenomeAssembly] { def show(g: GenomeAssembly): String = g.toString.toLowerCase }
  }

}
