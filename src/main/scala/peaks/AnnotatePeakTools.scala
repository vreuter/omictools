package omictools
package peaks


/**
 * Types and functions for working with data in context of peak annotation, e.g. {@code HOMER annotatePeaks.pl}.
 *
 * @author Vince Reuter
 */
object AnnotatePeakTools {

  import scala.io.Source
  import java.io.File
  import scala.sys.process._
  
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.show._
  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric._
  
  import AssemblyTools.GenomeAssembly
  import GenomeAssembly._
  import omictools.generic.Znn
  import omictools.FileTools.{ ExtantFile, safeMkdirParent }
  import ExtantFile._
  import omictools.{ CommandLike, CommandLikeWithOutput }

  private val program = "annotatePeaks.pl"

  /**
   * Minimal data required to create a command for {@code annotatePeaks.pl} from {@code HOMER}.
   */
  trait AnnotatePeaksMinimalData[-T] {
    /** Genomic assembly */
    def assembly: T => GenomeAssembly
    /** Alias for genomic assembly */
    def genome: T => GenomeAssembly = assembly
  }

  /** Implicits for working in context of {@code annotatePeaks.pl} from {@code HOMER}. */
  object AnnotatePeaksMinimalData {
    /** A {@code GenomeAssembly} itself provides sufficient data for {@code annotatePeaks.pl} operations. */
    implicit object AnnPeaksMinDataDirect extends AnnotatePeaksMinimalData[GenomeAssembly] {
      def assembly: GenomeAssembly => GenomeAssembly = ga => ga
    }
  }

  /**
   * Operations related to the {@code annotatePeaks.pl} program from {@code HOMER}.
   *
   * @tparam T The type of instance that admits the minimal data needed to provide the needed operations; 
   *    specifically, in the case of {@code annotatePeaks.pl}, this is the genome assembly. The input file 
   *    and other options are provided at operation call time.
   * @author Vince Reuter
   */
  implicit class AnnotatePeaksOps[-T](t: T)(implicit ev: AnnotatePeaksMinimalData[T]) {
    
    /** Create a command entity for {@code annotatePeaks.pl}, with no output file. */
    def getCommand(f: ExtantFile): CommandLike = CommandLike(rawCommand(f))
    
    /** Create a command entity for {@code annotatePeaks.pl}, with an output file. */
    def getCommand(infile: ExtantFile, outfile: File): CommandLikeWithOutput = 
      CommandLikeWithOutput(getCommand(infile), outfile)
    
    /** Run {@code annotatePeaks.pl}, with no output file. */
    def run(f: ExtantFile): Unit = getCommand(f).run()
    
    /** Run {@code annotatePeaks.pl}, with an output file. */
    def run(infile: ExtantFile, outfile: File): Unit = getCommand(infile, outfile).run()
    
    /** Get the raw text command for {@code annotatePeaks.pl} without an output file. */
    def rawCommand(f: ExtantFile): String = s"$program ${f.getPath} ${ev.assembly(t).show}"
    
    /** Get the raw text command for {@code annotatePeaks.pl} with output file. */
    def rawCommand(infile: ExtantFile, outfile: File): String = getCommand(infile, outfile).get
  }

  /** Parse subset of {@code annotatePeaks.pl} output file's fields. */
  def extractAnnotationsFields(lines: Iterator[String], indices: NEL[Znn]): Iterator[Either[String, Seq[String]]] = {
    require(lines.hasNext, "Empty lines for annotations extraction")
    implicit val toInt: Znn => Int = _.value
    val parse: String => Either[String, Seq[String]] = l => {
      val fields = l.split("\t")
      try { Right(indices.toList.toSeq.map(i => fields(i))) } catch { case _: ArrayIndexOutOfBoundsException => Left(l) }
    }
    lines.map(parse)
  }

  /** Parse subset of {@code annotatePeaks.pl} output file's fields. */
  def extractAnnotationsFields(f: ExtantFile, indices: NEL[Znn]): Iterator[Either[String, Seq[String]]] = {
    extractAnnotationsFields(Source.fromFile(f).getLines, indices)
  }

  // Default implementation of file name mapper to use when filtering annotations.
  private def makeFiltAnnsName: String => String = n => {
    val fields = n.split("\\.")
    (fields.toSeq.init ++ Seq("filtered", fields.last)).mkString(".")
  }
  
  /**
   * Take a subspace of annotations output by, e.g. {@code annotatePeaks.pl} from {@code HOMER}.
   *
   * @param annsFile Path to the full annotations file from which to pull a data subspace.
   * @param indices 0-based indices of the columns to pull from the annotations file.
   * @param makeName How to derive name for the filtered file from the original annotations file.
   * @return Either a {@code Left} containing a nonempty collection of faulty lines, or a {@code Right} 
   *    wrapping the path to the output file.
   */
  def subsetAnnotations(annsFile: ExtantFile, indices: NEL[Znn], makeName: String => String = makeFiltAnnsName): Either[NEL[String], File] = {
    val outfile = new File(annsFile.getParentFile, makeName(annsFile.getName))
    subsetAnnotations(annsFile, indices, outfile)
  }

  /**
   * Take a subspace of annotations output by, e.g. {@code annotatePeaks.pl} from {@code HOMER}.
   *
   * @param annsFile Path to the full annotations file from which to pull a data subspace.
   * @param indices 0-based indices of the columns to pull from the annotations file.
   * @param outfile Path to the output file to create.
   * @return Either a {@code Left} containing a nonempty collection of faulty lines, or a {@code Right} 
   *    wrapping the path to the output file.
   */
  def subsetAnnotations(annsFile: ExtantFile, indices: NEL[Znn], outfile: File): Either[NEL[String], File] = {
    import java.io.{ BufferedWriter => BW, FileWriter => FW }
    import cats.instances.string._
    import cats.syntax.eq._
    import cats.syntax.list._
    if (annsFile.getPath === outfile.getPath) throw new IllegalArgumentException(
      s"Input file is equivalent to output file -- ${annsFile.getPath} and ${outfile.getPath}, respectively.")
    safeMkdirParent(outfile)
    val out = new BW(new FW(outfile))
    try {
      (extractAnnotationsFields(annsFile, indices).foldLeft(Seq.empty[String]){ case(bads, maybeFields) => 
        maybeFields.fold(l => bads :+ l, fields => { out.write(fields.mkString("\t")); out.newLine(); bads }) 
      }).toList.toNel.toLeft(outfile)
    } finally { out.close() }
  }

}
