package omictools


/**
 * Tools for working with {@code DESeq2}.
 *
 * @author Vince Reuter
 */
object Deseq2Tools {
  import scala.annotation.tailrec
  import java.io.File
  import cats.{ Alternative, Order }
  import cats.instances.either._
  import cats.instances.list._
  import cats.syntax
  import eu.timepit.refined
  import refined.refineV
  import refined.api.{ Refined, Validate }
  import refined.numeric.NonNegative
  
  /** Type alias for either a failed or successful I/O operation */
  type ErrorOrWrite = Either[Throwable, Unit]

  /**
   * Basic notion of "condition" of sample, based simply on wrapped name.
   * @constructor
   * @param get The name of the condition
   * @author Vince Reuter
   */
  sealed case class BasicCondition(get: String)
  
  /** Convenience and implicits for working with instances of {@code BasicCondition} */
  object BasicCondition {
    import cats.instances.string._
    implicit val ordCondition: Order[BasicCondition] = new Order[BasicCondition] {
      def compare(a: BasicCondition, b: BasicCondition): Int = implicitly[Order[String]].compare(a.get, b.get)
    }
  }
  
  def generateContrasts(conditions: Seq[BasicCondition])(implicit ord: Order[BasicCondition]): Seq[(BasicCondition, BasicCondition)] = {
    comboPairs(conditions.sorted(ord.toOrdering).distinct)
  }
  
  def comboPairs[A](items: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def go(as: List[A], acc: Seq[List[(A, A)]]): Seq[List[(A, A)]] = as match {
      case a1 :: tail => if (tail.isEmpty) acc else go(tail, acc :+ (tail.map(a2 => a1 -> a2)))
      case _ => acc
    }
    go(items.toList, Seq()).flatten
  }

  def writeContrastsFile(outfile: File, conditions: Iterable[BasicCondition], sep: String): ErrorOrWrite = {
    import java.io.{ BufferedWriter => BW, FileWriter => FW }
    val makeLine: (BasicCondition, BasicCondition) => String = (c1, c2) => s"${c1.get}${sep}${c2.get}"
    Option(outfile.getParentFile).map(d => if (d != "") d.mkdirs())
    val out = new BW(new FW(outfile))
    try {
      generateContrasts(conditions.toSeq) foreach { case(c1, c2) => out.write(makeLine(c1, c2)); out.newLine() }
      Right(())
    }
    catch { case e: Throwable => Left(e) }
    finally { out.close() }
  }

  def writeContrastsFile(lines: Iterator[String], conditionIndex: Refined[Int, NonNegative], outfile: File, inSep: String, outSep: String): ErrorOrWrite = {
    import generic.TypeTransform.parseEithers
    parseEithers(parseConditions(lines, inSep, conditionIndex).toList) match {
      case (Nil, conditions) => writeContrastsFile(outfile, conditions, outSep)
      case (errLinePairs, _) => Left(new Exception(
        s"${errLinePairs.size} lines with condition-parsing errors:\n${errLinePairs.map{case(l, e) => s"$l: ${e.getMessage}"}.mkString("\n")}"))
    }
  }

  /* Read from a file, e.g. "sample annotations sheet," the collection of conditions for a collection of samples. */
  private def parseConditions(lines: Iterator[String], sep: String, index: Int): Seq[Either[(String, Throwable), BasicCondition]] = {
    import scala.io.Source
    require(index > -1, s"Negative conditions column index: ${index}")
    lines.toList.foldLeft(Seq.empty[Either[(String, Throwable), BasicCondition]]) { case(acc, l) => 
      try { acc :+ Right(BasicCondition(l.split(sep)(index))) } catch { case e: ArrayIndexOutOfBoundsException => acc :+ Left(l -> e) }
    }
  }

  implicit def toRefined[T, P](t: T)(implicit ev: Validate[T, P]): Either[String, Refined[T, P]] = refineV(t)
  implicit def fromRefined[T]: Refined[T, _] => T = _.value

}
