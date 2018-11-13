package omictools
package formats


/**
 * Tools for working with {@code BED}-formatted data.
 *
 * @author Vince Reuter
 */
object BedFormatTools {
  
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  
  import cats.data.Writer
  import cats.instances.vector._
  import mouse.boolean._
  
  import eu.timepit.refined
  import refined.{ refineV, W }
  import refined.api.Refined
  import refined.collection.Size
  import refined.numeric.Greater

  import Min4Fields._
  import omictools.generic.LinesSource
  import LinesSource._
  
  // BED record name is stored in the 4th field.
  private[this] val bedNameIndex = 3

  /* Convenient type alii */
  private[this] type AtLeast4 = Size[Greater[W.`3`.T]]
  type Min4Fields[A] = Refined[Seq[A], AtLeast4]
  type Min4Text = Min4Fields[String]

  /**
   * Convenience and implicits for working with sequences containing at least four elements.
   *
   * @author Vince Reuter
   */
  object Min4Fields {
    /** Automatically extract the sequence of fields from the refined type instance. */
    implicit def toSeq[A]: Min4Fields[A] => Seq[A] = _.value
    /** Replace the {@code A} at index {@code i} with a new {@code A}; this operation stays within the refined type's domain. */
    def replace[A](as: Min4Fields[A], i: Int, a: A): Either[String, Min4Fields[A]] =
      (i < 0 || i > as.size).fold(Left(s"Index out of bounds for collection of size ${as.size}: $i"), refineV[AtLeast4](as.updated(i, a)))
  }
  
  /**
   * Replace current BED field name with a name that encodes the record's positional information (chromosome, start, and end).
   *
   * @param fields The current collection of BED record data fields
   * @param sep Delimiter to use between location fields, to generate the new name
   * @return A new collection of fields, with updated name
   */
  def replaceName(fields: Min4Text, newName: String): Min4Text = Min4Fields.replace(fields, bedNameIndex, newName) match {
    case Left(msg) => throw new Exception(s"Error replacing BED name: $msg")
    case Right(fields) => fields
  }
  
  /**
   * Replace current BED field name with a name that encodes the record's positional information (chromosome, start, and end).
   *
   * @param fields The current collection of BED record data fields
   * @param sep Delimiter to use between location fields, to generate the new name
   * @return A new collection of fields, with updated name
   */
  def useLocationName(fields: Min4Text, sep: String = "_"): Min4Text = replaceName(fields, fields.take(bedNameIndex).mkString(sep))

  /**
   * In each of a collection of {@code BED} lines, replace the current name field with a name that aggregates the fields that 
   * constitute the location data, i.e. chromosome, start coordinate, and end coordinate.
   *
   * @param source The collection of BED lines in which name field of each is to be updated
   * @param outfile Path to output file to write
   * @param nameSep The delimiter to use between fields of the location-based name
   * @param burnHead Whether the first element, e.g. header, of {@code lines} should be discarded
   * @return Either a {@code Left} containing a collection of invalid lines, or a {@code Right} containing the path to the output file.
   */
  def replaceNamesWithLocationName[S : LinesSource](lines: S, outfile: File, nameSep: String, burnHead: Boolean = false): Either[Seq[String], File] = {
    import scala.collection.mutable.ListBuffer
    Option(outfile.getParentFile) map { d => { val dName = d.getName; if (!List(".", "./", "").contains(dName)) d.mkdirs() } }
    val out = new BW(new FW(outfile))
    val badLines = ListBuffer.empty[String]
    if (burnHead) lines.next()
    try { while (lines.hasNext) {
      val l = lines.next()
      refineV[AtLeast4](l.split("\t").toSeq).fold(
        _ => badLines += l, 
        fields => { out.write(useLocationName(fields, sep = nameSep).mkString("\t")); out.newLine() } )
    } } finally { out.close() }
    Either.cond(badLines.isEmpty, outfile, badLines.toList)
  }

}
