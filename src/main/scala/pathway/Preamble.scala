package omictools
package pathway


/**
 * Implicits and ancillary functions for the {@code pathway} subpackage.
 *
 * @author Vince Reuter
 */
object Preamble {

  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  
  /** A {@code ClustergramPartitioner} that does no row validation, and assumes gene ID is the 
   * first of a collection of fields and that label is the last of a collection of fields. */
  implicit val rubberStampBookendPartitioner = new ClustergramPartitioner {
    def validate: Seq[String] => Either[String, Unit] = _ => Right(())
    def parseGene: Seq[String] => String = _.head
    def parseLabel: Seq[String] => String = _.last
  }

  /**
   * Given a folder in which output is being placed, create a function with which to write file(s) logging errors.
   *
   * @param outdir Path to folder for output(s)
   * @return Function with which to handle "grouped errors," typically meaning a collection of faulty lines/text records 
   *    which together are associated with a particular, named "group." The "handling" is done by using the group name 
   *    to create a correspondingly named logfile within {@code outdir}, to which the faulty lines/records are then written.
   */
  def getErrorHandler: File => (NEL[(String, Seq[String])] => NEL[File]) = outdir => {
    groupedRecords => {
      val makeGroupErrorFile: String => File = g => new File(outdir, s"group${g}_errors.txt")
      groupedRecords map { case(g, rs) => {
        val errFile = makeGroupErrorFile(g)
        val err = new BW(new FW(errFile))
        try {
          rs.foreach(r => { err.write(r.mkString("\t")); err.newLine() })
          errFile
        } finally { err.close() }
      } }
    }
  }

}
