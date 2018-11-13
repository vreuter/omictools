package omictools

import org.scalatest.{ FunSuite, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks


/**
 * Tests of peak tools utility functions.
 *
 * @author Vince Reuter
 */
object PeakToolsTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  
  import java.io.{ File, FileWriter }
  
  def withFile(exec: (File, FileWriter) => Any) {
    ???
  }

  test("Peak merger output filepath is properly placed at end of command") (pending)
  test("Peak merger command contains each input filepath exactly once") (pending)
  test("Peak merger command conditionally and correctly includes blacklisted regions filepath") (pending)
  test("Peak merger command conditionally and correctly includes merge distance") (pending)
  test("Peak merger command includes each exclusion exactly once") (pending)

}
