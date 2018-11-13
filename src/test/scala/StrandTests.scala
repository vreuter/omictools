package omictools

import org.scalatest.FunSpec
import org.scalatest.Matchers._

/**
 * Tests for representation of genomic sequence strand.
 *
 * @author Vince Reuter
 */
class StrandTests extends FunSpec {
  
  describe("A sequencing read Strand") {
    
    import cats.Show
    import Strand._

    it("should display correctly as simple text") {
      implicitly[Show[Strand]].show(PlusStrand) shouldEqual "+"
      implicitly[Show[Strand]].show(MinusStrand) shouldEqual "-"
    }

    it("is equivalent to/from simple text") {
      import cats.syntax.show._
      val plusText = implicitly[Show[Strand]].show(PlusStrand)
      Strand(plusText).map(_.show) shouldEqual Right(plusText)
      val minusText = implicitly[Show[Strand]].show(MinusStrand)
      Strand(minusText).map(_.show) shouldEqual Right(minusText)
    }

  }

}
