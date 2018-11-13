package omictools
package formats


import scala.io.Source
import java.io.File
import java.util.regex.Pattern
import cats.{ Alternative, Show }
import cats.data.{ NonEmptyList => NEL }
import cats.instances.either._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.list._
import cats.syntax.show._
import mouse.boolean._
import eu.timepit.refined
import refined.{ refineV, W }
import refined.api.{ Refined, Validate }
import refined.numeric._
import refined.generic._
import omictools.generic.ScientificNotationTools._


/**
 * Functions and types for working with the {@code HOMER} motif file format.
 *
 * The motif file format is briefly described in the ''Motif files'' section 
 * of {@code HOMER}'s [[http://homer.ucsd.edu/homer/ngs/formats.html File Formats documentation]].
 * As mentioned there, the format is more thoroughly detailed in the documentation 
 * page on [[http://homer.ucsd.edu/homer/motif/creatingCustomMotifs.html Creating Custom Motif Matrices]].
 *
 * @author Vince Reuter
 */
object HomerMotifFormatTools {
  
  /** Designation of an entity has a {@code HOMER} motif section field. */
  sealed trait HomerMotifField

  //private case class PRangeValidDummy()
  private type PRange = Interval.Closed[W.`0.0`.T, W.`1.0`.T]
  type Probability = Refined[Double, PRange]
  private implicit def getProbValue: Probability => Double = _.value

  /**
   * Wrapper around a {@code HOMER} motif file section header datum
   *
   * @tparam T The type of datum wrapped
   * @author Vince Reuter
   */
  sealed trait HomerMotifDatum[T] extends HomerMotifField { def get: T }
  
  /** A motif sequence */
  final case class MotifSequence(get: String) extends HomerMotifDatum[String]
  object MotifSequence {
    implicit val showMotifSequence: Show[MotifSequence] = Show.show( (s: MotifSequence) => s">${s.get}" )
  }
  
  /** A motif name, often arbitrary and perhaps empty. */
  final case class MotifName(get: String) extends HomerMotifDatum[String]
  // TODO: handle the data-encoding name in the motif file (e.g. "Best guess", source, numeric value, etc.)
  object MotifName {
    implicit val showMotifName: Show[MotifName] = Show.show(_.get)
  }
  
  /** Per the [[http://homer.ucsd.edu/homer/motif/creatingCustomMotifs.html HOMER docs]], log-odds detection threshold for the motif */
  final case class MotifLogOdds(get: Double) extends HomerMotifDatum[Double]
  object MotifLogOdds {
    implicit val showMotifLogOdds: Show[MotifLogOdds] = Show.show( (lo: MotifLogOdds) => lo.get.isNaN.fold("nan", lo.get.toString) )
  }

  /** Logarithm of motif enrichment p-value. */
  final case class MotifLogPvalue(get: Double) extends HomerMotifDatum[Double]
  object MotifLogPvalue {
    implicit val showMotifLogPvalue: Show[MotifLogPvalue] = Show.show(_.get.toString)
  }

  /** Dummy instance for the "placeholder" field among a motif section header. */
  final case object MotifPlaceholder extends HomerMotifField {
    implicit def toEmpty[T]: HomerMotifDatum[Option[T]] = new HomerMotifDatum[Option[T]] { def get: Option[T] = Option.empty[T] }
  }

  sealed trait OccurrenceDatum {
    def prefix: String
    def count: Double
    def percent: Probability
  }

  object OccurrenceDatum {
    import scala.math.{ BigDecimal => BigDec }
    
    protected[HomerMotifFormatTools] def dataFromText(s: String)(expPfx: String): Either[String, (Double, Double)] = for {
      _ <- s.startsWith(expPfx).either(s"Invalid prefix (expected '$expPfx') on occurrence datum text: $s", ())
      _ <- s.endsWith("%)").either(s"Invalid suffix (expected '%)') on occurrence datum text: $s", ())
      //fields <- Right(s.stripPrefix(s"$expPfx:").stripSuffix("%)").split("("))
      fields <- Right(s.stripPrefix(s"$expPfx:").stripSuffix("%)").split(Pattern.quote("(")))
      cntPctTextPair <- (fields.length == 2).either(s"Invalid motif occurence field text: $s", fields(0) -> fields(1))
      cntPctPair <- try { Right(cntPctTextPair._1.toDouble -> cntPctTextPair._2.toDouble / 100) } catch { case e: NumberFormatException => Left(s"Error parsing occurrence datum -- ${e.getMessage}") }
    } yield (cntPctPair._1, cntPctPair._2)

    implicit def showOccDatum[D <: OccurrenceDatum]: Show[D] = 
      Show.show( (d: D) => s"${d.prefix}:${d.count}(${BigDec(100 * d.percent).setScale(2, BigDec.RoundingMode.HALF_UP).toString}%)" )
  }

  final case class MotifTargetSequenceOccurrence(count: Double, percent: Probability) extends OccurrenceDatum { def prefix: String = "T" }
  object MotifTargetSequenceOccurrence {
    def fromText(s: String): Either[String, MotifTargetSequenceOccurrence] = for {
      cntPct <- OccurrenceDatum.dataFromText(s)("T")
      p <- refineV[PRange](cntPct._2)
    } yield MotifTargetSequenceOccurrence(cntPct._1, p)
  }

  final case class MotifBackgroundSequenceOccurrence(count: Double, percent: Probability) extends OccurrenceDatum { def prefix: String = "B" }
  object MotifBackgroundSequenceOccurrence {
    def fromText(s: String): Either[String, MotifBackgroundSequenceOccurrence] = for {
      cntPct <- OccurrenceDatum.dataFromText(s)("B")
      p <- refineV[PRange](cntPct._2)
    } yield MotifBackgroundSequenceOccurrence(cntPct._1, p)
  }

  sealed trait PvalLike { def text: String }
  object PvalLike {
    implicit class DoublePvalue(val get: Double) extends PvalLike { def text: String = get.toString }
    implicit class SciNumPvalue(val get: ScientificNumber) extends PvalLike { def text: String = get.text }
    implicit def showPvalLike: Show[PvalLike] = Show.show(_.text)
  }

  /** Additional motif field containing subfields with target and background sequence occurrence information */
  final case class MotifOccurrenceData(target: MotifTargetSequenceOccurrence, background: MotifBackgroundSequenceOccurrence, pvalue: PvalLike) {
    import PvalLike._
    def rTextFields: Seq[String] = Seq(target.count, target.percent.value, background.count, background.percent.value).map(_.toString) :+ pvalue.show
  }

  object MotifOccurrenceData {
    import PvalLike._
    
    def fromText(s: String): Either[String, MotifOccurrenceData] = s.split(",").toList match {
      case tgt :: bkg :: pval :: Nil => for {
        t <- MotifTargetSequenceOccurrence.fromText(tgt)
        b <- MotifBackgroundSequenceOccurrence.fromText(bkg)
        p <- parsePvalue(pval)
      } yield MotifOccurrenceData(t, b, p)
      case _ => Left(s"Invalid motif occurrence field: $s")
    }

    def parsePvalue(text: String): Either[String, PvalLike] = {
      val fields = text.stripPrefix("P:").split("e")
      if (fields.length == 1) try { Right(new DoublePvalue(fields(0).toDouble)) } catch { case _: NumberFormatException => Left(s"Invalid p-value: $text") }
      else if (fields.length == 2) for {
        b <- try { ScientificNotationBase.buildSafe(fields(0).toDouble) } catch { case _: NumberFormatException => Left(s"Invalid scientific number base in: $text") }
        exp <- try { Right(fields(1).toInt) } catch { case _: NumberFormatException => Left(s"Invalid exponent (${fields(1)}) in alleged p-value text ($text)") }
      } yield new SciNumPvalue(ScientificNumber(b, exp))
      else Left(s"Invalid p-value text: $text")
    }

    implicit val showMotifOccurrenceData: Show[MotifOccurrenceData] = Show.show((d: MotifOccurrenceData) => List(d.target.show, d.background.show, d.pvalue.show).mkString(","))
  
  }

  //final case class MotifStatistics()

  case class HomerMotifData(seq: MotifSequence, name: MotifName, logOdds: MotifLogOdds, logPvalue: Option[MotifLogPvalue], occData: Option[MotifOccurrenceData]) {
    if (occData.nonEmpty && logPvalue.isEmpty) throw new IllegalArgumentException(s"Nonempty occurrence data + empty log p-value is prohibited")
    def homerTextFields: Seq[String] = Seq(seq.show, name.show, logOdds.show) ++ Seq(logPvalue.map(_.show), occData.map(_.show)).flatten
    def render(sep: String): String = homerTextFields.mkString(sep)
    override def toString: String = render("\t")
    def rTextFields: Seq[String] = {
      val first3 = Seq(seq.show.stripPrefix(">"), name.show.replaceAllLiterally("-", "_"), logOdds.show)
      (logPvalue, occData) match {
        case (None, None) => first3
        case (Some(p), Some(d)) => first3 ++ (p.show +: d.rTextFields)
        case (Some(p), None) => first3 :+ p.show
        case (None, Some(_)) => throw new IllegalStateException("Motif data instance with occurrence data but no log p-value")
      }
    }
  }
  
  object HomerMotifData {
    
    /**
     * Attempt 
     */
    def fromLine: String => Either[String, HomerMotifData] = l => fromFields(l.split("\t").toSeq)
    
    def fromFields: Seq[String] => Either[String, HomerMotifData] = fields => {
      if (fields.length < 3) Left(s"Too few fields (${fields.length} -- ${fields.mkString(", ")}) for HOMER motif line; need at least 3")
      else if (!fields(0).startsWith(">")) Left (s"Invalid motif sequence field (${fields(0)}) -- the motif sequence field must start with '>'")
      else {
        val seq = MotifSequence(fields(0).stripPrefix(">"))
        val name = MotifName(fields(1))
        for {
          logOdds <- try { Right(fields(2).toDouble) } catch { case _: NumberFormatException => 
            if (fields(2) === "nan" || fields(2) === "-nan") Right(Double.NaN) else Left(s"Invalid log-odds: ${fields(2)}") }
          logPval <- try { (fields.length > 3).fold(Right(Some(fields(3).toDouble)), Right(None)) } catch { case _: NumberFormatException => Left(s"Invalid log p-value: ${fields(3)}") }
          occData <- (fields.length > 5).fold(MotifOccurrenceData.fromText(fields(5)).map(od => Some(od)), Right(None))
        } yield HomerMotifData(seq, name, MotifLogOdds(logOdds), logPval.map(p => MotifLogPvalue(p)), occData)
      }
    }
    
    def motifFile2MaybedMotifRecords(f: File): Seq[Either[String, HomerMotifData]] = {
      require(f.isFile, s"Alleged path to motif file isn't a file: ${f.getPath}")
      Source.fromFile(f).getLines.foldLeft(Seq.empty[Either[String, HomerMotifData]]){ case(acc, l) => l.startsWith(">").fold(acc :+ fromLine(l), acc) }
    }
    
    def fromSingleFile: File => Either[String, HomerMotifData] = f => 
      f.isFile.fold(fromLine(Source.fromFile(f).getLines.next()), Left(s"Not a file: ${f.getPath}"))

    def readMotifFile(f: File): (List[String], List[HomerMotifData]) = Alternative[List].separate(motifFile2MaybedMotifRecords(f).toList)

    implicit val showMotifData: Show[HomerMotifData] = Show.fromToString[HomerMotifData]
  
  }

  def collectDenovo(d: File): Either[NEL[(File, String)], NEL[HomerMotifData]] = {
    require(d.isDirectory, s"Alleged HOMER folder path isn't a directory: ${d.getPath}")
    def makeKeyedFile: File => Option[(Int, File)] = f => {
      if (!f.isFile) None
      else {
        val (base, ext) = { val fields = f.getName.split("\\."); fields(0) -> fields(1) }
        if (ext =!= "motif") None
        else { try { Some(base.stripPrefix("motif").toInt -> f) } catch { case _: NumberFormatException => None } }
      }
    }
    val motifFiles = d.listFiles.toSeq.map(makeKeyedFile).flatten.sortBy(_._1).map(_._2)
    if (motifFiles.isEmpty) throw new Exception(s"No motif files in folder: ${d.getPath}")
    val (fails, succs) = motifFiles.foldLeft((Seq.empty[(File, String)], Seq.empty[HomerMotifData])){ case(acc, f) => 
      HomerMotifData.fromSingleFile(f).fold(msg => (acc._1 :+ (f -> msg), acc._2), d => (acc._1, acc._2 :+ d))
    }
    fails.isEmpty.either(fails.toList.toNel.get, succs.toList.toNel.get)
  }

}
