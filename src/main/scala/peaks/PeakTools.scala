package omictools
package peaks


/**
 * Tools for working with called peaks in sequencing data.
 *
 * @author Vince Reuter
 */
object PeakTools {
  import scala.collection.immutable.ListMap
  import scala.io.Source
  import java.io.{ BufferedWriter => BW, File, FileWriter => FW }
  import cats.data.{ NonEmptyList => NEL }
  import cats.syntax.show._
  import mouse.boolean._
  import eu.timepit.refined
  import refined.W
  import refined.api.Refined
  import refined.numeric._
  import omictools.generic.{ Znn }
  import omictools.FileTools.{ safeMkdirParent, ExtantFile }
  import ExtantFile._
  
  private type PosInt = Refined[Int, Positive]

  /** {@code SAF} file's header's fields. */
  val headerFieldsSAF = List("GeneID", "Chr", "Start", "End", "Strand")

  /** Entity specifying whether {@code featureCounts} should count reads or fragments. See {@code -p} option. */
  sealed trait FeatureCountsObjects
  
  /** Tell {@code featureCounts} to count fragments, not reads. */
  final case object FtcountsFragments extends FeatureCountsObjects
  
  /** Tell {@code featureCounts} to count reads, not fragments. */
  final case object FtcountsReads extends FeatureCountsObjects

  implicit def checkFileExists: File => ExtantFile = f => 
    ExtantFile(f).fold(msg => throw new IllegalArgumentException(s"Missing file: ${f.getPath}"), ef => ef)

  /**
   * Create a command with which to run {@code featureCounts}.
   *
   * @param readsFiles Paths to files (typically aligned BAMs) from which to counts reads or fragments overlapping with features.
   * @param features Path to file in which the features of interest (e.g., ChIP- or ATAC-seq peaks, or genes) are stored 
   *    in {@code SAF} or {@code GTF} format, per {@code featureCounts} requirement.
   * @param outfile Path to write output.
   * @param threads Number of threads to allow {@code featureCounts} to use.
   * @param objects What to count, indicating reads or fragments. See {@code featureCounts -help} for info on {@code -p}.
   * @param minQual Optionally, minimum mapping quality to count a read/fragment.
   * @param multimappers Whether to count ambiguously mapped reads. Each alignment is counted. {@code featureCounts} 
   *    inspects the {@code BAM} record's {@code NH} tag.
   * @param overlap Per {@code featureCounts}, "Assign reads to all their overlapping meta-features (or features if -f is specified)."
   * @return The command with which to run {@code featureCounts}
   */
  def getFeatureCountsCommand(readsFiles: NEL[ExtantFile], features: ExtantFile, outfile: File, threads: PosInt, 
    objects: FeatureCountsObjects, minQual: Option[PosInt], multimappers: Boolean = false, overlap: Boolean = false): String = {
    val fmt = features.value.getName.split("\\.").last match {
      case ("gtf" | "GTF") => "GTF"
      case ("saf" | "SAF") => "SAF"
      case _ => {
        if (features.show.contains("SAF")) "SAF"
        else throw new Exception(s"Annotations file has unrecognized extension: ${features.show}")
      }
    }
    val base = s"featureCounts -T ${threads.value}"
    val mid1 = objects match {
      case FtcountsFragments => s"$base -p"
      case _ => base
    }
    val mid2 = minQual.fold(mid1)(q => s"$mid1 -Q ${q.value}")
    val mid3 = multimappers.fold(s"$mid2 -M", mid2)
    val mid4 = overlap.fold(s"$mid3 -O", mid3)
    s"$mid4 -F $fmt -a ${features.show} -o ${outfile.getPath} ${readsFiles.toList.map(_.show).mkString(" ")}"
  }


  /**
   * Create command with which to merge called peak files, useful to create a "union" of peaks in at least one of a collection of samples.
   *
   * @param peakFiles Paths to files in which called peak data are stored.
   * @param outfile Path to file to which to write output.
   * @param exclusions Collection of chromosome name substrings that indicate a region in such a chromosome should be skipped.
   * @param blacklist Optionally, path to file of blacklisted regions.
   * @param mergeDistance Optionally, the interpeak distance to tolerate and merge peaks as one.
   * @return Command to execute in order to merge peaks, creating a sort of union.
   */
  def getMergeCommand(
    peakFiles: NEL[ExtantFile], outfile: File, 
    exclusions: Iterable[String] = List("rand", "chrUn"), 
    blacklist: Option[ExtantFile], mergeDistance: Option[PosInt]): String = {
    safeMkdirParent(outfile)
    val base = s"cat ${peakFiles.map(_.show).toList.mkString(" ")}"
    val mid1 = s"${exclusions.nonEmpty.fold(s"$base | grep -v ${exclusions.mkString("""\|""")}", base)} | cut -f1-3 | sort -k1,1 -k2,2n"
    val mid2 = blacklist.fold(mid1)(f => s"$mid1 | bedtools intersect -v -a stdin -b ${f.show}")
    val mid3 = mergeDistance.fold(mid2)(x => s"$mid2 | bedtools merge -d ${x.value} -i stdin")
    s"$mid3 > ${outfile.getPath}"
  }

  /**
   * Write merged peaks file in SAF format, e.g. for {@code featureCounts}.
   *
   * @param peakFile Path to file in which called peaks (or other genomic region data) are stored.
   *    The only requirement is that the first three fields be {@code BED3}.
   * @param outfile Path to the file to write as output.
   */
  def mergedPeaks2SAF(peakFile: ExtantFile, outfile: File): Unit = {
    import cats.Show
    import Strand._
    val sep = "\t"
    val header = headerFieldsSAF.mkString(sep)
    safeMkdirParent(outfile)
    val out = new BW(new FW(outfile))
    try {
      out.write(header); out.newLine()
      Source.fromFile(peakFile.value).getLines foreach { l => {
        val fields = l.split("\t")
        val (chr, start, end) = (fields(0), fields(1), fields(2))
        val strand = {
          if (fields.length > 5) Strand(fields(5)).fold(msg => throw new Exception(s"Invalid strand in line: $l"), _.show)
          else implicitly[Show[Strand]].show(PlusStrand)
        }
        val line = List(s"${chr}_${start}_${end}", chr, start, end, "+").mkString(sep)
        out.write(line); out.newLine()
      } }
    } finally { out.close() }
  }

  /**
   * From a file that stores location-encoding genomic region name along with a label/group identifier for the region, emit KV pairs.
   *
   * @param f The path to the file from which key-value pairs are to be emitted.
   * @param sep The delimiter between fields on each line of the file.
   * @param looseMatch Whether to allow more than 2 fields per line; regardless, the first 2 fields are the 2 parsed.
   * @return An iterator over {@code Either}s, in which the {@code Left} case wraps a faulty line and the {@code Right} case 
   *    wraps a successfully parsed key-value pair
   */
  def streamLabeledLocPeakKVLines(f: ExtantFile, sep: String = "\t", looseMatch: Boolean = false): Iterator[Either[String, (String, String)]] = { 
    val getRaw: String => Array[String] = _.split(sep)
    val check = looseMatch.fold((arr: Array[String]) => arr.size > 1, (arr: Array[String]) => arr.size == 2)
    val parse: String => Either[String, (String, String)] = s => { val fields = getRaw(s); check(fields).either(s, fields(0) -> fields(1)) }
    Source.fromFile(f).getLines.map(parse)
  }

  /**
   * Write {@code BED} file for {@code HOMER}, using file in which peak names encode {@code BED3} and are labeled, used in BED 5th field.
   *
   * @param infile Path to the input file
   * @param outfile Path to which to write output
   * @return Either a {@code Left} containing a nonempty collection of error messages, or a {@code Right} wrapping the output path.
   */
  def labeledLocPeakKV2HomerBed(infile: ExtantFile, outfile: File): Either[NEL[String], File] = {
    import cats.instances.string._    // For Show[String]
    import cats.syntax.list._         // For .toNel
    import omictools.peaks.{ HomerBedable, HomerBedParse }
    import HomerBedable._
    
    // Create the instance with which to make the BED lines for HOMER.
    val makeLine = implicitly[HomerBedParse[(String, String)]]
    
    /* Prep for I/O ops. */
    safeMkdirParent(outfile)
    val out = new BW(new FW(outfile))
    
    try {
      // Write each successfully created BED line for HOMER to the output file, accumulating any errors along the way.
      // Return the errors if any occurred; otherwise, return the path to the output file.
      (streamLabeledLocPeakKVLines(infile).map(_.map(kv => makeLine(kv).fold(_._2, l => l))).foldLeft(Seq.empty[String]){
        case(bads: Seq[String], errOrBed: Either[String, String]) => 
          errOrBed.fold(bads :+ _, l => { out.write(l); out.newLine(); bads })
      }).toList.toNel.toLeft(outfile)
    } finally { out.close() }
  }

  /**
   *
   */
  def applyAnnotationsToCounts(
    countsFile: ExtantFile, annsFile: ExtantFile, outfile: File, headersPresent: Boolean, 
    indices: List[Znn] = Nil, addLocFromPeak: Boolean = false): (ListMap[String, Seq[String]], Set[String], Set[String]) = {
    
    import mouse.boolean._

    // Facilitate automatic conversion (widening, basically) of refined integer to ordinary integer.
    implicit def nnInt2Int: Znn => Int = _.value
    
    val peak2Loc: String => Seq[String] = _.split("_").toSeq

    val attachAnns: (String, Seq[String], Seq[String]) => Seq[String] = addLocFromPeak.fold(
      (peak, cntDat, annDat) => peak2Loc(peak) ++ cntDat ++ annDat, (peak, cntDat, annDat) => cntDat ++ annDat)

    // Use the information regarding whether the input files have a header to determine how to handle the iniitial parse.
    val getLines: File => Iterator[String] = f => {
      val lines = Source.fromFile(f).getLines
      if (headersPresent) { lines.next(); lines } else lines
    }

    // The the indices of annotations fields to determine how to grab the fields subspace.
    val filtAnns: Seq[String] => Seq[String] = indices match {
      case Nil => {
        // When no subset of indices is specified, return all the fields but the first (peak name).
        (dat: Seq[String]) => dat.tail
      }
      case is => {
        // When subset of indices is specified, use exactly those; it's on the user to exclude peak index as needed.
        // TODO: improve usability in this regard.
        (dat: Seq[String]) => is.toSeq.map(i => dat(i))
      }
    }
    
    def parseLine(l: String, sep: String = "\t", f: Seq[String] => Seq[String] = arr => arr): (String, Seq[String]) = {
      val fields = l.split("\t").toSeq; fields(0) -> f(fields)
    }

    // Obtain from a single text line a key (peak) and sequence of fields.
    val readAnnsLine: String => (String, Seq[String]) = parseLine(_, f = filtAnns)
    
    val annsFields = getLines(annsFile).foldLeft(ListMap.empty[String, Seq[String]]){ case(m, l) => m + readAnnsLine(l) }
    val (unannotated, combined) = getLines(countsFile).foldLeft((Set.empty[String], ListMap.empty[String, Seq[String]])){
      case(acc, l) => {
        val (peak, counts) = parseLine(l, f = (arr: Seq[String]) => arr.tail)
        annsFields.get(peak).fold((acc._1 + peak, acc._2))(annsData => (acc._1, acc._2 + (peak, attachAnns(peak, counts, annsData))))
      }
    }
    (combined, unannotated, annsFields.keySet &~ combined.keySet)
  }

}






