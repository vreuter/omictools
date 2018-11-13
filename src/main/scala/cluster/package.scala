package omictools


/**
 * Package for working in cluster computing context
 * @author Vince Reuter
 */
package object cluster {

  import java.io.File
  import eu.timepit.refined
  import refined.W
  import refined.api.Refined
  import refined.numeric._
  import omictools.FileTools.safeMkdirParent

  private type PosInt = Refined[Int, Positive]

  /**
   * Add key resource variables to use in LSF context to submit command with {@code bsub}.
   *
   * @param cmd The command to update with cluster resource specification for job submission.
   * @param cores The number of CPUs to allocate.
   * @param memory Number of Gb of memory; be aware whether for you cluster configuration this is per-core or per-job.
   * @param hours Maximum runtime.
   * @param logfile Path to file to use for job logging
   * @return Command updated with prefix specifying LSF resources.
   */
  def bsubify(
    cmd: String, cores: PosInt, memory: PosInt, 
    hours: Refined[Int, Interval.Open[W.`0`.T, W.`168`.T]], logfile: File): String = {
    safeMkdirParent(logfile)
    s"bsub -n ${cores.value} -W ${hours.value}:00 -R 'rusage[mem=${memory.value}]' -o ${logfile.getPath} $cmd"
  }

}
