package omictools
package generic


/**
 * Entity that guarantees provision of a command as text.
 *
 * @author Vince Reuter
 */
trait WrappedCommand {
  def command: String
}


/**
 * Convenience and implicits for working with command wrappers.
 *
 * @author Vince Reuter
 */
object WrappedCommand {
  import cats.Show
  import experic.Program.ProgramLike
  implicit def asProgramLike[C <: WrappedCommand]: ProgramLike[C] = new ProgramLike[C] { def createCommand(c: C): String = c.command }
}
