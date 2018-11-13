/**
 * Package for -omics utility code.
 *
 * @author Vince Reuter
 */
package object omictools {

  import java.io.File
  import scala.language.postfixOps
  import scala.sys.process._
  import cats.Show
  import FileTools.safeMkdirParent
  
  /** An entity that may be regarded as a command and has some "command-y" behavior. */
  trait CommandLike extends Runnable {
    
    /** Raw text representation of the command. */
    protected[omictools] def raw: String
    
    /** Raw text representation */
    def get: String = raw
    
    /** Create a process associated with this command. */
    def build: ProcessBuilder = Process(get)
    
    /** Run process to which this command gives rise. */
    def run(): Unit = get.!
    
    /** Transform this command abstraction into one associated with an output path. */
    def withOutput(f: File): CommandLikeWithOutput = CommandLikeWithOutput(raw, f)

    override def toString: String = get

  }

  /** Syntactic convenenience for working with command-like entities. */
  object CommandLike {
    def apply(cmd: String): CommandLike = 
      new CommandLike { protected[omictools] def raw: String = cmd }
  }

  /** Entity that may be regarded as a command and is associated with a path to output. */
  trait CommandLikeWithOutput extends CommandLike {
    
    // TODO: consider the tradeoff incurred here with protentially executing 
    // the effect, vs. the benefit of safety provided to the user.

    /** Path to output for this command-like. */
    def outfile: File
    
    /** Get the full text command. */
    override def get: String = {
      safeMkdirParent(outfile)
      s"${raw} > ${outfile.getPath}"
    }
    
    /** Create the process that runs this command-like entity. */
    override def build: ProcessBuilder = {
      safeMkdirParent(outfile)
      Process(raw) #> outfile
    }
    
    /** Run this command's process. */
    override def run(): Unit = build.run()

  }

  /** Syntactic convenience for creating output-associated command-like entities. */
  object CommandLikeWithOutput {
    
    /** Create an output-associated command-like entity from the raw text command and output path. */
    def apply(cmd: String, out: File): CommandLikeWithOutput = 
      new CommandLikeWithOutput {
        protected[omictools] def raw: String = cmd
        def outfile: File = out
      }
    
    /** Create an output-associated command-like entity from a basic command entity and output path. */
    def apply(cmd: CommandLike, out: File): CommandLikeWithOutput = {
      safeMkdirParent(out)
      new CommandLikeWithOutput {
        protected[omictools] def raw: String = cmd.raw
        def outfile: File = out
      }
    }

  }

  /**
   * Automatically generate a {@code cats.Show} for a command-like, using the instance's {@code get} value.
   *
   * @tparam C The type of command-like entity for which to create a {@code Show} instance.
   * @return A {@code Show} instance for the command-like subtype specified desired.
   */
  implicit def showCommandLike[C <: CommandLike]: Show[C] = Show.show((c: C) => c.get)
  
}
