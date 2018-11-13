package omictools


/**
 * Tools for working with files.
 *
 * @author Vince Reuter
 */
object FileTools {
  import java.io.File
  import cats.syntax.list._
  import eu.timepit.refined
  import refined.refineV
  import refined.api.{ Refined, Validate }
  
  /**
   * Split a filepath into folder path and filename.
   *
   * @param f Path to the file to split
   * @return Pair in which first component is (optionally) the path to the file's containing folder, 
   *    and the second is the filename.
   */
  def split(f: File): (Option[File], String) = (Option(f.getParentFile), f.getName)
  
  /**
   * Insert a suffix in a filename, before the extension.
   *
   * @param fn Filename in which to insert the suffix
   * @param suffix Suffix to insert
   * @return Updated filename
   */
  def insertSuffix(fn: String, suffix: String): String = {
    fn.split("\\.").toList match {
      case n :: Nil => n ++ suffix
      case h :: t => ((h ++ suffix) :: t).mkString(".")
    }
  }

  /**
   * Insert a suffix in a filepath, before the extension.
   *
   * @param f Filepath in which to insert suffix
   * @param suffix The suffix to insert
   * @return Updated filepath
   */
  def insertSuffix(f: File, suffix: String): File = split(f) match { case(maybeFolder, fn) => {
    val newName = insertSuffix(fn, suffix)
    maybeFolder.fold(new File(newName))(new File(_, newName))
  } }
  
  /** Safe creation of parent folder of file. */
  def safeMkdirParent: File => Unit = f => Option(f.getParentFile) map { d =>
    if (d.isFile) throw new Exception(s"Parent of alleged path ${f.getPath} is a file: ${d.getPath}")
    if (!d.isDirectory && !List("", ".", "./").contains(d.getName)) d.mkdirs()
  }

  /**
   * Replace the extension on a filename with a substitute.
   *
   * @param fn Filename in which the extension is to be replaced.
   * @param ext The alternate extension.
   * @return The new filename.
   */
  def replaceExtension(fn: String, ext: String): String = fn.split("\\.").toList match {
    case h :: Nil => throw new Exception(s"Cannot replace extension when splitting on dot yields just one field; filename: $fn")
    case fields => (fields.init.toSeq :+ ext).mkString(".")
  }
  
  /**
   * Replace a file's extension with a given substitute.
   *
   * @param f Path to the file on which the extension is to be replaced
   * @param ext The new extension
   * @return The new filepath, updated to reflect the extension substitution.
   */
  def replaceExtension(f: File, ext: String): File = split(f) match { case(d, fn) => new File(f, replaceExtension(fn, ext)) }

  /** Package-private dummy case class for validation of a path as being a file. */
  protected[omictools] case class ExtantFileChecked()
  protected[omictools] case class NewFileChecked()
  protected[omictools] case class ExtantFolderChecked()
  protected[omictools] case class NewFolderChecked()

  /** Path validated as pointing to an extant file. */
  type ExtantFile = Refined[File, ExtantFileChecked]
  type NewFile = Refined[File, NewFileChecked]
  
  
  /** Syntactic convenience and implicits for working with {@code ExtantFile} instances. */
  object ExtantFile {
    import cats.Show
    
    /** Attempt validation of a path as an extant file. */
    def apply(f: File): Either[String, ExtantFile] = refineV[ExtantFileChecked](f)
    
    /** Run some effectful operation that returns a file, then validate that the file exists. */
    def execCheckExtant(run: => File): Either[String, ExtantFile] = apply(run)

    /** Show an extant file by the filepath. */
    implicit val showExtantFile: Show[ExtantFile] = Show.show(_.value.getPath)
    
    /** As needed, automatically widen the extant file refinement type to an ordinary file. */
    implicit def toFile: ExtantFile => File = _.value

    // Validation context for checking path as extant file.
    implicit val ValidateExtantFile: Validate.Plain[File, ExtantFileChecked] = 
      Validate.fromPredicate(_.isFile, f => s"(not a file: ${f.getPath})", ExtantFileChecked())

    implicit val ValidateNewFile: Validate.Plain[File, NewFileChecked] = 
      Validate.fromPredicate(!_.exists, p => s"(${p.getPath} exists)", NewFileChecked())

  }

  /** Path validated as pointing to an extant directory. */
  type ExtantFolder = Refined[File, ExtantFolderChecked]

  /** Syntactic convenience and implicits for working with {@code ExtantFolder} instances. */
  object ExtantFolder {
    
    import cats.Show
    
    implicit val ValidateExtantFolder: Validate.Plain[File, ExtantFolderChecked] = 
      Validate.fromPredicate(_.isDirectory, p => s"(not an extant folder: ${p.getPath})", ExtantFolderChecked())

    implicit val ValidateNewFolder: Validate.Plain[File, NewFolderChecked] = 
      Validate.fromPredicate(!_.exists, p => s"(${p.getPath} exists)", NewFolderChecked())
    
    /** Validate a path as an existing directory. */
    def apply(p: File): Either[String, ExtantFolder] = refineV[ExtantFolderChecked](p)
    
    /** Show an existing directory by its path. */
    implicit val showExtantFolder: Show[ExtantFolder] = Show.show(_.value.getPath)
  }

}
