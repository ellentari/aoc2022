package aoc

import cats.Eval
import cats.syntax.all._

import scala.annotation.tailrec

object Day7 extends App {

  type Size = Int

  sealed trait FileSystemObject extends Product with Serializable {
    def name: String
    def calculateSize: Eval[Size]
  }

  object FileSystemObject {

    case class File(name: String, size: Size) extends FileSystemObject {
      val calculateSize: Eval[Size] = Eval.now(size)
    }

    case class Directory(name: String, contents: List[FileSystemObject] = Nil) extends FileSystemObject {
      val calculateSize: Eval[Size] = contents.traverse(_.calculateSize).map(_.sum).memoize
      def calculatedSize: Size = calculateSize.value

      def directories: List[Directory] = contents.collect { case dir: Directory => dir }
      def findDirectory(name: String): Option[Directory] = directories.find(_.name == name)

      def withContents(newContents: List[FileSystemObject]): Directory = copy(contents = newContents)
      def updateContents(name: String, newContent: FileSystemObject): Directory =
        copy(contents = contents.map {
          case o: FileSystemObject if o.name == name => newContent
          case o => o
        })

      def collectAllSubdirectorySizes(p: Size => Boolean): List[Size] =
        collectInSubDirectories { case d: Directory if p(d.calculatedSize) => d.calculatedSize }

      def collectInSubDirectories[A](pf: PartialFunction[FileSystemObject, A]): List[A] = {
        @tailrec
        def loop(objects: List[FileSystemObject], acc: List[A]): List[A] =
          objects match {
            case Nil => acc.reverse
            case (d@Directory(_, contents)) :: tail => loop(contents ::: tail, pf.lift(d).toList ::: acc)
            case (file: File) :: tail => loop(tail, pf.lift(file).toList ::: acc)

          }

        loop(contents, pf.lift(this).toList)
      }

      override def toString: String = s"Directory($name)"
    }
  }

  sealed trait TerminalLineEntry extends Product with Serializable

  object TerminalLineEntry {

    sealed trait Command extends TerminalLineEntry
    sealed trait CommandOutput extends TerminalLineEntry

    case object ListContents extends Command

    sealed trait ListContentsOutput extends CommandOutput
    case class LSDirectory(name: String) extends ListContentsOutput
    case class LSFile(name: String, size: Size) extends ListContentsOutput

    case class ChangeDirectory(target: ChangeDirectory.Target) extends Command

    object ChangeDirectory {
      sealed trait Target extends Product with Serializable

      object Target {
        case object Up extends Target
        case class Path(value: String) extends Target
      }
    }

  }

  import FileSystemObject._
  import TerminalLineEntry.ChangeDirectory.Target._
  import TerminalLineEntry._

  case class CommandWithOutput(cmd: Command, outputReversed: List[CommandOutput] = Nil) {
    def output: List[CommandOutput] = outputReversed.reverse
    def addOutput(o: CommandOutput): CommandWithOutput = copy(outputReversed = o :: outputReversed)
  }

  def solvePart1(input: List[TerminalLineEntry], sizeLimit: Size): Size =
    discoverDirectoryStructure(input)
      .collectAllSubdirectorySizes(_ <= sizeLimit)
      .sum

  def solvePart2(input: List[TerminalLineEntry], totalSpace: Size, requiredFreeSpace: Size): Size = {
    val root = discoverDirectoryStructure(input)
    val freeSpace = totalSpace - root.calculatedSize
    val spaceToFree = requiredFreeSpace - freeSpace

    root
      .collectAllSubdirectorySizes(spaceToFree <= _)
      .min
  }

  private def discoverDirectoryStructure(input: List[TerminalLineEntry]): Directory = {
    @tailrec
    def unwindStack(stack: List[Directory]): Directory =
      stack match {
        case Nil => throw new RuntimeException("Unexpected stack state: stack is empty")
        case head :: Nil => head
        case top :: second :: tail => unwindStack(second.updateContents(top.name, top) :: tail)
      }

    val grouped = groupCommandsWithOutputs(input)
    val remainingStack = grouped.foldLeft(List.empty[Directory])(processCommand)

    unwindStack(remainingStack)
  }

  private def groupCommandsWithOutputs(input: List[TerminalLineEntry]): List[CommandWithOutput] =
    input.foldLeft(List.empty[CommandWithOutput]) {
      case (acc, cmd: Command) => CommandWithOutput(cmd) :: acc
      case (head :: tail, out: CommandOutput) => head.addOutput(out) :: tail
      case (acc, _: CommandOutput) => acc
    }.reverse

  private def processCommand(stack: List[Directory], command: CommandWithOutput): List[Directory] =
    (command.cmd, stack) match {
      case (ChangeDirectory(Up), top :: second :: tail) => second.updateContents(top.name, top) :: tail
      case (ChangeDirectory(Path("/")), _) => List(Directory("/"))
      case (ChangeDirectory(Path(directoryName)), head :: _) =>
        head.findDirectory(directoryName) match {
          case None =>
            throw new RuntimeException(s"Directory '${head.name}' doesn't contain a directory name '$directoryName' to go to")
          case Some(nextDirectory) => nextDirectory :: stack
        }
      case (ListContents, head :: tail) =>
        val contents = command.output.collect {
          case LSDirectory(name) => Directory(name)
          case LSFile(name, size) => File(name, size)
        }
        head.withContents(contents) :: tail
      case _ => throw new RuntimeException(s"Unexpected command-stack state: $command, $stack")
    }

  private def parseTerminalOutputLine(raw: String): TerminalLineEntry = raw match {
    case "$ ls" => ListContents
    case "$ cd .." => ChangeDirectory(ChangeDirectory.Target.Up)
    case s"$$ cd $path" => ChangeDirectory(ChangeDirectory.Target.Path(path))
    case s"dir $name" => LSDirectory(name)
    case s"$size $name" => LSFile(name, size.toInt)
  }

  private val sample = Input.asList("day7_sample.txt").map(parseTerminalOutputLine)
  private val input = Input.asList("day7.txt").map(parseTerminalOutputLine)

  private val Pt1SizeLimit = 100000
  private val Pt2TotalSpace = 70000000
  private val Pt2RequiredFreeSpace = 30000000

  println(solvePart1(sample, Pt1SizeLimit)) // 95437
  println(solvePart1(input, Pt1SizeLimit)) // 1367870

  println(solvePart2(sample, Pt2TotalSpace, Pt2RequiredFreeSpace)) // 24933642
  println(solvePart2(input, Pt2TotalSpace, Pt2RequiredFreeSpace)) // 549173

}
