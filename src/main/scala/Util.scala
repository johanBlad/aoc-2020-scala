import scala.util.Try
import scala.io.Source
import java.io.FileNotFoundException

object Util {
  def readFileTry(resourcePath: String) =
    Try(Source.fromResource(resourcePath).getLines)
      .recover(throw new FileNotFoundException(resourcePath))
      .isSuccess


  def readFile(resourcePath: String): Iterator[String] =
    Source.fromResource(resourcePath).getLines

  def readInputInt(input: Int): Vector[Int] = {
    val fileInput: Iterator[String] = readFile(s"$input.in")
    fileInput.map(e => e.toIntOption).toVector.flatten
  }
}
