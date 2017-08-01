import scala.io.Source

object telecode extends App{
  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield (ltr -> digit)

  def wordCode(word: String): String =
    word.toUpperCase.map(charCode)

  wordCode("scalaisfun")

  object H {
    val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt").getLines
    val words = in.toList filter (_ forall (_.isLetter))
    val wordsForNum: Map[String, Seq[String]] =
      words groupBy wordCode withDefaultValue List()
  }
  import H._

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue List()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        first <- wordsForNum(number take split)
        rest <- encode(number drop split)

      } yield first :: rest
    }.toSet
  }

  encode("7225247386")
//  println(encode("7225247386"))
//  encode("44")
}
