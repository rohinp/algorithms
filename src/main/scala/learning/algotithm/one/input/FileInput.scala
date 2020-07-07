package learning.algotithm.one.input

import scala.io.Source

object FileInput {
  val objectTuple: Iterator[(Int, Int)] =
    Source
      .fromResource("tinyUF.txt")
      .getLines
      .map(
        _.split(" ")
          .map(_.toInt)
          .toList
      )
      .map({
        case a :: b :: _ => (a, b)
      })
}
