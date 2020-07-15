package learning.algotithm.one

import learning.algotithm.one.unionfind._

trait Main {
  def execute(): Unit
}
object MainApp extends App {

  def clearScreen(): Unit = print("\u001b[2J")

  lazy val algoFactory: Map[Int, () => Unit] = Map(
    0 -> (() => {
    clearScreen()
    println("Exiting the program...")}),
    1 -> _001_QuickFind.execute,
    2 -> _002_QuickUnion.execute,
    3 -> _003_WeightedQuickUnion.execute
  )

  def cmdInterface(): Int = {
    println(
      """
        |
        |Please select algorithm from the list below to execute and observe the sequence of operations which take place
        |
        |0. Exit
        |1. Quick Find
        |2. Quick Union
        |3. Weighted Quick Union
        |
        |example: 1
        |""".stripMargin
    )
    scala.io.StdIn.readInt()
  }

  def loop(): Unit =
    algoFactory
      .get(cmdInterface())
      .fold({
        clearScreen()
        println(
          Console.RED + "Not a valid entry, please try again" + Console.RESET
        )
        loop()
      })(_())

  loop()
}
