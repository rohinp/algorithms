package learning.algotithm.one

import learning.algotithm.one.unionfind.{UF_v1, UF_v2}

trait Main {
  def execute():Unit
}
object MainApp extends App {
  val programToRun:Main = UF_v2
  programToRun.execute()
}
