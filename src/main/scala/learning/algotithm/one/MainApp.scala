package learning.algotithm.one

import learning.algotithm.one.unionfind.{_000_QuickFind, _001_QuickFind}

trait Main {
  def execute():Unit
}
object MainApp extends App {
  val programToRun:Main = _001_QuickFind
  programToRun.execute()
}
