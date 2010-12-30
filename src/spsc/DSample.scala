package spsc

object DSample {

  def main(args: Array[String]): Unit = {
    runDeforester("gApp(x, x)", Sample.program1)
    runDeforester("gRev(Cons(x, Cons(y, Nil())))", Sample.program3)
  }

  def runDeforester(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = Deforester.buildTree(target, Nil, program)
    println(sc)
    println()
  }

}