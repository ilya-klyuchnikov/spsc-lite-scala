package spsc

import Sample._

object SampleM {
  def main(args: Array[String]): Unit = {
    runSuperCompilerM(target1, program1)
    runSuperCompilerM(target2, program2)
    runSuperCompilerM(target3, program3)
    runSuperCompilerM(target4, program4)
    runSuperCompilerM(target5, program5)
    runSuperCompilerM(target6, program6)
    runSuperCompilerM(target7, program7)
    runSuperCompilerM(target8, program8)
  }

  def runSuperCompilerM(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new MultiResultSuperCompiler(program)
    val pts = sc.buildTrees(target)
    val ress = pts map { new ResidualProgramGenerator(_).result }
    
    println()
    println("** runSuperCompilerM **")
    println(target); println(program);
    println(pts.size + " results")

    
    for ((resTerm, resProgram) <- ress) {
      println()
      println("-------")
      println(resTerm); println(resProgram);

    }
  }
}