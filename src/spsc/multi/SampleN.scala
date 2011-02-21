package spsc.multi

import spsc.Sample._
import spsc._

object SampleN {
  def main(args: Array[String]): Unit = {
    runSuperCompiler(target1, program1)
    runSuperCompiler(target2, program2)
    runSuperCompiler(target3, program3)
    runSuperCompiler(target4, program4)
    runSuperCompiler(target5, program5)
    runSuperCompiler(target6, program6)
    runSuperCompiler(target7, program7)
    runSuperCompiler(target8, program8)
  }

  def runSuperCompiler(targetText: String, programText: String) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new SuperCompiler(program)
    val pt = sc.buildProcessTree(target)
    val resTerm = new NGenerator(pt).result
    println("** runSuperCompiler **")
    println(target); println(program);
    println()
    println(resTerm);
    println("-------")
  }
}