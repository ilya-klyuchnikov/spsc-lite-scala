package spsc.multi

import spsc.Sample._
import spsc._

object SampleM {
  val printResults = true

  def main(args: Array[String]): Unit = {

    runSuperCompilerM(target1, program1, SimpleWhistle)
    runSuperCompilerM(target1, program1, HEWhistle)
    runSuperCompilerM(target1, program1, HEWithRedexWhistle)
    runSuperCompilerM(target1, program1, HEByCouplingWhistle)
    runSuperCompilerM(target1, program1, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target2, program2, SimpleWhistle)
    runSuperCompilerM(target2, program2, HEWhistle)
    runSuperCompilerM(target2, program2, HEWithRedexWhistle)
    runSuperCompilerM(target2, program2, HEByCouplingWhistle)
    runSuperCompilerM(target2, program2, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target3, program3, SimpleWhistle)
    runSuperCompilerM(target3, program3, HEWhistle)
    runSuperCompilerM(target3, program3, HEWithRedexWhistle)
    runSuperCompilerM(target3, program3, HEByCouplingWhistle)
    runSuperCompilerM(target3, program3, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target4, program4, SimpleWhistle)
    runSuperCompilerM(target4, program4, HEWhistle)
    runSuperCompilerM(target4, program4, HEWithRedexWhistle)
    runSuperCompilerM(target4, program4, HEByCouplingWhistle)
    runSuperCompilerM(target4, program4, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target5, program5, SimpleWhistle)
    runSuperCompilerM(target5, program5, HEWhistle)
    runSuperCompilerM(target5, program5, HEWithRedexWhistle)
    runSuperCompilerM(target5, program5, HEByCouplingWhistle)
    runSuperCompilerM(target5, program5, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target6, program6, SimpleWhistle)
    runSuperCompilerM(target6, program6, HEWhistle)
    runSuperCompilerM(target6, program6, HEWithRedexWhistle)
    runSuperCompilerM(target6, program6, HEByCouplingWhistle)
    runSuperCompilerM(target6, program6, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target7, program7, SimpleWhistle)
    runSuperCompilerM(target7, program7, HEWhistle)
    runSuperCompilerM(target7, program7, HEWithRedexWhistle)
    runSuperCompilerM(target7, program7, HEByCouplingWhistle)
    runSuperCompilerM(target7, program7, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")

    runSuperCompilerM(target8, program8, SimpleWhistle)
    runSuperCompilerM(target8, program8, HEWhistle)
    runSuperCompilerM(target8, program8, HEWithRedexWhistle)
    runSuperCompilerM(target8, program8, HEByCouplingWhistle)
    runSuperCompilerM(target8, program8, HEByCouplingWithRedexWhistle)

  }

  def runSuperCompilerM(targetText: String, programText: String, whistle: Whistle) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new MultiResultSuperCompiler(whistle, program)

    println()
    println("** runSuperCompilerM with " + whistle.name + " **")
    println(target); println(program);

    val pts = sc.buildTrees(target)
    val ress = (pts map { x => new NGenerator(x).result } distinct) sortWith { _.size < _.size }

    println(pts.size + " trees")
    println(ress.size + " programs")

    if (printResults) {
      for (resTerm <- ress) {
        println()
        println("-------")
        println(resTerm);
      }
    }
  }
}