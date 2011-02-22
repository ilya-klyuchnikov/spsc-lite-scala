package spsc.multi

import spsc.Sample._
import spsc._

object SampleM {
  def main(args: Array[String]): Unit = {
    runSuperCompilerM(target1, program1, Whistles.simple)
    runSuperCompilerM(target1, program1, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target2, program2, Whistles.simple)
    runSuperCompilerM(target2, program2, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target3, program3, Whistles.simple)
    runSuperCompilerM(target3, program3, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    
    runSuperCompilerM(target4, program4, Whistles.simple)
    runSuperCompilerM(target4, program4, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target5, program5, Whistles.simple)
    runSuperCompilerM(target5, program5, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target6, program6, Whistles.simple)
    runSuperCompilerM(target6, program6, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target7, program7, Whistles.simple)
    runSuperCompilerM(target7, program7, Whistles.he)
    
    println("<<<<<<<<<<<")
    
    runSuperCompilerM(target8, program8, Whistles.simple)
    runSuperCompilerM(target8, program8, Whistles.he)
    
  }

  def runSuperCompilerM(targetText: String, programText: String, whistle: (Tree, Node) => Boolean) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new MultiResultSuperCompiler(whistle, program)

    println()
    println("** runSuperCompilerM **")
    println(target); println(program);

    val pts = sc.buildTrees(target)
    val ress = pts map { x => new NGenerator(x).result } distinct

    println(pts.size + " trees")
    println(ress.size + " programs")

    /*
    for (resTerm <- ress) {
      println()
      println("-------")
      println(resTerm);

    }*/
  }
}