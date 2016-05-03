package muscala

import java.io.File

import muscala.ConstraintObj.BadMatchException

import scala.reflect.runtime.universe._
import scala.tools.reflect.{ToolBox, ToolBoxError}

object Extractor {

  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  import tb._
  import u._


  def modifyOperator(t: Tree, conf: Configuration, op: String, cl: Transformer): Select = t match {
    case b@Select(t1, t2) => treeCopy.Select(b, t1, newTermName(conf.getMutation(op)))
    case _ => null
  }


  def parseScalaCode(t: String, conf: Configuration): Tree = {
    try {
      val tree = tb.parse(t)
      val newtree = new Transformer {
        override def transform(tree: Tree): Tree = {
          tree match {
            case f1@Select(id, name) =>
              name match {
                case TermNameTag(a) =>
                  if (conf.matchMutationTarget(a.toString)) {
                    println(a.toString)
                    f1 match {
                      case b@Select(t1, t2) => super.transform(treeCopy.Select(b, t1, newTermName(conf.getMutation(a.toString))))
                      case _ => null
                    }

                  }
                  else {
                    super.transform(tree)
                  }
                case _ => super.transform(tree)
              }
            case t => super.transform(t)
          }
        }
      }.transform(tree)
      newtree
    }
    catch {
      case ex: Exception =>
        ex.printStackTrace()
        throw new BadMatchException("ToolBox Match Error")
    }
  }

  def extractPreds(fileName: String, conf: Configuration): Tree = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = try source.mkString finally source.close()
    val transformedtree = parseScalaCode(lines, conf)
    return transformedtree
  }

  def saveToFile(path: String, code: Tree) = {
    val writer = new java.io.PrintWriter(path)
    try writer.write(showCode(code))
    finally writer.close()
  }

  def getRecursiveListOfFiles(dir: File): Array[File] = {
    val these = dir.listFiles
    these.filter(p => p.getName.contains(".scala")) ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  def main(args: Array[String]): Unit = {

    val conf = new Configuration("conf.txt")
    conf.loadMapping()
    println(conf.targetOp)
    println(conf.mutationMapping)
    val outputdir = "mutatedFiles"
    val inputdir = "."
    val dir = new File(outputdir)
    if (!dir.exists()) {
      dir.mkdir()
    }
    for (scalafile <- getRecursiveListOfFiles(new File(inputdir))) {
      val filename = scalafile.getName
      try {
        println(s"""Starting Mutation on  $filename  """)
        val mutated = Extractor.extractPreds(scalafile.getAbsolutePath, conf)
        saveToFile(outputdir + "/" + scalafile.getName, mutated)
        println(s"""Mutation passed on  $filename  """)
      } catch {
        case e: Exception =>
          println(s"""Mutation failed on  $filename . Skipping.... """)

      }
    }

  }

}
