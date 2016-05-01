package muscala

import muscala.ConstraintObj.BadMatchException

import scala.reflect.runtime.universe._
import scala.tools.reflect.{ToolBox, ToolBoxError}

object Extractor {

  val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  import tb._
  import u._


  def modifyOperator(t: Tree): Select = t match {
    case b@Select(t1, t2) => treeCopy.Select(b, t1, newTermName("$plus"))
    case _ => null
  }


  def parseScalaCode(t: String): Tree = {
    var controlPredicates = List[Tree]()
    try {
      val tree = tb.parse(t)
      val newtree = new Transformer {
        override def transform(tree: Tree): Tree = {
          tree match {
            case f1@Select(id, name) =>
              name match {
                case TermNameTag(a) =>
                  if (a.toString.equals("$minus")) {
                    modifyOperator(f1)
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
      case ex: ToolBoxError => throw new BadMatchException("ToolBox Match ErrorR")
    }
  }

  def extractPreds(fileName: String): Tree = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = try source.mkString finally source.close()
    val transformedtree = parseScalaCode(lines)

    return transformedtree
  }

  def main(args: Array[String]): Unit = {
    val mutated = Extractor.extractPreds("input.scala")
    println(mutated.toString())

  }

}
