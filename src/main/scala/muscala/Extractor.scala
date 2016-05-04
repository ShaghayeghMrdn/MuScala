package muscala

import java.io.File
import scala.io.Source
import org.apache.commons.io.FileUtils
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.tools.reflect.{ ToolBox, ToolBoxError }

object Extractor {

    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

    import tb._
    import u._

    def modifyOperator(t: Tree, conf: Configuration, op: String, cl: Transformer): Select = t match {
        case b @ Select(t1, t2) => treeCopy.Select(b, t1, newTermName(conf.getMutation(op)))
        case _ => null
    }

    def parseScalaCode(content: String, conf: Configuration): List[Tree] = {
        try {
            val tree = tb.parse(content)
            val newTreeList: ListBuffer[Tree] = ListBuffer()
            new Transformer {
                override def transform(tree: Tree): Tree = {
                    tree match {
                        case f1@Select(id, name) =>
                            name match {
                                case TermNameTag(a) =>
                                    if (conf.matchMutationTarget(a.toString)) {
                                        println(a)
                                        f1 match {
                                            case b @ Select(t1, t2) => {
                                                val newTree = this.transform(treeCopy.Select(b, t1, newTermName(conf.getMutation(a.toString))))
                                                newTreeList += newTree
                                                newTree
                                                }
                                            case _ => null
                                        }
                                    } else {
                                        this.transform(tree)
                                    }
                                case _ => this.transform(tree)
                            }
                        case t => this.transform(t)
                    }
                }
            }.transform(tree)
            newTreeList.toList
        } catch {
            case ex: Exception =>
                ex.printStackTrace()
                null
                //throw new BadMatchException("ToolBox Match Error")
        }
    }

    def extractPreds(fileName: String, conf: Configuration): List[Tree] = {
        val source = scala.io.Source.fromFile(fileName)
        val lines = try {
            var str = ""
            for (l <- source.getLines()) {
                if (l.startsWith("package")) {
                    packageMap += (fileName -> l)
                } else
                    str += l + "\n"
            }
            str
        } finally {
            source.close()
        }
        parseScalaCode(lines, conf)
    }

    def saveToFile(dir: String, path: File, code: Tree) = {

        val pack = packageMap.getOrElse(path.getAbsolutePath, "")
        val filepath = dir + "/" + path.getName
        var c = showCode(code).trim()

        if (c(0) == '{' && c(c.length - 1) == '}') {
            c = c.substring(1)
            c = c.substring(0, c.length - 1).trim
        }
        if (c.endsWith("()")) {
            c = c.substring(0, c.length - 2)
        }
        val writer = new java.io.PrintWriter(filepath)
        try
            writer.write(pack + "\n" + c)
        finally writer.close()
    }

    def getRecursiveListOfFiles(dir: File): Array[File] = {
        val these = dir.listFiles
        these.filter(p => p.getName.contains(".scala")) ++ these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
    }

    var packageMap: Map[String, String] = Map[String, String]()

    def main(args: Array[String]): Unit = {
        val conf = new Configuration("conf.txt")
        conf.loadMapping()
        println(conf.targetOp)
        println(conf.mutationMapping)
        val outputdir = "mutatedFiles"
        val inputdir = "calculator-master"
        val targetFiles = inputdir + "/src/main"
        val dir = new File(outputdir)
        if (!dir.exists()) {
            dir.mkdir()
        }

        val count = 0
        for (scalafile <- getRecursiveListOfFiles(new File(targetFiles))) {
            val filename = scalafile.getName
            try {
                println(s"""Starting Mutation on  $filename  """)
                val mutatedList = Extractor.extractPreds(scalafile.getAbsolutePath, conf)
                for (mutated <- mutatedList) {
                    val mutantDir = outputdir + "/mutant_" + filename + "_" + count.toString()
                    FileUtils.copyDirectory(new File(inputdir), new File(mutantDir))
                    saveToFile(mutantDir, scalafile, mutated)
                }
                println(s"""Mutation passed on  $filename  """)
            } catch {
                case e: Exception => {
                    println(e.getMessage())
                    println(s"""Mutation failed on  $filename . Skipping.... """)
                }

            }
        }
    }

}
