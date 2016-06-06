package muscala

/**
  * Created by malig on 6/4/16.
  */
object  MutationTool {


  def main (args: Array[String] ) {
    val conf = new Configuration("conf.txt").loadMapping().enableSparkMutation()
    val inputdir = "../MuExamples/scala-csv-master"
    val pathtosrc = "/src"   // usually /src/main
    val outputdir = "mutatedFiles"
    val ex = new Extractor()
    ex.run(conf,inputdir,pathtosrc, outputdir)
  }
}
