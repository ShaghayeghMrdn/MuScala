package muscala

/**
  * Created by malig on 5/3/16.
  */
class Configuration(filename: String) {
  val operatorMap: Map[String, String] = Map[String, String](
    "&&" -> "$amp$amp",
    "+" -> "$plus",
    "/" -> "$div",
    "*" -> "$times",
    "-" -> "$minus",
    "<" -> "$less",
    ">" -> "$greater",
    ">=" -> "$greater$eq",
    "<=" -> "$less$eq",
    "==" -> "$eq$eq",
    "!=" -> "$bang$eq"
  )
  var inverseOpMap : Map[String, String] = Map[String, String]()
  for(k <- operatorMap.keySet){
    inverseOpMap += (operatorMap(k) -> k)
  }
  var targetOp: List[String] = List()
  var mutationMapping: Map[String, String] = Map[String, String]()

  def matchMutationTarget(s: String): Boolean = {
 //   println(s)
        targetOp.contains(s)

  }

  def loadMapping(): Unit = {
    val source = scala.io.Source.fromFile(filename)
    try {
      val iter = source.getLines()
      for (a <- iter) {
        if (a.startsWith("TARGETOPERATORS=")) {
          val st = a.replaceFirst("TARGETOPERATORS=", "")
          targetOp = st.trim.filter(a => operatorMap.keySet.contains(a.toString)).map(a => operatorMap(a.toString)).toList
        } else if (a.startsWith("MUTATIONMAPPING=")) {
          val st = a.replaceFirst("MUTATIONMAPPING=", "")
          for (e <- st.trim.split(",")) {
            val op = e.split("->")(0).trim
            val t_op = e.split("->")(1).trim
            if (!operatorMap.keySet.contains(op) || !operatorMap.keySet.contains(t_op)) {
              println("Invalid mappings")
            } else {
              mutationMapping += (op -> t_op)
            }
          }
        }
      }
    } finally source.close()
  }

  def getMutation(s: String): String = {
    val b = operatorMap(mutationMapping(inverseOpMap(s)))
   // println(s""" $s -> $b """)
    b
  }
}
