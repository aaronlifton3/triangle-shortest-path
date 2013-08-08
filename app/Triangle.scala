import io.Source

object Triangle {

  class Node(val value: Int) {
    var left: Node = _
    var right: Node = _
    var inverseShortestPath: List[Int] = null
  }

  var tailLine: List[Node] = null
  val triangleFile: String = "generated_triangle.txt"

  def parseTriangle(filePath: String): Node = {
    var triangle: Node = null
    Source.fromFile(filePath).getLines.foreach { line =>
      val currLine = line.trim.split(" ").map((i: String) => new Node(i.toInt)).toList
      if (triangle == null) triangle = currLine(0)
      else
        currLine.indices.foreach { i: Int =>
          if (i > 0) tailLine(i - 1).right = currLine(i)
          if (i < tailLine.size) tailLine(i).left = currLine(i)
        }
      tailLine = currLine
    }
    triangle
  }

  val triangle: Node = parseTriangle(triangleFile)

  def sum(list: List[Int]) = list.foldLeft(0)((b, a) => b+a)

  def traverse(node: Node): List[Int] = {
    if (node.inverseShortestPath != null)
      return node.inverseShortestPath
    if (node.left == null && node.right == null) {
      List[Int](node.value)
    } else {
      val listLeft = traverse(node.left)
      val listRight = traverse(node.right)
      node.inverseShortestPath = node.value :: (if (sum(listLeft) > sum(listRight)) listLeft else listRight)
      node.inverseShortestPath
    }
  }

  def main(args: Array[String]) = {
    val inverseShortestPath = traverse(triangle)
    val sumPath = sum(inverseShortestPath)
    println(s"Sum (most weighted path): $sumPath")
    // inverseShortestPath.foreach(println)
  }

}