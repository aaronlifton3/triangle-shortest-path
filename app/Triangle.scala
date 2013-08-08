import io.Source

object Triangle {

  class Node(val value: Int) {
    var left: Node = _
    var right: Node = _
    var maxList: List[Int] = null
  }

  var lastLine: Array[Node] = null
  val triangleFile: String = "generated_triangle.txt"

  def parseTriangle(filePath: String): Node = {
    var triangle: Node = null
    Source.fromFile(filePath).getLines.foreach { line =>
      val thisLine = line.trim.split(" ").map((i: String) => new Node(i.toInt))
      if (triangle == null) triangle = thisLine(0)
      else
        thisLine.indices.foreach { i: Int =>
          if (i > 0) lastLine(i - 1).right = thisLine(i)
          if (i < lastLine.size) lastLine(i).left = thisLine(i)
        }
      lastLine = thisLine
    }
    triangle
  }

  val triangle: Node = parseTriangle(triangleFile)

  def sum(list: List[Int]) = list.foldLeft(0)((b, a) => b+a)

  def traverse(node: Node): List[Int] = {
    if (node.maxList != null)
      return node.maxList
    if (node.left == null && node.right == null) {
      List[Int](node.value)
    } else {
      val listLeft = traverse(node.left)
      val listRight = traverse(node.right)
      node.maxList = node.value :: (if (sum(listLeft) > sum(listRight)) listLeft else listRight)
      node.maxList
    }
  }

  def main(args: Array[String]) = {
    val maxList = traverse(triangle)
    val sumPath = sum(maxList)
    println(s"Sum (most weighted path): $sumPath")
    // maxList.foreach(println)
  }

}