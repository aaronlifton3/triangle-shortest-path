import io.Source

object Triangle {

  class Node(val value: Int) {
    var left: Option[Node] = _
    var right: Option[Node] = _
    var inverseShortestPath: List[Int] = _
  }

  val triangleFile: String = "generated_triangle.txt"

  def parseTriangle(filePath: String): Node = {
    var head: Node = null
    var prevLine: List[Node] = List[Node]()
    Source.fromFile(filePath).getLines.foreach { line =>
      val currLine = line.trim.split(" ").map((i: String) => new Node(i.toInt)).toList
      if (currLine.length == 1) head = currLine(0)
      else
        currLine.indices.foreach { i: Int =>
          if (i > 0) prevLine(i - 1).right = Some(currLine(i))
          if (i < prevLine.size) prevLine(i).left = Some(currLine(i))
        }
      prevLine = currLine
    }
    head
  }

  val head: Node = parseTriangle(triangleFile)

  def traverse(node: Node): List[Int] = {
    if (node.inverseShortestPath != null)
      return node.inverseShortestPath
    (node.left, node.right) match {
      case (Some(l: Node), Some(r: Node)) => {
        val listLeft = traverse(l)
        val listRight = traverse(r)
        node.inverseShortestPath = node.value :: (if (listLeft.sum > listRight.sum) listLeft else listRight)
        node.inverseShortestPath
      }
      case _ => List[Int](node.value)
    }
  }

  def main(args: Array[String]) = {
    val inverseShortestPath = traverse(head)
    val pathSum = inverseShortestPath.sum
    println(s"Sum (most weighted path): $pathSum")
    // inverseShortestPath.foreach(println)
  }

}