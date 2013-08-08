import io.Source

object Triangle {

  class Node(val value: Int) {
    var left: Option[Node] = _
    var right: Option[Node] = _
    var inverseShortestPath: List[Int] = _
  }

  var tailLine: List[Node] = null
  val triangleFile: String = "generated_triangle.txt"

  def parseTriangle(filePath: String): Node = {
    var head: Node = null
    Source.fromFile(filePath).getLines.foreach { line =>
      val currLine = line.trim.split(" ").map((i: String) => new Node(i.toInt)).toList
      if (head == null) head = currLine(0)
      else
        currLine.indices.foreach { i: Int =>
          if (i > 0) tailLine(i - 1).right = Some(currLine(i))
          if (i < tailLine.size) tailLine(i).left = Some(currLine(i))
        }
      tailLine = currLine
    }
    head
  }

  val head: Node = parseTriangle(triangleFile)

  def sum(list: List[Int]) = list.foldLeft(0)(_+_)

  def traverse(node: Node): List[Int] = {
    if (node.inverseShortestPath != null)
      return node.inverseShortestPath
    if (node.left == null && node.right == null) {
      List[Int](node.value)
    } else {
      val listLeft = traverse(node.left.get)
      val listRight = traverse(node.right.get)
      node.inverseShortestPath = node.value :: (if (sum(listLeft) > sum(listRight)) listLeft else listRight)
      node.inverseShortestPath
    }
  }

  def main(args: Array[String]) = {
    val inverseShortestPath = traverse(head)
    val pathSum = sum(inverseShortestPath)
    println(s"Sum (most weighted path): $pathSum")
    // inverseShortestPath.foreach(println)
  }

}