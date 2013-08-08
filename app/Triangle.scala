import io.Source

object Triangle {

  class Node(val value: Int) {
    var left: Option[Node] = _
    var right: Option[Node] = _
    var longestPath: List[Int] = List[Int]()
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
    if (node.longestPath.length > 0) return node.longestPath
    (node.left, node.right) match {
      case (Some(l: Node), Some(r: Node)) => {
        val listLeft = traverse(l)
        val listRight = traverse(r)
        node.longestPath = node.value :: (if (listLeft.sum > listRight.sum) listLeft else listRight)
        node.longestPath
      }
      case _ => List[Int](node.value)
    }
  }

  def main(args: Array[String]) = {
    val longestPath = traverse(head)
    val pathSum = longestPath.sum
    println(s"Sum (most weighted path): $pathSum")
    // longestPath.foreach(println)
  }

}