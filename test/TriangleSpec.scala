import org.scalatest._
import org.scalatest.FlatSpec

class TriangleSpec extends FlatSpec {
  var lastLine: Array[Triangle.Node] = null
  val triangleFile: String = "test_triangle.txt"
  val triangle: Triangle.Node = Triangle.parseTriangle(triangleFile)

  it should "have initial node val of 0" in {
    assert(triangle.value === 0)
  }

  it should "have left child val of 1" in {
    assert(triangle.left.value === 1)
  }

  it should "have right child val of 0" in {
    assert(triangle.right.value === 0)
  }

  it should "have a max path of 3" in {
    val traversedTriangle = Triangle.traverse(triangle)
    val traversedSum = Triangle.sum(traversedTriangle)
    assert(traversedSum === 3)
  }

  it should "find the longest path" in {
    val traversedTriangle = Triangle.traverse(triangle)
    assert(traversedTriangle === List(0,1,1,1))
  }
}
