import org.scalatest._
import org.scalatest.FlatSpec

class TriangleSpec extends FlatSpec {
  var lastLine: List[Triangle.Node] = null
  val triangleFile: String = "test_triangle.txt"
  val triangle: Triangle.Node = Triangle.parseTriangle(triangleFile)

  it should "have correct initial node value" in {
    assert(triangle.value === 0)
  }

  it should "have correct left child value" in {
    assert(triangle.left.get.value === 1)
  }

  it should "have correct right child value" in {
    assert(triangle.right.get.value === 0)
  }

  it should "have correct max path value" in {
    val traversedTriangle = Triangle.traverse(triangle)
    val traversedSum = traversedTriangle.sum
    assert(traversedSum === 3)
  }

  it should "find the correct route for the longest path" in {
    val traversedTriangle = Triangle.traverse(triangle)
    assert(traversedTriangle === List(0,1,1,1))
  }
}
