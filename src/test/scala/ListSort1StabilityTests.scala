import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers
import org.lafros.ListSort1

class ListSort1StabilityTests extends ListSort1[(Int, Int)] with FunSuite with ShouldMatchers {
  type Pair = (Int, Int)

  implicit val basedOn1stFieldOnly = new math.Ordering[Pair] {
    def compare(p1: Pair, p2: Pair) =
      if (p1._1 < p2._1) -1
      else if (p1._1 > p2._1) 1
      else 0
  }

  def test(pairs: List[Pair]) {
    val before = pairs map (_._2)
    //
    // built-in sort is stable - all tests then passed
    //val after = pairs.sorted map (_._2)
    val after = sort(pairs)(basedOn1stFieldOnly) map (_._2)
    after should equal(before)
  }

  def test(pairs: Pair*): Unit = test(pairs.toList)

  test("(1, 1), (1, 2)") {
    test((1, 1), (1, 2))
  }

  test("(1, 1), (1, 2), (1, 3)") {
    test((1, 1), (1, 2), (1, 3))
  }

  test("(1, 1), (1, 2), (1, 3), (1, 4)") {
    test((1, 1), (1, 2), (1, 3), (1, 4))
  }

  test("(1, 1), (1, 2), (1, 3), (1, 4), (1, 5)") {
    test((1, 1), (1, 2), (1, 3), (1, 4), (1, 5))
  }

  test("(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6)") {
    test((1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6))
  }

  test("(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)") {
    test((1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7))
  }

  test("(1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8)") {
    test((1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8))
  }
}
