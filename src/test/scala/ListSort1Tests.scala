import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers
import org.lafros.ListSort1

class ListSort1Tests extends ListSort1[Int] with FunSuite with ShouldMatchers {
  def test(ints: List[Int]) {
    sort(ints) should equal(ints.sorted)
  }

  def test(ints: Int*): Unit = test(ints.toList)

  def test(ints: Range): Unit = test(ints.toList)

  test("Nil") {
    test(Nil)
  }

  test("1") {
    test(1)
  }

  test("1, 2") {
    test(1, 2)
  }

  test("2, 1") {
    test(2, 1)
  }

  test("1-3") {
    test(1 to 3)
  }

  test("3-1") {
    test(3 to 1 by -1)
  }

  test("1-4") {
    test(1 to 4)
  }

  test("4-1") {
    test(4 to 1 by -1)
  }

  test("1-5") {
    test(1 to 5)
  }

  test("5-1") {
    test(5 to 1 by -1)
  }

  test("1-6") {
    test(1 to 6)
  }

  test("6-1") {
    test(6 to 1 by -1)
  }

  test("1-7") {
    test(1 to 7)
  }

  test("7-1") {
    test(7 to 1 by -1)
  }

  test("1-8") {
    test(1 to 8)
  }

  test("8-1") {
    test(8 to 1 by -1)
  }

  test("1-9") {
    test(1 to 9)
  }

  test("9-1") {
    test(9 to 1 by -1)
  }

  test("1-10") {
    test(1 to 10)
  }

  test("10-1") {
    test(10 to 1 by -1)
  }

  test("1-11") {
    test(1 to 11)
  }

  test("11-1") {
    test(11 to 1 by -1)
  }

  test("1-12") {
    test(1 to 12)
  }

  test("12-1") {
    test(12 to 1 by -1)
  }

  test("1-13") {
    test(1 to 13)
  }

  test("13-1") {
    test(13 to 1 by -1)
  }

  test("1-14") {
    test(1 to 14)
  }

  test("14-1") {
    test(14 to 1 by -1)
  }

  test("1-15") {
    test(1 to 15)
  }

  test("15-1") {
    test(15 to 1 by -1)
  }
}
