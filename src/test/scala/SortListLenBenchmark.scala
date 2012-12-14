import org.lafros.ListSort1

object SortListLenBenchmark extends AbBenchmark
with ListSort1[Int] with java.io.Serializable {
  type Generated = (List[Int], Int)

  def operation = "sort(list, len)"
  def generated(size: Int) = ((0 until size).toList, size)
  def use(generated: Generated) {
    val (list, size) = generated
    sort(list, size)
  }
}
