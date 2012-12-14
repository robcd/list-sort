import org.lafros.ListSort1

object SortListBenchmark extends AbBenchmark
with ListSort1[Int] with java.io.Serializable {
  type Generated = List[Int]

  def generated(size: Int) = (0 until size).toList
  def use(generated: Generated) = sort(generated)
}
