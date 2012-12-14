object ListSortedBenchmark extends AbBenchmark
with java.io.Serializable {
  type Generated = List[Int]

  def operation = "list.sorted"
  def generated(size: Int) = (0 until size).toList
  def use(generated: Generated) = generated.sorted
}
