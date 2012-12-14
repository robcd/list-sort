object ArraySortedBenchmark extends AbBenchmark
with java.io.Serializable {
  type Generated = Array[Int]

  def operation = "array.sorted"
  def generated(size: Int) = (0 until size).toArray
  def use(generated: Generated) = generated.sorted
}
