import org.scalameter.api._

abstract class AbBenchmark extends PerformanceTest {
  // use separate JVM instance, with 2GB heap
  lazy val executor = SeparateJvmsExecutor(Executor.Warmer.Default(),
                                           Aggregator.min, // use minimum running time / size
                                           new Measurer.Default) // fixed number of mmnts / size
  lazy val reporter = new LoggingReporter // print to console
  lazy val persistor = Persistor.None // don't store

  type Generated

  def generated(size: Int): Generated
  def use(generated: Generated)

  lazy val gen = {
    val sizes = Gen.range("size")(30000, 150000, 30000)
    //        ^^^^^^^^^
    // predefined 'basic' (cf. 'composed') generator, that generates the Ints that would be
    // contained in a Range constructed using the specified values; here, from 300k to 1500k, in
    // steps of 300k; "size": all basic generators require a name, which should describe each Int
    // generated

    for {
      size <- sizes               // org.scalameter.Gen[Int]
    //} yield ((0 until size).toList, size) // org.scalameter.Gen[(List, size)]

    } yield generated(size) // org.scalameter.Gen[(List, size)]
  }

  // hence, each of the following Ranges in turn will be substituted for `r` below:
  //
  // 0--3k
  // 0--6k
  // 0--9k
  // 0-12k
  // 0-15k

  performance of "List" in {
    measure method "sort" in {
      using(gen) in use
    }
  }
}
