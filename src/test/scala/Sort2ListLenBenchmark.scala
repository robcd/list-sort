/*
 * Copyright 2012 Latterfrosken Software Development Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import org.scalameter.api._
import org.lafros.ListSort2
/**
 * @author Rob Dickens
 */
object Sort2ListLenBenchmark extends PerformanceTest with ListSort2[Int] {
  //
  // hangs if use SeparateJvmsExecutor unless use
  // scala.concurrent.ExecutionContext.Implicits.global in ListSort2.scala
  //
  //lazy val executor = SeparateJvmsExecutor(Executor.Warmer.Default(),
  lazy val executor = LocalExecutor(Executor.Warmer.Default(),
                                           Aggregator.min,
                                           new Measurer.Default)
  lazy val reporter = new LoggingReporter
  lazy val persistor = Persistor.None

  val gen = {
    val sizes = Gen.range("size")(30000, 150000, 30000)
    for {
      size <- sizes                       // org.scalameter.Gen[Int]
    } yield ((0 until size).toList, size) // org.scalameter.Gen[(List[Int], Int)]
  }

  performance of "sort(list, len)" in {
    using(gen) in {
      case (list, size) => sort(list, size)
    }
  }
}

