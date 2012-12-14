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
import org.lafros.ListSort1
/**
 * @author Rob Dickens
 */
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
