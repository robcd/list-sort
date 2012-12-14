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
package org.lafros
/**
 * API trait, to be extended by each implementation (thereby allowing its own API to remain
 * hidden).
 *
 * @author Rob Dickens
 */
trait ListSort[A] {
  type As = List[A]
  /**
   * convenience, for use where the length of `as` in not available separately. */
  def sort(as: As)(implicit ordering: math.Ordering[A]): As = sort(as, as.size)(ordering)
  /*
   * @param len length of `as`, which the impl'n should not obtain itself, since only
   * constant-time list-operations should be performed. */
  def sort(as: As, len: Int)(implicit ordering: math.Ordering[A]): As
}
