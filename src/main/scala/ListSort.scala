package org.lafros
/**
 * API trait, to be extended by each implementation (thereby allowing its own API to remain
 * hidden). */
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
