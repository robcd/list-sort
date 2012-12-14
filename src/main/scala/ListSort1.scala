package org.lafros
/**
 * sorts the immutable.List by using only further immutable.Lists, rather than by copying the
 * elements to a mutable array (as is done by the built-in sort methods). Sorting is done by
 * first recognising that the number of elements in the list may be reduced to a sum of factors
 * which are decreasing powers of 2; e.g. 15 = 8 + 4 + 2 + 1. The List to be sorted is then
 * considered to be the concatenation of subsequences of these lengths, and is sorted in two
 * stages:
 *  1. each subsequence is sorted and prepended to a List
 *  2. each element of the tail of that list is then merged with the head
 *
 * Each subsequence in stage 1. is sorted by repeatedly considering it to be the concatenation
 * of pairs of sorted subsequences, initially of length, 1, which are then merged.
 */
trait ListSort1[A] extends ListSort[A] {
  /**
   * In stage 1., each subsequence must be sorted into either low-to-high or high-to-low order,
   * as determined by one of these */
  sealed trait SubseqOrders {
    def nSubseqs: Int
    def first: Boolean
    def last: Boolean
    def lowToHigh(iSubseq: Int, prev: Boolean): Boolean =
      if (iSubseq == 0) first
      else if (nSubseqs - iSubseq == 1) last
      else !prev
  }
  object SubseqOrders {
    def apply(len: Int) = {
      val n1s = Integer.bitCount(len)
                                       // required orders of subsequences
      if (n1s == 1)        One         // <
      else if (n1s%2 == 0) Even(n1s)   // >(<>...<>)>
      else                 OddGt1(n1s) // >(<>...<>)<<
    }
  }
  /**
   * If there is only One subsequence, is should have low-to-high ordering:
   * < */
  case object One extends SubseqOrders {
    val nSubseqs = 1
    val first = true
    val last = true
  }
  /**
   * An even number of subsequences should have the following orders (which will be encountered
   * in reverse order when traversing the list of sorted lists to be merged):
   * >(<>...<>)> */
  case class Even(nSubseqs: Int) extends SubseqOrders {
    val first = false
    val last = false
  }
  /**
   * An odd number of subsequences (> 1) should have the following orders (which will be
   * encountered in reverse order when traversing the list of sorted lists to be merged):
   * >(<>...<>)<< */
  case class OddGt1(nSubseqs: Int) extends SubseqOrders {
    val first = false
    val last = true
  }

  def sort(as: As, len: Int)(implicit ordering: math.Ordering[A]): As = if (len < 2) as else {
    val subseqOrders = SubseqOrders(len)
    //println("subseqOrders: "+ subseqOrders)

    val ass = {
      @annotation.tailrec
      def sortEachPowOf2(as: As, len: Int, iSubseq: Int, prevLowToHigh: Boolean,
                         res: List[As]): List[As] = if (as.isEmpty) res else {
        val sortLen = {
          import math._
          val index = (log(len)/log(2)).toInt
          pow(2, index).toInt
        }
        val lowToHigh = subseqOrders.lowToHigh(iSubseq, prevLowToHigh)
        val (sorted, rem) = sort1stPowOf2(as, sortLen, lowToHigh)
        sortEachPowOf2(rem, len - sortLen, iSubseq + 1, lowToHigh, sorted::res)
      }
      sortEachPowOf2(as, len, 0, false, Nil)
    }
    //println("ass: "+ ass)

    merge(ass.head, ass.tail, subseqOrders.nSubseqs%2 == 0)
  }

  def merge(as: As, ass: List[As], lowToHigh: Boolean)
           (implicit ordering: math.Ordering[A]): As = if (ass.isEmpty) as else {
    @annotation.tailrec
    def mergePairOfSortedAs(as1: As, as2: As, res: As): As = {
      //println("as1: "+ as1 +" as2: "+ as2 +" res: "+ res)
      (as1, as2) match {
        case (Nil, Nil) => res
        case (Nil, hd::tl) => mergePairOfSortedAs(as1, tl, hd::res)
        case (hd::tl, Nil) => mergePairOfSortedAs(tl, as2, hd::res)
        case (hd1::tl1, hd2::tl2) =>
          if (ordering.lt(hd1, hd2) == lowToHigh) // ensures stability
             mergePairOfSortedAs(as1, tl2, hd2::res)
        else mergePairOfSortedAs(tl1, as2, hd1::res)
      }
    }
    merge(mergePairOfSortedAs(as, ass.head, Nil), ass.tail, !lowToHigh)
  }

  def sort1stPowOf2(as: As, len: Int, lowToHigh: Boolean)
                   (implicit ordering: math.Ordering[A]): (As, As) = len match {
    case 1 => (as, Nil)
    case 2 => mergePairOfSortedSubseqs(as, as.tail, 1, lowToHigh)
    case _ =>
      //println("len: "+ len)
      val half = len/2
    val (res1, as1) = sort1stPowOf2(as,  half, !lowToHigh)
    val (res2, as2) = sort1stPowOf2(as1, half, !lowToHigh)
    val (res3, _) = mergePairOfSortedSubseqs(res1, res2, half, lowToHigh)
    (res3, as2)
  }

  def mergePairOfSortedSubseqs(as1: As, as2: As, subLen: Int, lowToHigh: Boolean)
                              (implicit ordering: math.Ordering[A]): (As, As) = {
    @annotation.tailrec
    def mergePairOfSortedSubseqs(as1: As, as2: As, n1: Int, n2: Int, res: As): (As, As) = {
      // println("as1: "+ as1 +" as2: "+ as2 +" n1: "+ n1 +" n2: "+ n2
      //         +" lowToHigh: "+ lowToHigh +" res: "+ res)
      if (n1 == 0) {
        if (n2 == 0) (res, as2)
        else mergePairOfSortedSubseqs(as1, as2.tail, n1, n2 - 1, as2.head::res)
      }
      else if (n2 == 0)
        mergePairOfSortedSubseqs(as1.tail, as2, n1 - 1, n2, as1.head::res)
      else {
        val (hd1::tl1, hd2::tl2) = (as1, as2)
        if (ordering.gt(hd1, hd2) == lowToHigh) // ensures stability
          mergePairOfSortedSubseqs(tl1, as2, n1 - 1, n2, hd1::res)
        else
          mergePairOfSortedSubseqs(as1, tl2, n1, n2 - 1, hd2::res)
      }
    }
    mergePairOfSortedSubseqs(as1, as2, subLen, subLen, Nil)
  }
}
