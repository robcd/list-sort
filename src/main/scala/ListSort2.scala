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
import concurrent.{Await, duration, ExecutionContext, future, Future, Promise}
import duration.Duration
//import ExecutionContext.Implicits.global
/**
 * as per ListSort1 except that attempts to introduce parallisation, using Futures.
 *
 * @author Rob Dickens
 */
trait ListSort2[A] extends ListSort[A] {
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
      //
      // Sort2ListLenBenchmark.scala hangs if it uses SeparateJmvsExecutor unless replace ec
      // with the following imported value
      //
      //import ExecutionContext.Implicits.global
      implicit val ec = {
        import java.util.concurrent.Executors
        val exor = Executors.newSingleThreadExecutor
        //val exor = Executors.newFixedThreadPool(2)
        //val exor = Executors.newCachedThreadPool
        ExecutionContext.fromExecutor(exor)
      }
      @annotation.tailrec
      def sortEachPowOf2(as: As, len: Int, iSubseq: Int, prevLowToHigh: Boolean,
                         res: List[Future[As]]): List[Future[As]] = if (as.isEmpty) res else {
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

    val as1 = Await.result(ass.head, Duration.Inf)
    merge(as1, ass.tail, subseqOrders.nSubseqs%2 == 0)
  }

  def merge(as1: As, ass: List[Future[As]], lowToHigh: Boolean)
           (implicit ordering: math.Ordering[A]): As = if (ass.isEmpty) as1 else {
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
    val as2 = Await.result(ass.head, Duration.Inf)
    merge(mergePairOfSortedAs(as1, as2, Nil), ass.tail, !lowToHigh)
  }

  def sort1stPowOf2(as: As, len: Int, lowToHigh: Boolean)
                   (implicit ordering: math.Ordering[A],
                    executionContext: ExecutionContext): (Future[As], As) =
    if (len <= 256) {
      val (res, as1) = sort1stLowPowOf2(as, len, lowToHigh)(ordering)
      (Promise.successful(res).future, as1)
    }
    else {
      //println("len: "+ len)
      val half = len/2
      val (fres1, as1) = sort1stPowOf2(as,  half, !lowToHigh) // can proceed in parallel with
                                                              // mergePairOfSortedSubseqs
      val (fres2, as2) = sort1stPowOf2(as1, half, !lowToHigh)
      val fres3 = for {
        res1 <- fres1
        res2 <- fres2
      } yield {
        val (res3, _) = mergePairOfSortedSubseqs(res1, res2, half, lowToHigh)
        res3
      }
      (fres3, as2)
    }

  def sort1stLowPowOf2(as: As, len: Int, lowToHigh: Boolean)
                      (implicit ordering: math.Ordering[A]): (As, As) = len match {
    case 1 => (as, Nil)
    case 2 => mergePairOfSortedSubseqs(as, as.tail, 1, lowToHigh)
    case _ =>
      //println("len: "+ len)
      val half = len/2
    val (res1, as1) = sort1stLowPowOf2(as,  half, !lowToHigh)
    val (res2, as2) = sort1stLowPowOf2(as1, half, !lowToHigh)
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
