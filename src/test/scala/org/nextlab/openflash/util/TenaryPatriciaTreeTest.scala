package org.nextlab.openflash.util

import org.scalatest.funsuite.AnyFunSuite

class TptFunSuite extends AnyFunSuite {

  def getSegment[V](segmentized: Segmentized[V]): Segment = {
    while ((segmentized.hasNext)) {
      segmentized.proceed()
    }
    segmentized.current
  }

  test("The segment operations") {
    val str1 = "*****"
    val str2 = "0*1**"
    val str3 = "***1*"
    val str4 = "**11*"
    val str5 = "**00*"
    val str6 = "**"

    implicit val segmentize: String => Segmentized[String] = new StringSegmentizer[String](_, 4)

    val seg1 = getSegment(str1)
    val seg2 = getSegment(str2)
    val seg3 = getSegment(str3)
    val seg4 = getSegment(str4)
    val seg5 = getSegment(str5)
    val seg6 = getSegment(str6)

    assert(seg1 == seg6)
    assert(seg2 < seg1)
    assert(seg3 < seg1)
    assert(seg4 < seg3)
    assert(seg5 ~ seg6)
    assert(seg5 < seg6)
    assert((seg4 & seg5) == seg6)
    assert(seg6 == getSegment(segmentize(str6).assertNext(0, 0)))
  }

  test("The tenary patricia tree should have the right structure") {

    implicit val segmentize: String => Segmentized[String] = new StringSegmentizer[String](_, 3)

    val tpt = new TenaryPatriciaTree[String, Set[String]](3)

    val str0 = "**"
    val str1 = "*****"
    val str2 = "0*1**"
    val str3 = "***1*"
    val str4 = "**11*"
    val str5 = "**00*"

    tpt += str0
    tpt += str1
    tpt.dump()
    println("Testing searching for " + str2)
    println(tpt ? (str2))
    assert((tpt ? (str2)).impl == Set(str0, str1))
    println("Testing insertion of " + str2)
    tpt += str2
    tpt.dump()
    println("Testing searching for " + str3)
    println(tpt ? (str3))
    assert((tpt ? (str3)).impl == Set(str0, str1, str2))
    println("Testing insertion of " + str3)
    tpt += str3
    tpt.dump()
    println("Testing searching for " + str4)
    println(tpt ? (str4))
    assert((tpt ? (str4)).impl == Set(str0, str1, str2, str3))
    println("Testing insertion of " + str3)
    tpt += str4
    tpt.dump()
    println("Testing searching for " + str5)
    println(tpt ? (str5))
    assert((tpt ? (str5)).impl == Set(str0, str1, str3))
  }

  test("TPT with sorted set") {
    import scala.collection.mutable.SortedSet

    type V = (Int, String)
    type T = SortedSet[V]

    implicit val ordering = Ordering.by[V, Int](_._1)

    val tpt = new TenaryPatriciaTree(3, new SortedSetHandles[V](true))

    val p0 = (1, "**")
    val p1 = (13, "*****")
    val p2 = (5, "0*1**")
    val p3 = (3, "***1*")
    val p4 = (6, "**11*")
    val p5 = (9, "**00*")

    implicit val segmentize: V => Segmentized[V] = x => new StringSegmentizer[V](x._2, 3)

    tpt += p0
    tpt += p1
    tpt.dump()
    println("Testing searching for " + p2)
    println(tpt ? (p2))
    assert((tpt ? (p2)).impl.toSet == Set(p0))
    println("Testing insertion of " + p2)
    tpt += p2
    tpt.dump()
    println("Testing searching for " + p3)
    println(tpt ? (p3))
    assert((tpt ? (p3)).impl.toSet == Set(p0))
    println("Testing insertion of " + p3)
    tpt += p3
    tpt.dump()
    println("Testing searching for " + p4)
    println(tpt ? (p4))
    assert((tpt ? (p4)).impl.toSet == Set(p0, p2, p3))
    println("Testing insertion of " + p4)
    tpt += p4
    tpt.dump()
    println("Testing searching for " + p5)
    println(tpt ? (p5))
    assert((tpt ? (p5)).impl.toSet == Set(p0, p3))
  }
}
