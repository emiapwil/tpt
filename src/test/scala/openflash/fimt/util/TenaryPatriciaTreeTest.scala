package openflash.fimt.util

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

    implicit val segmentize: String => Segmentized[String] = new StringSegmentizer(_, 4)

    val seg1 = getSegment(str1)
    val seg2 = getSegment(str2)
    val seg3 = getSegment(str3)
    val seg4 = getSegment(str4)
    val seg5 = getSegment(str5)
    val seg6 = getSegment(str6)

    assert(seg2 < seg1)
    assert(seg3 < seg1)
    assert(seg4 < seg3)
    assert(seg5 ~ seg6)
    assert((seg4 & seg5) == seg6)
    assert(seg6 == getSegment(segmentize(str6).assertNext(0, 0)))
  }

  test("The tenary patricia tree should have the right structure") {
    val patriciaTree = new TenaryPatriciaTree[String, Set[String]](3)
    val str1 = "*****"
    val str2 = "0*1**"
    val str3 = "***1*"
    val str4 = "**11*"
    val str5 = "**00*"

    implicit val segmentize: String => Segmentized[String] = new StringSegmentizer(_, 3)

    patriciaTree += str1
    patriciaTree.dump()
    println("Testing searching for " + str2)
    println(patriciaTree ? (str2))
    println("Testing insertion of " + str2)
    patriciaTree += str2
    patriciaTree.dump()
    println("Testing searching for " + str3)
    println(patriciaTree ? (str3))
    println("Testing insertion of " + str3)
    patriciaTree += str3
    patriciaTree.dump()
    println("Testing searching for " + str4)
    println(patriciaTree ? (str4))
    println("Testing insertion of " + str3)
    patriciaTree += str4
    patriciaTree.dump()
    println("Testing searching for " + str5)
    println(patriciaTree ? (str5))
  }

}
