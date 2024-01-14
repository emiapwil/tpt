package openflash.fimt.util

class ListFunSuite extends AnyFunSuite {

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
