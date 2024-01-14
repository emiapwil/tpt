package openflash.fimt.util

// Segment is reversed: lowest bit in segment is highest bit in var
// segment(0) = predicate & anchor(nVar -1)
case class Segment(val value: BigInt, val mask: BigInt, val len: Int) {
  import Segment._

  override def equals(x: Any): Boolean = x match {
    case s: Segment => (value == s.value) && (mask == s.mask)
    case _ => false
  }

  def isEmpty: Boolean = len == 0

  def +(rhs: Segment): Segment =
    Segment((rhs.value << len) + value, (rhs.mask << len) + mask, len + rhs.len)

  def append(rhs: Segment): Segment = this + rhs

  def lowestBit(x: BigInt): BigInt = x & (-x)

  def &(rhs: Segment): Segment = {
    val lmin = Math.min(len, rhs.len)
    val maskOfMask = (BigInt(1) << lmin) - 1

    val diffMask = lowestBit((mask ^ rhs.mask) & maskOfMask)
    val maskLen = if (diffMask == 0) lmin else log2(diffMask)
    val commonMask = mask & ((BigInt(1) << maskLen) - 1)

    val diffValue = lowestBit((value ^ rhs.value) & commonMask)
    val valueLen = if (diffValue == 0) maskLen else log2(diffValue)
    val commonValueMask = if (diffValue == 0) commonMask else (diffValue - 1) & commonMask
    val commonValue = value & commonValueMask

    Segment(commonValue, commonValueMask, valueLen)
  }

  /**
    * rhs is a prefix of lhs
    */
  def ~(rhs: Segment): Boolean = {
    if (len < rhs.len) false else {
      val maskOfMask = (BigInt(1) << rhs.len) - 1
      if ((mask & maskOfMask) != rhs.mask) false else {
        (value & rhs.mask) == (rhs.value & rhs.mask)
      }
    }
  }

  /**
    * lhs is a superset of rhs
    *
    * @param rhs
    * @return
    */
  def >(rhs: Segment): Boolean = if ((mask & rhs.mask) != mask) false else {
    (value & mask) == (rhs.value & mask)
  }

  /**
    * lhs is a subset of rhs
    *
    * @param rhs
    * @return
    */
  def <(rhs: Segment): Boolean = rhs > this

  def <<(offset: Int): Segment = {
    Segment(value >> offset, mask >> offset, len - offset)
  }

  // if bit at pos is 0, the value is masked
  def isMasked(pos: Int) = mask.testBit(pos) == false

  def mask(pos: Int): Segment = {
    if (isMasked(pos)) this else {
      val newMask = mask.clearBit(pos)
      Segment(value & newMask, newMask, len)
    }
  }

  def unmask(pos: Int, newValue: Int): Segment = {
    val anchorMask = BigInt(1) << pos
    val anchorValue = BigInt(newValue) << pos
    if (isMasked(pos)) {
      Segment(value | anchorValue, mask.setBit(pos), len)
    } else {
      if ((value & anchorMask) == anchorValue) { this } else { null }
    }
  }

  def binaryString: String = (0 until len).map(
    i => {
      if (((mask >> i) & 1) == 0) "*" else {
        ((value >> i) & 1).toString
      }
    }
  ).mkString("")
}

trait Segmentized[V] {

  def isEmpty: Boolean

  def nextPair: (Int, Int)

  def hasNext: Boolean

  def current: Segment

  def next: Segment

  def proceed(steps: Int = 1): Unit

  def cut: Segmentized[V]

  def assertNext(next: Int, nextMask: Int = 1): Segmentized[V]
}

abstract class BaseSegmentizer[T](maxDepth: Int) extends Segmentized[T] {
  var currentSeg = Segment(0, 0, 0)

  def currentPos = currentSeg.len

  override def isEmpty = currentPos == 0

  override def hasNext = currentPos < maxDepth

  override def current = currentSeg

  override def next = {
    val np = nextPair
    current + Segment(np._1, np._2, 1)
  }

  override def proceed(steps: Int = 1) = {
    for (i <- 0 until steps) {
      if (hasNext)
        currentSeg = next
    }
  }

}

class MaskedIntSegmentizer[T](value: BigInt, mask: BigInt,
                              lower: Int, upper: Int,
                              maxDepth: Int) extends BaseSegmentizer[T](maxDepth) {

  // currentPos -> the highest in [lower, upper)
  def pos = upper - currentPos - 1

  override def nextPair = if (!mask.testBit(pos)) (0, 0) else {
    if (value.testBit(pos)) (1, 1) else (0, 1)
  }

  override def cut: Segmentized[T] = if (!hasNext) null else {
    new MaskedIntSegmentizer[T](value, mask,
                                lower, upper - currentPos, maxDepth - currentPos)
  }

  override def assertNext(next: Int, nextMask: Int): Segmentized[T] = {
    if (!hasNext) {
      return null
    }
    val np = nextPair
    if ((nextMask | np._2) != nextMask) null else {
      if ((next & np._2) != np._1) { null } else {
        val newValue = if (next == 0) value.clearBit(pos) else value.setBit(pos)
        val newMask = if (nextMask == 0) mask else mask.setBit(pos)
        new MaskedIntSegmentizer[T](newValue, newMask,
                                    lower, upper - currentPos, maxDepth - currentPos)
      }
    }
  }
}

class StringSegmentizer[V](value: String,
                           maxDepth: Int) extends BaseSegmentizer[V](maxDepth) {

  override def nextPair: (Int, Int) = value.charAt(currentPos) match {
    case '0' => (0, 1)
    case '1' => (1, 1)
    case _ => (0, 0)
  }

  override def hasNext = currentPos < Math.min(value.length(), maxDepth)

  override def cut: Segmentized[V] = {
    if (!hasNext) {
      return null
    }
    val substr = value.substring(currentPos)
    new StringSegmentizer[V](substr, maxDepth - currentPos)
  }

  override def assertNext(next: Int, nextMask: Int): Segmentized[V] = {
    if (!hasNext) {
      return null
    }
    val np = nextPair
    if ((nextMask | np._2) != nextMask) null else {
      if ((next & np._2) != np._1) { null } else {
        val nextStr = if (nextMask == 0) "*" else next.toString
        val substr = nextStr + value.substring(currentPos + 1)
        new StringSegmentizer[V](substr, maxDepth - currentPos)
      }
    }
  }
}

object Segment {

  val maxSegmentLen = 256

  val log2: Map[BigInt, Int] = (0 until maxSegmentLen)
      .map(x => (BigInt(1) << x) -> x).toMap

  def couple[V](value: V, segmentized: Segmentized[V]): (V, Segmentized[V])
    = (value, segmentized)
}
