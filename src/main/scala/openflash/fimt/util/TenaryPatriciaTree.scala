package openflash.fimt.util

import scala.collection.mutable.SortedSet

trait Handle[V, T] {

  def isEmpty: Boolean

  def +(coupled: (V, Segmentized[V])): Handle[V, T]

  def -(coupled: (V, Segmentized[V])): Handle[V, T]

  def |(rhs: Handle[V, T]): Handle[V, T]

  def impl: T

}

trait Handles[V, T] {

  val empty: Handle[V, T]

  def init(elems: V*): Handle[V, T]

  def couple(elem: V, seg: Segmentized[V]): (V, Segmentized[V])

  def lookup(handle: Handle[V, T], elem: V): Handle[V, T]
}

class SetHandles[V] extends Handles[V, Set[V]] {

  type T = Set[V]

  val empty = init()

  override def init(elems: V*): Handle[V, T] = new SetHandle(elems)

  override def couple(elem: V, seg: Segmentized[V]) = (elem, seg)

  override def lookup(handle: Handle[V, T], elem: V) = handle match {
    case sh: SetHandle => sh
    case _ => empty
  }

  class SetHandle(val value: Iterable[V]) extends Handle[V, T] {
    override def impl = value.toSet

    override def isEmpty = impl.isEmpty

    override def +(coupled: (V, Segmentized[V])) = new SetHandle(impl + coupled._1)

    override def -(coupled: (V, Segmentized[V])) = new SetHandle(impl - coupled._1)

    override def |(rhs: Handle[V, T]) = new SetHandle(impl | rhs.impl)

    override def equals(rhs: Any): Boolean = rhs match {
      case sh: SetHandle => impl == sh.impl
      case _ => false
    }

    override def toString(): String = impl.toString()
  }
}

class SortedSetHandles[V](val minFirst: Boolean)(implicit val order: Ordering[V])
    extends Handles[V, SortedSet[V]] {

  type T = SortedSet[V]

  val empty = init()

  override def init(elems: V*): Handle[V, T] = new SSetHandle(elems)

  override def couple(elem: V, seg: Segmentized[V]) = (elem, seg)

  override def lookup(handle: Handle[V, T], elem: V): Handle[V, T] = handle match {
    case sh: SSetHandle => {
      val filtered = if (minFirst) sh.impl.rangeTo(elem) else sh.impl.rangeFrom(elem)
      new SSetHandle(filtered)
    }
    case _ => empty
  }

  class SSetHandle(value: Iterable[V]) extends Handle[V, T] {
    override def impl = SortedSet.from(value.toSeq)

    override def isEmpty = impl.isEmpty

    override def +(coupled: (V, Segmentized[V])) = {
      impl += coupled._1
      this
    }

    override def -(coupled: (V, Segmentized[V])) = {
      impl -= coupled._1
      this
    }

    override def |(rhs: Handle[V, T]) = new SSetHandle(impl | rhs.impl)

    override def equals(rhs: Any): Boolean = rhs match {
      case sh: SSetHandle => impl == sh.impl
      case _ => false
    }

    override def toString(): String = impl.toString()
  }
}

class TenaryPatriciaTree[V, T](val maxDepth: Int,
                               val handlers: Handles[V, T] = new SetHandles[V]) {

  var root: Node = null

  val empty = handlers.empty

  def makeEmpty = handlers.init()

  class Node(var cond: Segment, var depth: Int,
             var values: Handle[V, T] = makeEmpty,
             var lch: Node = null, var rch: Node = null,
             var wildcard: Node = null){

    def subsetL: Handle[V, T] = if (lch == null) empty else lch.subsets

    def subsetR: Handle[V, T] = if (rch == null) empty else rch.subsets

    def subsetW: Handle[V, T] = if (wildcard == null) empty else wildcard.subsets

    var subsets: Handle[V, T] = values | subsetL | subsetR | subsetW

    def isLast: Boolean = depth + cond.len >= maxDepth

    def isLeaf: Boolean = ((lch == null) && (rch == null) && (wildcard == null))
  }

  def getLongestMatch(cond: Segment, segmentized: Segmentized[V]): Segment = {
    while ((segmentized.hasNext) && (cond ~ segmentized.next)) {
      segmentized.proceed()
    }
    segmentized.current
  }

  def clear() = {
    root = null
  }

  def search(node: Node, value: V, segmentized: Segmentized[V]): Handle[V, T] = {
    if (node == null) {
      return empty
    }
    segmentized.proceed(node.cond.len)
    if ((node.cond > segmentized.current) || (node.cond < segmentized.current)) {
      // the segment is matched

      if ((!segmentized.hasNext) || (node.isLeaf)) {
        // the whole subtree is matched, return subsets
        println("subset:", node.subsets, value, handlers.lookup(node.subsets, value))
        return handlers.lookup(node.subsets, value)
      }

      // otherwise search down subtrees
      println("values: ", node.values, value, handlers.lookup(node.values, value))
      lazy val nset = handlers.lookup(node.values, value)

      lazy val wseg = segmentized.cut
      lazy val wset = if (wseg == null) empty else {
        search(node.wildcard, value, wseg)
      }
      lazy val lseg = segmentized.assertNext(0)
      lazy val lset = if (lseg == null) empty else {
        search(node.lch, value, lseg)
      }
      lazy val rseg = segmentized.assertNext(1)
      lazy val rset = if (rseg == null) empty else {
        search(node.rch, value, rseg)
      }
      return segmentized.nextPair match {
        case (0, 0) => wset | lset | rset | nset
        case (0, 1) => wset | lset | nset
        case (1, 1) => wset | rset | nset
        case _ => empty
      }
    }
    return empty
  }

  def insert(node: Node, value: V, segmentized: Segmentized[V], depth: Int): Node = {
    if (depth > maxDepth) {
      return null
    }
    if (node == null) {
      // Tree is empty, create a new node
      while (segmentized.hasNext) segmentized.proceed()
      return new Node(segmentized.current, depth, handlers.init(value))
    }
    val longestMatch = getLongestMatch(node.cond, segmentized)
    val finalNode = if (node.cond.len == longestMatch.len) { node } else {
      // need to split
      val common = longestMatch
      splitNode(node, common)
    }
    finalNode.subsets = finalNode.subsets + handlers.couple(value, segmentized)
    if ((!segmentized.hasNext) || (finalNode.isLast)) {
      // cannot proceed, return
      finalNode.values = finalNode.values + handlers.couple(value, segmentized)
      return finalNode
    }

    lazy val wseg = segmentized.assertNext(0, 0)
    lazy val lseg = segmentized.assertNext(0)
    lazy val rseg = segmentized.assertNext(1)

    if (wseg != null) {
      finalNode.wildcard = insert(finalNode.wildcard, value, wseg, depth + finalNode.cond.len)
    } else if (lseg != null) {
      finalNode.lch = insert(finalNode.lch, value, lseg, depth + finalNode.cond.len)
    } else {
      finalNode.rch = insert(finalNode.rch, value, rseg, depth + finalNode.cond.len)
    }
    return finalNode
  }

  def splitNode(node: Node, segment: Segment): Node = {
    val offset = segment.len

    val depth = node.depth
    val newSegment = (node.cond << offset)
    node.cond = newSegment
    node.depth = node.depth + offset

    return if (newSegment.isMasked(0)) {
      new Node(segment, depth, makeEmpty, null, null, node)
    } else if (newSegment.unmask(0, 0) != null) {
      new Node(segment, depth, makeEmpty, node, null, null)
    } else {
      new Node(segment, depth, makeEmpty, null, node, null)
    }
  }

  def delete(node: Node, value: V, segmentized: Segmentized[V]): Node = {
    if (node == null) {
      return node
    }
    segmentized.proceed(node.cond.len)
    if (node.cond != segmentized.current) {
      // mismatch: return
      return node
    }
    node.subsets = node.subsets - handlers.couple(value, segmentized)

    if ((!segmentized.hasNext) || (node.isLast)) {
      // remove from value
      node.values = node.values - handlers.couple(value, segmentized)
      return if (node.values.isEmpty) null else node
    }

    lazy val wseg = segmentized.assertNext(0, 0)
    lazy val lseg = segmentized.assertNext(0)
    lazy val rseg = segmentized.assertNext(1)

    if (wseg != null) {
      node.wildcard = delete(node.wildcard, value, wseg)
    } else if (lseg != null) {
      node.lch = delete(node.lch, value, lseg)
    } else {
      node.rch = delete(node.rch, value, rseg)
    }

    return compactNode(node)
  }

  def compactNode(node: Node): Node = {
    if (node.subsets.isEmpty) null // the whole subtree is empty
    else if (node.values.isEmpty) {
      // node is a split point
      // if there is only one child, replace node with it
      val onlyChild = if (node.wildcard == null) {
        if (node.lch == null) node.rch
        else if (node.rch == null) node.lch
        else null
      } else {
        if (node.lch == null && node.rch == null) node.wildcard
        else null
      }
      if (onlyChild == null) node
      else {
        onlyChild.cond = node.cond + onlyChild.cond
        onlyChild.depth = node.depth
        onlyChild
      }
    } else node
  }

  def +=(value: V)(implicit segmentize: V => Segmentized[V]): TenaryPatriciaTree[V, T] = {
    root = insert(root, value, segmentize(value), 0)
    this
  }

  def ?(value: V)(implicit segmentize: V => Segmentized[V]): Handle[V, T] =
    search(root, value, segmentize(value))

  def -=(value: V)(implicit segmentize: V => Segmentized[V]): TenaryPatriciaTree[V, T] = {
    root = delete(root, value, segmentize(value))
    this
  }

  def recursiveDump(node: Node, treeDepth: Int): Unit = {
    if (node == null) {
      return
    }
    val condStr = Seq.fill(treeDepth)("-").mkString("") + node.cond.binaryString
    val valueStr = if (node.values.isEmpty) "" else " -> " + node.values
    val subsetStr = if (node.subsets.isEmpty) "" else " " + node.subsets
    println(condStr + " " + valueStr + " " + subsetStr)
    recursiveDump(node.lch, treeDepth + node.cond.len)
    recursiveDump(node.rch, treeDepth + node.cond.len)
    recursiveDump(node.wildcard, treeDepth + node.cond.len)
  }

  def dump() = recursiveDump(root, 0)
}
