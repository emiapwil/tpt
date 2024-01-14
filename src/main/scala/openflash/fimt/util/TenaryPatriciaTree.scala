package openflash.fimt.util

trait Handle[V, T] {

  def isEmpty: Boolean

  def +(coupled: (V, Segmentized[V])): Handle[V, T]

  def -(coupled: (V, Segmentized[V])): Handle[V, T]

  def |(rhs: Handle[V, T]): Handle[V, T]

  def impl: T

}

trait Handles[V, T] {

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

class TenaryPatriciaTree[V, T](val maxDepth: Int,
                               val handlers: Handles[V, T] = new SetHandles[V]) {

  var root: Node = null

  val empty = handlers.init()

  class Node(var cond: Segment, var depth: Int,
             var values: Handle[V, T] = empty,
             var lch: Node = null, var rch: Node = null,
             var wildcard: Node = null){

    def isLast: Boolean = depth + cond.len >= maxDepth

    def isLeaf: Boolean = (values.isEmpty)
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
    while (segmentized.current.len < node.cond.len)
      segmentized.proceed()
    if (node.cond > segmentized.current) {
      if (node.isLeaf) {
        return node.values
      }
      val lseg = segmentized.assertNext(0)
      val rseg = segmentized.assertNext(1)
      val lset = if (lseg == null) empty else {
        search(node.lch, value, lseg)
      }
      val rset = if (rseg == null) empty else {
        search(node.rch, value, rseg)
      }
      return lset | rset
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
    val finalNode = if (node.cond == longestMatch) { node } else {
      val common = node.cond & longestMatch
      val lch = split(node, common.len, 0)
      val rch = split(node, common.len, 1)
      new Node(common, depth, empty, lch, rch)
      // need to split
    }
    if (finalNode.isLeaf || finalNode.isLast) {
      // cannot proceed, insert now
      return new Node(finalNode.cond, depth,
                      finalNode.values + handlers.couple(value, segmentized))
    }
    val lseg = segmentized.assertNext(0)
    val rseg = segmentized.assertNext(1)
    val lch = if (lseg == null) finalNode.lch else {
      insert(finalNode.lch, value, lseg, depth + finalNode.cond.len)
    }
    val rch = if (rseg == null) finalNode.rch else {
      insert(finalNode.rch, value, rseg, depth + finalNode.cond.len)
    }
    return new Node(finalNode.cond, depth, empty, lch, rch)
  }

  def split(node: Node, offset: Int, next: Int): Node = {
    val seg = (node.cond << offset).unmask(0, next)
    if (seg == null) null else {
      new Node(seg, node.depth + offset, node.values, node.lch, node.rch)
    }
  }

  def merge(x: Node, y: Node): Node = {
    if (x == null) {
      return y
    }
    if (y == null) {
      return x
    }
    if (x.cond == y.cond) {
      val lch = merge(x.lch, y.lch)
      val rch = merge(x.rch, y.rch)
      val values = if (!x.isLeaf) empty else x.values | y.values
      return new Node(x.cond.mask(0), x.depth, values, lch, rch)
    }
    val common = x.cond & y.cond
    val xlch = split(x, common.len, 0)
    val xrch = split(x, common.len, 1)
    val ylch = split(y, common.len, 0)
    val yrch = split(y, common.len, 1)
    val lch = merge(xlch, ylch)
    val rch = merge(xrch, yrch)
    return new Node(common, x.depth, empty, lch, rch)
  }

  def delete(node: Node, value: V, segmentized: Segmentized[V]): Node = {
    if (node == null) {
      return node
    }
    segmentized.proceed(node.cond.len)
    if (node.isLeaf) {
      val remained = node.values - handlers.couple(value, segmentized)
      if (remained.isEmpty) {
        return null
      }
      return new Node(node.cond, node.depth, remained)
    }
    val lseg = segmentized.assertNext(0)
    val rseg = segmentized.assertNext(1)
    val lch = if (lseg == null) node.lch else {
      delete(node.lch, value, lseg)
    }
    val rch = if (rseg == null) node.rch else {
      delete(node.rch, value, rseg)
    }
    if ((lch == null) && (rch == null)) {
      return null
    }
    if ((lch == null) || (rch == null)) {
      val sub = if (lch == null) rch else lch
      val cond = node.cond + sub.cond
      val values = sub.values
      return new Node(cond, node.depth, values)
    }
    if (lch.isLeaf && rch.isLeaf) {
      if ((lch.cond << 1) == (rch.cond << 1)) {
        if (lch.values == rch.values) {
          val cond = node.cond + lch.cond.mask(0)
          val values = lch.values | rch.values
          return new Node(cond, node.depth, values)
        }
      }
    }
    return new Node(node.cond, node.depth, empty, lch, rch)
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
    println(Seq.fill(treeDepth)("-").mkString("") + node.cond.binaryString)
    if (node.isLeaf) {
      println(Seq.fill(treeDepth)(" ").mkString("") + " -> " + node.values)
      return
    }
    recursiveDump(node.lch, treeDepth + 1)
    recursiveDump(node.rch, treeDepth + 1)
  }

  def dump() = recursiveDump(root, 0)
}
