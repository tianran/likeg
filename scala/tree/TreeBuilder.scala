package tree

import scala.collection.mutable

class TreeBuilder[N <: TreeNode] {
  protected[this] val vroot = new TreeNode
  protected[this] val linear = new TreeNodeLinearOrder[N]

  def root: N = {
    assert(vroot.children.length == 1)
    vroot.children.head.asInstanceOf[N]
  }

  lazy val result: Tree[N] = {
    val ret = new Tree[N](root, linear)
    ret.init()
    ret
  }

  /** Insert node x at index ni. */
  protected[this] def insert(ni: Int, x: N): Unit = {
    for (i <- ni until linear.nodes.length) linear.inds(linear.nodes(i)) += 1
    assert(!linear.contains(x))
    linear.inds(x) = ni
    linear.ns.insert(ni, x)
  }

  /** Insert node x before node n. */
  protected[this] def insertBefore(n: N, x: N): Unit = insert(linear.index(n), x)

  /** Insert node x after node n. */
  protected[this] def insertAfter(n: N, x: N): Unit = insert(linear.index(n) + 1, x)

  /** Append node x. */
  protected[this] def append(x: N): Unit = {
    assert(!linear.contains(x))
    linear.inds(x) = linear.nodes.length
    linear.ns.append(x)
  }

  /** Sort the current tree nodes according to ord */
  protected[this] def sortCurrent(ord: Ordering[N]): Unit = {
    val queue = mutable.PriorityQueue.empty[N](ord.reverse)
    root.recur({n => queue.enqueue(n)}, {_ => })
    while(queue.nonEmpty) append(queue.dequeue())
  }

  /** Add node c as a child of node p. */
  protected[this] def addChild(p: TreeNode, c: TreeNode): Unit = {
    p.ch.append(c)
  }

  /** Change the parent of node c from node p to node pnew. */
  protected[this] def changeParent(p: TreeNode, c: TreeNode, pnew: TreeNode): Unit = {
    p.ch.remove(p.ch.indexOf(c))
    pnew.ch.append(c)
  }

  /** Delete node c from children of node p; all children of node c rewired to node p. */
  protected[this] def delChild(p: TreeNode, c: TreeNode): Unit = {
    p.ch.remove(p.ch.indexOf(c))
    p.ch.appendAll(c.children)
  }

  /** Delete node c from children of node p; all descendants of node c are lost. */
  protected[this] def purge(p: TreeNode, c: TreeNode): Unit = {
    p.ch.remove(p.ch.indexOf(c))
  }
}
