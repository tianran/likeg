package tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** minimal information of a tree node. */
class TreeNode {

  protected[tree] val _c: ArrayBuffer[TreeNode] = ArrayBuffer.empty
  /** Return children nodes. */
  def children[N >: this.type <: TreeNode]: IndexedSeq[N] = _c.asInstanceOf[IndexedSeq[N]]

  def recur[N >: this.type <: TreeNode](pre: N => Unit, post: N => Unit): Unit = {
    pre(this)
    children[N].foreach(_.recur[N](pre, post))
    post(this)
  }

  private[this] val _f = mutable.Map.empty[Any, Any]
  def getFeature[T](name: Any): T = _f(name).asInstanceOf[T]
  def setFeature(name: Any, v: Any): Unit = { _f(name) = v }
  def hasFlag(name: String): Boolean = _f.contains(name)
  def setFlag(name: String): Unit = { _f(name) = null }
  def getOrUpdate[T](name: Any, op: => T): T = _f.getOrElseUpdate(name, op).asInstanceOf[T]
  def removeNames(names: Any*): Unit = names.foreach(_f.remove)
}
