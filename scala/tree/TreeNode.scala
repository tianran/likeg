package tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** minimal information of a tree node. */
class TreeNode {

  protected[tree] val ch: ArrayBuffer[TreeNode] = ArrayBuffer.empty
  /** Return children nodes. */
  def children[N >: this.type <: TreeNode]: IndexedSeq[N] = ch.asInstanceOf[IndexedSeq[N]]

  def recur[N >: this.type <: TreeNode](pre: N => Unit, post: N => Unit): Unit = {
    pre(this)
    children[N].foreach(_.recur[N](pre, post))
    post(this)
  }

  private[this] val features = mutable.Map.empty[Any, Any]
  def getFeature[T](name: Any): T = features(name).asInstanceOf[T]
  def setFeature(name: Any, v: Any): Unit = { features(name) = v }
  def hasFlag(name: String): Boolean = features.contains(name)
  def setFlag(name: String): Unit = { features(name) = null }
  def getOrUpdate[T](name: Any, op: => T): T = features.getOrElseUpdate(name, op).asInstanceOf[T]
  def removeNames(names: Any*): Unit = names.foreach(features.remove)
}
