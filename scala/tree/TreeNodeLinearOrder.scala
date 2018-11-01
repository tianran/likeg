package tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Define a linear order on tree nodes. */
class TreeNodeLinearOrder[N <: TreeNode] {

  private[tree] val _nodes = ArrayBuffer.empty[N]
  private[tree] val _indices = mutable.Map.empty[N, Int]

  def nodes: IndexedSeq[N] = _nodes
  def contains(n: N): Boolean = _indices.contains(n)
  def index(n: N): Int = _indices(n)
  def ordering: Ordering[N] = Ordering.by(_indices)
}
