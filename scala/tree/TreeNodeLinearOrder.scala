package tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Define a linear order on tree nodes. */
class TreeNodeLinearOrder[N <: TreeNode] {

  private[tree] val ns = ArrayBuffer.empty[N]
  private[tree] val inds = mutable.Map.empty[N, Int]

  def nodes: IndexedSeq[N] = ns
  def contains(n: N): Boolean = inds.contains(n)
  def index(n: N): Int = inds(n)
  def ordering: Ordering[N] = Ordering.by(inds)
}
