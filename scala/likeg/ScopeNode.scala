package likeg

import eg.EGNode
import tree.TreeNode

import scala.collection.mutable.ArrayBuffer

class ScopeNode extends TreeNode {
  private[this] var pn: ScopeNode = _
  def parent: ScopeNode = pn

  private[likeg] def addParent(p: ScopeNode): Unit = {
    assert(pn == null)
    pn = p
    p.ch.append(this)
  }

  private val egns = ArrayBuffer.empty[EGNode]
  def egNodes: IndexedSeq[EGNode] = egns

  private[likeg] def append(egN: EGNode): Unit = {
    egN.setFeature("__ScopeNode", this)
    def loop(x: ScopeNode): Unit = {
      x.egns.append(egN)
      if (x.parent != null) loop(x.parent)
    }
    loop(this)
  }
}
