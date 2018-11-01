package likeg

import eg.EGNode
import tree.TreeNode

import scala.collection.mutable.ArrayBuffer

class ScopeNode extends TreeNode {
  private[this] var _p: ScopeNode = _
  def parent: ScopeNode = _p

  private[likeg] def addParent(p: ScopeNode): Unit = {
    assert(_p == null)
    _p = p
    p._c.append(this)
  }

  private val _egnodes = ArrayBuffer.empty[EGNode]
  def egNodes: IndexedSeq[EGNode] = _egnodes

  private[likeg] def append(egN: EGNode): Unit = {
    egN.setFeature("__ScopeNode", this)
    def loop(x: ScopeNode): Unit = {
      x._egnodes.append(egN)
      if (x.parent != null) loop(x.parent)
    }
    loop(this)
  }
}
