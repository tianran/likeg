package likeg

import SDRel.SDRel
import tree.TreeNode

/** Node of a Stanford Dependency Tree. */
class SDTreeNode(val surf: String, val pennPOS: String, val headRel: SDRel) extends TreeNode {
  override def toString: String = surf
}
