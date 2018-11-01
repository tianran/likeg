package likeg

import SDLabel.SDLabel
import tree.TreeNode

/** Node of a Stanford Dependency Tree. */
class SDTreeNode(val surf: String, val pennPOS: String, val headRel: SDLabel) extends TreeNode {
  override def toString: String = surf
}
