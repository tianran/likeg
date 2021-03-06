package likeg

import tree.TreeNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AuxTreeNode {

  val Array(labelMerge, labelPurge, labelNeg, labelQuant, labelCc, labelMark) =
    Array("_MERGE", "_PURGE", "_NEG", "_QUANT", "_CC", "_MARK")

  object AuxTreeNodeType extends Enumeration {
    val Relation, COP, NN = Value
  }
}

/** Node of an auxiliary tree, converted from Stanford Dependency Tree and
  *  can be used for extracting relations. */
class AuxTreeNode extends TreeNode {
  import AuxTreeNode.AuxTreeNodeType

  /** SDTreeNodes mapped to this likeg.AuxTreeNode */
  val src: ArrayBuffer[SDTreeNode] = ArrayBuffer.empty

  /** largely the same as labelSD in likeg.SDTreeNode
    * but also include the following special labels:
    * _MERGE: this node should be merged to parent
    * _PURGE: this node and all its descendants should be deleted
    * _NEG: negation
    * _QUANT: quantifiers (determiners & numerics)
    * _CC: conjugation marker
    * _MARK: mark
    */
  var label: String = _

  /** there are the following types:
    * Relation: node in a relation
    * COP: copula
    * NN: other mentions
    */
  var nodeType: AuxTreeNodeType.Value = AuxTreeNodeType.NN

  val scopeInfo: mutable.Map[String, ArrayBuffer[SDTreeNode]] = mutable.Map.empty
}
