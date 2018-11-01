package likeg

import tree.TreeNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object AuxTreeNode {

  val Array(srel_MERGE, srel_PUNCT, srel_NEG, srel_QUANT, srel_CC, srel_MARK) =
    Array("_MERGE", "_PUNCT", "_NEG", "_QUANT", "_CC", "_MARK")

  object AuxTreeNodeType extends Enumeration {
    val Relation, COP, NN = Value
  }
}

/** Node of an auxiliary tree, converted from Stanford Dependency Tree and can be used for extracting relations. */
class AuxTreeNode extends TreeNode {
  import AuxTreeNode.AuxTreeNodeType

  /** SDTreeNodes mapped to this likeg.AuxTreeNode */
  val src: ArrayBuffer[SDTreeNode] = ArrayBuffer.empty

  /** largely the same as headRel in likeg.SDTreeNode
    * but also include the following special types:
    * _MERGE: this node should be merged to parent
    * _PUNCT: punctuation
    * _NEG: negation
    * _QUANT: quantifiers (determiners & numerics)
    * _CC: conjugation marker
    * _MARK: mark
    */
  var rel: String = _

  /** there are the following types:
    * Relation: node in a relation
    * COP: copula
    * NNP: named entity (proper noun)
    * NN: other mentions
    */
  var nodeType: AuxTreeNodeType.Value = AuxTreeNodeType.NN

  val scopeInfo: mutable.Map[String, ArrayBuffer[SDTreeNode]] = mutable.Map.empty
}
