package likeg

import eg.{BiRel, DefVar}

import scala.collection.mutable.ArrayBuffer

object RelBuilder {

  type RelInfo = (DefVar, (Char, String), List[(Char, AuxTreeNode)])

  val Array(nodeUp, nodeDown, nodeCusp, labelUp, labelDown) = Array('-', '+', '^', '<', '>')

  def startUp(arg1: DefVar, l1: String): RelInfo = {
    (arg1, (labelUp, l1), Nil)
  }

  def startDown(arg1: DefVar, l1: String): RelInfo = {
    (arg1, (labelDown, l1), Nil)
  }

  def goUp(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, sl1, lst) = ri
    (arg1, sl1, (nodeUp, n) :: lst)
  }

  def goDown(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, sl1, lst) = ri
    val sgn = if (lst.isEmpty) {
      if (sl1._1 == labelUp) nodeCusp else nodeDown
    } else {
      if (lst.head._1 == nodeUp) nodeCusp else nodeDown
    }
    (arg1, sl1, (sgn, n) :: lst)
  }

  def turnDown(ri: RelInfo): RelInfo = {
    val (arg1, sl1, (nup, n) :: tail) = ri
    assert(nup == nodeUp)
    (arg1, sl1, (nodeCusp, n) :: tail)
  }

  def equality(arg1: DefVar, arg2: DefVar): BiRel[Pred] = new BiRel[Pred](null, arg1, arg2)
}

class RelBuilder(ord: Ordering[SDTreeNode]) {
  import RelBuilder._

  def finish(ri: RelInfo, l2: String, arg2: DefVar): BiRel[Pred] = {
    val (arg1, sl1, lst) = ri
    if (lst.isEmpty) {
      if (arg1.hasFlag("__reification") || arg2.hasFlag("__reification")) {
        new BiRel[Pred](new Pred(sl1._1 + sl1._2, ArrayBuffer.empty[SDTreeNode]), arg1, arg2)
      } else {
        equality(arg1, arg2)
      }
    } else {
      val nodes = ArrayBuffer.empty[SDTreeNode]
      for ((sgn, n) <- lst) {
        n.src.foreach(_.setFeature("__sgn", sgn))
        nodes.appendAll(n.src)
      }
      val evi = nodes.sorted(ord)
      val str = (for (n <- evi) yield n.getFeature[Char]("__sgn") + n.surf.toLowerCase).mkString(" ")
      nodes.foreach(_.removeNames("__sgn"))

      val str_sl1 = if (sl1._2 == null) str else "%s %s:1".format(str, sl1._1 + sl1._2)
      val str_sl1_sl2 = if (arg2 == null) str_sl1 else "%s %s:2".format(str_sl1, labelDown + l2)
      new BiRel[Pred](new Pred(str_sl1_sl2, evi), arg1, arg2)
    }
  }
}