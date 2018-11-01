package likeg

import eg.{BiRel, DefVar}

import scala.collection.mutable.ArrayBuffer

object RelBuilder {

  type RelInfo = (DefVar, (Char, String), List[(Char, AuxTreeNode)])

  val Array(nodeUp, nodeDown, nodeCusp, relUp, relDown) = Array('-', '+', '^', '<', '>')

  def startUp(arg1: DefVar, r1: String): RelInfo = {
    (arg1, (relUp, r1), Nil)
  }

  def startDown(arg1: DefVar, r1: String): RelInfo = {
    (arg1, (relDown, r1), Nil)
  }

  def goUp(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, rd1, lst) = ri
    (arg1, rd1, (nodeUp, n) :: lst)
  }

  def goDown(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, rd1, lst) = ri
    val sgn = if (lst.isEmpty) {
      if (rd1._1 == relUp) nodeCusp else nodeDown
    } else {
      if (lst.head._1 == nodeUp) nodeCusp else nodeDown
    }
    (arg1, rd1, (sgn, n) :: lst)
  }

  def turnDown(ri: RelInfo): RelInfo = {
    val (arg1, rd1, (nup, n) :: tail) = ri
    assert(nup == nodeUp)
    (arg1, rd1, (nodeCusp, n) :: tail)
  }

  def equality(arg1: DefVar, arg2: DefVar): BiRel[Pred] = new BiRel[Pred](null, arg1, arg2)
}

class RelBuilder(ord: Ordering[SDTreeNode]) {
  import RelBuilder._
  import LikeGBuilder.advclLike

  def finish(ri: RelInfo, r2: String, arg2: DefVar): BiRel[Pred] = {
    val (arg1, rd1, lst) = ri
    if (lst.isEmpty) {
      if (arg1.hasFlag("__reification") || arg2.hasFlag("__reification")) {
        new BiRel[Pred](new Pred(rd1._1 + rd1._2, ArrayBuffer.empty[SDTreeNode]), arg1, arg2)
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
      val str_r1 = if (rd1._2 == null) str else "%s %s:1".format(str, rd1._1 + rd1._2)
      val str_r1_r2 = if (arg2 == null) str_r1 else "%s %s:2".format(str_r1, relDown + r2)
      new BiRel[Pred](new Pred(str_r1_r2, evi), arg1, arg2)
    }
  }
}