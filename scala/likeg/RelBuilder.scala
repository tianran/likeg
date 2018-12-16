package likeg

import eg.{BiRel, DefVar}

import scala.collection.mutable.ArrayBuffer

object RelBuilder {

  type RelInfo = (DefVar, (Char, String), List[(Char, AuxTreeNode)])

  val Array(normalNeutral, normalUp, normalDown, depUp1, depUp2, depDown1, depDown2) =
    Array('^', '/', '\\', '!', '@', '1', '2')

  val rcLike: Set[String] = Set(SDLabel.rcmod, SDLabel.partmod, SDLabel.infmod).map(_.toString)

  def startUp(arg1: DefVar, l1: String): RelInfo = {
    (arg1, (depUp1, l1), Nil)
  }

  def startDown(arg1: DefVar, l1: String): RelInfo = {
    (arg1, (depDown1, l1), Nil)
  }

  def goUp(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, sl1, lst) = ri
    (arg1, sl1, (normalUp, n) :: lst)
  }

  def goDown(ri: RelInfo, n: AuxTreeNode): RelInfo = {
    val (arg1, sl1, lst) = ri
    val sgn = if (lst.isEmpty) {
      if (sl1._1 == depUp1 || rcLike(sl1._2)) normalNeutral else normalDown
    } else {
      if (lst.head._1 == normalUp) normalNeutral else normalDown
    }
    (arg1, sl1, (sgn, n) :: lst)
  }

  def turnUp(ri: RelInfo): RelInfo = {
    val (arg1, sl1, (cusp, n) :: tail) = ri
    assert(cusp == normalNeutral, cusp)
    assert(sl1._1 == depUp1, sl1)
    (arg1, sl1, (normalUp, n) :: tail)
  }

  def equality(arg1: DefVar, arg2: DefVar): BiRel[Pred] = new BiRel[Pred](null, arg1, arg2)
}

class RelBuilder(ord: Ordering[SDTreeNode]) {
  import RelBuilder.{RelInfo, equality, depDown2}

  def finish(ri: RelInfo, l2: String, arg2: DefVar): BiRel[Pred] = {
    val (arg1, sl1, lst) = ri
    if (lst.isEmpty) {
      if (arg1.hasFlag("__reification") || arg2.hasFlag("__reification")) {
        new BiRel[Pred](new Pred(ArrayBuffer(sl1._1 + sl1._2),
          ArrayBuffer.empty[SDTreeNode]), arg1, arg2)
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
      val wds = for (n <- evi) yield n.getFeature[Char]("__sgn") + n.surf.toLowerCase
      nodes.foreach(_.removeNames("__sgn"))

      if (sl1._2 != null) wds.append(sl1._1 + sl1._2)
      if (arg2 != null) wds.append(depDown2 + l2)
      new BiRel[Pred](new Pred(wds, evi), arg1, arg2)
    }
  }
}