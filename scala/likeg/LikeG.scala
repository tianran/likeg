package likeg

import eg.{BiRel, DefVar, EGNode}

import scala.collection.mutable.ArrayBuffer

class LikeG(val rootScope: ScopeNode) {

  def relsUniqueIDs(egns: IndexedSeq[EGNode]): IndexedSeq[BiRel[Pred]] = {
    val vars = ArrayBuffer.empty[DefVar]
    val ret = ArrayBuffer.empty[BiRel[Pred]]
    for (egn <- egns) egn match {
      case v: DefVar =>
        vars.append(v)
      case r: BiRel[_] =>
        if (r.pred == null) {
          r.arg1.getOrUpdate[ArrayBuffer[DefVar]]("__==", ArrayBuffer.empty[DefVar]).append(r.arg2)
          r.arg2.getOrUpdate[ArrayBuffer[DefVar]]("__==", ArrayBuffer.empty[DefVar]).append(r.arg1)
        } else {
          ret.append(r.asInstanceOf[BiRel[Pred]])
        }
    }
    var counter = 0
    def recur(x: DefVar): Boolean = {
      val ret = !x.hasFlag("__visited")
      if (ret) {
        x.id = counter
        x.setFlag("__visited")
        if (x.hasFlag("__==")) x.getFeature[IndexedSeq[DefVar]]("__==").foreach(recur)
      }
      ret
    }
    for (x <- vars) if (recur(x)) counter += 1
    vars.foreach(_.removeNames("__visited", "__=="))
    ret
  }

  override def toString: String = {
    relsUniqueIDs(rootScope.egNodes).mkString("", "\n", "\n")
  }
}
