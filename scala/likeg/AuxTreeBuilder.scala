package likeg

import tree.{Tree, TreeBuilder}

import scala.collection.mutable.ArrayBuffer

object AuxTreeBuilder {

  def sd2aux(sdN: SDTreeNode): AuxTreeNode = sdN.getOrUpdate("__sd2aux", {
    val auxN = new AuxTreeNode
    auxN.src.append(sdN)
    auxN.rel = sdN.headRel.toString
    auxN
  })

  val modRels = Set(
    SDRel.goeswith,
    SDRel.mwe,
    SDRel.prt,
    SDRel.nn,
    SDRel.amod,
    SDRel.advmod,
    SDRel.number,
    SDRel.quantmod,
    SDRel.expl,
    SDRel.aux,
    SDRel.auxpass)

  val punctRels = Set(
    SDRel.punct,
    SDRel.discourse,
    SDRel.cop)

  def hasConj(sdN: SDTreeNode): Boolean = sdN.getOrUpdate("__hasConj", {
    sdN.children.exists(_.headRel == SDRel.conj)
  })

  val inNNPRels = Set(
    SDRel.poss,
    SDRel.prep,
    SDRel.pobj,
    SDRel.pcomp)

  val depCRels: Set[String] = Set(
    SDRel.partmod,
    SDRel.infmod).map(_.toString)

  val govCRels: Set[String] = Set(
    SDRel.nsubj,
    SDRel.nsubjpass,
    SDRel.dobj,
    SDRel.iobj,
    SDRel.pobj,
    SDRel.pcomp,
    SDRel.acomp,
    SDRel.csubj,
    SDRel.csubjpass,
    SDRel.xcomp).map(_.toString)

  def hasCop(sdN: SDTreeNode): Boolean = sdN.getOrUpdate("__hasCop", {
    sdN.children.exists(_.headRel == SDRel.cop)
  })
}

/** Build AuxTree from SDTree */
class AuxTreeBuilder(sdtree: Tree[SDTreeNode]) extends TreeBuilder[AuxTreeNode] {
  import AuxTreeBuilder._
  import AuxTreeNode._

  /* copy the original sdtree to start: */
  sdtree.root.recur({sdN =>
    val sdP = sdtree.getParent(sdN)
    if (sdP == null) addChild(vroot, sd2aux(sdN)) else addChild(sd2aux(sdP), sd2aux(sdN))
  }, {_=>})

  /* special rel */
  for (sdN <- sdtree.linear.nodes) {
    if (modRels(sdN.headRel)) {
      sd2aux(sdN).rel = srel_MERGE
    } else if (punctRels(sdN.headRel)) {
      sd2aux(sdN).rel = srel_PUNCT
    } else if (sdN.headRel == SDRel.neg) {
      sd2aux(sdN).rel = srel_NEG
    } else if (Set(SDRel.det, SDRel.predet, SDRel.num)(sdN.headRel)) {
      sd2aux(sdN).rel = srel_QUANT
    } else if (Set(SDRel.cc, SDRel.preconj)(sdN.headRel)) {
      sd2aux(sdN).rel = srel_CC
    } else if (sdN.headRel == SDRel.mark) {
      sd2aux(sdN).rel = srel_MARK
    }
  }

  /* merge NNP */
  for (sdN <- sdtree.linear.nodes; if sdN.pennPOS.startsWith("NNP") && !hasConj(sdN)) {
    val rec = ArrayBuffer.empty[SDTreeNode]
    def loop(x: SDTreeNode): Unit = if (inNNPRels(x.headRel)) {
      val xp = sdtree.getParent(x)
      if (xp != null) {
        rec.append(x)
        if (xp.pennPOS.startsWith("NNP")) {
          if (!hasConj(xp)) rec.foreach(sd2aux(_).rel = srel_MERGE)
        } else loop(xp)
      }
    }
    loop(sdN)
  }

  /* merge conj */
  for (sdN <- sdtree.linear.nodes; if Set(SDRel.conj, SDRel.cc, SDRel.preconj)(sdN.headRel)) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      if (sd2aux(sdP).rel == srel_MERGE) {
        sd2aux(sdN).rel = srel_MERGE
      } else {
        val sc = sdtree.sortedChildren(sdP)
        if (sc.indexWhere(sd2aux(_).rel == srel_MERGE, sc.indexOf(sdN) + 1) != -1) {
          sd2aux(sdN).rel = srel_MERGE
        }
      }
    }
  }

  /* possessive */
  for (sdN <- sdtree.linear.nodes; if sdN.headRel == SDRel.possessive) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      if (sd2aux(sdP).rel == srel_MERGE) {
        sd2aux(sdN).rel = srel_MERGE
      } else sd2aux(sdP).rel = srel_PUNCT
    }
  }

  /* rcmod */
  for (sdN <- sdtree.linear.nodes; if sdN.headRel == SDRel.rcmod) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      // find a path from a W-word to sdN
      val ub = sdtree.linear.index(sdN)
      def loop(i: Int): Unit = if (i < ub) {
        val n = sdtree.linear.nodes(i)
        if (n.pennPOS.charAt(0) == 'W') {
          val rec = ArrayBuffer(n)
          def loop2(x: SDTreeNode): Boolean = {
            val xp = sdtree.getParent(x)
            if (xp == sdN) {
              if (rec.length == 1 && n.headRel == SDRel.nsubj && hasCop(sdN)) { // "A, which is B ..."
                sd2aux(n).rel = srel_PUNCT
                sd2aux(sdN).rel = SDRel.appos.toString
              }
              rec.foreach(sd2aux(_).rel = srel_MERGE)
              false
            } else if (xp == sdP) {
              true
            } else {
              rec.append(xp)
              loop2(xp)
            }
          }
          if (loop2(n)) loop(i + 1)
        } else loop(i + 1)
      }
      loop(sdtree.linear.index(sdP) + 1)
    }
  }

  /* at this stage, we are done with setting rel. */
  /* now we actually merge AuxTreeNodes. */
  root.setFeature("__mergeTo", root.src)
  root.recur({x =>
    for (c <- x.children[AuxTreeNode]) c.setFeature("__mergeTo",
      if (c.rel == srel_MERGE) x.getFeature("__mergeTo") else c.src)
  }, {x =>
    if (x.rel == srel_MERGE) {
      x.getFeature[ArrayBuffer[SDTreeNode]]("__mergeTo").appendAll(x.src)
    }
    x.removeNames("__mergeTo")
  })
  root.recur({x =>
    for (c <- x.children.toArray; if c.rel == srel_MERGE) delChild(x, c)
  }, {_ =>})
  root.recur({x =>
    for (c <- x.children.toArray; if c.rel.charAt(0) == '_') {
      if (c.rel != srel_PUNCT) {
        x.scopeInfo.getOrElseUpdate(c.rel, ArrayBuffer.empty) ++= c.src
      }
      purge(x, c)
    }
  }, {_ =>})

  /* next, we set nodeType */
  root.recur({x =>
    for (c <- x.children[AuxTreeNode]) {
      if (depCRels(c.rel)) c.nodeType = AuxTreeNodeType.Relation
      if (govCRels(c.rel)) x.nodeType = AuxTreeNodeType.Relation
    }
  }, {_ =>})

  /* COP */
  root.recur({x =>
    if (hasCop(x.src.head)) {
      x.nodeType = if (x.rel == SDRel.rcmod.toString) AuxTreeNodeType.Relation else AuxTreeNodeType.COP
    }
  }, {_ =>})

  /* conj */
  root.recur({x =>
    for (c <- x.children; if c.rel == SDRel.conj.toString) {
      x.nodeType match {
        case AuxTreeNodeType.Relation =>
          if (c.nodeType == AuxTreeNodeType.NN) c.nodeType = AuxTreeNodeType.Relation
        case AuxTreeNodeType.COP =>
          if (c.src.head.pennPOS.startsWith("VB")) c.nodeType = AuxTreeNodeType.Relation
          else if (c.nodeType == AuxTreeNodeType.NN) c.nodeType = AuxTreeNodeType.COP
        case AuxTreeNodeType.NN =>
          c.nodeType = AuxTreeNodeType.NN
      }
    }
  }, {_ =>})

  /* set up linear order */
  sortCurrent(Ordering.by[AuxTreeNode, SDTreeNode](_.src.head)(sdtree.linear.ordering))

  /* clean up */
  sdtree.linear.nodes.foreach(_.removeNames("__sd2aux", "__hasConj", "__hasCop"))
}
