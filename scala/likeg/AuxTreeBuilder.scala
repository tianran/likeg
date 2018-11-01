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
    SDLabel.goeswith,
    SDLabel.mwe,
    SDLabel.prt,
    SDLabel.nn,
    SDLabel.amod,
    SDLabel.advmod,
    SDLabel.number,
    SDLabel.quantmod,
    SDLabel.expl,
    SDLabel.aux,
    SDLabel.auxpass)

  val punctRels = Set(
    SDLabel.punct,
    SDLabel.discourse,
    SDLabel.cop)

  def hasConj(sdN: SDTreeNode): Boolean = sdN.getOrUpdate("__hasConj", {
    sdN.children.exists(_.headRel == SDLabel.conj)
  })

  val inNNPRels = Set(
    SDLabel.poss,
    SDLabel.prep,
    SDLabel.pobj,
    SDLabel.pcomp)

  val depCRels: Set[String] = Set(
    SDLabel.partmod,
    SDLabel.infmod).map(_.toString)

  val govCRels: Set[String] = Set(
    SDLabel.nsubj,
    SDLabel.nsubjpass,
    SDLabel.dobj,
    SDLabel.iobj,
    SDLabel.pobj,
    SDLabel.pcomp,
    SDLabel.acomp,
    SDLabel.csubj,
    SDLabel.csubjpass,
    SDLabel.xcomp).map(_.toString)

  def hasCop(sdN: SDTreeNode): Boolean = sdN.getOrUpdate("__hasCop", {
    sdN.children.exists(_.headRel == SDLabel.cop)
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
    } else if (sdN.headRel == SDLabel.neg) {
      sd2aux(sdN).rel = srel_NEG
    } else if (Set(SDLabel.det, SDLabel.predet, SDLabel.num)(sdN.headRel)) {
      sd2aux(sdN).rel = srel_QUANT
    } else if (Set(SDLabel.cc, SDLabel.preconj)(sdN.headRel)) {
      sd2aux(sdN).rel = srel_CC
    } else if (sdN.headRel == SDLabel.mark) {
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
  for (sdN <- sdtree.linear.nodes; if Set(SDLabel.conj, SDLabel.cc, SDLabel.preconj)(sdN.headRel)) {
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
  for (sdN <- sdtree.linear.nodes; if sdN.headRel == SDLabel.possessive) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      if (sd2aux(sdP).rel == srel_MERGE) {
        sd2aux(sdN).rel = srel_MERGE
      } else sd2aux(sdP).rel = srel_PUNCT
    }
  }

  /* rcmod */
  for (sdN <- sdtree.linear.nodes; if sdN.headRel == SDLabel.rcmod) {
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
              if (rec.length == 1 && n.headRel == SDLabel.nsubj && hasCop(sdN)) { // "A, which is B ..."
                sd2aux(n).rel = srel_PUNCT
                sd2aux(sdN).rel = SDLabel.appos.toString
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
      x.nodeType = if (x.rel == SDLabel.rcmod.toString) AuxTreeNodeType.Relation else AuxTreeNodeType.COP
    }
  }, {_ =>})

  /* conj */
  root.recur({x =>
    for (c <- x.children; if c.rel == SDLabel.conj.toString) {
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
