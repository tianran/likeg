package likeg

import tree.{Tree, TreeBuilder}

import scala.collection.mutable.ArrayBuffer

object AuxTreeBuilder {

  def sd2aux(sdN: SDTreeNode): AuxTreeNode = sdN.getOrUpdate("__sd2aux", {
    val auxN = new AuxTreeNode
    auxN.src.append(sdN)
    auxN.label = sdN.labelSD.toString
    auxN
  })

  val toMerge = Set(
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

  val toPurge = Set(
    SDLabel.punct,
    SDLabel.discourse,
    SDLabel.cop)

  val toNeg = Set(SDLabel.neg)
  val toQuant = Set(SDLabel.det, SDLabel.predet, SDLabel.num)
  val toCC = Set(SDLabel.cc, SDLabel.preconj)
  val toMark = Set(SDLabel.mark)

  def hasConj(sdN: SDTreeNode): Boolean = sdN.getOrUpdate("__hasConj", {
    sdN.children.exists(_.labelSD == SDLabel.conj)
  })

  val inNNP = Set(
    SDLabel.poss,
    SDLabel.prep,
    SDLabel.pobj)

  val depRels: Set[String] = Set(
    SDLabel.partmod,
    SDLabel.infmod).map(_.toString)

  val govRels: Set[String] = Set(
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
    sdN.children.exists(_.labelSD == SDLabel.cop)
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

  /* special labels */
  for (sdN <- sdtree.linear.nodes) {
    if (toMerge(sdN.labelSD)) {
      sd2aux(sdN).label = label_MERGE
    } else if (toPurge(sdN.labelSD)) {
      sd2aux(sdN).label = label_PURGE
    } else if (toNeg(sdN.labelSD)) {
      sd2aux(sdN).label = label_NEG
    } else if (toQuant(sdN.labelSD)) {
      sd2aux(sdN).label = label_QUANT
    } else if (toCC(sdN.labelSD)) {
      sd2aux(sdN).label = label_CC
    } else if (toMark(sdN.labelSD)) {
      sd2aux(sdN).label = label_MARK
    }
  }

  /* merge NNP */
  for (sdN <- sdtree.linear.nodes; if sdN.pennPOS.startsWith("NNP") && !hasConj(sdN)) {
    val rec = ArrayBuffer.empty[SDTreeNode]
    def loop(x: SDTreeNode): Unit = if (inNNP(x.labelSD)) {
      val xp = sdtree.getParent(x)
      if (xp != null) {
        rec.append(x)
        if (xp.pennPOS.startsWith("NNP")) {
          if (!hasConj(xp)) rec.foreach(sd2aux(_).label = label_MERGE)
        } else loop(xp)
      }
    }
    loop(sdN)
  }

  /* merge conj */
  for (sdN <- sdtree.linear.nodes; if Set(SDLabel.conj, SDLabel.cc, SDLabel.preconj)(sdN.labelSD)) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      if (sd2aux(sdP).label == label_MERGE) {
        sd2aux(sdN).label = label_MERGE
      } else {
        val sc = sdtree.sortedChildren(sdP)
        if (sc.indexWhere(sd2aux(_).label == label_MERGE, sc.indexOf(sdN) + 1) != -1) {
          sd2aux(sdN).label = label_MERGE
        }
      }
    }
  }

  /* possessive */
  for (sdN <- sdtree.linear.nodes; if sdN.labelSD == SDLabel.possessive) {
    val sdP = sdtree.getParent(sdN)
    if (sdP != null) {
      if (sd2aux(sdP).label == label_MERGE) {
        sd2aux(sdN).label = label_MERGE
      } else sd2aux(sdP).label = label_PURGE
    }
  }

  /* rcmod */
  for (sdN <- sdtree.linear.nodes; if sdN.labelSD == SDLabel.rcmod) {
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
              if (rec.length == 1 && n.labelSD == SDLabel.nsubj && hasCop(sdN)) { // "A, which is B ..."
                sd2aux(n).label = label_PURGE
                sd2aux(sdN).label = SDLabel.appos.toString
              }
              rec.foreach(sd2aux(_).label = label_MERGE)
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
      if (c.label == label_MERGE) x.getFeature("__mergeTo") else c.src)
  }, {x =>
    if (x.label == label_MERGE) {
      x.getFeature[ArrayBuffer[SDTreeNode]]("__mergeTo").appendAll(x.src)
    }
    x.removeNames("__mergeTo")
  })
  root.recur({x =>
    for (c <- x.children.toArray; if c.label == label_MERGE) delChild(x, c)
  }, {_ =>})
  root.recur({x =>
    for (c <- x.children.toArray; if c.label.charAt(0) == '_') {
      if (c.label != label_PURGE) {
        x.scopeInfo.getOrElseUpdate(c.label, ArrayBuffer.empty) ++= c.src
      }
      purge(x, c)
    }
  }, {_ =>})

  /* next, we set nodeType */
  root.recur({x =>
    for (c <- x.children[AuxTreeNode]) {
      if (depRels(c.label)) c.nodeType = AuxTreeNodeType.Relation
      if (govRels(c.label)) x.nodeType = AuxTreeNodeType.Relation
    }
  }, {_ =>})

  /* COP */
  root.recur({x =>
    if (hasCop(x.src.head)) {
      x.nodeType = if (x.label == SDLabel.rcmod.toString) AuxTreeNodeType.Relation else AuxTreeNodeType.COP
    }
  }, {_ =>})

  /* conj */
  root.recur({x =>
    for (c <- x.children; if c.label == SDLabel.conj.toString) {
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
