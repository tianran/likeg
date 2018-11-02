package likeg

import java.io.PrintWriter

import eg.DefVar
import AuxTreeNode.AuxTreeNodeType
import tree.Tree

import scala.collection.mutable.ArrayBuffer

object LikeGBuilder {

  val nnRels: Set[String] = Set(
    SDLabel.dobj,
    SDLabel.iobj,
    SDLabel.pobj,
    SDLabel.pcomp,
    SDLabel.acomp,
    SDLabel.xcomp,
    SDLabel.prep,
    SDLabel.poss,
    SDLabel.tmod,
    SDLabel.npadvmod).map(_.toString)

  val advclLike: Set[String] = Set(
    SDLabel.ROOT,
    SDLabel.advcl,
    SDLabel.parataxis).map(_.toString)

  val clauseArgRels: Set[String] = Set(
    SDLabel.ccomp,
    SDLabel.csubj,
    SDLabel.csubjpass,
    SDLabel.pcomp).map(_.toString)

  val clauseRels: Set[String] = advclLike ++ clauseArgRels

  val coreArgRels: Set[String] = Set(
    SDLabel.nsubj,
    SDLabel.nsubjpass,
    SDLabel.dobj,
    SDLabel.iobj).map(_.toString)

  val argRels: Set[String] = coreArgRels ++ clauseArgRels ++ Array(
    SDLabel.dep,
    SDLabel.tmod,
    SDLabel.npadvmod,
    SDLabel.prep,
    SDLabel.pobj).map(_.toString)

  def fromSDTree(sdtree: Tree[SDTreeNode]): LikeG = {
    import RelBuilder._
    val relBuilder = new RelBuilder(sdtree.linear.ordering)

    val auxtree = new AuxTreeBuilder(sdtree).result

    def existsChildrenInConj(n: AuxTreeNode, cond: AuxTreeNode => Boolean) = {
      def loop(x: AuxTreeNode): Boolean = (x.label == SDLabel.conj.toString) && {
        val xp = auxtree.getParent(x)
        (xp != null) && {
          xp.children.exists(y => auxtree.linear.ordering.lt(n, y) && cond(y)) || loop(xp)
        }
      }
      n.children.exists(cond) || loop(n)
    }

    def hasConj(n: AuxTreeNode): Boolean = n.getOrUpdate("__hasConj", {
      n.children.exists(_.label == SDLabel.conj.toString)
    })

    def allConjDesc(n: AuxTreeNode): ArrayBuffer[AuxTreeNode] = n.getOrUpdate("__allConjDesc", {
      val ret = ArrayBuffer(n)
      for (x <- auxtree.sortedChildren(n); if x.label == SDLabel.conj.toString) {
        ret.appendAll(allConjDesc(x))
      }
      ret
    })

    object Status extends Enumeration {
      type Status = Value
      val Inherit, Normal, Core = Value
    }
    import Status._

    val rootScope = new ScopeNode
    auxtree.root.setFeature("__currentScope", rootScope)

    def normalVar(n: AuxTreeNode) = {
      val prevScope = n.getFeature[ScopeNode]("__currentScope")
      val newScope = new ScopeNode
      newScope.addParent(prevScope)
      for (k <- Array(AuxTreeNode.label_NEG, AuxTreeNode.label_QUANT, AuxTreeNode.label_CC)) n.scopeInfo.get(k) match {
        case Some(v) => newScope.setFeature(k, v)
        case None => //PASS
      }
      if (n.label == SDLabel.conj.toString) newScope.setFlag("__conj")
      n.setFeature("__currentScope", newScope)
      for (c <- n.children[AuxTreeNode]) {
        c.setFeature("__currentScope", if (auxtree.linear.ordering.lt(c, n)) {
          prevScope
        } else {
          newScope
        })
      }

      val ret = new DefVar
      newScope.append(ret)
      //if not covered by nnRels, create unary predicate now
      // note that some nnRel as conj siblings will play out AFTER this node returns
      // so we have to do this check
      if (n.src.head.pennPOS.startsWith("NNP") || !existsChildrenInConj(n, y => nnRels(y.label))) {
        newScope.append(relBuilder.finish(goDown(startDown(ret, null), n), null, null))
      }
      n.setFeature("__normalVar", ret)
      ret
    }

    def scopeVar(n: AuxTreeNode) = {
      val prevScope = n.getFeature[ScopeNode]("__currentScope")
      val ret = new DefVar
      prevScope.append(ret)
      val newScope = new ScopeNode
      newScope.addParent(prevScope)
      n.scopeInfo.get(AuxTreeNode.label_MARK) match {
        case Some(v) => newScope.setFeature(AuxTreeNode.label_MARK, v)
        case None => //PASS
      }
      ret.setFeature("__reification", newScope)
      newScope.setFeature("__scopeVar", ret)
      n.setFeature("__scopeVar", ret)
      n.setFeature("__currentScope", newScope)
      ret
    }

    def nnRelExtension(n: AuxTreeNode): (RelInfo, ScopeNode) = n.getOrUpdate("__nnRelExtension", {
      val nvar = n.getFeature[DefVar]("__normalVar")
      val nscope = nvar.getFeature[ScopeNode]("__ScopeNode")
      (goDown(startDown(nvar, null), n), nscope)
    })

    def propagateConj(p: AuxTreeNode, c: AuxTreeNode): ArrayBuffer[(RelInfo, ScopeNode)] = {
      val cscope = c.getFeature[ScopeNode]("__currentScope")
      val cargs = auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, p))
        .filter(x => argRels(x.label))
      cargs.reverseIterator.find(x => coreArgRels(x.label)) match {
        case Some(x) =>
          x.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
        case None => if (cargs.nonEmpty) {
          cargs.head.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
        } else if (clauseRels(p.label)) {
          ArrayBuffer.empty[(RelInfo, ScopeNode)]
        } else {
          p.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__inheritHalf")
        }
      }
    }

    def conjStartDown(p: AuxTreeNode, c: AuxTreeNode): ArrayBuffer[(RelInfo, ScopeNode)] = {
      val conjs = ArrayBuffer(p)
      for (x <- auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, c)); if x.label == SDLabel.conj.toString) {
        conjs.appendAll(allConjDesc(x))
      }

      val ret = ArrayBuffer.empty[(RelInfo, ScopeNode)]
      val bvars = ArrayBuffer.empty[DefVar]
      if (nnRels(c.label)) {
        for (x <- conjs) {
          if (x.src.head.pennPOS.startsWith("NNP")) {
            bvars.append(x.getFeature[DefVar]("__normalVar"))
          } else {
            ret.append(nnRelExtension(x))
          }
        }
      } else {
        for (x <- conjs) bvars.append(x.getFeature[DefVar]("__normalVar"))
      }
      if (bvars.length >= 2) {
        val avar = new DefVar
        for (bvar <- bvars) {
          bvar.getFeature[ScopeNode]("__ScopeNode").append(equality(bvar, avar))
        }
        val ppscope = p.getFeature[ScopeNode]("__normalVar").getFeature[ScopeNode]("__ScopeNode").parent
        ppscope.append(avar)
        ret.append((startDown(avar, c.label), ppscope))
      } else if (bvars.length == 1) {
        val bvar = bvars.head
        ret.append((startDown(bvar, c.label), bvar.getFeature[ScopeNode]("__ScopeNode")))
      }
      ret
    }

    def conjStartUp(n: AuxTreeNode): ArrayBuffer[RelInfo] = {
      val ret = if (hasConj(n)) {
        val avar = new DefVar
        n.getFeature[DefVar]("__normalVar").getFeature[ScopeNode]("__ScopeNode").parent.append(avar)
        for (x <- allConjDesc(n)) {
          val xvar = x.getFeature[DefVar]("__normalVar")
          val xscope = xvar.getFeature[ScopeNode]("__ScopeNode")
          xscope.append(equality(xvar, avar))
        }
        avar
      } else n.getFeature[DefVar]("__normalVar")
      ArrayBuffer(startUp(ret, n.label))
    }

    auxtree.root.setFeature("__inheritHalf", ArrayBuffer.empty[(RelInfo, ScopeNode)])
    auxtree.recurSorted({n =>
      val inherit = n.getFeature[IndexedSeq[(RelInfo, ScopeNode)]]("__inheritHalf")
      // fail-safe rule to ensure coverage: if not going to be covered, make it NN
      n.nodeType match {
        case AuxTreeNodeType.Relation =>
          if (inherit.isEmpty) {
            auxtree.sortedChildren(n).find(y => argRels(y.label)) match {
              case Some(y) =>
                if (!argRels(n.label) && !existsChildrenInConj(n,
                  x => auxtree.linear.ordering.lt(y, x) && x.label != SDLabel.conj.toString)) {
                  allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
                }
              case None =>
                allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
            }
          } else if (clauseRels(n.label)) {
            val c = n.children.count(_.label != SDLabel.conj.toString)
            if (c == 0) {
              allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
            } else if (c == 1) {
              n.label = SDLabel.prep.toString
            }
          } else {
            if (!existsChildrenInConj(n, _.label != SDLabel.conj.toString)) {
              allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
            }
          }
        case AuxTreeNodeType.COP =>
          if (clauseRels(n.label) || inherit.isEmpty) {
            if (n.children.forall(_.label == SDLabel.conj.toString)) {
              allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
            }
          }
        case AuxTreeNodeType.NN => //PASS
      }

      n.nodeType match {
        case AuxTreeNodeType.Relation =>
          n.setFeature("__status", Inherit)
          n.setFeature("__extself", if (clauseRels(n.label)) {
            val svar = scopeVar(n)
            for ((half, scope) <- inherit) {
              scope.append(relBuilder.finish(half, n.label, svar))
            }
            ArrayBuffer.empty[(RelInfo, ScopeNode)]
          } else {
            for ((half, scope) <- inherit) yield (goDown(half, n), scope)
          })
          n.setFeature("__extconj", ArrayBuffer.empty[(RelInfo, ScopeNode)])

          val prevScope = n.getFeature[ScopeNode]("__currentScope")
          if (n.scopeInfo.contains(AuxTreeNode.label_NEG) || hasConj(n)) {
            val newScope = new ScopeNode
            newScope.addParent(prevScope)
            for (k <- Array(AuxTreeNode.label_NEG, AuxTreeNode.label_CC)) n.scopeInfo.get(k) match {
              case Some(v) => newScope.setFeature(k, v)
              case None => //PASS
            }
            n.setFeature("__currentScope", newScope)
            for (c <- n.children[AuxTreeNode]) {
              c.setFeature("__currentScope", if (auxtree.linear.ordering.lt(c, n)) {
                prevScope
              } else if (c.label == SDLabel.conj.toString) {
                val conjScope = new ScopeNode
                conjScope.addParent(newScope)
                conjScope.setFlag("__conj")
                for (k <- Array(AuxTreeNode.label_NEG, AuxTreeNode.label_CC)) c.scopeInfo.get(k) match {
                  case Some(v) => conjScope.setFeature(k, v)
                  case None => //PASS
                }
                conjScope
              } else {
                newScope
              })
            }
          } else n.children[AuxTreeNode].foreach(_.setFeature("__currentScope", prevScope))

        case AuxTreeNodeType.NN =>
          if (inherit.nonEmpty && hasConj(n)) {
            val avar = new DefVar
            n.getFeature[ScopeNode]("__currentScope").append(avar)
            for ((half, scope) <- inherit) {
              scope.append(relBuilder.finish(half, n.label, avar))
            }
            n.setFeature("__conjVar", avar)
            val nvar = normalVar(n)
            n.getFeature[ScopeNode]("__currentScope").append(equality(avar, nvar))
          } else {
            val nvar = normalVar(n)
            if (n.hasFlag("__conjVar")) {
              n.getFeature[ScopeNode]("__currentScope").append(
                equality(n.getFeature[DefVar]("__conjVar"), nvar))
            } else {
              for ((half, scope) <- inherit) {
                scope.append(relBuilder.finish(half, n.label, nvar))
              }
            }
          }

        case AuxTreeNodeType.COP =>
          val cvar = if (clauseRels(n.label)) {
            val ret = scopeVar(n)
            normalVar(n)
            ret
          } else {
            normalVar(n)
          }
          for ((half, scope) <- inherit) {
            scope.append(relBuilder.finish(half, n.label, cvar))
          }
      }
    }, {(p, c) =>
      c.setFeature("__inheritHalf", p.nodeType match {
        case AuxTreeNodeType.Relation =>
          if (advclLike(c.label)) {// send scope var to child
            if (p.hasFlag("__scopeVar")) {
              val svar = p.getFeature[DefVar]("__scopeVar")
              val scope = svar.getFeature[ScopeNode]("__ScopeNode")
              ArrayBuffer((startDown(svar, c.label), scope))
            } else {
              ArrayBuffer.empty[(RelInfo, ScopeNode)]
            }
          } else if (c.label == SDLabel.conj.toString) {
            propagateConj(p, c)
          } else {
            if (p.label == SDLabel.conj.toString && auxtree.linear.ordering.lt(c, p) &&
              p.getFeature[Status]("__status") == Inherit) {
              ArrayBuffer.empty[(RelInfo, ScopeNode)]
            } else {
              p.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__extconj") ++
                p.getFeature[IndexedSeq[(RelInfo, ScopeNode)]]("__extself")
            }
          }
        case AuxTreeNodeType.NN =>
          if (c.label == SDLabel.conj.toString) {
            if (p.hasFlag("__conjVar")) {
              c.setFeature("__conjVar", p.getFeature[DefVar]("__conjVar"))
            }
            ArrayBuffer.empty[(RelInfo, ScopeNode)]
          } else {
            conjStartDown(p, c)
          }

        case AuxTreeNodeType.COP =>
          if (c.label == SDLabel.conj.toString) {
            propagateConj(p, c)
          } else {
            conjStartDown(p, c)
          }
      })
    }, {(p, c) =>
      if (p.nodeType == AuxTreeNodeType.Relation) {
        if (c.label == SDLabel.conj.toString && c.nodeType == AuxTreeNodeType.Relation) {
          (p.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__extconj") ++=
            c.getFeature[IndexedSeq[(RelInfo, ScopeNode)]]("__extconj")) ++=
            c.getFeature[IndexedSeq[(RelInfo, ScopeNode)]]("__extself")
        } else p.getFeature[Status]("__status") match {
          case Inherit =>
            if (argRels(c.label)) {
              val pscope = p.getFeature[ScopeNode]("__currentScope")
              val cret = c.getFeature[IndexedSeq[RelInfo]]("__retHalf")
              p.setFeature("__extself", for (half <- cret) yield (goDown(half, p), pscope))
              p.setFeature("__status", if (coreArgRels(c.label)) Core else Normal)
            }
          case _ =>
            if (coreArgRels(c.label)) {
              val pscope = p.getFeature[ScopeNode]("__currentScope")
              val cret = c.getFeature[IndexedSeq[RelInfo]]("__retHalf")
              p.setFeature("__extself", for (half <- cret) yield (goDown(half, p), pscope))
              p.setFeature("__status", Core)
            }
        }
      }
    }, {n => n.setFeature("__retHalf", n.nodeType match {
      case AuxTreeNodeType.Relation =>
        if (clauseRels(n.label)) {
          val svar = n.getFeature[DefVar]("__scopeVar")
          ArrayBuffer(startUp(svar, n.label))
        } else {
          for ((half, _) <- n.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__extconj") ++
            n.getFeature[IndexedSeq[(RelInfo, ScopeNode)]]("__extself")) yield turnUp(half)
        }
      case AuxTreeNodeType.NN =>
        conjStartUp(n)
      case AuxTreeNodeType.COP =>
        if (clauseRels(n.label)) {
          val svar = n.getFeature[DefVar]("__scopeVar")
          ArrayBuffer(startUp(svar, n.label))
        } else conjStartUp(n)
    })})
    // clean up
    auxtree.linear.nodes.foreach(_.removeNames("__hasConj",
      "__allConjDesc",
      "__currentScope",
      "__conjVar",
      "__normalVar",
      "__scopeVar",
      "__nnRelExtension",
      "__inheritHalf",
      "__status",
      "__extconj",
      "__extself",
      "__retHalf"))

    new LikeG(rootScope)
  }

  def main(args: Array[String]): Unit = {
    val fns = args

    fns.par.foreach { fn =>
      val (file, out) = if (fn == "-") {
        (io.Source.fromInputStream(System.in), new PrintWriter(System.out))
      } else {
        (io.Source.fromFile(fn), new PrintWriter(fn + ".likeg"))
      }

      for (g <- SDTreeCoNLLBuilder.read(file.getLines()).map(fromSDTree)) {
        out.println(g)
      }

      out.close()
      file.close()
    }
  }
}
