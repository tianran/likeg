package likeg

import java.io.PrintWriter

import eg.DefVar
import AuxTreeNode.AuxTreeNodeType
import tree.Tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LikeGBuilder {

  val nnRels: Set[String] = Set(
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
    SDLabel.prep,
    SDLabel.pobj).map(_.toString)

  def fromSDTree(sdtree: Tree[SDTreeNode]): LikeG = {
    import RelBuilder._
    val relBuilder = new RelBuilder(sdtree.linear.ordering)

    val auxtree = new AuxTreeBuilder(sdtree).result

    def countChildrenInConj(n: AuxTreeNode, cond: AuxTreeNode => Boolean) = {
      def loop(x: AuxTreeNode, c: Int): Int = {
        if (x.rel == SDLabel.conj.toString) {
          val xp = auxtree.getParent(x)
          if (xp != null) {
            val nc = c + xp.children.count(y => auxtree.linear.ordering.lt(n, y) && cond(y))
            loop(xp, nc)
          } else c
        } else c
      }
      loop(n, n.children.count(cond))
    }

    def hasConj(n: AuxTreeNode): Boolean = n.getOrUpdate("__hasConj", {
      n.children.exists(_.rel == SDLabel.conj.toString)
    })

    def allConjDesc(n: AuxTreeNode): ArrayBuffer[AuxTreeNode] = n.getOrUpdate("__allConjDesc", {
      val ret = ArrayBuffer(n)
      for (x <- auxtree.sortedChildren(n); if x.rel == SDLabel.conj.toString) {
        ret.appendAll(allConjDesc(x))
      }
      ret
    })

    val rootScope = new ScopeNode
    auxtree.root.setFeature("__currentScope", rootScope)

    def normalVar(n: AuxTreeNode) = {
      val prevScope = n.getFeature[ScopeNode]("__currentScope")
      val newScope = new ScopeNode
      newScope.addParent(prevScope)
      for (k <- Array(AuxTreeNode.srel_NEG, AuxTreeNode.srel_QUANT, AuxTreeNode.srel_CC)) n.scopeInfo.get(k) match {
        case Some(v) => newScope.setFeature(k, v)
        case None => //PASS
      }
      if (n.rel == SDLabel.conj.toString) newScope.setFlag("__conj")
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
      if (n.src.head.pennPOS.startsWith("NNP") || countChildrenInConj(n, y => nnRels(y.rel)) == 0) {//if not convered by nnRels later, create unary predicate now
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
      n.scopeInfo.get(AuxTreeNode.srel_MARK) match {
        case Some(v) => newScope.setFeature(AuxTreeNode.srel_MARK, v)
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

    def conjStartDown(p: AuxTreeNode, c: AuxTreeNode): ArrayBuffer[(RelInfo, ScopeNode)] = {
      val conjs = ArrayBuffer(p)
      for (x <- auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, c)); if x.rel == SDLabel.conj.toString) {
        conjs.appendAll(allConjDesc(x))
      }

      val ret = ArrayBuffer.empty[(RelInfo, ScopeNode)]
      val avar = new DefVar
      var flag = false
      if (nnRels(c.rel)) {
        for (x <- conjs) {
          if (x.src.head.pennPOS.startsWith("NNP")) {
            val xvar = x.getFeature[DefVar]("__normalVar")
            val xscope = x.getFeature[ScopeNode]("__currentScope")
            xscope.append(equality(xvar, avar))
            flag = true
          } else {
            ret.append(nnRelExtension(x))
          }
        }
      } else {
        for (x <- conjs) {
          val xvar = x.getFeature[DefVar]("__normalVar")
          val xscope = x.getFeature[ScopeNode]("__currentScope")
          xscope.append(equality(xvar, avar))
          flag = true
        }
      }
      if (flag) {
        val cscope = c.getFeature[ScopeNode]("__currentScope")
        cscope.append(avar)
        ret.append((startDown(avar, c.rel), cscope))
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
      ArrayBuffer(startUp(ret, n.rel))
    }

    auxtree.root.setFeature("__inheritHalf", ArrayBuffer.empty[(RelInfo, ScopeNode)])
    auxtree.recurSorted({n =>
      val inherit = n.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__inheritHalf")
      if (n.nodeType != AuxTreeNodeType.NN) {// fail-safe rule: if no enough args, make it NN
        val c = countChildrenInConj(n, y => argRels(y.rel)) //each argRel will return
        if (c == 0 || (c == 1 && (clauseRels(n.rel) || (inherit.isEmpty && !argRels(n.rel))))) {
          allConjDesc(n).foreach(_.nodeType = AuxTreeNodeType.NN)
        }
      }

      n.nodeType match {
        case AuxTreeNodeType.Relation =>
          val returns = mutable.Map.empty[AuxTreeNode, ArrayBuffer[RelInfo]]
          n.setFeature("__returns", returns)

          val extensions = mutable.Map.empty[AuxTreeNode, ArrayBuffer[(RelInfo, ScopeNode)]]
          extensions(null) = if (clauseRels(n.rel)) {
            val svar = scopeVar(n)
            for ((half, scope) <- inherit) {
              scope.append(relBuilder.finish(half, n.rel, svar))
            }
            ArrayBuffer.empty[(RelInfo, ScopeNode)]
          } else {
            for ((half, scope) <- inherit) yield (goDown(half, n), scope)
          }
          n.setFeature("__extensions", extensions)

          val prevScope = n.getFeature[ScopeNode]("__currentScope")
          if (n.scopeInfo.contains(AuxTreeNode.srel_NEG) || hasConj(n)) {
            val newScope = new ScopeNode
            newScope.addParent(prevScope)
            for (k <- Array(AuxTreeNode.srel_NEG, AuxTreeNode.srel_CC)) n.scopeInfo.get(k) match {
              case Some(v) => newScope.setFeature(k, v)
              case None => //PASS
            }
            n.setFeature("__currentScope", newScope)
            for (c <- n.children[AuxTreeNode]) {
              c.setFeature("__currentScope", if (auxtree.linear.ordering.lt(c, n)) {
                prevScope
              } else if (c.rel == SDLabel.conj.toString) {
                val conjScope = new ScopeNode
                conjScope.addParent(newScope)
                conjScope.setFlag("__conj")
                for (k <- Array(AuxTreeNode.srel_NEG, AuxTreeNode.srel_CC)) c.scopeInfo.get(k) match {
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
              scope.append(relBuilder.finish(half, n.rel, avar))
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
                scope.append(relBuilder.finish(half, n.rel, nvar))
              }
            }
          }

        case AuxTreeNodeType.COP =>
          val cvar = if (clauseRels(n.rel)) {
            val ret = scopeVar(n)
            normalVar(n)
            ret
          } else {
            normalVar(n)
          }
          for ((half, scope) <- inherit) {
            scope.append(relBuilder.finish(half, n.rel, cvar))
          }
      }
    }, {(p, c) =>
      c.setFeature("__inheritHalf", p.nodeType match {
        case AuxTreeNodeType.Relation =>
          if (advclLike(c.rel)) {// send scope var to child
            if (p.hasFlag("__scopeVar")) {
              val svar = p.getFeature[DefVar]("__scopeVar")
              val scope = svar.getFeature[ScopeNode]("__ScopeNode")
              ArrayBuffer((startDown(svar, c.rel), scope))
            } else {
              ArrayBuffer.empty[(RelInfo, ScopeNode)]
            }
          } else if (c.rel == SDLabel.conj.toString) {
            val cscope = c.getFeature[ScopeNode]("__currentScope")
            val cargs = auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, p))
              .filter(x => argRels(x.rel))
            cargs.reverseIterator.find(x => coreArgRels(x.rel)) match {
              case Some(x) =>
                c.setFeature("__inheritFrom", x)
                x.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
              case None => if (cargs.nonEmpty) {
                c.setFeature("__inheritFrom", cargs.head)
                cargs.head.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
              } else if (clauseRels(p.rel)) {
                ArrayBuffer.empty[(RelInfo, ScopeNode)]
              } else {
                c.setFeature("__inheritFrom", null)
                p.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__inheritHalf")
              }
            }
          } else {// select an appropriate one from __extensions as the __inheritHalf passed to child
            val extensions = p.getFeature[mutable.Map[AuxTreeNode, ArrayBuffer[(RelInfo, ScopeNode)]]]("__extensions")
            val cargs = auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, c))
              .filter(x => argRels(x.rel))
            cargs.reverseIterator.find(x => coreArgRels(x.rel)) match {
              case Some(x) =>
                extensions(x)
              case None => if (cargs.nonEmpty) {
                extensions(cargs.head)
              } else if (p.rel == SDLabel.conj.toString && auxtree.linear.ordering.lt(c, p)) {
                ArrayBuffer.empty[(RelInfo, ScopeNode)]
              } else {
                extensions(null)
              }
            }
          }
        case AuxTreeNodeType.NN =>
          if (c.rel == SDLabel.conj.toString) {
            if (p.hasFlag("__conjVar")) {
              c.setFeature("__conjVar", p.getFeature[DefVar]("__conjVar"))
            }
            ArrayBuffer.empty[(RelInfo, ScopeNode)]
          } else {
            conjStartDown(p, c)
          }

        case AuxTreeNodeType.COP =>
          if (c.rel == SDLabel.conj.toString) {
            val cscope = c.getFeature[ScopeNode]("__currentScope")
            val cargs = auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, p))
              .filter(x => argRels(x.rel))
            cargs.reverseIterator.find(x => coreArgRels(x.rel)) match {
              case Some(x) =>
                x.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
              case None => if (cargs.nonEmpty) {
                cargs.head.getFeature[ArrayBuffer[RelInfo]]("__retHalf").map((_, cscope))
              } else if (clauseRels(p.rel)) {
                ArrayBuffer.empty[(RelInfo, ScopeNode)]
              } else {
                p.getFeature[ArrayBuffer[(RelInfo, ScopeNode)]]("__inheritHalf")
              }
            }
          } else {
            conjStartDown(p, c)
          }
      })
    }, {(p, c) =>
      if (p.nodeType == AuxTreeNodeType.Relation && argRels(c.rel)) {
        val cret = c.getFeature[ArrayBuffer[RelInfo]]("__retHalf")
        val returns = p.getFeature[mutable.Map[AuxTreeNode, ArrayBuffer[RelInfo]]]("__returns")
        val extensions = p.getFeature[mutable.Map[AuxTreeNode, ArrayBuffer[(RelInfo, ScopeNode)]]]("__extensions")
        returns(c) = ArrayBuffer.empty[RelInfo]
        extensions(c) = ArrayBuffer.empty[(RelInfo, ScopeNode)]
        val conjs = ArrayBuffer(p)
        for (x <- auxtree.sortedChildren(p).takeWhile(auxtree.linear.ordering.lt(_, c)); if x.rel == SDLabel.conj.toString) {
          conjs.appendAll(allConjDesc(x).filter(_.nodeType == AuxTreeNodeType.Relation))
        }
        for (x <- conjs) {
          val xscope = x.getFeature[ScopeNode]("__currentScope")
          returns(c).appendAll(for (half <- cret) yield goUp(half, x))
          extensions(c).appendAll(for (half <- cret) yield (goDown(half, x), xscope))
        }
      }
    }, {n => n.setFeature("__retHalf", n.nodeType match {
      case AuxTreeNodeType.Relation =>
        if (clauseRels(n.rel)) {
          val svar = n.getFeature[DefVar]("__scopeVar")
          ArrayBuffer(startUp(svar, n.rel))
        } else {
          val returns = n.getFeature[mutable.Map[AuxTreeNode, ArrayBuffer[RelInfo]]]("__returns")
          val cargs = auxtree.sortedChildren(n).filter(x => argRels(x.rel))
          cargs.reverseIterator.find(x => coreArgRels(x.rel)) match {
            case Some(x) =>
              returns(x)
            case None => if (cargs.nonEmpty) {
              returns(cargs.head)
            } else {
              ArrayBuffer.empty[RelInfo]
            }
          }
        }
      case AuxTreeNodeType.NN =>
        conjStartUp(n)
      case AuxTreeNodeType.COP =>
        if (clauseRels(n.rel)) {
          val svar = n.getFeature[DefVar]("__scopeVar")
          ArrayBuffer(startUp(svar, n.rel))
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
      "__inheritFrom",
      "__extensions",
      "__returns",
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
