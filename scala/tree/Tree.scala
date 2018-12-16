package tree

class Tree[N <: TreeNode](val root: N, val linear: TreeNodeLinearOrder[N]) {

  private[tree] def init(): Unit = {
    root.setFeature((this, "parent"), null)
    root.recur({n =>
      assert(linear.contains(n))
      assert(!n.hasFlag("__visited"))
      n.setFlag("__visited")
      n.children[N].foreach(_.setFeature((this, "parent"), n))
    }, {_ => })
    for (n <- linear.nodes) {
      assert(n.hasFlag("__visited"))
      n.removeNames("__visited")
    }
  }

  def getParent(n: N): N = n.getFeature((this, "parent"))
  def sortedChildren(n: N): IndexedSeq[N] =
    n.getOrUpdate((this, "sortedChildren"), n.children.sorted(linear.ordering))

  def recurSorted(pre: N => Unit, preC: (N, N) => Unit, postC: (N, N) => Unit,
                  post: N => Unit, n: N = root): Unit = {
    val sc = sortedChildren(n)
    pre(n)
    for (x <- sc) {
      preC(n, x)
      recurSorted(pre, preC, postC, post, x)
      postC(n, x)
    }
    post(n)
  }
 }
