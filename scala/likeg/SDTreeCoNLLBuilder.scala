package likeg

import tree.{Tree, TreeBuilder}

import scala.collection.mutable.ArrayBuffer

/** Build Stanford Dependency Tree from CoNLL format. */
class SDTreeCoNLLBuilder(lines: Iterator[String]) extends TreeBuilder[SDTreeNode] {
  private[this] val parentIndex = ArrayBuffer.empty[Int]
  private[this] def loop_read(line: String): Unit = {
    if (line.nonEmpty) {
      if (line.charAt(0) != '#') {
        val Array(_, surf, _, _, pennPOS, _, headIDStr, headRelStr, _, _) = line.split('\t')
        append(new SDTreeNode(surf, pennPOS, SDRel.fromString(headRelStr)))
        parentIndex.append(headIDStr.toInt - 1)
      }
      loop_read(lines.next())
    }
  }
  loop_read(lines.next())
  for ((n, pi) <- linear.nodes zip parentIndex) {
    if (pi == -1) addChild(vroot, n) else addChild(linear.nodes(pi), n)
  }
}

object SDTreeCoNLLBuilder {

  def read(lines: Iterator[String]): Iterator[Tree[SDTreeNode]] = new Iterator[Tree[SDTreeNode]] {
    override def hasNext: Boolean = lines.hasNext
    override def next(): Tree[SDTreeNode] = new SDTreeCoNLLBuilder(lines).result
  }

  def main(args: Array[String]): Unit = {
    val fn = args(0)

    val file = io.Source.fromFile(fn)
    for (sdtree <- read(file.getLines())) println(sdtree.linear.nodes.mkString(" "))
    file.close()
  }
}
