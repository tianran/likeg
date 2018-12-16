package likeg

class Pred(val words: IndexedSeq[String], val evidence: IndexedSeq[SDTreeNode]) {
  override def toString: String = words.mkString(" ")
}
