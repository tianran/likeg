package eg

/** Binary relation
  *  when pred == null, it represents equality
  *  when arg2 == null, it is a unary relation */
class BiRel[T >: Null](val pred: T, val arg1: DefVar, val arg2: DefVar) extends EGNode {
  override def toString: String =
    if (pred == null) {
      "==(%s %s)".format(arg1.id, arg2.id)
    } else if (arg2 == null) {
      "%s (%s)".format(pred, arg1.id)
    } else {
      "%s (%s %s)".format(pred, arg1.id, arg2.id)
    }
}
