package eg

/** Defining a variable */
class DefVar extends EGNode {
  var id: String = _

  override def toString: String = "[%s]".format(id)
}
