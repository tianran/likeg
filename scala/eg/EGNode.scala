package eg

import scala.collection.mutable

abstract class EGNode {
  private[this] val _f = mutable.Map.empty[Any, Any]
  def getFeature[T](name: Any): T = _f(name).asInstanceOf[T]
  def setFeature(name: Any, v: Any): Unit = { _f(name) = v }
  def hasFlag(name: String): Boolean = _f.contains(name)
  def setFlag(name: String): Unit = { _f(name) = null }
  def getOrUpdate[T](name: Any, op: => T): T = _f.getOrElseUpdate(name, op).asInstanceOf[T]
  def removeNames(names: Any*): Unit = names.foreach(_f.remove)
}
