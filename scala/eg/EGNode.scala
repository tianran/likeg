package eg

import scala.collection.mutable

abstract class EGNode {
  private[this] val features = mutable.Map.empty[Any, Any]
  def getFeature[T](name: Any): T = features(name).asInstanceOf[T]
  def setFeature(name: Any, v: Any): Unit = { features(name) = v }
  def hasFlag(name: String): Boolean = features.contains(name)
  def setFlag(name: String): Unit = { features(name) = null }
  def getOrUpdate[T](name: Any, op: => T): T = features.getOrElseUpdate(name, op).asInstanceOf[T]
  def removeNames(names: Any*): Unit = names.foreach(features.remove)
}
