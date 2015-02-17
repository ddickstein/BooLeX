package typechecker

import scala.collection.mutable.HashMap

abstract class BoolexType
case object BooleanType extends BoolexType
case class CircuitType(inputs: Int, outputs: Int) extends BoolexType
case object BooleanPromiseType extends BoolexType // maybe make it a class so we bind promise to a symbol?

class BoolexScope() {
  private var scopes = List.empty[HashMap[String, BoolexType]]
  private var owners = List.empty[String]
  startScope("TOP")

  def startScope(parent: String) {
    scopes = HashMap.empty[String, BoolexType] +: scopes
    owners = parent +: owners
  }

  def endScope {
    if (scopes.length == 1) {
      sys.error("[Internal Error] Tried to end top-level scope.")
    } else {
      scopes = scopes.tail
      owners = owners.tail
    }
  }

  def getOwner: String = owners.head

  def inContext(parent: String): Boolean = owners.contains(parent)
  
  def getSymbolType(symbol: String): Option[BoolexType] = scopes.find(_.contains(symbol)).flatMap(_.get(symbol))

  def addSymbol(symbol: String, typ: BoolexType): Boolean = {
    if (getSymbolType(symbol).nonEmpty) {
      return false
    } else {
      scopes.head.put(symbol, typ)
      return true
    }
  }
}
