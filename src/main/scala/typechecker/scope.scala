package typechecker

import scala.collection.mutable.HashMap

sealed abstract class BoolexType
final case object BooleanType extends BoolexType
final case class CircuitType(inputs: Int, outputs: Int) extends BoolexType
final case object BooleanPromiseType extends BoolexType // maybe make it a class so we bind promise to a symbol?
final case object IncompleteType extends BoolexType // stub type we will complete at a later stage

final class BoolexScope {
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

  def containsSymbol(symbol: String): Boolean = scopes.exists(_.contains(symbol))

  def addSymbol(symbol: String, typ: BoolexType = IncompleteType): Boolean = {
    if (getSymbolType(symbol).nonEmpty) {
      return false
    } else {
      scopes.head.put(symbol, typ)
      return true
    }
  }

  def completeType(symbol: String, typ: BoolexType): Boolean = {
    scope = scopes.find(_.contains(symbol))
    if (scope.get(symbol).contains(IncompleteType)) {
      scope.put(symbol, typ)
      return true
    } else {
      return false
    }
  }
}
