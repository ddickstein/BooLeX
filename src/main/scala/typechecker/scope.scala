package typechecker

import scala.collection.mutable.{HashMap => MHashMap}
import scala.util.parsing.input.Position

sealed abstract class BoolexType
final case object BooleanType extends BoolexType
final case class BooleanPromiseType(pos: Position) extends BoolexType
final case class CircuitType(inputs: Int, outputs: Int) extends BoolexType
final case class PartialCircuitType(inputs: Int) extends BoolexType

final class BoolexScope {
  private var scopes = List.empty[MHashMap[String, BoolexType]]
  private var owners = List.empty[String]
  startScope("TOP")

  def startScope(parent: String) {
    scopes = MHashMap.empty[String, BoolexType] +: scopes
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

  def getSymbolType(symbol: String): Option[BoolexType] = {
    return scopes.find(_.contains(symbol)).flatMap(_.get(symbol))
  }

  def containsSymbol(symbol: String): Boolean = {
    return scopes.exists(_.contains(symbol))
  }

  def addSymbol(symbol: String, typ: BoolexType): Boolean = {
    if (getSymbolType(symbol).nonEmpty) {
      return false
    } else {
      scopes.head.put(symbol, typ)
      return true
    }
  }

  def completeCircuit(symbol: String, outputs: Int): Boolean = (for {
    scope <- scopes.find(_.contains(symbol))
    typ <- scope.get(symbol)
    if typ.isInstanceOf[PartialCircuitType]
  } yield {
    scope.put(
      symbol,
      CircuitType(typ.asInstanceOf[PartialCircuitType].inputs, outputs)
    )
  }).nonEmpty

  def fillPromise(symbol: String): Boolean = {
    val scopeOpt = scopes.find(_.contains(symbol))
    if (
      scopeOpt.nonEmpty &&
      scopeOpt
        .flatMap(_.get(symbol))
        .exists(!_.isInstanceOf[BooleanPromiseType])
    ) {
      return false
    } else {
      scopeOpt.getOrElse(scopes.head).put(symbol, BooleanType)
      return true
    }
  }

  // return all promises made by this scope
  def getPersonalPromises: Seq[(String, Position)] = (for {
    (symbol, typ) <- scopes.head
    if typ.isInstanceOf[BooleanPromiseType]
  } yield {
    (symbol -> typ.asInstanceOf[BooleanPromiseType].pos)
  }).toList
}
