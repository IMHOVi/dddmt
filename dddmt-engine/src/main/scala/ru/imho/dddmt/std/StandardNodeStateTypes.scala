package ru.imho.dddmt.std

import ru.imho.dddmt.core.Base._

object StandardNodeStateTypes {
  case class LongNodeState(val value: Long) extends NodeState
  case class LongNodeStateA(val value: Long) extends NodeStateA
  
  abstract class LongNodeStateType extends NodeStateType {
    type ThisNodeState = LongNodeState
    type ThisNodeStateA = LongNodeStateA
  }
  
  trait FTime extends LongNodeStateType {
    /**
     * Aggregate's value is that of most recent of the predecessors
     */
    def aggregate(nprec: List[ThisNodeState]): ThisNodeStateA = 
      LongNodeStateA(nprec.map(_.value).max)
    def needsRebuilding(n: Option[ThisNodeStateA], nsaprec: ThisNodeStateA): Boolean =
      nsaprec.value > n.getOrElse(nonexistentA).value
      
    def fromLongTime(tm: Long) = new ThisNodeState(tm)
    def fromLongTimeA(tm: Long) = new ThisNodeStateA(tm)
    val nonexistent = new ThisNodeState(0)    
    val nonexistentA = new ThisNodeStateA(0)    
  }
  
  object MTime extends FTime {
    val id = "mtime"
  }

  def asList: List[NodeStateType] = 
    MTime :: 
    Nil
}