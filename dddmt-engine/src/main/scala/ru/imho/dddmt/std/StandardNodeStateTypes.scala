package ru.imho.dddmt.std

import ru.imho.dddmt.core.Base._

object StandardNodeStateTypes {

  val timeFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
  
  case class MsTimeNodeState(val value: Long) extends NodeState {
    override def toString = "msTime:" + timeFormat.format(new java.util.Date(value))
    }
  case class MsTimeNodeStateA(val value: Long) extends NodeStateA {
    override def toString = "msTimeA:" + timeFormat.format(new java.util.Date(value))
    }
  
  trait FTime extends NodeStateType {
    
    type ThisNodeState = MsTimeNodeState
    type ThisNodeStateA = MsTimeNodeStateA
    
    /**
     * Aggregate's value is that of most recent of the predecessors
     */
    def aggregate(nprec: List[ThisNodeState]): ThisNodeStateA = 
      MsTimeNodeStateA(nprec.map(_.value).max)
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