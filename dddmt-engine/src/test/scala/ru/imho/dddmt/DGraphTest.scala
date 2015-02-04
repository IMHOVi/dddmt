package ru.imho.dddmt


import Base._
import DGraph._
import org.scalatest.FlatSpec
import org.scalamock.scalatest.MockFactory
import java.net.URI

class DGraphTest extends FlatSpec with MockFactory  { 
  import DGraphTest._
    
  "DGraph" should "Build ftp -> hdfs graph" in {
    val period = stub[Period]         
    val ntech = stub[NodeTechnology]
    
    val nsftp: NodeSpace = new LongNodeSpace("nsftp", ntech)
	val nslogs: NodeSpace = new LongNodeSpace("nslogs", ntech)
    
    val nsDepMap = Map((nslogs, nsftp) -> DepAttr(MTime))
    val weavingPolicy = new DefaultWeavingPolicy(nsDepMap)
    
    val g = DGraph.buildDGraph(period, nsDepMap, weavingPolicy)
	println(g)
	
	val nf1 = nsftp.nodeOf(LongParameterValue(1))
	val nf2 = nsftp.nodeOf(LongParameterValue(2))
	val nl1 = nslogs.nodeOf(LongParameterValue(1))
	val nl2 = nslogs.nodeOf(LongParameterValue(2))
	
	assert(g.get(nf1).isDirectPredecessorOf(g get nl1))
	assert(g.get(nf2).isDirectPredecessorOf(g get nl2))
	assert(g.get(nf1).isIndependentOf(g get nl2))
	
	val s = new DGraph(g)
    println(s.dot)
  }
  
  
  def outdatedNodes(nsr: List[URI] => List[NodeState], nsar: URI => Option[NodeStateA]) = {
    val period = stub[Period]         
    val ntech = stub[NodeTechnology]
    
    (ntech newNodeStateResolver _) when (MTime) returns nsr
    (ntech newNodeStateAggregateResolver _) when (MTime) returns nsar
    
    val nsftp: NodeSpace = new LongNodeSpace("nsftp", ntech)
	val nslogs: NodeSpace = new LongNodeSpace("nslogs", ntech)
    
    val nsDepMap = Map((nslogs, nsftp) -> DepAttr(MTime))
    val weavingPolicy = new DefaultWeavingPolicy(nsDepMap)
    
    val dg = new DGraph(period, nsDepMap, weavingPolicy)
    
    val dgs = dg.newSession
    
    dgs.leftUpTraverser.collect    
  }
  
  
  "DGraph" should "Detect changes (mtime)" in {
    assertResult(Nil)(outdatedNodes(_.map(u => LongNodeState(10)), u => Some(LongNodeStateA(10))))    
    assertResult(10)(outdatedNodes(_.map(u => LongNodeState(11)), u => Some(LongNodeStateA(10))).size)
    
    val r = outdatedNodes(
        _.map(u => LongNodeState(if(u.getSchemeSpecificPart.toLong == 3) 11 else 10)), 
        u => Some(LongNodeStateA(10)))
    assertResult(1)(r.size)   
    assertResult(
        "List((Node(nodespace:nsftp,LongParameterValue(3)),Map(nodespace:nsftp -> LongNodeStateA(11))))")(
         r.toString)
  } 
}

object DGraphTest {
  case class LongNodeState(val value: Long) extends NodeState
  case class LongNodeStateA(val value: Long) extends NodeStateA
  
  abstract class LongNodeStateType extends NodeStateType {
    type ThisNodeState = LongNodeState
    type ThisNodeStateA = LongNodeStateA
  }
  
  object MTime extends LongNodeStateType {
    /**
     * Aggregate's value is that of most recent of the predecessors
     */
    def aggregate(nprec: List[ThisNodeState]): ThisNodeStateA = 
      LongNodeStateA(nprec.map(_.value).max)
    def needsRebuilding(n: ThisNodeStateA, nsaprec: ThisNodeStateA): Boolean =
      nsaprec.value > n.value    
  }

  object LongParameterType extends ParameterType {
    val id = "testPT"
    def getValues(p: Period) = (1 to 10).map(LongParameterValue(_)).toStream
  }
  
  case class LongParameterValue(val value: Long) extends ParameterValue {
    val parameterType = LongParameterType
    def serial = value
    def conformsTo(other: ParameterValue) = other match {
      case LongParameterValue(v) if v == value => true
      case _ => false
    }
  }
  
  class LongNodeSpace(id: String, ntech: NodeTechnology) extends
  	AbstractNodeSpace("nsftp", LongParameterType, ntech) {
      def uri(parameterValue: ParameterValue): URI = 
        new URI("long", parameterValue.asInstanceOf[LongParameterValue].value.toString, null)
    }
  
  
}