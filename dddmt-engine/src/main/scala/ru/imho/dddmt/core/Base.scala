package ru.imho.dddmt.core

import java.net.URI

/**
 * DDDMT Base entities
 * 
 * @author VVybornov
 *
 */
object Base {
  
  /**
   * Something IDentifiable, with ID immutable
   */
  
  trait Identifiable {
    val id: String
  }
  
  /**
   * Time period (range) where to generate all parameters 
   */
  trait Period { }
  
  /**
   * The node state describing state of this sole node
   */
  trait NodeState {
    //val nodeStateType: NodeStateType
  }

  /**
   * The node state aggregate describing group state (e.g., state of all of or some of node's predecessors)
   */
  trait NodeStateA {
    //val nodeStateType: NodeStateType
  }
  
  type URINodeStateResolver = List[URI] => List[NodeState]
  
  type URINodeStateAResolver = URI => Option[NodeStateA]
  
  
  /**
   * For example: HdfsFile, HdfsFolder, FtpFile, Bin
   */
  trait NodeTechnology extends Identifiable { 
    def newNodeStateResolver(nst: NodeStateType): URINodeStateResolver
    
    def newNodeStateAggregateResolver(nst: NodeStateType): URINodeStateAResolver
  }
  
  /**
   * For example: Null, Day, Hour
   */
  trait ParameterType extends Identifiable {
    /**
     * Gets a stream of parameter values for the entire (closed) period 
     */
    def getValues(p: Period): Stream[ParameterValue]
  }
  
  trait ParameterValue {
    val parameterType: ParameterType
    /**
     * This provides bijection between all values of a ParameterType and Longs (separate for each ParameterType)
     */
    def serial: Long
    
    /**
     * Whether this value conforms to some other value. 
     * E.g. an hour conforms to its day.
     * 
     * Conformance is: a) reflexive (a value conforms to itself) and b) transitive
     */
    def conformsTo(other: ParameterValue): Boolean
    
    def <=(other: ParameterValue) = parameterValueOrdering.lteq(this, other)
  }
  
  implicit val parameterValueOrdering: Ordering[ParameterValue] = 
    Ordering.by(p => (p.parameterType.id, p.serial))
  
  /**
   * NodeSpace is a collection of nodes referring to same physical entity.
   * Different implementations of NodeSpace provide support for various combinations of parameter type 
   * and backing technology.  
   * For example: DailyHdfsFolder, HourlyHdfsFile
   */
  trait NodeSpace extends Identifiable {
    val parameterType: ParameterType
    val nodeTechnology: NodeTechnology
    def uri(parameterValue: ParameterValue): URI
    
    def nodeOf(parameterValue: ParameterValue) = 
      Node(this, parameterValue)
    
    def newNodeStateResolver(nst: NodeStateType): NodeStateResolver = {
      val nsr = nodeTechnology.newNodeStateResolver(nst)
      nl => nsr(nl.map(n => uri(n.parameterValue)))
    }
    
    def newNodeStateAggregateResolver(nst: NodeStateType): NodeStateAResolver = {
      val nsar = nodeTechnology.newNodeStateAggregateResolver(nst)
      n => nsar(uri(n.parameterValue))
    }
    
    def nodeStream(p: Period): Stream[Node] = parameterType.getValues(p).map(nodeOf)
    
   }
  
  abstract class AbstractNodeSpace(val id: String, val parameterType: ParameterType, val nodeTechnology: NodeTechnology)
    extends NodeSpace 	
  
  implicit val nodeSpaceOrdering: Ordering[NodeSpace] = Ordering.by(_.id)
  
  /**
   * Node is most precise unit of dependency. A dependency is established between two nodes.
   * On the other hand, node is is a "virtualized" unit of dataset operation. 
   * It may be a concrete HDFS file, folder, FTP file, etc.
   */
  case class Node(val nodeSpace: NodeSpace, val parameterValue: ParameterValue)
  
  implicit val nodeOrdering: Ordering[Node] = Ordering.by(n => (n.nodeSpace, n.parameterValue)) 
  
  type NodeStateResolver = List[Node] => List[NodeState]
  
  type NodeStateAResolver = Node => Option[NodeStateA]
  
  
  /**
   * "Nodespace _1 depends on _2"
   */
  type NSDep = (NodeSpace, NodeSpace)
  
  /**
   * Dependency Attributes for a pair of node spaces
   * @param nodeStateType used to detect obsolescence 
   */
  case class DepAttr(val nodeStateType: NodeStateType)
  
  type NSDepMap = Map[NSDep, DepAttr]
  
  /**
   * WeavingPolicy tells for any two nodes (n, nprec) whether n depends on nprec. Also
   * supplies DepAttr.
   */
  type WeavingPolicy = (Node, Node) => Option[DepAttr]
  
  /**
   * Default weaving policy.
   * n depends on nprec iff n's nodespace depends on that of nprec, and n.param conforms to nprec.param
   */
  class DefaultWeavingPolicy(nsDepMap: NSDepMap) extends WeavingPolicy {
    def apply(n: Node, nprec: Node): Option[DepAttr] = {
      val rv = nsDepMap.get((n.nodeSpace, nprec.nodeSpace))
      if (rv.isDefined && n.parameterValue.conformsTo(nprec.parameterValue)) rv else None
    }
  }
  
  
  /**
   * Collection of NSAs describing node's full state. This construct supports the situation where
   * a node has dependencies on two or more groups with different technologies each.
   * Keys are nodespaces preceding to that of the node; values are NSAs on the correspondent nodespace
   * dependency graph
   */
  type FullNodeStateA = Map[NodeSpace, NodeStateA]  
  
  
  /**
   * Node obsolescence detection policy
   */
  trait ObsolescenceDetectionPolicy {
    
    /**
     * Retrieves actual node state, along with figuring out if the node needs to be rebuilt.
     * 
     * @param nsa The node state aggregate, if any, associated with the current state of the node, retrieved from 
     * either file system itself or state storage. The current NSA contains aggregate state of all node's 
     * predecessors at the moment the node has been built.
     * @param nsprec actual state of all node's predecessors, from which the aggregate shall be built.
     * The list is ordered. The order is guaranteed to survive across restarts.
     * @return `(newNSA, ifNodeNeedsRebuilding)`. `newNSA` is the NSA associated with `nsprec` (i.e., actual NSA).  
     */
    def getActualNodeState(nsa: Option[NodeStateA], nsprec: List[NodeState]): (NodeStateA, Boolean)
  }
  
  /**
   * Node state metatype
   */
  trait NodeStateType extends Identifiable with ObsolescenceDetectionPolicy {
    type ThisNodeState <: NodeState
    type ThisNodeStateA <: NodeStateA
    def aggregate(nprec: List[ThisNodeState]): ThisNodeStateA
    def needsRebuilding(n: Option[ThisNodeStateA], nsaprec: ThisNodeStateA): Boolean
    
    def getActualNodeState(nsa: Option[NodeStateA], nsprec: List[NodeState]): (NodeStateA, Boolean) = {
      val nsaact = aggregate { nsprec map (_.asInstanceOf[ThisNodeState]) }

      (nsaact, needsRebuilding(nsa.map(_.asInstanceOf[ThisNodeStateA]), nsaact))      
    }
  }
  

}