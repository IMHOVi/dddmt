package ru.imho.dddmt.core

import Base._
import scala.annotation.tailrec
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge._
import scalax.collection.io.dot._
import scala.Option.option2Iterable
import scala.collection.immutable.Stream.consWrapper


/**
 * Inventory graph-related methods
 * 
 * @author VVybornov
 */
class DGraph(g: DGraph.DGraphT) {
  import DGraph._
  
  def this(period: Period, nsDepMap: NSDepMap, weavingPolicy: WeavingPolicy) =
    this(DGraph.buildDGraph(period, nsDepMap, weavingPolicy))
  
  /**
   * Returns DOT language description of the graph suitable for visualization through Graphviz or Vis.js
   */
  def dot = {
    val dotRoot = DotRootGraph(directed = true, id = Some("G"))

    def nodeLabel[N <: DGraphT#NodeT](n: N) = n.toString() //+ "/" + ranks(g get n.value)

    def edgeTransformer(e: DGraphT#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
      Some((dotRoot,
        DotEdgeStmt(
          nodeLabel(e.edge.from),
          nodeLabel(e.edge.to))))

    g.toDot(dotRoot, edgeTransformer)
  }
  
  def validate = if(!g.isAcyclic)
    throw new IllegalStateException("There are cycles in inventory graph")
  
  def newSession = new DGraphSession(g, buildResolverAggregate(g))
  
}

object DGraph {
  //type Edge = NodeStateType

  def validateNSDepMap(nsDepMap: NSDepMap) = {
    def buildNSGraph(nsDepMap: NSDepMap) = Graph.from(
      edges = for (((ns, nsprec), nst) <- nsDepMap) yield LDiEdge(nsprec, ns)(nst))
      
    if (!buildNSGraph(nsDepMap).isAcyclic)
      throw new IllegalStateException("There are cycles in nodespace dependency graph")
  }
    
  type DGraphT = Graph[Node, LDiEdge]

  def buildDGraph(period: Period, nsDepMap: NSDepMap, weavingPolicy: WeavingPolicy): DGraphT = {
    val edges = for (
      n <- nsDepMap.map(_._1._1).toSet.flatMap((_: NodeSpace).nodeStream(period));
      nprec <- nsDepMap.map(_._1._2).toSet.flatMap((_: NodeSpace).nodeStream(period));
      wp <- weavingPolicy(n, nprec) if (n != nprec)
    ) yield LDiEdge(nprec, n)(wp.nodeStateType)

    Graph.from(edges = edges)
  }
  
  
  /**
   * A number of functions for node state resolution
   * @param ns Resolves predecessor nodes' actual states
   * @param nsa Resolves current state aggregate of the node in focus
   * @param nst Resolves node state type type between node spaces (`n => nprec => st`)
   */
  class ResolverAggregate(
	var ns: NodeSpace => NodeStateType => NodeStateResolver, 
    var nsa: NodeSpace => NodeStateType => NodeStateAResolver,
    var nst: NodeSpace => NodeSpace => NodeStateType)
  
  def buildResolverAggregate(g: DGraphT) = {
    //build actual NSDepMap
    val m = g.edges.map(e => (e._1.nodeSpace, e._2.nodeSpace) -> e.label.asInstanceOf[NodeStateType]).toMap
    
    type TTemp = (
        Map[(NodeSpace, NodeStateType), NodeStateResolver],
        Map[(NodeSpace, NodeStateType), NodeStateAResolver])
        
    val (fns, fnsa) = m.foldLeft[TTemp]((Map.empty, Map.empty)){ 
      case ((mns, mnsa), ((nprec, n), nst)) =>
        (if(mns contains (nprec, nst)) mns
          else mns + (((nprec, nst), nprec.newNodeStateResolver(nst))),
         if(mnsa contains(n, nst)) mnsa
          else mnsa + (((n, nst), n.newNodeStateAggregateResolver(nst)))
              )
    }
    
    new ResolverAggregate(
        ns => nst => fns((ns, nst)), 
        ns => nst => fnsa((ns, nst)), 
        ns => nsprec => m((nsprec, ns))
        )
  }
  
  type NodeInfo = (Node, FullNodeStateA)
  

  /**
   * Container for inventory graph.
   * 
   * @param g input graph
   * @param resolver state resolver
   */
  class DGraphSession(g: DGraphT, resolver: ResolverAggregate) {
        
    /**
     * This function takes all n's predecessors, factors them out by nodespace,
     * and then for each ns determines if n is outdated WRT predecessors on that nodespace
     * 
     * @return For each NS, the actual NSA of predecessor nodes, if it is detected that rebuild is needed. 
     * `None` if `n` is actual and does not need rebuilding. 
     */
    def needsRebuilding(n: g.NodeT): Option[FullNodeStateA] = {      
      val predecessors = 
        n.diPredecessors.map { p: g.NodeT => (p: Node, p.nodeSpace) }
      val factored = 
        for ((pns, pnset) <- predecessors.groupBy(_._2); nst = resolver.nst(n.nodeSpace)(pns) )
          yield (pns, (nst, resolver.ns(pns)(nst)(pnset.map(_._1: Node).toList.sorted)))
      val actualNSA = 
        for ((pns, (nst, l)) <- factored)
          yield (pns, nst.getActualNodeState(resolver.nsa(n.nodeSpace)(nst)(n), l))         
      if(actualNSA.find(_._2._2).isDefined)  
        Some(for((pns, (nsa, _)) <- actualNSA) yield (pns, nsa))
      else
        None
    }
   
    /**
     * Rank of `n` is max of length of path to `n` (ending at `n`)   
     */
    private lazy val rank: scala.collection.Map[g.NodeT, Int] = {
      val result = scala.collection.mutable.HashMap.empty[g.NodeT, Int]
      @tailrec
      def iter(rank: Int, nodes: Set[g.NodeT]): Unit = 
      	if (!nodes.isEmpty) { 
      	  result ++= nodes.map((_, rank))
      	  iter(rank + 1, nodes.flatMap(_.diSuccessors))
      	}
      
      iter(0, g.nodes.toSet)
      result
    }  
        
    /**
     * The traverser scans the graph for obsolete nodes. 
     * Dependencies of all obsolete nodes found within same traversal session are 
     * excluded from scan for this session.
     * Traversal session is a sequence of calls `(ni, ts) = ts.take until not empty`   
     */
    trait TraversalState {
      /**
       * the node, along with the new context where scan excludes the node returned  
       */
      type Result = (NodeInfo, TraversalState)
      
      /**
       * Extracts one obsolete node out of this traverser, if any
       * @return the node found, if any, along with the new context `ncx`, where 
       * scan for obsolete nodes excludes the node returned. None if no more obsolete nodes
       * available for this scan 
       */
      def takeOne: Option[Result]
      /**
       * A stream good for scanning for a number of obsolete nodes using simple take in turn
       *
       * @param ns list of nodes where to prepend result of scan
       *
       */

      private def s: Stream[Result] = takeOne match {
        case Some((n, ncx)) => (n, ncx) #:: ncx.s
        case None => Stream.Empty
      } 
      
      private def resultOf(inOp: Stream[Result] => Stream[Result]): (List[NodeInfo], TraversalState) = {
        val l = inOp(s).toList
        if(l.isEmpty) (Nil, this) else (l.map(_._1), l.last._2)
      }
      
      def take(count: Int): (List[NodeInfo], TraversalState) = 
        resultOf(_.take(count))
        
      def collect: List[NodeInfo] = 
        resultOf(identity)._1
    } 
    
    
    /**
     * This traverser tries to do as much job as possible at a lower layer before passing to
     * higher. Successors (both direct and transitive) of obsolete nodes are excluded from scan.
     * 
     */
    
    def leftUpTraverser: TraversalState = {
      class Cx(remaining: List[g.NodeT], exclusions: Set[g.NodeT]) extends TraversalState {
       @tailrec
        final def takeOne: Option[Result] = remaining match {
          case Nil =>
            None
          case n :: r =>
            if (exclusions contains n) {
              //bypass, adding n's descendants to exclusions 
              new Cx(r, exclusions ++ n.diSuccessors).takeOne
            } else {
              needsRebuilding(n) match {
                case Some(fnsa) =>
                  Some(((n: Node, fnsa), new Cx(r, exclusions ++ n.diSuccessors)))
                case None =>
                  new Cx(r, exclusions).takeOne
              }
            }
        }
      }
      
      new Cx(g.nodes.toList.sortBy(rank(_)), Set.empty)
    }
        
  }


}