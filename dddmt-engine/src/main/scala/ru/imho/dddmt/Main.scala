package ru.imho.dddmt

import ru.imho.dddmt.config.typesafe.UniverseBuilder
import ru.imho.dddmt.std.StandardParameterTypes._
import ru.imho.dddmt.core._
import Base._

/**
 * Driver program
 * 
 * @author VVybornov
 *
 */
object Main {
  
  def main(args: Array[String]): Unit = {
    
    val period = YMDHRangePeriod(
        YMDHParameterType.fromString(args(0)), 
        YMDHParameterType.fromString(args(1)))
    val u = UniverseBuilder.buildUniverse(None)
    
    DGraph.validateNSDepMap(u.nsDeps)
    
    val dg = new DGraph(period, u.nsDeps, new DefaultWeavingPolicy(u.nsDeps))
 
    //println(dg.dot)
    val outdated = dg.newSession.leftUpTraverser.collect
    println(outdated)
  }

}