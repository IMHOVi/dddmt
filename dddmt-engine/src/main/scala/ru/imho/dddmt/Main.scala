package ru.imho.dddmt

import ru.imho.dddmt.config.typesafe.UniverseBuilder
import ru.imho.dddmt.std.StandardParameterTypes._
import ru.imho.dddmt.core._
import Base._
import org.slf4j.LoggerFactory
import ru.imho.dddmt.core.impl.DGraph

/**
 * Driver program
 * 
 * @author VVybornov
 *
 */
object Main {
  val logger = LoggerFactory.getLogger("Main")
  
  def main(args: Array[String]): Unit = {
    var build = true
    var dot = false
    var maxCommandsPerIteration = 16
    
    val period = YMDHRangePeriod(
        YMDHParameterType.fromString(args(0)), 
        YMDHParameterType.fromString(args(1)))
    val u = UniverseBuilder.buildUniverse(None)
    
    DGraph.validateNSDepMap(u.nsDeps)
    
    val dg = new DGraph(period, u.nsDeps, new DefaultWeavingPolicy(u.nsDeps))
 
    if(dot) {
      println("------------------------------------------------")
      println(dg.dot)
      println("------------------------------------------------")
    }
    
    def iteration: Boolean = {
      val outdated = dg.newSession.leftUpTraverser.collect
      logger.debug("Outdated: {}", outdated)
      val commands = outdated.take(maxCommandsPerIteration).map { case (n, a) => 
        try {
          u.jobFactories(n.nodeSpace.id).newJob(n.nodeSpace, n.parameterValue)
        } catch {
          case t: NoSuchElementException => sys.error(s"No build command defined for ns `${n.nodeSpace.id}`")
        }
      }
      true
    }
  }

}