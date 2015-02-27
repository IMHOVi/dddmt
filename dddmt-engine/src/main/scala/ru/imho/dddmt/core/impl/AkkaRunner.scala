package ru.imho.dddmt.core.impl

import ru.imho.dddmt.core.BaseExec._
import akka.actor._
import akka.routing._
import scala.collection.mutable.ArrayBuffer

class AkkaRunner(parallelism: Int, timingPolicy: TimingPolicy, failurePolicy: FailurePolicy) 
extends Runner with Actor {
  
  class JobActor extends Actor {
    var current: Option[Job] = None
    def receive = {
      case _ => Unit
    }
  }
  
  def receive = {
    case _ => Unit
  }
  
  val r = context.actorOf(Props[JobActor].withRouter(RoundRobinPool(nrOfInstances = parallelism)))
  
  val req = new ArrayBuffer[Job]
  val resp = new ArrayBuffer[JobStatus]
    
  def execSync(jobs: List[Job]): List[JobStatus] = {
    Nil
  }
}

object AkkaRunner {
  val as = ActorSystem("AkkaRunner")
  
  def apply(parallelism: Int, timingPolicy: TimingPolicy, failurePolicy: FailurePolicy) = 
    as.actorOf(Props(new AkkaRunner(parallelism, timingPolicy, failurePolicy)))
    
  abstract sealed class Cmd
  case class Run(job: Job) extends Cmd
  case class Kill() extends Cmd
  
}