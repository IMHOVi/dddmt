package ru.imho.dddmt.core

import Base._

object BaseExec {

  trait JobFactory {
    def newJob(ns: NodeSpace, param: ParameterValue): Job
  }

  abstract sealed class JobStatus
  case class JobStatusSuccess() extends JobStatus
  case class JobStatusSkipped() extends JobStatus
  case class JobStatusKilled() extends JobStatus
  case class JobStatusFailure(error: Throwable) extends JobStatus

  trait Job {
    /**
     * Asynchronously execute the job. Must return immediately.
     *
     * @param completion should be called by the job as it completes
     */
    def execute(completion: JobStatus => Unit): Unit
    
    /**
     * Kills a running job. 
     */
    def kill: Unit
  }

  /**
   * Timing policy for executing jobs
   *
   * @param hardJobTimeout Number of seconds to allow a job to run.
   *        The job is killed as this timer expires.
   * @param softDeadline Time point (ms since Epoch) when to stop launching new jobs.
   * @param hardDeadline Time point (ms since Epoch) when to kill all running jobs and
   *         return immediately.
   */
  case class TimingPolicy(
    val hardJobTimeout: Int,
    val softDeadline: Option[Long],
    val hardDeadline: Option[Long])
    
  
  /**
   * What we do if a job fails
   */
  object FailurePolicies extends Enumeration {
    /**
     * ignore failed jobs and proceed as usual
     */
    val	ignore = Value
    /**
     * wait for already running jobs to complete and return. Launch no new tasks.
     */
    val	finish = Value
    /**
     * abort all running tasks and return ASAP
     */
    val failfast = Value
  } 
  
  type FailurePolicy = FailurePolicies.Value
    
  trait Runner {
    /**
     * Execute specified jobs, trying to parallelize work according to the policy settings.
     * 
     * @param jobs list of jobs to run
     * @return 
     */
    def execSync(jobs: List[Job]): List[JobStatus]
  }

}