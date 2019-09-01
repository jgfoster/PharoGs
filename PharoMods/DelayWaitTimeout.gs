set compile_env: 2

category: 'testing'
method: DelayWaitTimeout
isExpired
	<PharoGsError>
	self _gsError "
	""Did this timeout fire before the associated semaphore was signaled?""
	^expired"
%

category: 'deprecated'
method: DelayWaitTimeout
schedulerSignalWaitingProcess
	<PharoGsError>
	self _gsError "
	""Release the given process from the semaphore it is waiting on.
	This method relies on running at highest priority so that it cannot be preempted
	by the process being released.""
	beingWaitedOn := false.
	""Release the process but only if it is still waiting on its original list""
	process suspendingList == delaySemaphore ifTrue:[
		expired := true.
		process suspend; resume.
		].
"
%

category: 'private'
method: DelayWaitTimeout
setDelay: anInteger forSemaphore: aSemaphore
	<PharoGsError>
	self _gsError "
	super setDelay: anInteger forSemaphore: aSemaphore.
	process := Processor activeProcess.
	expired := false."
%

category: 'private'
method: DelayWaitTimeout
timingPrioritySignalExpired
	<PharoGsError>
	self _gsError "
	""Release the given process from the semaphore it is waiting on.
	This method relies on running at highest priority so that it cannot be preempted
	by the process being released.""
	beingWaitedOn := false.
	""Release the process but only if it is still waiting on its original list""
	process suspendingList == delaySemaphore ifTrue:[
		expired := true.
		process suspend; resume.
		].
"
%

category: 'waiting'
method: DelayWaitTimeout
wait
	<PharoGsError>
	self _gsError "
	""Wait until either the semaphore is signaled or the delay times out""
	[self schedule.
	""It is critical that the following has no suspension point so that
	the test and the wait primitive are atomic. In addition, if the delay
	is no longer being waited on while entering the way we know that it 
	is expired because the delay has already fired.""
	beingWaitedOn 
		ifTrue:[delaySemaphore wait]
		ifFalse:[expired := true]] ensure:[self unschedule].
	^self isExpired
"
%

category: 'waiting'
method: DelayWaitTimeout
waitOnCompletion: completionBlock onTimeout: timeoutBlock
	<PharoGsError>
	self _gsError "
	""Wait until either the semaphore is signaled or the delay times out.
	If the delay times out execute timeoutBlock, otherwise if the semaphore is signaled execute completionBlock.
	Return the value returned by the executed block.""
	^ self wait
		ifTrue: [ timeoutBlock value ]
		ifFalse: [ completionBlock value]"
%

set compile_env: 0
