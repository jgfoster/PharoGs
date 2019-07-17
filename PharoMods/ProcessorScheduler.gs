set compile_env: 2

category: 'background process'
classmethod: ProcessorScheduler
relinquishProcessorForMicroseconds: anInteger 
	"Platform specific. This primitive is used to return processor cycles 
    to the host operating system when Pharo's idle process is running 
    (i.e., when no other Pharo process is runnable). On some platforms, 
    this primitive causes the entire Pharo application to sleep for 
    approximately the given number of microseconds. No Pharo process can 
    run while the Pharo application is sleeping, even if some external 
    event makes it runnable. On the Macintosh, this primitive simply 
    calls GetNextEvent() to give other applications a chance to run. 
    On platforms without a host operating system, it does nothing. 
    This primitive should not be used to add pauses to a Pharo process; 
    use a Delay instead." 
    <PharoGs>

    "don't fail if primitive is not implemented, just do nothing"
%

category: 'accessing'
method: ProcessorScheduler
activePriority 
	"Answer the priority level of the currently running Process."
	
	<PharoGs>
	^self @env0:activePriority
%

category: 'accessing'
method: ProcessorScheduler
activeProcess 
	"Answer the currently running Process."
	
	<PharoGs>
	^self @env0:activeProcess
%

category: 'private'
method: ProcessorScheduler
anyProcessesAbove: highestPriority
	"Do any instances of Process exist with higher priorities?"
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ProcessorScheduler
backgroundProcess
	"Answer the background process"
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ProcessorScheduler
highestPriority
	"Answer the number of priority levels currently available for use." 
	"NB: If you are looking to set a priority when forking a process, 
	please use the methods in 'priority names' protocol"

	<PharoGs>
	^self @env0:highestPriority
%

category: 'accessing'
method: ProcessorScheduler
highestPriority: newHighestPriority
	
	<PharoGsError>
	^self _gsError
%

category: 'priority names'
method: ProcessorScheduler
highIOPriority
	"Answer the priority at which the most time critical input/output  
	processes should run. An example is the process handling input from a  
	network."
	
	<PharoGs>
	^self @env0:highIOPriority
%

category: 'self evaluating'
method: ProcessorScheduler
isSelfEvaluating
	
	<PharoGs>
	^self == Processor
%

category: 'priority names'
method: ProcessorScheduler
lowestPriority 
	"Return the lowest priority that is allowed with the scheduler"
	
	<PharoGs>
	^self @env0:lowestPriority
%

category: 'priority names'
method: ProcessorScheduler
lowIOPriority
	"Answer the priority at which most input/output processes should run.  
	Examples are the process handling input from the user (keyboard,  
	pointing device, etc.) and the process distributing input from a network."
	
	<PharoGs>
	^self @env0:lowIOPriority
%

category: 'CPU usage tally'
method: ProcessorScheduler
nextReadyProcess
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ProcessorScheduler
preemptedProcess
	
	<PharoGsError>
	^self _gsError
%

category: 'printing'
method: ProcessorScheduler
printOn: aStream 
	
	<PharoGs>
	self isSelfEvaluating ifFalse: [^super printOn: aStream]. 
	aStream nextPutAll: #Processor
%

category: 'removing'
method: ProcessorScheduler
remove: aProcess ifAbsent: aBlock  
	"Remove aProcess from the list on which it is waiting for the processor  
	and answer aProcess. If it is not waiting, evaluate aBlock."
	
	<PharoGsError>
	^self _gsError
%

category: '*Reflectivity'
method: ProcessorScheduler
rfactiveProcess
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ProcessorScheduler
scanSchedule: aBlock startingAt: topPriority 
	
	<PharoGsError>
	^self _gsError
%

category: 'process state change'
method: ProcessorScheduler
suspendFirstAt: aPriority  
	
	<PharoGsError>
	^self _gsError
%

category: 'process state change'
method: ProcessorScheduler
suspendFirstAt: aPriority ifNone: noneBlock  
	
	<PharoGsError>
	^self _gsError
%

category: 'priority names'
method: ProcessorScheduler
systemBackgroundPriority 
	"Answer the priority at which system background processes should run.  
	Examples are an incremental garbage collector or status checker."
	
	<PharoGs>
	^self @env0:systemBackgroundPriority
%

category: 'CPU usage tally'
method: ProcessorScheduler
tallyCPUUsageFor: seconds 
	
	<PharoGsError>
	^self _gsError
%

category: 'CPU usage tally'
method: ProcessorScheduler
tallyCPUUsageFor: seconds every: msec 
	
	<PharoGsError>
	^self _gsError
%

category: 'process state change'
method: ProcessorScheduler
terminateActive 
	"Terminate the process that is currently running."
	
	<PharoGs>
	^self @env0:activeProcess @env0:terminate
%

category: 'priority names'
method: ProcessorScheduler
timingPriority
	"Answer the priority at which the system processes keeping track of real  
	time should run." 
	"BEFORE USING IT: please read the 'Original word of advice' in the 
	DelayScheduler class comment.  
	TL;DR: for top priority processes use 'Processor timingPriority - 1' 
	except if you really know what you are doing!"
	
	<PharoGs>
	^self @env0:timingPriority
%

category: 'priority names'
method: ProcessorScheduler
userBackgroundPriority
	"Answer the priority at which user background processes should run."
	
	<PharoGs>
	^self @env0:userBackgroundPriority
%

category: 'priority names'
method: ProcessorScheduler
userInterruptPriority
	"Answer the priority at which user processes desiring immediate service  
	should run. Processes run at this level will preempt the window  
	scheduler and should, therefore, not consume the processor forever."
	
	<PharoGs>
	^self @env0:userInterruptPriority
%

category: 'priority names'
method: ProcessorScheduler
userSchedulingPriority
	"Answer the priority at which the window scheduler should run."
	
	<PharoGs>
	^self @env0:userSchedulingPriority
%

category: 'accessing'
method: ProcessorScheduler
waitingProcessesAt: aPriority
	
	<PharoGsError>
	^self _gsError
%

category: 'process state change'
method: ProcessorScheduler
yield 
	"Give other Processes at the current priority a chance to run." 
	<PharoGs>

    ^self @env0:yield
%

set compile_env: 0
