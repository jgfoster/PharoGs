set compile_env: 0

run
"This is a global in Pharo"
Pharo
	at: #'Processor' put: (Globals at: #'Processor');
	yourself.
%

set compile_env: 2

category: 'accessing'
classmethod: Processor
activePriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:activePriority 
%

category: 'accessing'
classmethod: Processor
activeProcess

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:activeProcess 
%

category: 'private'
classmethod: Processor
anyProcessesAbove: highestPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:anyProcessesAbove: highestPriority 
%

category: 'accessing'
classmethod: Processor
backgroundProcess

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:backgroundProcess 
%

category: 'priority names'
classmethod: Processor
highIOPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:highIOPriority 
%

category: 'accessing'
classmethod: Processor
highestPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:highestPriority 
%

category: 'accessing'
classmethod: Processor
highestPriority: newHighestPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:highestPriority: newHighestPriority 
%

category: 'self evaluating'
classmethod: Processor
isSelfEvaluating

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:isSelfEvaluating 
%

category: 'priority names'
classmethod: Processor
lowestPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:lowestPriority 
%

category: 'priority names'
classmethod: Processor
lowIOPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:lowIOPriority 
%

category: 'CPU usage tally'
classmethod: Processor
fonextReadyProcesso

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:nextReadyProcess 
%

category: 'accessing'
classmethod: Processor
preemptedProcess

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:preemptedProcess 
%

category: 'printing'
classmethod: Processor
printOn: aStream

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:printOn: aStream 
%

category: 'removing'
classmethod: Processor
remove: aProcess ifAbsent: aBlock

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:remove: aProcess ifAbsent: aBlock 
%

category: '*Reflectivity'
classmethod: Processor
rfactiveProcess

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:rfactiveProcess 
%

category: 'accessing'
classmethod: Processor
scanSchedule: aBlock startingAt: topPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:scanSchedule: aBlock startingAt: topPriority 
%

category: 'process state change'
classmethod: Processor
suspendFirstAt: aPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:suspendFirstAt: aPriority 
%

category: 'process state change'
classmethod: Processor
suspendFirstAt: aPriority ifNone: noneBlock

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:suspendFirstAt: aPriority ifNone: noneBlock 
%

category: 'priority names'
classmethod: Processor
systemBackgroundPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:systemBackgroundPriority 
%

category: 'XX'
classmethod: Processor
foo

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:foo 
%

category: 'CPU usage tally'
classmethod: Processor
tallyCPUUsageFor: seconds

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:tallyCPUUsageFor: seconds 
%

category: 'CPU usage tally'
classmethod: Processor
tallyCPUUsageFor: seconds every: msec

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:tallyCPUUsageFor: seconds every: msec 
%

category: 'process state change'
classmethod: Processor
terminateActive

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:terminateActive 
%

category: 'priority names'
classmethod: Processor
timingPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:timingPriority 
%

category: 'priority names'
classmethod: Processor
userBackgroundPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:userBackgroundPriority 
%

category: 'priority names'
classmethod: Processor
userInterruptPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:userInterruptPriority 
%

category: 'priority names'
classmethod: Processor
userSchedulingPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:userSchedulingPriority 
%

category: 'accessing'
classmethod: Processor
waitingProcessesAt: aPriority

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:waitingProcessesAt: aPriority 
%

category: 'process state change'
classmethod: Processor
yield

	<PharoGs>
    ^ProcessorScheduler @env0:scheduler @env2:yield 
%

set compile_env: 0
