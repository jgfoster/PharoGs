set compile_env: 2

category: 'instance creation'
classmethod: MonitorDelay
signalLock: aSemaphore afterMSecs: anInteger inMonitor: aMonitor queue: anOrderedCollection
	<PharoGsError>
	self _gsError "
	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ (self new setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection) schedule; yourself"
%

category: 'deprecated'
method: MonitorDelay
schedulerSignalWaitingProcess
	<PharoGsError>
	self _gsError "
	""The delay time has elapsed; signal the waiting process.""
	beingWaitedOn := false.
	monitor signalLock: delaySemaphore inQueue: queue.
"
%

category: 'private'
method: MonitorDelay
setDelay: anInteger forSemaphore: aSemaphore monitor: aMonitor queue: anOrderedCollection
	<PharoGsError>
	self _gsError "
	monitor := aMonitor.
	queue := anOrderedCollection.
	self setDelay: anInteger forSemaphore: aSemaphore."
%

category: 'private'
method: MonitorDelay
timingPrioritySignalExpired
	<PharoGsError>
	self _gsError "
	""The delay time has elapsed; signal the waiting process.""
	beingWaitedOn := false.
	monitor signalLock: delaySemaphore inQueue: queue.
"
%

set compile_env: 0
