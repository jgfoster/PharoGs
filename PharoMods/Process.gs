set compile_env: 2

category: 'process specific'
classmethod: Process
allocatePSKey: aPSVariable

	<PharoGs>
	| index |
	self psKeysSema critical: [
		| keys |
		keys := self psKeys.
		index := keys indexOf: aPSVariable.
		index == 0 ifTrue: [
			index := keys indexOf: nil.
			index == 0 
				ifTrue: [
					index := keys add: aPSVariable; size ]
				ifFalse: [
					"Yes, this is slow, but we have to make sure that if we reusing index,
					all existing processes having value at given index reset to nil.
					We don't care if new processes will be created during this loop,
					since their env variable will be initially nil anyways, hence nothing to reset "
					self @env0:allInstancesInMemory do: [:p | p resetPSValueAt: index ].
					keys at: index put: aPSVariable.
					]
		].

		aPSVariable isInheritable ifTrue: [ 
			keys := self inheritablePSKeys.
			(keys includes: index) ifFalse: [ keys add: index ]]
		]
	].

	^ index
%

category: 'accessing'
classmethod: Process
inheritablePSKeys

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Process_InheritablePSKeys' 
		ifAbsentPut: [Array new].
%

category: 'accessing'
classmethod: Process
psKeys

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Process_PSKeys' 
		ifAbsentPut: [Array new].
%

category: 'accessing'
classmethod: Process
psKeysSema

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Process_PSKeysSema' 
		ifAbsentPut: [Semaphore forMutualExclusion].
%

category: 'process specific'
classmethod: Process
updateInheritableKeys
"
	self updateInheritableKeys
"
	| keys |
	keys := self inheritablePSKeys.
	keys size: 0.
	ProcessSpecificVariable allSubclasses 
		select: [ :each | each isInheritable ] 
		thenDo: [ :each | keys add: each soleInstance index].
%

category: 'changing suspended state'
method: Process
activateReturn: aContext value: value 
	"Activate 'aContext return: value', so execution will return to aContext's sender '"

	<PharoGsError>
	self _gsError
%

category: 'printing'
method: Process
browserPrintString
	
	<PharoGs>
	^self browserPrintStringWith: self suspendedContext
%

category: 'printing'
method: Process
browserPrintStringWith: anObject

	<PharoGsError>
	| stream | 
	stream := (String new: 100) writeStream. 
	stream nextPut: $(. 
	self priority printOn: stream. 
	self isSuspended 
		ifTrue: [stream nextPut: $s]. 
	stream nextPutAll: ') '. 
	stream nextPutAll: self name. 
	stream nextPut: $:. 
	stream space. 
	stream nextPutAll: anObject asString. 
	^ stream contents
%

category: 'accessing'
method: Process
calleeOf: aContext
	"Return the context whose sender is aContext.  Return nil if aContext is on top.  
	Raise error if aContext is not in process chain."

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
complete: aContext
	"Run self until aContext is popped or an unhandled error is raised.  
	Return self's new top context, unless an unhandled error was raised 
	then return the signaler context (rather than open a debugger)."

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
completeStep: aContext
	"Resume self until aContext is on top, or if already on top, complete next step"

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
completeTo: aContext
	"Resume self until aContext is on top"

	<PharoGsError>
	self _gsError
%

category: 'accessing'
method: Process
copyStack

	<PharoGsError>
	self _gsError
%

category: 'debugging'
method: Process
debug
	
	<PharoGs>
	^ self debugWithTitle: 'Debug'.
%

category: 'debugging'
method: Process
debug: context title: title
	"Open debugger on self with context shown on top"

	<PharoGs>
	^ self debug: context title: title full: false. 
%

category: 'debugging'
method: Process
debug: context title: title full: bool

	<PharoGs>
	^ UIManager default 
		debugProcess: self  
		context: context  
		label: title  
		fullView: bool
%

category: 'debugging'
method: Process
debugWithTitle: title
	"Open debugger on self"

	<PharoGs>
	| context | 
	context := self isActiveProcess ifTrue: [self thisContext] ifFalse: [self suspendedContext]. 
	^ self debug: context title: title full: true. 
%

category: 'accessing'
method: Process
effectiveProcess
	"effectiveProcess is a mechanism to allow process-faithful debugging.  The debugger executes code 
	 on behalf of processes, so unless some effort is made the identity of Processor activeProcess is not 
	 correctly maintained when debugging code.  The debugger uses evaluate:onBehalfOf: to assign the 
	 debugged process as the effectiveProcess of the process executing the code, preserving process 
	 identity."

	<PharoGsError>
	self _gsError
%

category: 'private'
method: Process
environmentKeyNotFound

	<PharoGs>
	self error: 'Environment key not found'
%

category: 'private'
method: Process
evaluate: aBlock onBehalfOf: aProcess
	"Evaluate aBlock setting effectiveProcess to aProcess.  Used 
	 in the execution simulation machinery to ensure that 
	 Processor activeProcess evaluates correctly when debugging."

	<PharoGsError>
	self _gsError
%

category: 'initialization'
method: Process
initialize

	<PharoGs>
	super initialize. 
%

category: 'changing suspended state'
method: Process
install: aContext
	"Replace the suspendedContext with aContext."

	<PharoGsError>
	self _gsError
%

category: 'process specific'
method: Process
installEnvIntoForked: newProcess 

	<PharoGsError>
	self _gsError
%

category: '*Reflectivity'
method: Process
isActive: n

	<PharoGsError>
	self _gsError
%

category: 'testing'
method: Process
isActiveProcess

	<PharoGs>
	^ self == Processor activeProcess
%

category: '*Reflectivity'
method: Process
isMeta

	<PharoGsError>
	self _gsError
%

category: 'testing'
method: Process
isTerminated

	<PharoGs>
	^Processor @env0:allProcesses @env0:includes: self
%

category: 'testing'
method: Process
isSuspended

	<PharoGs>
	^Processor @env0:suspendedProcesses @env0:includes: self
%

category: 'testing'
method: Process
isTerminating
	"lazy initialization is a fallback only for processes that existed before this addition"

	<PharoGsError>
	self _gsError
%

category: '*Reflectivity'
method: Process
level

	<PharoGsError>
	self _gsError
%

category: 'printing'
method: Process
longPrintOn: stream 

	<PharoGsError>
	| ctxt | 
	super printOn: stream. 
	stream cr. 
	ctxt := self suspendedContext. 
	[ ctxt isNil ] 
		whileFalse: [  
			stream space. 
			ctxt printOn: stream. 
			stream cr. 
			ctxt := ctxt sender ]
%

category: 'accessing'
method: Process
name

	<PharoGs>
	^self @env0:environmentAt: #'name'
%

category: 'accessing'
method: Process
name: aString 

	<PharoGs>
	^self @env0:environmentAt: #'name' put: aString
%

category: 'signaling'
method: Process
on: exception do: handlerAction 
	"This method inject new bottom context into process with exception handler.  
	It uses context jump tricks to achieve it"

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
popTo: aContext
	"Pop self down to aContext by remote returning from aContext's callee.  
	Unwind blocks will be executed on the way. 
	
	This is done by pushing a new context on top which executes 'aContext callee return' 
	then resuming self until aContext is reached.  This way any errors raised in an unwind 
	block will get handled by senders in self and not by senders in the activeProcess. 
	
	If an unwind block raises an error that is not handled then the popping stops at the 
	error and the signalling context is returned, othewise aContext is returned."

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
popTo: aContext value: aValue
	"Replace the suspendedContext with aContext, releasing all contexts  
	 between the currently suspendedContext and it."

	<PharoGsError>
	self _gsError
%

category: 'printing'
method: Process
printOn: aStream 

	<PharoGs>
	super printOn: aStream. 
%

category: 'accessing'
method: Process
priority
	"Answer the priority of the receiver."

	<PharoGs>
	^self @env0:priority
%

category: 'accessing'
method: Process
priority: anInteger
	"Set the receiver's priority to anInteger."

	<PharoGs>
	(anInteger between: Processor lowestPriority and: Processor highestPriority) 
		ifTrue: [ self @env0:priority: anInteger ] 
		ifFalse: [ self error: 'Invalid priority: ' , anInteger printString ]
%

category: 'process specific'
method: Process
psValueAt: index
	"Answer a process-specific value at given index, or nil if value at given index is not defined"

	<PharoGs>
	^self @env0:environmentAt: index
%

category: 'process specific'
method: Process
psValueAt: index put: value
	"Set a value for given index in process-specific storage"" 
	""NOTE: this method are PRIVATE. 
	Do not use it directly, instead use ProcessSpecificVariable (or its subclasses) "

	<PharoGs>
	^self @env0:environmentAt: index put: value
%

category: 'signaling'
method: Process
pvtSignal: anException list: aList 
	"Private. This method is used to signal an exception from another 
	process...the receiver must be the active process.  If the receiver  
	was previously waiting on a Semaphore, then return the process 
	to the waiting state after signaling the exception and if the Semaphore 
	has not been signaled in the interim" 
	"Since this method is not called in a normal way, we need to take care 
	that it doesn't directly return to the caller (because I believe that could 
	have the potential to push an unwanted object on the caller's stack)."

	<PharoGsError>
	self _gsError
%

category: 'process specific'
method: Process
resetPSValueAt: index
	"NOTE: this method are PRIVATE. "

	<PharoGs>
	^self @env0:environmentAt: index put: nil
%

category: 'changing process state'
method: Process
primitiveResume 
	"Primitive. Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Fail if the receiver is  
	already waiting in a queue (in a Semaphore or ProcessScheduler).  
	Essential. See Object documentation whatIsAPrimitive." 
	<PharoGs>

    self @env0:resume.
	Processor yield.	"In GemStone, #resume of a higher priority process does not run it!"
%

category: 'changing suspended state'
method: Process
restartTop
	"Rollback top context and replace with new method.  Assumes self is suspended"

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
restartTopWith: method
	"Rollback top context and replace with new method.  Assumes self is suspended"

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
resume
	"Allow the process that the receiver represents to continue. Put   
	the receiver in line to become the activeProcess. Check for a nil  
	suspendedContext, which indicates a previously terminated Process that  
	would cause a vm crash if the resume attempt were permitted"

	<PharoGs>
	self @env0:resume.
	Processor yield.	"In GemStone, #resume of a higher priority process does not run it!"
%

category: 'changing suspended state'
method: Process
return: aContext value: value
	"Pop thread down to aContext's sender.  '
	Execute any unwind blocks on the way.  
	See #popTo: comment and #runUntilErrorOrReturnFrom: for more details."

	<PharoGsError>
	self _gsError
%

category: '*Reflectivity'
method: Process
rfeffectiveProcess
	"same as #effectiveProcess but for internal use for metalink activation"

	<PharoGsError>
	self _gsError
%

category: 'changing process state'
method: Process
run
	"Suspend current process and execute self instead"

	<PharoGsError>
	self _gsError
%

category: '*Reflectivity'
method: Process
shiftLevelDown

	<PharoGsError>
	self _gsError
%

category: '*Reflectivity'
method: Process
shiftLevelUp

	<PharoGsError>
	self _gsError
%

category: 'signaling'
method: Process
signalException: anException
	"Signal an exception in the receiver process...if the receiver is currently
	suspended, the exception will get signaled when the receiver is resumed.  If 
	the receiver is blocked on a Semaphore, it will be immediately re-awakened
	and the exception will be signaled; if the exception is resumed, then the receiver
	will return to a blocked state unless the blocking Semaphore has excess signals"

	"If we are the active process, go ahead and signal the exception"
	self isActiveProcess ifTrue: [^anException signal].

	<PharoGs>
	self @env0:signalException: anException
%

category: 'changing suspended state'
method: Process
step

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
step: aContext

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
stepToCallee

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
stepToHome: aContext

	<PharoGsError>
	self _gsError
%

category: 'changing suspended state'
method: Process
stepToSendOrReturn

	<PharoGsError>
	self _gsError
%

category: 'changing process state'
method: Process
suspend 
	"Primitive. Stop the process that the receiver represents in such a way  
	that it can be restarted at a later time (by sending the receiver the  
	message resume). If the receiver represents the activeProcess, suspend it.  
	Otherwise remove the receiver from the list of waiting processes. 
	The return value of this method is the list the receiver was previously on (if any)."
	<PharoGs>

    ^self @env0:suspend 
%

category: 'accessing'
method: Process
suspendedContext

	<PharoGsError>
	self _gsError
%

category: 'private'
method: Process
suspendedContext: aContext

	<PharoGsError>
	self _gsError
%

category: 'accessing'
method: Process
suspendingList
	"Answer the list on which the receiver has been suspended."

	<PharoGs>
	^self @env0:waitingOn
%

category: 'changing process state'
method: Process
terminate
	"Stop the process that the receiver represents forever.  
	Unwind to execute pending ensure:/ifCurtailed: blocks before terminating."

	<PharoGs>
	self @env0:terminate
%

set compile_env: 0
