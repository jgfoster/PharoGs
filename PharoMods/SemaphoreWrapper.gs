set compile_env: 0
! ------------------- Class definition for SemaphoreWrapper
expectvalue /Class
doit
Object subclass: 'SemaphoreWrapper'
  instVarNames: #( semaphore)
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: Pharo
  options: #( dbTransient)

%
expectvalue /Class
doit
SemaphoreWrapper comment: 
'Because a Semaphore is non-persistent (cannot be committed) in GemStone, 
I am a wrapper that recreates a semaphore for each session. The class-side 
Semaphore instance-creation methods are replaced to return an instance of 
me or my subclass.'
%
expectvalue /Class
doit
SemaphoreWrapper category: 'Kernel'
%

set compile_env: 2

category: 'private'
method: SemaphoreWrapper
_semaphore

    <PharoGs>
	^semaphore ifNil: [semaphore := Semaphore basicNew initSignals].
%

category: 'comparing'
method: SemaphoreWrapper
= anObject

    <PharoGs>
    "This and #'hash' are not delegated to the real Semaphore since
     we need hash and equality to be stable between sessions."
	^ self == anObject
%

category: 'initialization'
method: 
consumeAllSignals

    <PharoGs>
	^self _semaphore consumeAllSignals
%

category: 'mutual exclusion'
method: SemaphoreWrapper
critical: mutuallyExcludedBlock	

    <PharoGs>
	^self _semaphore critical: mutuallyExcludedBlock
%

category: 'mutual exclusion'
method: SemaphoreWrapper
critical: mutuallyExcludedBlock ifCurtailed: terminationBlock

    <PharoGs>
	^self _semaphore critical: mutuallyExcludedBlock ifCurtailed: terminationBlock
%

category: 'mutual exclusion'
method: SemaphoreWrapper
critical: mutuallyExcludedBlock ifError: errorBlock

    <PharoGs>
	^self _semaphore critical: mutuallyExcludedBlock ifError: errorBlock
%

category: 'mutual exclusion'
method: SemaphoreWrapper
critical: mutuallyExcludedBlock ifLocked: alternativeBlock

    <PharoGs>
	^self _semaphore critical: mutuallyExcludedBlock ifLocked: alternativeBlock
%

category: 'process termination handling'
method: SemaphoreWrapper
handleProcessTerminationOfWaitingContext: suspendedContext

    <PharoGs>
	^self _semaphore handleProcessTerminationOfWaitingContext: suspendedContext
%

category: 'comparing'
method: SemaphoreWrapper
hash

    <PharoGs>
    "This and #'=' are not delegated to the real Semaphore since
     we need hash and equality to be stable between sessions."
	^ self identityHash
%

category: 'initialization'
method: SemaphoreWrapper
initSignals

    <PharoGs>
	^self _semaphore initSignals
%

category: 'testing'
method: SemaphoreWrapper
isSignaled

    <PharoGs>
	^self _semaphore isSignaled
%

category: 'initialization'
method: SemaphoreWrapper
resumeProcess: aProcess

    <PharoGs>
	^self _semaphore resumeProcess: aProcess
%

category: 'communication'
method: SemaphoreWrapper
signal

    <PharoGs>
	^self _semaphore signal
%

category: 'initialize-release'
method: SemaphoreWrapper
terminateProcess

    <PharoGs>
	^self _semaphore terminateProcess
%

category: 'communication'
method: SemaphoreWrapper
wait

    <PharoGs>
	^self _semaphore wait
%

category: 'communication'
method: SemaphoreWrapper
wait: aDuration

    <PharoGs>
	^self _semaphore wait: aDuration
%

category: 'communication'
method: SemaphoreWrapper
wait: aDuration onCompletion: completionBlock onTimeout: timeoutBlock

    <PharoGs>
	^self _semaphore wait: aDuration onCompletion: completionBlock onTimeout: timeoutBlock
%

category: 'communication'
method: SemaphoreWrapper
waitTimeoutMSecs: anInteger

    <PharoGs>
	^self _semaphore waitTimeoutMSecs: anInteger
%

category: 'communication'
method: 
waitTimeoutSeconds: anInteger

    <PharoGs>
	^self _semaphore waitTimeoutSeconds: anInteger
%

category: 'communication'
method: SemaphoreWrapper
waitTimeoutSeconds: anInteger onCompletion: completionBlock onTimeout: timeoutBlock

    <PharoGs>
	^self _semaphore waitTimeoutSeconds: anInteger onCompletion: completionBlock onTimeout: timeoutBlock
%

set compile_env: 0
