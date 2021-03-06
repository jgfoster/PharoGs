set compile_env: 2

category: 'instance creation'
classMethod: Semaphore
forMutualExclusion

    <PharoGs>
	"Answer an instance of me that contains a single signal. This new 
	instance can now be used for mutual exclusion (see the critical: message 
	to Semaphore)."
	^MutexWrapper new
%

category: 'instance creation'
classMethod: Semaphore
new

    <PharoGs>
	"Answer a new instance of Semaphore that contains no signals."
	^SemaphoreWrapper new
%

category: 'communication'
method: Semaphore
signal 
	"Primitive. Send a signal through the receiver. If one or more processes  
	have been suspended trying to receive a signal, allow the first one to  
	proceed. If no process is waiting, remember the excess signal. Essential.  
	See Object documentation whatIsAPrimitive." 

	<PharoGs> 
    ^self @env0:signal
%

category: 'communication'
method: Semaphore
wait 
	"Primitive. The active Process must receive a signal through the receiver  
	before proceeding. If no signal has been sent, the active Process will be  
	suspended until one is sent. Essential. See Object documentation  
	whatIsAPrimitive." 

	<PharoGs> 
    ^self @env0:wait
%

category: 'communication'
method: Semaphore
waitTimeoutMSecs: anInteger 
	"Wait on this semaphore for up to the given number of milliseconds, then timeout.  
	Return true if the deadline expired, false otherwise." 

	<PharoGs>
	^self @env0:waitForMilliseconds: anInteger
%

set compile_env: 0
