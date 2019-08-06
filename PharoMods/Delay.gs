set compile_env: 2

category: 'instance creation'
classmethod: Delay
forDuration: aDuration
 	"Return a new Delay for the given duration."
 
	<PharoGs>
	^ self forMilliseconds: aDuration asMilliSeconds
%

category: 'instance creation'
classmethod: Delay
forMilliseconds: aNumber
	<PharoGs>
	^self @env0:forMilliseconds: aNumber
%

category: 'instance creation'
classmethod: Delay
forSeconds: aNumber
	<PharoGs>
	^self @env0:forSeconds: aNumber
%

category: 'public'
method: Delay
delaySemaphore
	"GemStone does not have a separate semaphore; you can send signal to the Delay instance"

	<PharoGs>
	^self
%

category: 'GemStone'
method: Delay
signal
	"GemStone does not have a separate semaphore; you can send signal to the Delay instance"
	
	<PharoGs>
	self @env0:signal
%

category: 'delaying'
method: Delay
wait
	<PharoGs>
	self @env0:wait
%

set compile_env: 0
