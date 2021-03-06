set compile_env: 2

category: 'api-system'
method: DelayMicrosecondTicker
nowTick 
	 "Copied from Time class >> primUTCMicrosecondsClock. 
	 Answer the number of micro-seconds ellapsed since Squeak epoch. 
	 That is since 00:00 on the morning of January 1, 1901 UTC. 
	 At least a 60-bit unsigned integer is used internally which is enough for dates up to year 38435. 
	 Essential. See Object documentation whatIsAPrimitive. " 

	<PharoGs> 
    ^Time primUTCMicrosecondsClock
%

category: 'private-primitives'
method: DelayMicrosecondTicker
primSignal: aSemaphore atUTCMicroseconds: aLargePositiveInteger 
	"Signal the semaphore when the microsecond clock reaches the value of the second 
    argument. Fail if the first argument is neither a Semaphore nor nil. 
    Essential. See Object documentation whatIsAPrimitive." 

    <PharoGs>
	aSemaphore @env0:ifNil: [^self].
	[
		(Globals @env0:at: #'Delay') 
			@env0:waitForMilliseconds: 
				(aLargePositiveInteger @env0:- self nowTick) @env0:// 1000.
		aSemaphore signal.
	] @env0: fork.
%

set compile_env: 0
