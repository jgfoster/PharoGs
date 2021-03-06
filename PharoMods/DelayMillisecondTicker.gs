set compile_env: 2

category: 'api-system'
method: DelayMillisecondTicker
nowTick 
	 "Copied from Time class >> primMillisecondClock. 
	 Answer the number of micro-seconds ellapsed since Squeak epoch. 
	 That is since 00:00 on the morning of January 1, 1901 UTC. 
	 At least a 60-bit unsigned integer is used internally which is 
     enough for dates up to year 38435. 
	 Essential. See Object documentation whatIsAPrimitive. " 

	<PharoGs> 
    ^Time primMillisecondClock
%

category: 'private-primitives'
method: DelayMillisecondTicker
primSignal: aSemaphore atMilliseconds: aSmallInteger 
	"Signal the semaphore when the millisecond clock reaches the value of the 
    second argument. Fail if the first argument is neither a Semaphore nor nil. 
    Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
