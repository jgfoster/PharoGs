set compile_env: 2

category: 'primitives'
classmethod: Time
millisecondClockValue 
	"Answer the number of milliseconds since the millisecond clock 
    was last reset or rolled over. 
	Answer 0 if the primitive fails." 

    <primitive: 651>
	<PharoGs>
    ^self @env0:_primitiveFailed: #millisecondClockValue
%

category: 'primitives'
classmethod: Time
primMillisecondClock 
	"Primitive. Answer the number of milliseconds since the millisecond clock 
	 was last reset or rolled over. Answer zero if the primitive fails. 
	As an alternative you can use #primUTCMillisecondsClock which does not overflow." 

    <primitive: 651>
	<PharoGs>
    ^self @env0:_primitiveFailed: #primMillisecondClock
%

category: 'primitives'
classmethod: Time
primUTCMicrosecondsClock 
	"Answer the number of micro-seconds ellapsed since Squeak epoch. 
	That is since 00:00 on the morning of January 1, 1901 UTC. 
	At least a 60-bit unsigned integer is used internally which is enough for dates up to year 38435. 
	Essential. See Object documentation whatIsAPrimitive. " 

    "
| year1901 year1970 |
year1901 := (Globals at: #DateAndTime) year: 1901 day: 1 hour: 0 minute: 0 second: 0.
year1970 := (Globals at: #DateAndTime) year: 1970 day: 1 hour: 0 minute: 0 second: 0.
(year1970 - year1901) asSeconds * 1000 * 1000.
 2177452800000000
    "
	<PharoGs>

    ^((Globals @env0:at: #System) 
        @env0:_timeGmtFloat * 1000000) asInteger    "since January 1, 1970"
        + 2177452800000000
%

set compile_env: 0
