set compile_env: 2

category: 'primitives'
method: LocalTimeZone
primOffset 
	"The offset from UTC in minutes, with positive offsets being towards the east. 
	(San Francisco is in UTC -07*60 and Paris is in UTC +02*60 
    (daylight savings is not in effect)." 
	<PharoGs> 

    ^(Globals @env0:at: #'Time') @env0:gmtOffsetSeconds negated // 60
%

set compile_env: 0
