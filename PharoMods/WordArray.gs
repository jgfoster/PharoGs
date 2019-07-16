set compile_env: 2

category: 'accessing'
method: WordArray
atAllPut: value 
	"Fill the receiver with the given value" 

	<PharoGs> 
	super atAllPut: value
%

category: 'array arithmetic primitives'
method: WordArray
primAddArray: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) + (other at: i) 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primAddScalar: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) + other. 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primDivArray: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) / (other at: i) 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primDivScalar: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) / other. 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primMulArray: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) * (other at: i) 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primMulScalar: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) * other. 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primSubArray: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) - (other at: i) 
	]. 
	^ result. 
%

category: 'array arithmetic primitives'
method: WordArray
primSubScalar: rcvr and: other into: result 

	<PharoGs> 
	1 to: rcvr size do: [:i | 
		result at: i put: (rcvr at: i) - other. 
	]. 
	^ result. 
%

category: 'accessing'
method: WordArray
replaceFrom: start to: stop with: replacement startingAt: repStart  

	<PharoGs> 
	^super replaceFrom: start to: stop with: replacement startingAt: repStart 
%

set compile_env: 0
