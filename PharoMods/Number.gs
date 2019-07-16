set compile_env: 2

category: 'arithmetic'
method: Number
@ y  
	"Primitive. Answer a Point whose x value is the receiver and whose y  
	value is the argument. Optional. No Lookup. See Object documentation  
	whatIsAPrimitive." 

	<PharoGs> 
    ^Point x: self y: y
%

category: 'private'
method: Number
_nonZeroGte: aNumber

	<PharoGs>
	^self @env0:_nonZeroGte: aNumber
%

set compile_env: 0
