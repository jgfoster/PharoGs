set compile_env: 2

category: 'arithmetic'
method: Number
@ y  
	"Primitive. Answer a Point whose x value is the receiver and whose y  
	value is the argument. Optional. No Lookup. See Object documentation  
	whatIsAPrimitive." 

	<PharoGsDone> 
    ^Point x: self y: y
%

set compile_env: 0
