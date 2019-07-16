set compile_env: 2

category: 'private'
method: DoubleWordArray
atAllPut: value 
	"Fill the receiver with the given value" 

	<PharoGs> 
    1 to: self size do: [ :index | self at: index put: value].
%

category: 'private'
method: DoubleWordArray
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in 
    the receiver starting at index, repStart, in the collection, replacement. 
    Answer the receiver. Range checks are performed in the primitive only. 
    Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 608>
	<PharoGs> 
    self @env0:replaceFrom: start to: stop with: replacement startingAt: repStart
%

set compile_env: 0
