set compile_env: 2

category: 'instance creation'
classmethod: DoubleByteArray
new: anInteger

    <PharoGs>
    ^(super new: anInteger)
        atAllPut: 0;
        yourself
%

category: 'private'
method: DoubleByteArray
atAllPut: value 
	"Fill the receiver with the given value" 

	<PharoGs> 
    1 to: self size do: [ :index | self at: index put: value].
%

category: 'private'
method: DoubleByteArray
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in 
    the receiver starting at index, repStart, in the collection, replacement. 
    Answer the receiver. Range checks are performed in the primitive only. 
    Optional. See Object documentation whatIsAPrimitive." 

	<PharoGs> 
    self @env0:replaceFrom: start to: stop with: replacement startingAt: repStart
%

set compile_env: 0
