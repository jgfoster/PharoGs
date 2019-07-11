set compile_env: 2

category: 'accessing'
classmethod: Array
new: sizeRequested  
	"Answer an instance of this class with the number of indexable 
	variables specified by the argument, sizeRequested. 
	 
	This is a shortcut (direct call of primitive, no #initialize, for performance" 
    <primitive: 53>
	<PharoGs> 
	^ self basicNew: sizeRequested  "Exceptional conditions will be handled in basicNew:"
%

category: 'accessing'
method: Array
atWrap: index  
    "Answer the index'th element of the receiver.  If index is out of bounds, 
    let it wrap around from the end to the beginning until it is in bounds." 
     
    "Optimized to go through the primitive if possible" 
     
    "(#(11 22 33) atWrap: 3) >>> 33" 
    "(#(11 22 33) atWrap: 2) >>> 22" 
    "(#(11 22 33) atWrap: 4) >>> 11" 
     
	<primitive: 699>
    <PharoGs> 
    ^ self at: index - 1 \\ self size + 1
%

category: 'accessing'
method: Array
atWrap: index put: anObject 
	"Optimized to go through the primitive if possible" 
	"Can't use primitive since GemStone can increase Array size!"

	<PharoGs> 
	^ self at: index - 1 \\ self size + 1 put: anObject
%

category: 'accessing'
method: Array
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 608>
	<PharoGs> 
	super replaceFrom: start to: stop with: replacement startingAt: repStart
%

set compile_env: 0
