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

category: 'converting'
method: Array
elementsExchangeIdentityWith: otherArray 
	"This primitive performs a bulk mutation, causing all pointers to the elements of the 
	 receiver to be replaced by pointers to the corresponding elements of otherArray. 
	 At the same time, all pointers to the elements of otherArray are replaced by 
	 pointers to the corresponding elements of this array.  The identityHashes remain 
	 with the pointers rather than with the objects so that objects in hashed structures 
	 should still be properly indexed after the mutation." 
	
	<PharoGsError>
	self @env0:error: 'This is not how GemStone implements #become: '.
%

category: 'converting'
method: Array
elementsForwardIdentityTo: otherArray 
	"This primitive performs a bulk mutation, causing all pointers to the elements of the 
	 receiver to be replaced by pointers to the corresponding elements of otherArray. 
	 The identityHashes remain with the pointers rather than with the objects so that 
	 the objects in this array should still be properly indexed in any existing hashed 
	 structures after the mutation." 
	
	<PharoGsError>
	self @env0:error: 'GemStone does not support one-way become'.
%

category: 'converting'
method: Array
elementsForwardIdentityTo: otherArray copyHash: copyHash 
	"This primitive performs a bulk mutation, causing all pointers to the elements of the 
	 receiver to be replaced by pointers to the corresponding elements of otherArray. 
	 If copyHash is true, the identityHashes remain with the pointers rather than with the 
	 objects so that the objects in the receiver should still be properly indexed in any 
	 existing hashed structures after the mutation.  If copyHash is false, then the hashes 
	 of the objects in otherArray remain unchanged.  If you know what you're doing this 
	 may indeed be what you want." 
	
	<PharoGsError>
	self @env0:error: 'GemStone does not support one-way become'.
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
