set compile_env: 2

category: 'private'
classmethod: CompiledMethod
handleFailingFailingNewMethod: numberOfBytes header: headerWord 
	"This newMethod:header: gets sent after handleFailingBasicNew: has done a full 
	 garbage collection and possibly grown memory.  If this basicNew: fails then the 
	 system really is low on space, so raise the OutOfMemory signal. 
	 Primitive. Answer an instance of this class with the number of indexable variables 
	 specified by the argument, headerWord, and the number of bytecodes specified 
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes 
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough 
	 memory available. Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

category: 'private'
classmethod: CompiledMethod
handleFailingNewMethod: numberOfBytes header: headerWord 
	"This newMethod:header: gets sent after newMethod:header: has failed 
	 and allowed a scavenging garbage collection to occur.  The scavenging 
	 collection will have happened as the VM is activating the (failing) basicNew:. 
	 If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient 
	 space and a global garbage collection is required.  Retry after garbage 
	 collecting and growing memory if necessary. 
	 Primitive. Answer an instance of this class with the number of indexable variables 
	 specified by the argument, headerWord, and the number of bytecodes specified 
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes 
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough 
	 memory available. Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

category: 'accessing'
method: CompiledMethod
flushCache 
	"Tell the virtual machine to remove all references to this method from its 
    method lookup caches, and to discard any optimized version of the method, 
    if it has any of these. This must be done whenever a method is modified in 
    place, such as modifying its literals or machine code, to reflect the 
    revised code.  c.f. Behavior>>flushCache & Symbol>>flushCache.  
    Essential. See MethodDictionary class comment." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
