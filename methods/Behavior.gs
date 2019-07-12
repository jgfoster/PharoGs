set compile_env: 2

category: 'adding/removing methods'
method: Behavior
adoptInstance: anInstance 
	"Change the class of anInstance to me. 
	Primitive (found in Cog and new VMs)  follows the same rules as primitiveChangeClassTo:, but returns the 	class rather than the modified instance" 

	<PharoGsDone> 
	anInstance @env0:changeClassTo: self. 
	^self
%

category: 'accessing instances and variables'
method: Behavior
allInstances 
	"Answer all instances of the receiver." 

	<PharoGsDone> 
	^self realClass @env0:allInstances
%

category: 'accessing instances and variables'
method: Behavior
allInstancesOrNil 
	"Answer all instances of the receiver." 
	
	<PharoGsDone> 
	[
		^self realClass @env0:allInstances
	] @env0:on: AlmostOutOfMemory do: [:ex | 
		^nil
	]
%

category: 'reflective operations'
method: Behavior
basicIdentityHash 
	"Answer a 22 bits unsigned SmallInteger, whose value is related to the receiver's identity 
	 and unique among the behaviors (i.e. 2 different Behaviors cannot have the same identityHash). 
	 
	 Behavior implements identityHash to allow the VM to use an object representation which 
	 does not include a direct reference to an object's class in an object.  If the VM is using 
	 this implementation then classes are held in a class table and instances contain the index 
	 of their class in the table.  A class's class table index is its identityHash so that an instance 
	 can be created without searching the table for a class's index.  The VM uses this primitive 
	 to enter the class into the class table, assigning its identityHash with an as yet unused 
	 class table index. If this primitive fails it means that the class table is full.  In Spur as of 
	 2014 there are 22 bits of classTable index and 22 bits of identityHash per object. 
	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive." 

	<primitive: 321>
	<PharoGsDone> 
	^self @env0:basicIdentityHash
%

category: 'reflective operations'
method: Behavior
basicNew: sizeRequested 
	"Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive. 
	 
	 If the primitive fails because space is low then the scavenger will run before the 
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK." 

	<PharoGsDone> 
	^self @env0:_basicNew: sizeRequested
%

category: 'instance creation'
method: Behavior
basicNew 
	"Primitive. Answer an instance of the receiver (which is a class) with no  
	 indexable variables. Fail if the class is indexable. Essential. See Object  
	 documentation whatIsAPrimitive. 
	 
	 If the primitive fails because space is low then the scavenger will run 
	 before the method is activated.  Check that space was low and retry 
	 via handleFailingBasicNew if so." 

	<primitive: 50>
	<PharoGsDone> 
	self isVariable ifTrue: [^self basicNew: 0]. 
	^self @env0:basicNew
%

category: 'accessing instances and variables'
method: Behavior
byteSizeOfInstance 
	"Answer the total memory size of an instance of the receiver." 

	<PharoGsError>
	self @env0:error: 'Use Object>>sizeInMemory on an instance'
%

category: 'accessing instances and variables'
method: Behavior
byteSizeOfInstanceOfSize: basicSize 
	"Answer the total memory size of an instance of the receiver 
	 with the given number of indexable instance variables." 

	<PharoGsError>
	self @env0:error: 'Use Object>>sizeInMemory on an instance'
%

category: 'private'
method: Behavior
flushCache 
	"Tell the virtual machine to remove the contents of its method lookup caches, 
	if it has any. This must be done when the system modifies the class hierarchy 
	so that message lookups reflect the revised organization.  
	c.f. Symbol>>flushCache & 	CompiledMethod>>flushCache.  
	Essential. See MethodDictionary class comment." 

	<PharoGsError>
	self @env0:error: 'GemStone does this automatically'
%

category: 'private'
method: Behavior
handleFailingBasicNew 
	"handleFailingBasicNew gets sent after basicNew has failed and allowed 
	 a scavenging garbage collection to occur.  The scavenging collection 
	 will have happened as the VM is activating the (failing) basicNew.  If 
	 handleFailingBasicNew fails then the scavenge failed to reclaim sufficient 
	 space and a global garbage collection is required.  Retry after garbage 
	 collecting and growing memory if necessary. 
	 Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive." 

	<PharoGsError>
	self @env0:error: 'GemStone does this automatically'
%

category: 'private'
method: Behavior
handleFailingBasicNew: sizeRequested 
	"handleFailingBasicNew: gets sent after basicNew: has failed and allowed 
	 a scavenging garbage collection to occur.  The scavenging collection 
	 will have happened as the VM is activating the (failing) basicNew:.  If 
	 handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient 
	 space and a global garbage collection is required.  Retry after garbage 
	 collecting and growing memory if necessary. 
	 Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive." 

	<PharoGsError>
	self @env0:error: 'GemStone does this automatically'
%

category: 'private'
method: Behavior
handleFailingFailingBasicNew 
	"This basicNew gets sent after handleFailingBasicNew: has done a full 
	 garbage collection and possibly grown memory.  If this basicNew fails 
	 then the system really is low on space, so raise the OutOfMemory signal. 
	 Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive." 

	<PharoGsError>
	self @env0:error: 'GemStone does this automatically'
%

category: 'private'
method: Behavior
handleFailingFailingBasicNew: sizeRequested 
	"This basicNew: gets sent after handleFailingBasicNew: has done a full 
	 garbage collection and possibly grown memory.  If this basicNew: fails 
	 then the system really is low on space, so raise the OutOfMemory signal. 
	 Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive." 

	<PharoGsError>
	self @env0:error: 'GemStone does this automatically'
%

category: 'accessing instances and variables'
method: Behavior
someInstance 
	"Primitive. Answer the first instance in the enumeration of all instances  
	of the receiver. Fails if there are none. Essential. See Object  
	documentation whatIsAPrimitive." 
	
	<PharoGsDone> 
	| list |
	list := SystemRepository @env0:listInstances: { self } limit: 1.
	(list @env0:at: 1) == 0 @env0:ifTrue: [self @env0:error: 'No instances!'].
	^(list @env0:at: 2) @env0:at: 1.
%

set compile_env: 0
