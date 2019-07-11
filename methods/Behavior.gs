set compile_env: 2

category: 'adding/removing methods'
method: Behavior
adoptInstance: anInstance 
	"Change the class of anInstance to me. 
	Primitive (found in Cog and new VMs)  follows the same rules as primitiveChangeClassTo:, but returns the 	class rather than the modified instance" 

	<PharoGs> 
	anInstance @env0:changeClassTo: self. 
	^self
%

category: 'accessing instances and variables'
method: Behavior
allInstances 
	"Answer all instances of the receiver." 

	<PharoGs> 
	^self realClass @env0:allInstances
%

category: 'accessing instances and variables'
method: Behavior
allInstancesOrNil 
	"Answer all instances of the receiver." 
	
	<PharoGs> 
	[
		^self realClass @env0:allInstances
	] @env0:on: AlmostOutOfMemory do: [:ex | 
		^nil
	]
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
	<PharoGs> 
	self isVariable ifTrue: [^self basicNew: 0]. 
	^self @env0:basicNew
%

category: 'accessing instances and variables'
method: Behavior
someInstance 
	"Primitive. Answer the first instance in the enumeration of all instances  
	of the receiver. Fails if there are none. Essential. See Object  
	documentation whatIsAPrimitive." 
	
	<PharoGs> 
	| list |
	list := SystemRepository @env0:listInstances: { self } limit: 1.
	(list @env0:at: 1) == 0 @env0:ifTrue: [self @env0:error: 'No instances!'].
	^(list @env0:at: 2) @env0:at: 1.
%

set compile_env: 0
