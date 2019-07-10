set compile_env: 2

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

category: 'instance creation'
classmethod: ByteString
basicNew: sizeRequested 
	"Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive. 
	 
	 If the primitive fails because space is low then the scavenger will run before the 
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK." 

	<primitive: 53>
	<PharoGs> 
	self isVariable ifFalse: 
		[self error: self printString, ' cannot have variable sized instances']. 
	^self @env0:basicNew: sizeRequested
%

set compile_env: 0
