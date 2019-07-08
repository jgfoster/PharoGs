category: 'accessing'
set compile_env: 2
method: ByteString
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 297>
	<PharoGs> 
"	<primitive: 105> "
	replacement class == WideString ifTrue: [ 
		self becomeForward: (WideString from: self). 
	].  
	super replaceFrom: start to: stop with: replacement startingAt: repStart. 

%
set compile_env: 0
category: 'accessing'
set compile_env: 2
method: ByteString
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by 
	the index. Fail if the index is not an Integer or is out of bounds, or if 
	the argument is not a Character. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 293>
	<PharoGs> 
	"<primitive: 64> "
	aCharacter isCharacter  
		ifFalse:[^self errorImproperStore]. 
	aCharacter isOctetCharacter ifFalse:[ 
		"Convert to WideString"
		self becomeForward: (WideString from: self). 
		^self at: index put: aCharacter. 
	]. 
	index isInteger 
		ifTrue: [self errorSubscriptBounds: index] 
		ifFalse: [self errorNonIntegerIndex]
%
set compile_env: 0
category: 'instance creation'
set compile_env: 2
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
"	<primitive: 71 error: ec> 
	ec == #'insufficient object memory' ifTrue: 
		[^self handleFailingBasicNew: sizeRequested]. 
	self isVariable ifFalse: 
		[self error: self printString, ' cannot have variable sized instances']. 
	self primitiveFailed"
%
set compile_env: 0
