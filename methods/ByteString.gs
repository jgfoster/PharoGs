set compile_env: 2

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
	<PharoGsDone> 
	self isVariable ifFalse: 
		[self error: self printString, ' cannot have variable sized instances']. 
	^self @env0:basicNew: sizeRequested
%

category: 'accessing'
method: ByteString
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 297>
	<PharoGsDone> 
	replacement class == WideString ifTrue: [ 
		self becomeForward: (WideString from: self). 
	].  
	super replaceFrom: start to: stop with: replacement startingAt: repStart. 
%

category: 'accessing'
method: ByteString
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by 
	the index. Fail if the index is not an Integer or is out of bounds, or if 
	the argument is not a Character. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 293>
	<PharoGsDone> 
	aCharacter isCharacter 
		ifFalse:[^self errorImproperStore].
	aCharacter isOctetCharacter ifFalse:[
		"Convert to WideString"
		self becomeForward: (WideString from: self).
		^self at: index put: aCharacter.
	].
	index isInteger
		ifTrue: [ (index between: 1 and: self size)
				ifFalse: [ self errorSubscriptBounds: index ] ]
		ifFalse: [self errorNonIntegerIndex].
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: aCharacter ].
%

set compile_env: 0
