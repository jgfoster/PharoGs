set compile_env: 2

category: 'accessing'
method: ByteString
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 297>
	<PharoGs> 
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
	<PharoGs> 
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
