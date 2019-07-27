set compile_env: 2

category: 'instance creation'
classmethod: ByteString
basicNew: sizeRequested 
	"Primitive. Answer an instance of this class with the number of indexable 
	 variables specified by the argument, sizeRequested.  Fail if this class is not 
	 indexable or if the argument is not a positive Integer, or if there is not 
	 enough memory available. Essential. See Object documentation whatIsAPrimitive. 
	 
	 If the primitive fails because space is low then the scavenger will run before the 
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK. '" 

	<primitive: 53>
	<PharoGs> 
	self isVariable ifFalse: 
		[self error: self printString, ' cannot have variable sized instances']. 
	^self @env0:basicNew: sizeRequested
%

category: 'primitives'
classmethod: ByteString
compare: string1 with: string2 collated: order 
	"Return 1, 2 or 3, if string1 is <, =, or > string2, with the collating order of characters given by the order array." 

	<PharoGs> 
	| len1 len2 c1 c2 | 
	len1 := string1 size. 
	len2 := string2 size. 
	1 to: (len1 min: len2) do: 
		[:i | 
		c1 := order at: (string1 basicAt: i) + 1. 
		c2 := order at: (string2 basicAt: i) + 1. 
		c1 = c2 ifFalse:  
			[c1 < c2 ifTrue: [^ 1] ifFalse: [^ 3]]]. 
	len1 = len2 ifTrue: [^ 2]. 
	len1 < len2 ifTrue: [^ 1] ifFalse: [^ 3]. 
%

category: 'primitives'
classmethod: ByteString
findFirstInString: aString  inSet: inclusionMap  startingAt: start 

	<PharoGs> 
	| i stringSize | 
	inclusionMap size ~= 256 ifTrue: [ ^0 ]. 
	i := start. 
	stringSize := aString size. 
	[ i <= stringSize and: [ (inclusionMap at: (aString basicAt: i) + 1) = 0 ] ] whileTrue: [  
		i := i + 1 ]. 
	i > stringSize ifTrue: [ ^0 ]. 
	^i
%

category: 'primitives'
classmethod: ByteString
indexOfAscii: anInteger inString: aString startingAt: start 

	<PharoGs> 
	| stringSize | 
	stringSize := aString size. 
	start to: stringSize do: [:pos | 
		(aString basicAt: pos) = anInteger ifTrue: [^ pos]]. 
	^ 0
%

category: 'primitives'
classmethod: ByteString
stringHash: aString initialHash: speciesHash 

	<PharoGs> 
	| stringSize hash low | 
	stringSize := aString size. 
	hash := speciesHash bitAnd: 16rFFFFFFF. 
	1 to: stringSize do: [:pos | 
		hash := hash + (aString basicAt: pos). 
		"Begin hashMultiply"
		low := hash bitAnd: 16383. 
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF. 
	]. 
	^ hash
%

category: 'primitives'
classmethod: ByteString
translate: aString from: start  to: stop  table: table 
	"translate the characters in the string by the given table, in place" 

	<PharoGs> 
	start to: stop do: [ :i | 
		aString at: i put: (table at: (aString basicAt: i) + 1) ]
%

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
at: index  
	"Primitive. Answer the Character stored in the field of the receiver 
	indexed by the argument. Fail if the index argument is not an Integer or 
	is out of bounds. Essential. See Object documentation whatIsAPrimitive." 

	<primitive: 69>
	<PharoGs> 
	^self @env0:at: index
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

category: 'accessing'
method: ByteString
byteAt: anIndex

	<PharoGs> 
	^(self @env0:at: anIndex) codePoint
%


category: 'accessing'
method: ByteString
byteAt: index put: value 

	<primitive: 1002>
	<PharoGs> 
	^self @env0:at: index put: value asCharacter
%

set compile_env: 0
