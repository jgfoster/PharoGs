set compile_env: 2

category: 'arithmetic'
method: LargeInteger
* anInteger  
	"Primitive. Multiply the receiver by the argument and answer with an 
	Integer result. Fail if either the argument or the result is not a 
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive. " 

    <primitive: 260>
	<PharoGsDone> 
	^self @env0:* anInteger
%

category: 'arithmetic'
method: LargeInteger
+ anInteger  
	"Primitive. Add the receiver to the argument and answer with an 
	Integer result. Fail if either the argument or the result is not a 
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive." 

    <primitive: 258>
	<PharoGsDone> 
	^self @env0:+ anInteger
%

category: 'arithmetic'
method: LargeInteger
- anInteger  
	"Primitive. Subtract the argument from the receiver and answer with an 
	Integer result. Fail if either the argument or the result is not a 
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive." 

    <primitive: 259>
	<PharoGsDone> 
	^self @env0:- anInteger
%

category: 'arithmetic'
method: LargeInteger
/ anInteger  
	"Primitive. Divide the receiver by the argument and answer with the 
	result if the division is exact. Fail if the result is not a whole integer. 
	Fail if the argument is 0. Fail if either the argument or the result is not 
	a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive. " 

    <primitive: 261>
	<PharoGsDone> 
	^self @env0:/ anInteger
%

category: 'arithmetic'
method: LargeInteger
// anInteger  
	"Primitive. Divide the receiver by the argument and return the result. 
	Round the result down towards negative infinity to make it a whole 
	integer. Fail if the argument is 0. Fail if either the argument or the 
	result is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). 
	Optional. See Object documentation whatIsAPrimitive. " 

    <primitive: 263>
	<PharoGsDone> 
	^self @env0:/ anInteger
%

category: 'comparing'
method: LargeInteger
< anInteger  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is less than the argument. Otherwise answer false. Fail if the 
	argument is not a SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). 
	Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 20>
	<PharoGsDone> 
	^self @env0:< anInteger
%

category: 'comparing'
method: LargeInteger
<= anInteger  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is less than or equal to the argument. Otherwise answer false. 
	Fail if the argument is not a SmallInteger or a LargePositiveInteger less 
	than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 21>
	<PharoGsDone> 
	^self @env0:<= anInteger
%

category: 'comparing'
method: LargeInteger
> anInteger  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than the argument. Otherwise answer false. Fail if 
	the argument is not a SmallInteger or a LargePositiveInteger less than 
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 22>
	<PharoGsDone> 
	^self @env0:> anInteger
%

category: 'comparing'
method: LargeInteger
>= anInteger  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than or equal to the argument. Otherwise answer 
	false. Fail if the argument is not a SmallInteger or a LargePositiveInteger 
	less than 2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 23>
	<PharoGsDone> 
	^self @env0:> anInteger
%

category: 'arithmetic'
method: LargeInteger
quo: anInteger  
	"Primitive. Divide the receiver by the argument and return the result. 
	Round the result down towards zero to make it a whole integer. Fail if 
	the argument is 0. Fail if either the argument or the result is not a 
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive." 

    <primitive: 262>
	<PharoGsDone> 
	^self @env0:quo: anInteger
%

category: 'arithmetic'
method: LargeInteger
rem: aNumber  
	"Remainder defined in terms of quo:. See super rem:. 
	This is defined only to speed up case of large integers." 

	<PharoGsDone> 
	^self @env0:rem: aNumber
%

category: 'arithmetic'
method: LargeInteger
\\ aNumber  
	"Primitive. Take the receiver modulo the argument. The result is the 
	remainder rounded towards negative infinity, of the receiver divided 
	by the argument. Fail if the argument is 0. Fail if either the argument 
	or the result is not a SmallInteger or a LargePositiveInteger less than 
	2-to-the-30th (1073741824). Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 264>
	<PharoGsDone> 
	aNumber isInteger 
		ifTrue: 
			[| neg qr q r | 
			neg := self negative == aNumber negative == false. 
			qr := self digitDiv: aNumber neg: neg. 
			q := qr first normalize. 
			r := qr last normalize. 
			^(q negative 
				ifTrue: [r isZero not] 
				ifFalse: [q isZero and: [neg]]) 
					ifTrue: [r + aNumber] 
					ifFalse: [r]]. 
	^super \\ aNumber 
%

category: 'system primitives'
method: LargeInteger
digitAt: index  
	"Primitive. Answer the value of an indexable field in the receiver.   
	LargePositiveInteger uses bytes of base two number, and each is a 
	'digit' base 256.  Fail if the argument (the index) is not an Integer 
	or is out of bounds. Essential.  
	See Object documentation whatIsAPrimitive." 
	<PharoGsError>

	self @env0:error: 'GemStone implementation may differ'.
%

category: 'system primitives'
method: LargeInteger
digitAt: index put: value  
	"Primitive. Store the second argument (value) in the indexable field of  
	the receiver indicated by index. Fail if the value is negative or is larger  
	than 255. Fail if the index is not an Integer or is out of bounds. Answer  
	the value that was stored. Essential. See Object documentation  
	whatIsAPrimitive." 
	<PharoGsError>

	self @env0:error: 'GemStone implementation may differ'.
%

category: 'system primitives'
method: LargeInteger
digitLength 
	"Primitive. Answer the number of indexable fields in the receiver. This  
	value is the same as the largest legal subscript. Essential. See Object  
	documentation whatIsAPrimitive." 
	<PharoGsError>

	self @env0:error: 'GemStone implementation may differ'.
%

category: 'system primitives'
method: LargeInteger
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop 
	in the receiver starting at index, repStart, in the collection, 
	replacement. Answer the receiver. Range checks are performed in 
	the primitive only. Optional. See Object documentation whatIsAPrimitive." 
	<PharoGsError>

	self @env0:error: 'GemStone implementation may differ'.
%


! -- LargeNegativeInteger

category: 'arithmetic'
method: LargeNegativeInteger
normalize 
	"Check for leading zeroes and return shortened copy if so" 

	<PharoGsDone> 
    ^self + 0
%

! -- LargePositiveInteger

category: 'arithmetic'
method: LargePositiveInteger
normalize 
	"Check for leading zeroes and return shortened copy if so" 

	<PharoGsDone> 
    ^self + 0
%

category: 'bit manipulation'
method: LargePositiveInteger
hashMultiply 
	"This is a multiplication of hashes by 1664525 mod 2^28 written to avoid 
	overflowing into large integers. 
	 The primitive is able to perform the operation with modulo arihmetic. 
	 
	Example of usage: 
	  hash 
	    ^ (super hash + variableName hash) hashMultiply 	 
	" 
	<PharoGsError>

	self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
