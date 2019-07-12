set compile_env: 2

category: 'arithmetic'
method: SmallInteger
* aNumber  
	"Primitive. Multiply the receiver by the argument and answer with the 
	result if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger. Essential. No Lookup. See Object documentation whatIsAPrimitive." 

	<primitive: 260>
	<PharoGsDone> 
	^ super * aNumber
%

category: 'arithmetic'
method: SmallInteger
+ aNumber  
	"Primitive. Subtract the argument from the receiver and answer with the 
	result if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger. Essential. No Lookup. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 258>
	<PharoGsDone> 
	^super + aNumber
%

category: 'arithmetic'
method: SmallInteger
- aNumber  
	"Primitive. Subtract the argument from the receiver and answer with the 
	result if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger. Essential. No Lookup. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 259>
	<PharoGsDone> 
	^super - aNumber
%

category: 'arithmetic'
method: SmallInteger
/ aNumber  
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards negative infinity to make it a 
	whole integer. Fail if the argument is 0 or is not a SmallInteger. 
	Essential. No Lookup. See Object documentation whatIsAPrimitive. " 

	<primitive: 261>
	<PharoGsDone> 
	aNumber isZero ifTrue: [^(ZeroDivide dividend: self) signal]. 
	^(aNumber isMemberOf: SmallInteger) 
		ifTrue: [(Fraction numerator: self denominator: aNumber) reduced] 
		ifFalse: [super / aNumber]
%

category: 'arithmetic'
method: SmallInteger
// aNumber  
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards negative infinity to make it a 
	whole integer. Fail if the argument is 0 or is not a SmallInteger. 
	Essential. No Lookup. See Object documentation whatIsAPrimitive. " 

	<primitive: 12>
	<PharoGsDone> 
	^ super // aNumber 	"Do with quo: if primitive fails"
%

category: 'comparing'
method: SmallInteger
= aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is equal to the argument. Otherwise answer false. Fail if the 
	argument is not a SmallInteger. Essential. No Lookup. See Object 
	documentation whatIsAPrimitive. " 

	<primitive: 7>
	<PharoGsDone> 
	^super = aNumber
%

category: 'comparing'
method: SmallInteger
>= aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than or equal to the argument. Otherwise answer 
	false. Fail if the argument is not a SmallInteger. Optional. No Lookup. 
	See Object documentation whatIsAPrimitive." 

	<primitive: 6>
	<PharoGsDone> 
	^super >= aNumber
%

category: 'comparing'
method: SmallInteger
< aNumber  
	"Primitive. Compare the receiver with the argument and answer with 
	true if the receiver is less than the argument. Otherwise answer false. 
	Fail if the argument is not a SmallInteger. Essential. No Lookup. See 
	Object documentation whatIsAPrimitive." 

	<primitive: 3> 
	<PharoGsDone>
	^super < aNumber
%

category: 'comparing'
method: SmallInteger
<= aNumber  
	"Primitive. Compare the receiver with the argument and answer with 
	true if the receiver is less than the argument. Otherwise answer false. 
	Fail if the argument is not a SmallInteger. Essential. No Lookup. See 
	Object documentation whatIsAPrimitive." 

	<primitive: 21> 
	<PharoGsDone>
	^super <= aNumber
%

category: 'comparing'
method: SmallInteger
> aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than the argument. Otherwise answer false. Fail if 
	the argument is not a SmallInteger. Essential. No Lookup. See Object 
	documentation whatIsAPrimitive." 

	<primitive: 4>
	<PharoGsDone> 
	^super > aNumber
%

category: 'bit manipulation'
method: SmallInteger
bitShift: arg  
	"Primitive. Answer an Integer whose value is the receiver's value shifted 
	left by the number of bits indicated by the argument. Negative arguments 
	shift right. The receiver is interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 17>
	<PharoGsDone> 
	self >= 0 ifTrue: [^ super bitShift: arg]. 
	^ arg >= 0 
		ifTrue: [(self negated bitShift: arg) negated] 
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].
%

category: 'converting'
method: SmallInteger
asCharacter

	<PharoGsDone> 
	^Character @env0:codePoint: self
%

category: 'converting'
method: SmallInteger
asFloat 
	"Primitive. Answer a Float that represents the value of the receiver. 
	Essential. See Object documentation whatIsAPrimitive." 

	<primitive: 138>
	<PharoGsDone> 
	^self @env0:asFloat
%

category: 'bit manipulation'
method: SmallInteger
bitAnd: arg  
	"Primitive. Answer an Integer whose bits are the logical AND of the 
	receiver's bits and those of the argument, arg. 
	Numbers are interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 14>
	<PharoGsDone> 
	^self @env0:bitAnd: arg
%

category: 'bit manipulation'
method: SmallInteger
bitOr: arg  
	"Primitive. Answer an Integer whose bits are the logical OR of the 
	receiver's bits and those of the argument, arg. 
	Numbers are interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 15>
	<PharoGsDone> 
	^self @env0:bitOr: arg
%

category: 'bit manipulation'
method: SmallInteger
bitXor: arg  
	"Primitive. Answer an Integer whose bits are the logical XOR of the 
	receiver's bits and those of the argument, arg. 
	Numbers are interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 16>
	<PharoGsDone> 
	^self @env0:bitXor: arg
%

category: 'bit manipulation'
method: SmallInteger
hashMultiply 
	"This is a multiplication of hashes by 1664525 mod 2^28 written to avoid overflowing into large integers. 
	 The primitive is able to perform the operation with modulo arihmetic. 
	 
	Example of usage: 
	  hash 
	    ^ (super hash + variableName hash) hashMultiply 	 
	" 

	<PharoGsDone> 
	| low | 
	low := self bitAnd: 16383. 
	^(16r260D * low + ((16r260D * (self bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) 
			bitAnd: 16r0FFFFFFF
%

category: 'arithmetic'
method: SmallInteger
quo: aNumber  
	"Primitive. Divide the receiver by the argument and answer with the  
	result. Round the result down towards zero to make it a whole integer.  
	Fail if the argument is 0 or is not a SmallInteger. Optional. See Object  
	documentation whatIsAPrimitive." 
	
	<primitive: 262>
	aNumber = 0 ifTrue: [^ (ZeroDivide dividend: self) signal]. 
	(aNumber isMemberOf: SmallInteger) 
		ifFalse: [^ super quo: aNumber]. 
	(aNumber = -1 and: [self = self class minVal]) 
		ifTrue: ["result is aLargeInteger" ^ self negated]. 
	^self @env0:quo: aNumber
%

category: '*Reflectivity'
method: SmallInteger
rfMinus: aNumber  
	"Primitive. Subtract the argument from the receiver and answer with the 
	result if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger. Essential. No Lookup. See Object documentation 
	whatIsAPrimitive." 

	<PharoGsDone>
	(Reflection classOf: aNumber) == SmallInteger
		ifFalse: [self @env0:error: 'Argument must be a SmallInteger'].
	^self - aNumber
%

category: '*Reflectivity'
method: SmallInteger
rfPlus: aNumber  
	"Primitive. Add the receiver to the argument and answer with the result 
	if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive." 

	<PharoGsDone>
	(Reflection classOf: aNumber) == SmallInteger
		ifFalse: [self @env0:error: 'Argument must be a SmallInteger'].
	^self + aNumber
%

category: 'arithmetic'
method: SmallInteger
\\ aNumber  
	"Primitive. Take the receiver modulo the argument. The result is the 
	remainder rounded towards negative infinity, of the receiver divided by 
	the argument Fail if the argument is 0 or is not a SmallInteger. Optional. 
	No Lookup. See Object documentation whatIsAPrimitive." 

	<primitive: 264>
	<PharoGsDone> 
	^ super \\ aNumber 	"will use // to compute it if primitive fails"
%

category: 'arithmetic'
method: SmallInteger
~= aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is not equal to the argument. Otherwise answer false. Fail if 
	the argument is not a SmallInteger. Essential. No Lookup. See Object 
	documentation whatIsAPrimitive." 

	<primitive: 8> 
	<PharoGsDone> 
	^super ~= aNumber
%

set compile_env: 0
