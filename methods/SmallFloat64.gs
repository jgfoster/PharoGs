set compile_env: 2

category: 'arithmetic'
method: SmallFloat64
* aNumber  
	"Primitive. Answer the result of multiplying the receiver by aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 102>
	<PharoGsDone>
	FloatingPointError _checkFpStatus .
	^self @env0:* aNumber
%

category: 'arithmetic'
method: SmallFloat64
+ aNumber  
	"Primitive. Answer the sum of the receiver and aNumber. Essential. 
	Fail if the argument is not a Float. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 106>
	<PharoGsDone>
	FloatingPointError _checkFpStatus .
	^self @env0:+ aNumber
%

category: 'arithmetic'
method: SmallFloat64
- aNumber  
	"Primitive. Answer the difference between the receiver and aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 107>
	<PharoGsDone>
	FloatingPointError _checkFpStatus .
	^self @env0:- aNumber
%

category: 'arithmetic'
method: SmallFloat64
/ aNumber  
	"Primitive. Answer the result of dividing receiver by aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 108>
	<PharoGsDone>
	FloatingPointError _checkFpStatus .
	^self @env0:/ aNumber
%

category: 'comparing'
method: SmallFloat64
< aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is less than the argument. Otherwise return false. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 118>
	<PharoGsDone>
	^self @env0:< aNumber
%

category: 'comparing'
method: SmallFloat64
<= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is less than or equal to the argument. Otherwise return 
	false. Fail if the argument is not a Float. Optional. See Object 
	documentation whatIsAPrimitive." 

	<primitive: 121>
	<PharoGsDone>
	^self @env0:<= aNumber
%

category: 'comparing'
method: SmallFloat64
= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is equal to the argument. Otherwise return false. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 119>
	<PharoGsDone>
	^self @env0:= aNumber
%

category: 'comparing'
method: SmallFloat64
> aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is greater than the argument. Otherwise return false. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

	<PharoGsDone>
	^aNumber < self
%

category: 'comparing'
method: SmallFloat64
>= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is greater than or equal to the argument. Otherwise return 
	false. Fail if the argument is not a Float. Optional. See Object documentation  
	whatIsAPrimitive. " 

	<PharoGsDone>
	^aNumber <= self
%

category: 'comparing'
method: SmallFloat64
~= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is not equal to the argument. Otherwise return false. 
	Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 177>
	<PharoGsDone>
	^self @env0:~= aNumber
%

category: 'comparing'
method: SmallFloat64
basicIdentityHash 
	"Answer an integer unique to the receiver." 
	"primitive 171 is the primitiveImmediateAsInteger primitive that converts SmallIntegers to themselves, Characters to their integer codes, and SmallFloat64 to an integer representing their bits.  e.g. 
#(-0.0 0.0 -1.0 1.0) collect: [:n| n -> n identityHash hex -> (n identityHash bitAnd: 2 << 64 - 1) hex] 
{-0.0->'-16r1000000000000000'->'16r1F000000000000000' . 
  0.0->'16r0'->'16r0' . 
 -1.0->'-16r810000000000000'->'16r1F7F0000000000000' . 
  1.0->'16r7F0000000000000'->'16r7F0000000000000'}" 

	<primitive: 321>
	<PharoGsDone>
	^self @env0:basicIdentityHash
%

category: 'mathematical functions'
method: SmallFloat64
arcTan 
	"Answer the angle in radians. 
	 Optional. See Object documentation whatIsAPrimitive." 

	<PharoGsDone>
	^self @env0:_mathPrim: 4
%

category: 'mathematical functions'
method: SmallFloat64
exp 
	"Answer E raised to the receiver power. 
	 Optional. See Object documentation whatIsAPrimitive."  

	<PharoGsDone>
	^self @env0:_mathPrim: 0
%

category: 'mathematical functions'
method: SmallFloat64
exponent 
	"Primitive. Consider the receiver to be represented as a power of two 
	multiplied by a mantissa (between one and two). Answer with the 
	SmallInteger to whose power two is raised. Optional. See Object 
	documentation whatIsAPrimitive." 

	<PharoGsDone>
	^self @env0:_mathPrim: 28
%

category: 'truncation and round off'
method: SmallFloat64
fractionPart 
	"Primitive. Answer a Float whose value is the difference between the  
	receiver and the receiver's asInteger value. Optional. See Object  
	documentation whatIsAPrimitive." 

	<PharoGsDone>
	^self @env0:fractionPart
%

category: 'mathematical functions'
method: SmallFloat64
ln 
	"Answer the natural logarithm of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive." 

	<PharoGsDone>
	^self @env0:_mathPrim: 7
%

category: 'mathematical functions'
method: SmallFloat64
sin 
	"Answer the sine of the receiver taken as an angle in radians. 
	 Optional. See Object documentation whatIsAPrimitive." 

	<PharoGsDone>
	^self @env0:_mathPrim: 2
%

category: 'mathematical functions'
method: SmallFloat64
sqrt 
	"Answer the square root of the receiver.  
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 104>
	<PharoGsDone>
	^self @env0:sqrt
%

category: 'mathematical functions'
method: SmallFloat64
timesTwoPower: anInteger  
	"Primitive. Answer with the receiver multiplied by 2.0 raised 
	to the power of the argument. 
	Optional. See Object documentation whatIsAPrimitive." 

	<PharoGsDone>
	anInteger < -29 ifTrue: [^ self * (2.0 raisedToInteger: anInteger)]. 
	anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat]. 
	anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]. 
	^ self * (2.0 raisedToInteger: anInteger)
%

category: 'mathematical functions'
method: SmallFloat64
truncated 
	"Answer with a SmallInteger equal to the value of the receiver without  
	its fractional part. The primitive fails if the truncated value cannot be  
	represented as a SmallInteger. In that case, the code below will compute  
	a LargeInteger truncated value. 
	Essential. See Object documentation whatIsAPrimitive. " 

    <PharoGsDone>
    ^self @env0:_truncated: true
%

set compile_env: 0
