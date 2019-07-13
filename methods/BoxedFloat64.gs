set compile_env: 2

category: 'math functions'
classmethod: BoxedFloat64
basicNew: sizeRequested  
	"Primitive. Answer an instance of this class with the number 
	 of indexable variables specified by the argument, sizeRequested. 
	 Fail if this class is not indexable or if the argument is not a 
	 positive Integer, or if there is not enough memory available.  
	 Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsDone>
    ^self @env0:_basicNew: sizeRequested
%

category: 'arithmetic'
method: BoxedFloat64
* aNumber  
	"Primitive. Answer the result of multiplying the receiver by aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 102>
    <PharoGsDone>
    ^self @env0:* aNumber
%

category: 'arithmetic'
method: BoxedFloat64
+ aNumber  
	"Primitive. Answer the sum of the receiver and aNumber. Essential. 
	Fail if the argument is not a Float. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 106>
    <PharoGsDone>
    ^self @env0:+ aNumber
%

category: 'arithmetic'
method: BoxedFloat64
- aNumber  
	"Primitive. Answer the difference between the receiver and aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 107>
    <PharoGsDone>
    ^self @env0:- aNumber
%

category: 'arithmetic'
method: BoxedFloat64
/ aNumber  
	"Primitive. Answer the result of dividing receiver by aNumber. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 108>
    <PharoGsDone>
    ^self @env0:/ aNumber
%

category: 'comparing'
method: BoxedFloat64
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
method: BoxedFloat64
<= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is less than or equal to the argument. Otherwise return 
	false. Fail if the argument is not a Float. Optional. See Object 
	documentation whatIsAPrimitive." 

    <primitive: 121>
    <PharoGsDone>
    ^self @env0:< aNumber
%

category: 'comparing'
method: BoxedFloat64
= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is equal to the argument. Otherwise return false. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 119>
    <PharoGsDone>
    ^self @env0:< aNumber
%

category: 'comparing'
method: BoxedFloat64
> aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is greater than the argument. Otherwise return false. 
	Fail if the argument is not a Float. Essential. See Object documentation 
	whatIsAPrimitive." 

    <PharoGsDone>
    ^aNumber < self
%

category: 'comparing'
method: BoxedFloat64
>= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is greater than or equal to the argument. Otherwise return 
	false. Fail if the argument is not a Float. Optional. See Object documentation  
	whatIsAPrimitive. " 

    <PharoGsDone>
    ^aNumber <= self
%

category: 'math functions'
method: BoxedFloat64
arcTan 
	"Answer the angle in radians. 
	 Optional. See Object documentation whatIsAPrimitive." 

    <PharoGsDone>
    ^self @env0:_mathPrim: 4
%

category: 'accessing'
method: BoxedFloat64
basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an  
	indexable element in the receiver. Fail if the argument index is not an  
	Integer or is out of bounds. Essential. Do not override in a subclass. See  
	Object documentation whatIsAPrimitive. 
	This version of basicAt: is specifically for floats, answering the most significant 
	word for index 1 and the least significant word for index 2.  This alows the VM 
	to store floats in whatever order it chooses while it appears to the image that 
	they are always in big-endian/PowerPC order." 

	<PharoGsError>
	self @env0:error: 'Does GemStone store a BoxedFloat64 in big-endian format?'
%

category: 'accessing'
method: BoxedFloat64
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument  
	value in the indexable element of the receiver indicated by index. Fail  
	if the index is not an Integer or is out of bounds. Or fail if the value is  
	not of the right type for this kind of collection. Answer the value that  
	was stored. Essential. Do not override in a subclass. See Object  
	documentation whatIsAPrimitive. 
	This version of basicAt: is specifically for floats, answering the most significant 
	word for index 1 and the least significant word for index 2.  This alows the VM 
	to store floats in whatever order it chooses while it appears to the image that 
	they are always in big-endian/PowerPC order." 

	<PharoGsError>
	self @env0:error: 'Does GemStone store a BoxedFloat64 in big-endian format?'
%

category: 'math functions'
method: BoxedFloat64
exp 
	"Answer E raised to the receiver power. 
	 Optional. See Object documentation whatIsAPrimitive."  

    <PharoGsDone>
    ^self @env0:_mathPrim: 0
%

category: 'math functions'
method: BoxedFloat64
exponent 
	"Primitive. Consider the receiver to be represented as a power of two 
	multiplied by a mantissa (between one and two). Answer with the 
	SmallInteger to whose power two is raised. Optional. See Object 
	documentation whatIsAPrimitive." 

    <PharoGsDone>
    ^self @env0:_mathPrim: 28
%

category: 'truncation and round off'
method: BoxedFloat64
fractionPart 
	"Primitive. Answer a Float whose value is the difference between the  
	receiver and the receiver's asInteger value. Optional. See Object  
	documentation whatIsAPrimitive." 

    <PharoGsDone>
	^self - self truncated asFloat
%

category: 'math functions'
method: BoxedFloat64
ln 
	"Answer the natural logarithm of the receiver. 
	 Optional. See Object documentation whatIsAPrimitive." 

    <PharoGsDone>
    ^self @env0:_mathPrim: 7
%

category: 'math functions'
method: BoxedFloat64
sin 
	"Answer the sine of the receiver taken as an angle in radians. 
	 Optional. See Object documentation whatIsAPrimitive." 

    <PharoGsDone>
    ^self @env0:_mathPrim: 2
%

category: 'math functions'
method: BoxedFloat64
sqrt 
	"Answer the square root of the receiver.  
	 Optional. See Object documentation whatIsAPrimitive." 

    <primitive: 104>
    <PharoGsDone>
    ^self @env0:sqrt
%

category: 'math functions'
method: BoxedFloat64
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

category: 'truncation and round off'
method: BoxedFloat64
truncated 
	"Answer with a SmallInteger equal to the value of the receiver without  
	its fractional part. The primitive fails if the truncated value cannot be  
	represented as a SmallInteger. In that case, the code below will compute  
	a LargeInteger truncated value. 
	Essential. See Object documentation whatIsAPrimitive. " 

    <PharoGsDone>
    ^self @env0:_truncated: true
%

category: 'math functions'
method: BoxedFloat64
~= aNumber  
	"Primitive. Compare the receiver with the argument and return true 
	if the receiver is not equal to the argument. Otherwise return false. 
	Fail if the argument is not a Float. Optional. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 177>
    <PharoGsDone>
    ^self @env0:~= aNumber
%

set compile_env: 0
