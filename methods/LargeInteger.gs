set compile_env: 2

category: 'arithmetic'
method: LargeInteger
* anInteger  
	"Primitive. Multiply the receiver by the argument and answer with an 
	Integer result. Fail if either the argument or the result is not a 
	SmallInteger or a LargePositiveInteger less than 2-to-the-30th (1073741824). Optional. See 
	Object documentation whatIsAPrimitive. " 

    <primitive: 260>
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
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
	<PharoGs> 
	^self @env0:quo: anInteger
%

category: 'arithmetic'
method: LargeInteger
rem: aNumber  
	"Remainder defined in terms of quo:. See super rem:. 
	This is defined only to speed up case of large integers." 

	<PharoGs> 
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
	<PharoGs> 
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

! -- LargeNegativeInteger

category: 'arithmetic'
method: LargeNegativeInteger
normalize 
	"Check for leading zeroes and return shortened copy if so" 

	<PharoGs> 
    ^self + 0
%

! -- LargePositiveInteger

category: 'arithmetic'
method: LargePositiveInteger
normalize 
	"Check for leading zeroes and return shortened copy if so" 

	<PharoGs> 
    ^self + 0
%

set compile_env: 0
