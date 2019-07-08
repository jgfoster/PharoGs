category: 'arithmetic'
set compile_env: 2
method: SmallInteger
// aNumber  
	"Primitive. Divide the receiver by the argument and answer with the 
	result. Round the result down towards negative infinity to make it a 
	whole integer. Fail if the argument is 0 or is not a SmallInteger. 
	Essential. No Lookup. See Object documentation whatIsAPrimitive. " 

	<primitive: 12>
	<PharoGs> 
"	<primitive: 12> "
	^ super // aNumber 	"Do with quo: if primitive fails"
%
set compile_env: 0
category: 'arithmetic'
set compile_env: 2
method: SmallInteger
- aNumber  
	"Primitive. Subtract the argument from the receiver and answer with the 
	result if it is a SmallInteger. Fail if the argument or the result is not a 
	SmallInteger. Essential. No Lookup. See Object documentation 
	whatIsAPrimitive." 

	<primitive: 259>
	<PharoGs> 
	^super - aNumber
%
set compile_env: 0
category: 'comparing'
set compile_env: 2
method: SmallInteger
= aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is equal to the argument. Otherwise answer false. Fail if the 
	argument is not a SmallInteger. Essential. No Lookup. See Object 
	documentation whatIsAPrimitive. " 

	<primitive: 7>
	<PharoGs> 
"	<primitive: 7> "
	^super = aNumber
%
set compile_env: 0
category: 'comparing'
set compile_env: 2
method: SmallInteger
>= aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than or equal to the argument. Otherwise answer 
	false. Fail if the argument is not a SmallInteger. Optional. No Lookup. 
	See Object documentation whatIsAPrimitive." 

	<primitive: 6>
	<PharoGs> 
"	<primitive: 6> "
	^super >= aNumber
%
set compile_env: 0
category: 'comparing'
set compile_env: 2
method: SmallInteger
< aNumber  
	"Primitive. Compare the receiver with the argument and answer with 
	true if the receiver is less than the argument. Otherwise answer false. 
	Fail if the argument is not a SmallInteger. Essential. No Lookup. See 
	Object documentation whatIsAPrimitive." 

	<primitive: 3> 
	<PharoGs>
	^super < aNumber
%
set compile_env: 0
category: 'comparing'
set compile_env: 2
method: SmallInteger
> aNumber  
	"Primitive. Compare the receiver with the argument and answer true if 
	the receiver is greater than the argument. Otherwise answer false. Fail if 
	the argument is not a SmallInteger. Essential. No Lookup. See Object 
	documentation whatIsAPrimitive." 

	<primitive: 4>
	<PharoGs> 
"	<primitive: 4> "
	^super > aNumber
%
set compile_env: 0
category: 'bit manipulation'
set compile_env: 2
method: SmallInteger
bitShift: arg  
	"Primitive. Answer an Integer whose value is the receiver's value shifted 
	left by the number of bits indicated by the argument. Negative arguments 
	shift right. The receiver is interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 17>
	<PharoGs> 
"	<primitive: 17> "
	self >= 0 ifTrue: [^ super bitShift: arg]. 
	^ arg >= 0 
		ifTrue: [(self negated bitShift: arg) negated] 
		ifFalse: [(self bitInvert bitShift: arg) bitInvert].
%
set compile_env: 0
