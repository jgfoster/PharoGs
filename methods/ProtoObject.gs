set compile_env: 2

category: 'reflective operations'
method: ProtoObject
basicIdentityHash 
	"Answer a 22 bits unsigned SmallInteger whose value is related to the receiver's identity. 
	 
	Primitive. Fails if the receiver is an immediate. Essential. 
	See Object documentation whatIsAPrimitive. 
	 
	Do not override, use #identityHash instead" 

	<primitive: 321>
	<PharoGs> 
	^self @env0:class
%

category: 'class membership'
method: ProtoObject
class 
	"Primitive. Answer the object which is the receiver's class. Essential. See  
	Object documentation whatIsAPrimitive." 

	<primitive: 610>
	<PharoGs> 
	^self @env0:class
%

category: 'introspection'
method: ProtoObject
instVarsInclude: anObject 
"Answers true if anObject is among my named or indexed instance variables, and false otherwise" 

	<PharoGs> 
	1 to: self class instSize do: 
		[:i | (self instVarAt: i) == anObject ifTrue: [^ true]]. 
	1 to: self basicSize do: 
		[:i | (self basicAt: i) == anObject ifTrue: [^ true]]. 
	^ false
%

category: 'memory scanning'
method: ProtoObject
nextInstance 
	"Primitive. Answer the next instance after the receiver in the  
	enumeration of all instances of this class. Fails if all instances have been  
	enumerated. Essential. See Object documentation whatIsAPrimitive." 

	<PharoGs> 
	self @env0:error: 'Not supported in GemStone'
%

category: 'memory scanning'
method: ProtoObject
nextObject 
	"Primitive. Answer the next object after the receiver in the  
	enumeration of all objects. Return 0 when all objects have been  
	enumerated." 

	<PharoGs> 
	self @env0:error: 'Not supported in GemStone'
%

category: 'comparing'
method: ProtoObject
rfIsEqual: anObject  
	"Primitive. Answer whether the receiver and the argument are the same  
	object (have the same object pointer). Do not redefine the message == in  
	any other class! Essential. No Lookup. Do not override in any subclass.  
	See Object documentation whatIsAPrimitive." 
	<metaLinkOptions: #( #+ optionDisabledLink)> 
	<PharoGs> 

	^(Reflection oopOf: self) == (Reflection oopOf: anObject)
%

category: 'comparing'
method: ProtoObject
_reservedSelector_equal_equal_: anObject  
	"Primitive. Answer whether the receiver and the argument are the same  
	object (have the same object pointer). Do not redefine the message == in  
	any other class! Essential. No Lookup. Do not override in any subclass.  
	See Object documentation whatIsAPrimitive." 

	<PharoGs> 
	^self == anObject
%

category: 'comparing'
method: ProtoObject
_reservedSelector_tilde_tilde_: anObject 
	"Primitive. Answer whether the receiver and the argument are different objects 
	(do not have the same object pointer). Do not redefine the message ~~ in  
	any other class! Optional (Assuming == is essential). No Lookup. Do not override in any subclass.  
	See Object documentation whatIsAPrimitive." 

	<PharoGs> 
	^self ~~ anObject
%

set compile_env: 0
