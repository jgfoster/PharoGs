set compile_env: 2

category: 'instance creation'
classmethod: Character
value: anInteger 
	"Answer the Character whose value is anInteger." 

	<primitive: 72>
	<PharoGsDone> 
	^self @env0:codePoint: anInteger
%

category: 'comparing'
method: Character
= aCharacter  
	"Primitive. Answer if the receiver and the argument are the 
	 same object (have the same object pointer). Optional. See 
	 Object documentation whatIsAPrimitive." 

	<PharoGsDone> 
	"optimized selector, so no primitive"
	^self == aCharacter
%

category: 'accessing'
method: Character
asciiValue 
	"Answer the receiver's character code. 
	 This will be ascii for characters with value <= 127, 
	 and Unicode for those with higher values."

	<primitive: 71>
	<PharoGsDone> 
	self @env0:codePoint
%

category: 'accessing'
method: Character
asInteger 
	"Answer the receiver's character code." 

	<primitive: 71>
	<PharoGsDone> 
	^self @env0:asInteger
%

category: 'comparing'
method: Character
basicIdentityHash 
	"Answer the receiver's character code. 
	 The value answered is unsigned. It can in theory be in the full 
	 poisitive SmallInteger range, but based on Unicode, it is fair 
	 to assume that the value is in the range [ 0 ; 16r3FFFFF ]" 

	<primitive: 71>
	<PharoGsDone> 
	self @env0:hash
%

category: 'comparing'
method: Character
hash 
	"Hash is reimplemented because = is implemented. 
	 Answer the receiver's character code." 

	<primitive: 71>
	<PharoGsDone> 
	self @env0:hash
%

set compile_env: 0
