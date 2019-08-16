set compile_env: 2

category: 'instance creation'
classmethod: WideString
from: aString 

    <PharoGs>
    ^self @env0:withAll: aString
%

category: 'accessing'
method: WideString
at: index 
	"Answer the Character stored in the field of the receiver indexed by the 
	 argument.  Primitive.  Fail if the index argument is not an Integer or is out 
	 of bounds.  Essential.  See Object documentation whatIsAPrimitive." 

    <primitive: 655>
	<PharoGs>
    ^self @env0:at: index 
%

category: 'accessing'
method: WideString
at: index put: aCharacter 
	"Store the Character into the field of the receiver indicated by the index. 
	 Primitive.  Fail if the index is not an Integer or is out of bounds, or if the 
	 argument is not a Character.  Essential.  See Object documentation whatIsAPrimitive." 

    <primitive: 667>
	<PharoGs>
    ^self @env0:at: index put: aCharacter
%

category: 'accessing'
method: WideString
basicAt: index 

	<PharoGs>
	^(self at: index) codePoint
%

category: 'accessing'
method: WideString
basicAt: index put: anInteger

	<PharoGs>
	self at: index put: (Character codePoint: anInteger).
	^anInteger
%

category: 'accessing'
method: WideString
replaceFrom: start to: stop with: replacement startingAt: repStart  

	<PharoGs>
    ^self @env0:replaceFrom: start to: stop with: replacement startingAt: repStart  
%

category: 'accessing'
method: WideString
size

	<PharoGs>
	^self @env0:size
%

category: 'accessing'
method: WideString
wordAt: index 

    <primitive: 1073>
	<PharoGs>
    ^self @env0:codePointAt: index
%

category: 'accessing'
method: WideString
wordAt: index put: anInteger 

	<PharoGs>
    ^self at: index put: (Character withValue: anInteger)
%

set compile_env: 0
