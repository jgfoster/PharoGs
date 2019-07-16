set compile_env: 2

category: 'private'
method: WideSymbol
privateAt: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by 
	the index. Fail if the index is not an Integer or is out of bounds, or if 
	the argument is not a Character. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 667>
	<PharoGs> 
    ^self @env0:at: index put: aCharacter
%

category: 'private'
method: WideSymbol
wordAt: index 

     <primitive: 1073>
	<PharoGs>
    ^self @env0:codePointAt: index
%

set compile_env: 0
