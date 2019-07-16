set compile_env: 2

category: 'accessing'
method: ByteSymbol
at: index  
	"Primitive. Answer the Character stored in the field of the receiver 
	indexed by the argument. Fail if the index argument is not an Integer or 
	is out of bounds. Essential. See Object documentation whatIsAPrimitive." 

    <primitive: 69>
    <PharoGs> 
	^self @env0:at: index
%

category: 'accessing'
method: ByteSymbol
byteAt: index  

    <primitive: 69>
    <PharoGs> 
	^self @env0:at: index
%

category: 'comparing'
method: ByteSymbol
findSubstring: key in: body startingAt: start matchTable: matchTable 
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned." 

	<PharoGs> 
	^super findSubstring: key in: body startingAt: start matchTable: matchTable
%

category: 'private'
method: ByteSymbol
privateAt: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by 
	the index. Fail if the index is not an Integer or is out of bounds, or if 
	the argument is not a Character. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 293>
    <PharoGs> 
	^self @env0:at: index put: aCharacter
%

set compile_env: 0
