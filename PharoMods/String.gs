set compile_env: 2

category: 'testing'
classMethod: String
isBytes

	<PharoGs>
	^true
%

category: 'instance creation'
classMethod: String
new: sizeRequested  
	"Return a new instance with the number of indexable variables specified by the argument." 
	 
	<PharoGs>
	^self == String  
		ifTrue:[ByteString new: sizeRequested] 
		ifFalse:[self @env0:new: sizeRequested] 
%

category: 'converting'
method: String
asSymbol

	<PharoGs>
	^self @env0:asSymbol
%

category: 'accessing'
method: String
at: anIndex

	<primitive: 69>
	<PharoGs>
	^self @env0:at: anIndex
%

category: 'comparing'
method: String
findSubstringViaPrimitive: key in: body startingAt: start matchTable: matchTable 
	"Answer the index in the string body at which the substring key first occurs, 
	at or beyond start.  The match is determined using matchTable, which can be 
	used to effect, eg, case-insensitive matches.  If no match is found, zero will 
	be returned. 
	The algorithm below is not optimum -- it is intended to be translated to C 
	which will go so fast that it wont matter." 
	<PharoGs>

	| index | 
	key size = 0 ifTrue: [^ 0]. 
	(start max: 1) to: body size - key size + 1 do: 
		[:startIndex | 
		index := 1. 
			[(matchTable at: (body basicAt: startIndex+index-1) + 1) 
				= (matchTable at: (key basicAt: index) + 1)] 
				whileTrue: 
				[index = key size ifTrue: [^ startIndex]. 
				index := index+1]]. 
	^ 0 
" 
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 1 matchTable: CaseSensitiveOrder 1 
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 2 matchTable: CaseSensitiveOrder 7 
' ' findSubstring: 'abc' in: 'abcdefabcd' startingAt: 8 matchTable: CaseSensitiveOrder 0 
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseSensitiveOrder 0 
' ' findSubstring: 'abc' in: 'abcdefABcd' startingAt: 2 matchTable: CaseInsensitiveOrder 7 
" 
%

category: 'private'
method: String
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the 
	receiver starting at index, repStart, in the collection, replacement. 
	Answer the receiver. Range checks are performed in the primitive only. 
	Optional. See Object documentation whatIsAPrimitive." 

	"GemStone:
	Replaces the elements of the receiver between the indexes startIndex and
	stopIndex inclusive with the elements of aSeqCollection starting at repIndex.
	If aSeqCollection is identical to the receiver, the source and
	destination blocks may overlap.

	The primitive supports directly the case where 
	(charCollection isKindOfClass: CByteArray) == true
	with repIndex being one-based .

	Returns the receiver."

	<primitive: 297>
	<PharoGs>
	^self @env0:replaceFrom: start to: stop with: replacement startingAt: repStart
%

set compile_env: 0
