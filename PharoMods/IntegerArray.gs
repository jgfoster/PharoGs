set compile_env: 2

category: 'instance creation'
classmethod: IntegerArray
new: anInteger

    <PharoGs>
    ^(super new: anInteger)
        atAllPut: 0;
        yourself
%

category: 'accessing'
method: IntegerArray
at: index 
    <PharoGs>

	| word | 
	word := self basicAt: index. 
	word < 16r3FFFFFFF ifTrue:[^word]. "Avoid LargeInteger computations" 
	^word >= 16r80000000	"Negative?!" 
		ifTrue:["word - 16r100000000" 
				(word bitInvert32 + 1) negated] 
		ifFalse:[word]
%

category: 'accessing'
method: IntegerArray
at: index put: anInteger 
    <PharoGs>

	| word | 
	anInteger < 0 
		ifTrue:["word := 16r100000000 + anInteger" 
				word := (anInteger + 1) negated bitInvert32] 
		ifFalse:[word := anInteger]. 
	self  basicAt: index put: word. 
	^anInteger
%

category: 'private'
method: IntegerArray
primFill: aPositiveInteger 
	"Fill the receiver, an indexable bytes or words object, 
    with the given positive integer. The range of possible 
    fill values is [0..255] for byte arrays and [0..(2^32 - 1)] 
    for word arrays." 
    <PharoGs>

    ^self @env0:atAllPut: aPositiveInteger
%

set compile_env: 0
