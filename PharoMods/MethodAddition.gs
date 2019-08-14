set compile_env: 2

category: 'operations'
method: MethodAddition
createCompiledMethod

   	<PharoGs>
    [[
        compiledMethod := myClass 
            @env0:compileMethod: text asString
            dictionaries: System @env0:myUserProfile @env0:symbolList
            category: category @env0:asSymbol
            intoMethodDict: false
            intoCategories: nil
            environmentId: 2.
    ] @env0:on: (Globals @env0:at: #'CompileError') do: [:ex | ^nil].
    ] @env0:on: (Globals @env0:at: #'CompileWarning') do: [:ex | ex @env0:resume].
    selector := compiledMethod selector.
	priorMethodOrNil := myClass compiledMethodAt: selector ifAbsent: [ nil ].
	priorCategoryOrNil := myClass organization categoryOfElement: selector.
%

category: 'operations'
method: MethodAddition
fixBinarySelectorsInLiteralArrayIn: aString

	| literalArrayDepth prev readStream writeStream |
	prev := nil.
	literalArrayDepth := 0.
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd.
	] whileFalse: [
		| next |
		next := readStream next.
		
		"deal with comments"
		(next == $" and: [prev ~~ $$]) ifTrue: [
			writeStream nextPut: $"; nextPutAll: (readStream upTo: $"); nextPut: $".
			prev := $".
		] ifFalse: [
			"deal with strings"
		(next == $' and: [prev ~~ $$]) ifTrue: [ "'"
			writeStream nextPut: $'; nextPutAll: (readStream upTo: $'); nextPut: $'.
			prev := $'.
		] ifFalse: [
		
		"deal with start of literal Array"
		(next == $# and: [readStream peekFor: $(]) ifTrue: [
			writeStream nextPutAll: '#('.
			prev := $(.
			literalArrayDepth := literalArrayDepth + 1.
		] ifFalse: [
			"here we are inside a literal array"
		literalArrayDepth > 0 ifTrue: [
				"a nested literal array"
			(next == $( and: [prev ~~ $# and: [prev ~~ $$]]) ifTrue: [
				writeStream nextPutAll: '#('.
				prev := next.
				literalArrayDepth := literalArrayDepth + 1.
			] ifFalse: [
				"end of a literal array"
			next == $) ifTrue: [
				writeStream nextPut: next.
				prev := next.
				literalArrayDepth := literalArrayDepth - 1.
			] ifFalse: [
			
			"handle binary selectors (this is why we are doing the work)"
			((next isSpecial or: [next == $;]) and: [prev ~~ $$]) ifTrue: [
				prev ~~ $# ifTrue: [
					writeStream nextPutAll: ' #'''.
				].
				writeStream nextPut: next.
				prev := next.
				[
					(readStream peek isSpecial or: [readStream peek == $;]).
				] whileTrue: [
					writeStream nextPut: (prev := readStream next).
				].
				writeStream nextPut: $'. "'"
			
			"other characters inside a literal array"
			] ifFalse: [
				writeStream nextPut: next.
				prev := next.
			]]]
			"other characters outside of a literal array"
		] ifFalse: [
			writeStream nextPut: next.
			prev := next.
		]]]]
	].
	^writeStream contents.
%

category: 'operations'
method: MethodAddition
fixSelectorAsBlockIn: aString

	| literalArrayDepth prev readStream writeStream |
	prev := nil.
	literalArrayDepth := 0.
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd.
	] whileFalse: [
		| next |
		next := readStream next.
		
		"deal with comments"
		(next == $" and: [prev ~~ $$]) ifTrue: [
			writeStream nextPut: $"; nextPutAll: (readStream upTo: $"); nextPut: $".
			prev := $".
		] ifFalse: [
			"deal with strings"
		(next == $' and: [prev ~~ $$]) ifTrue: [ "'"
			writeStream nextPut: $'; nextPutAll: (readStream upTo: $'); nextPut: $'.
			prev := $'.
		] ifFalse: [
		
		"look for 'ifNotNil: #selector'"
		(next == $i and: [readStream peekFor: $f]) ifTrue: [
			| position char x |
			position := readStream position.
			((x := readStream next: 7) = 'NotNil:' and: [(x := readStream skipSeparators; next) == $#]) ifTrue: [
                writeStream nextPutAll: 'ifNotNil: [:value | value '.
                [
                    (readStream atEnd not and: [(char := readStream peek) isAlphaNumeric or: [char == $_]]).
                ] whileTrue: [
                    writeStream nextPut: readStream next.
                ].
                writeStream nextPut: (prev := $]).
            ] ifFalse: [
                readStream position: position.
                writeStream nextPutAll: 'if'.
                prev := $f.
            ].
		] ifFalse: [
			writeStream nextPut: next.
			prev := next.
		]]].
	].
	^writeStream contents.
%

category: 'operations'
method: MethodAddition
installMethod

   	<PharoGs>
    [
        | string |
        string := text asString.
        string := self fixBinarySelectorsInLiteralArrayIn: string.
        string := self fixSelectorAsBlockIn: string.
        compiledMethod := myClass 
            @env0:compileMethod: string
            dictionaries: System @env0:myUserProfile @env0:symbolList
            category: category @env0:asSymbol
            intoMethodDict: nil
            intoCategories: nil
            environmentId: 2.
    ] @env0:on: (Globals @env0:at: #'CompileWarning') do: [:ex | ex @env0:resume].
%

set compile_env: 0
