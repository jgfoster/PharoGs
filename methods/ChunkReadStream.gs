category: 'decorated'
set compile_env: 2
method: ChunkReadStream
parseLangTagFor: aString
	<PharoGs>

	| string peek runsRaw foundTag |
	string := aString.
	"Test for ]lang[ tag"
	peek := self skipSeparators; peek.
	peek = $] ifFalse: [ ^ string ].  "no tag"
	foundTag := (decoratedStream upTo: $[).
	foundTag = ']lang' ifTrue: [
		runsRaw := self basicNextChunk.
		string := self decodeString: aString andRuns: runsRaw
	] ifFalse: [
		"If we find a ] but not a lang tag, there is something really wrong and we are not ready to parse that"
		^ string , self basicNextChunk
	].
	^ string
%
set compile_env: 0
