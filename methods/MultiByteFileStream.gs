category: 'fileIn/Out'
set compile_env: 2
method: MultiByteFileStream
nextChunk
	<PharoGs>

	"Answer the contents of the receiver, up to the next terminator
	character. Doubled terminators indicate an embedded terminator
	character."
	self skipSeparators.
	^ self
		parseLangTagFor: (self basicChunk
				ifNil: [String
						new: 1000
						streamContents: [:stream | 
							| character state |
							[(character := self next) == nil
								or: [character == $!
										and: [state := converter saveStateOf: self.
											self next ~~ $!]]]
								whileFalse: [stream nextPut: character].
							character
								ifNotNil: [converter restoreStateOf: self with: state.]]])
%
set compile_env: 0
category: 'fileIn/Out'
set compile_env: 2
method: MultiByteFileStream
nextPreamble
	<PharoGs>

	"Assuming that preamble part does not contain ]lang[ tag"
	self skipSeparators.
	^ self basicChunk
				ifNil: [String
						new: 1000
						streamContents: [:stream | 
							| character state |
							[(character := self next) == nil
								or: [character == $!
										and: [state := converter saveStateOf: self.
											self next ~~ $!]]]
								whileFalse: [stream nextPut: character].
							character
								ifNotNil: [converter restoreStateOf: self with: state.]]]
%
set compile_env: 0
