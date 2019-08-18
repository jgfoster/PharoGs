set compile_env: 0

run
(Object subclass: 'GrGemStonePlatform'
	instVarNames: #( )
	classVars: #()
	classInstVars: #()
	poolDictionaries: {}
	inDictionary: UserGlobals
	newVersionOf: (UserGlobals at: #GrGemStonePlatform ifAbsent: [nil])
	description: 'Seaside'
	options: #()
) category: 'Debugging-Core'.
%

set compile_env: 2

category: '*seaside-gemstone-flow'
method: GrGemStonePlatform
callbackMarker

	| level aFrame visitTaskMethod |
	visitTaskMethod := WATaskVisitor compiledMethodAt: #visitTask:.
	level := 1.
	[ (aFrame := GsProcess _frameContentsAt: level) ~~ nil ]
		whileTrue: [
			(((aFrame at: 10) isKindOf: WACallback) or: [ (aFrame at: 1) == visitTaskMethod ])
				ifTrue: [ ^aFrame at: 1 ].
			level := level + 1 ].
	^nil
%

category: '*seaside-gemstone-flow'
method: GrGemStonePlatform
seasideSuspendFlowDo: aBlock
	| marker |
	(marker := self callbackMarker) == nil
		ifTrue: [ ^WAGsInvalidCallbackContext signal: 'You can only #call: and #answer: from within a callback or a Task.' ].
	^aBlock value: (WAPartialContinuation to: marker offset: 0 markerBlock: [ self callbackMarker ])
%

set compile_env: 0
