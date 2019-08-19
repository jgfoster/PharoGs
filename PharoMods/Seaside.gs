
set compile_env: 0

category: '*seaside-gemstone-flow'
method: GRPharoPlatform
partialContinuation

	<PharoGs>
	| level aFrame visitTaskMethod |
	visitTaskMethod := WATaskVisitor compiledMethodAt: #visitTask: environmentId: 2.
	level := 3.	"this method is level 1 and #'seasideSuspendFlowDo:' is level 2"
	[ 
		(aFrame := GsProcess _frameContentsAt: level) ~~ nil.
	] whileTrue: [
		(((aFrame at: 10) isKindOf: WACallback) 
		or: [ (aFrame at: 1) == visitTaskMethod ]) ifTrue: [ 
			^GsProcess 
				partialContinuationFromLevel: 3
				to: level
		].
		level := level + 1.
	].
	^nil
%

set compile_env: 2

category: '*seaside-gemstone-flow'
method: GRPharoPlatform
seasideSuspendFlowDo: aBlock

	<PharoGs>
	| gsProcess |
	(gsProcess := self @env0:partialContinuation) == nil
		ifTrue: [ ^self error: 'You can only #call: and #answer: from within a callback or a Task.' ].
	^aBlock value: (WAPartialContinuation new initializeGsProcess: gsProcess; yourself)
%

category: '*seaside-gemstone-flow'
method: WAPartialContinuation
initializeGsProcess: aGsProcess

	<PharoGs>
	values := aGsProcess
%

set compile_env: 0
