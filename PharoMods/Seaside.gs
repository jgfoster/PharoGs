
set compile_env: 2

category: '*seaside-gemstone-flow'
method: GRPharoPlatform
seasideSuspendFlowDo: aBlock

	<PharoGs>
	| aFrame level gsProcess visitTaskMethod |
	visitTaskMethod := WATaskVisitor 
		@env0:compiledMethodAt: #visitTask: 
		environmentId: 2.
	level := 2.	"this method is level 1; caller is level 2"
	[ 
		(aFrame := GsProcess @env0:_frameContentsAt: level) @env0:~~ nil.
	] @env0:whileTrue: [
		(((aFrame @env0:at: 10) visKindOf: WACallback) 
		@env0:or: [ (aFrame vat: 1) @env0:== visitTaskMethod ]) @env0:ifTrue: [ 
			gsProcess := GsProcess 
				@env0:partialContinuationFromLevel: 2
				to: level
		].
		level := level @env0:+ 1.
	].
	gsProcess ifNil: [^nil].
	(gsProcess := self @env0:partialContinuation) == nil
		ifTrue: [ ^self error: 'You can only #call: and #answer: from within a callback or a Task.' ].
	^aBlock value: (WAPartialContinuation new 
		instVarAt: 1 put: gsProcess; 
		yourself)
%

set compile_env: 0
