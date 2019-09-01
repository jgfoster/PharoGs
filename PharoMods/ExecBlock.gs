
set compile_env: 2

category: '*Debugging-Core'
method: ExecBlock
abstractBytecodeMessagesDo: aBlock 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ExecBlock
argumentCount 
	"Answer the number of arguments that must be used to evaluate this block"
	
	<PharoGs>
	^self @env0:argumentCount
%

category: 'accessing'
method: ExecBlock
argumentNames 
	
	<PharoGsError>
	^self _gsError
%

category: 'scheduling'
method: ExecBlock
asContext 
	
	<PharoGsError>
	^self _gsError
%

category: 'private'
method: ExecBlock
asContextWithSender: aContext 
	
	<PharoGsError>
	^self _gsError
%

category: '*Jobs'
method: ExecBlock
asJob 
	
	<PharoGs>
	^ Job block: self
%

category: 'private'
method: ExecBlock
asMinimalRepresentation 
	
	<PharoGs>
	^self
%

category: 'exceptions'
method: ExecBlock
assert 
	
	<PharoGs>
	self value ifFalse: [AssertionFailure signal: 'Assertion failed'] 
%

category: 'exceptions'
method: ExecBlock
assertWithDescription: aStringOrABlock 
	
	<PharoGs>
	| value | 
	self value 
		ifTrue: [ ^ self ]. 
	value := aStringOrABlock value. 
	AssertionFailure signal: value
%

category: '*Text-Core'
method: ExecBlock
asText 
	
	<PharoGs>
	^ self asString asText
%

category: '*Kernel-Chronology-Extras'
method: ExecBlock
bench 
	"Return how many times the receiver can get executed in 5 seconds.  
	Answer a string with meaningful description. 
	See #benchFor: which returns a BenchmarkResult" 
	 
	"[3.14 printString] bench" 
	 
	<PharoGs>
	| benchmarkResult | 
	benchmarkResult := self benchFor: 5 seconds. 
	^ benchmarkResult shortPrintString
%

category: '*Kernel-Chronology-Extras'
method: ExecBlock
benchFor: duration 
	"Run me for duration and return a BenchmarkResult" 
	 
	"[ 100 factorial ] benchFor: 2 seconds" 
	 	 
	<PharoGs>
	| count run started | 
	count := 0. 
	run := true. 
	[ duration wait. run := false ] forkAt: Processor timingPriority - 1. 
	started := Time millisecondClockValue. 
	[ run ] whileTrue: [ self value. count := count + 1 ]. 
	^ BenchmarkResult new  
		iterations: count;  
		elapsedTime: (Time millisecondsSince: started) milliSeconds;  
		yourself
%

category: 'accessing'
method: ExecBlock
blockCreationBytecodeMessage 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ExecBlock
copiedValueAt: i 

    <PharoGs>
	^self basicAt: i
%

category: 'private'
method: ExecBlock
copyForSaving 
	
	<PharoGsError>
	^self _gsError
%

category: 'evaluating'
method: ExecBlock
cull: anArg 
	"Execute the receiver with one or zero arguments depending on the receiver" 
	"([ 12 ] cull: 13) 
	>>> 12  
	" 
	"([:x | x + 12] cull: 3) 
	>>> 15 
	" 

	<PharoGs>
	^ self argumentCount == 0
		ifTrue: [ self value ]
		ifFalse: [ self value: anArg ]
%

category: 'evaluating'
method: ExecBlock
cull: firstArg cull: secondArg 
	"Execute the receiver with one or two arguments depending on the receiver" 
	"([:x | x + 1] cull: 13 cull: 12) 
	>>> 14 
	" 
	"([:x :y | x + y] cull: 3 cull: 2) 
	>>> 5 
	" 

	<PharoGs>
	^self numArgs < 2  
		ifTrue: [self cull: firstArg] 
		ifFalse: [self value: firstArg value: secondArg] 
%

category: 'evaluating'
method: ExecBlock
cull: firstArg cull: secondArg cull: thirdArg 
	
	<PharoGs>
	^self numArgs < 3  
		ifTrue: [self cull: firstArg cull: secondArg] 
		ifFalse: [self value: firstArg value: secondArg value: thirdArg] 
%

category: 'evaluating'
method: ExecBlock
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg 
	
	<PharoGs>
	^self numArgs < 4  
		ifTrue: [self cull: firstArg cull: secondArg cull: thirdArg] 
		ifFalse: [self value: firstArg value: secondArg value: thirdArg value: fourthArg] 
%

category: 'controlling'
method: ExecBlock
doWhileFalse: conditionBlock 
	"Evaluate the receiver once, then again as long the value of conditionBlock is false."
	
	<PharoGs>
	| result | 
	[result := self value. 
	conditionBlock value] whileFalse. 
	^ result
%

category: 'controlling'
method: ExecBlock
doWhileTrue: conditionBlock 
	"Evaluate the receiver once, then again as long the value of conditionBlock is true."
	
	<PharoGs>
	| result | 
	[result := self value. 
	conditionBlock value] whileTrue. 
	^ result
%

category: 'evaluating'
method: ExecBlock
durationToRun 
	"Answer the duration taken to execute this block."
	
	<PharoGs>
	^ self timeToRun 
%

category: 'accessing'
method: ExecBlock
endPC 
	
	<PharoGsError>
	^self _gsError
%

category: 'exceptions'
method: ExecBlock
ensure: aBlock 
	"Evaluate a termination block after evaluating the receiver, regardless of 
	 whether the receiver's evaluation completes.  N.B.  This method is *not* 
	 implemented as a primitive.  Primitive 198 always fails.  The VM uses prim 
	 198 in a context's method as the mark for an ensure:/ifCurtailed: activation."
 
    "GemStone: Evaluate the receiver.  
    Evaluate aBlock after evaluating the receiver,
    or before any return from a block that would return to the sender.
    Returns result of evaluating the receiver.

    aBlock must be a zero-arg instance of ExecBlock, otherwise an
    error is generated. "

    <primitive: 2017>  "marks frame with ENSURE_Mark_NIL, always fails"
    <PharoGs>
    | result |
    result := self value.  "execute the receiver"
    self @env0:_removeEnsure ifNotNil: [:b | 
        b value "normal execution of argument"
    ].
    ^ result
%

category: '*metacello-core-scripting'
method: ExecBlock
execute: projectSpecBlock against: aScriptExecutor 
	
	<PharoGs>
    aScriptExecutor executeBlock: self do: projectSpecBlock
%

category: 'scheduling'
method: ExecBlock
fork 
	
	<PharoGs>
	^ self newProcess resume
%

category: 'scheduling'
method: ExecBlock
forkAndWait 
	"Suspend current process and execute self in new process, 
	when it completes resume current process"
	
	<PharoGs>
	| semaphore | 
	semaphore := Semaphore new. 
	[self ensure: [semaphore signal]] fork. 
	semaphore wait. 
%

category: 'scheduling'
method: ExecBlock
forkAt: priority  
	"Create and schedule a Process running the code in the receiver 
	at the given priority. Answer the newly created process."

	<PharoGs>
	^ self newProcess 
		priority: priority; 
		resume
%

category: 'scheduling'
method: ExecBlock
forkAt: priority named: name 
	"Create and schedule a Process running the code in the receiver at the 
	given priority and having the given name. Answer the newly created  
	process."
	
	<PharoGs>
	| forkedProcess | 
	forkedProcess := self newProcess. 
	forkedProcess priority: priority. 
	forkedProcess name: name. 
	^ forkedProcess resume
%

category: 'scheduling'
method: ExecBlock
forkNamed: aString 
	"Create and schedule a Process running the code in the receiver and 
	having the given name."
	
	<PharoGs>
	^ self newProcess name: aString; resume
%

category: 'testing'
method: ExecBlock
hasMethodReturn 
	
	<PharoGsError>
	^self _gsError
%

category: '*Slot-Core'
method: ExecBlock
hasTemporaryVariableNamed: aName 
	
	<PharoGs>
	^(self tempNames includes: aName)
%

category: 'accessing'
method: ExecBlock
home 
	
	<PharoGsError>
	^self _gsError
%

category: 'exceptions'
method: ExecBlock
ifCurtailed: aBlock 
	"Evaluate the receiver with an abnormal termination action. 
	 Evaluate aBlock only if execution is unwound during execution 
	 of the receiver.  If execution of the receiver finishes normally do 
	 not evaluate aBlock.  N.B.  This method is *not* implemented as a 
	 primitive.  Primitive 198 always fails.  The VM uses prim 198 in a 
	 context's method as the mark for an ensure:/ifCurtailed: activation. '
	| complete result | 
	<primitive: 198> 
	result := self valueNoContextSwitch. 
	complete := true. 
	^result" 

    "GemStone: Evaluate the receiver and return its result. 
    If abnormal termination of the receiver occurs, terminationBlock is 
    evaluated. The value returned from the evaluation of terminationBlock 
    is discarded.

    Activation of an exception handler from within the receiver is not in and 
    of itself an abnormal termination. However, if the exception handler for 
    an exception that is not resumable results in termination of the receiver 
    or if its handler block contains a return statement that results in
    abnormal termination of the receiver, then terminationBlock will be evaluated 
    after evaluation of the exception handler.

    If an abnormal termination result in the termination of multiple blocks 
    which were evaluated using either #ensure: or #ifCurtailed: the respective 
    terminationBlocks will be executed in the reverse of the order in which 
    the corresponding receiver blocks were evaluated."
    <PharoGs>

    | wasCurtailed result |
    wasCurtailed := true.
    ^[
        result := self value.
        wasCurtailed := false.
        result
    ] ensure:[
        wasCurtailed ifTrue: aBlock
    ].
%

category: 'evaluating'
method: ExecBlock
ifError: errorHandlerBlock 
	"Evaluate the block represented by the receiver, and normally return its value.  
	If an error occurs, the errorHandlerBlock is evaluated, and its value is instead 
	returned.  The errorHandlerBlock must accept zero, one, or two parameters 
	(the error message and the receiver)." 
	"Examples: 
		[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?']. 
		[1 / 0] ifError: [:err :rcvr | 
			'ZeroDivide' = err 
				ifTrue: [Float infinity] 
				ifFalse: [self error: err]] "

	<PharoGs>
	^ self on: Error do: [:ex | 
		errorHandlerBlock cull: ex description cull: ex receiver]
%

category: 'testing'
method: ExecBlock
isBlock 
	
	<PharoGs>
	^true
%

category: 'testing'
method: ExecBlock
isClean 
	
	<PharoGsError>
	^self _gsError
%

category: 'testing'
method: ExecBlock
isClosure 
	
	<PharoGs>
	^true
%

category: 'testing'
method: ExecBlock
isDead 
	
	<PharoGs>
	^false
%

category: 'private'
method: ExecBlock
isValid 
	
	<PharoGs>
	^true
%

category: 'accessing'
method: ExecBlock
method 
	
	<PharoGsError>
	^self _gsError
%

category: 'scheduling'
method: ExecBlock
newProcess 
	"Answer a Process running the code in the receiver. The process is not  
	scheduled. 
	IMPORTANT! Debug stepping this deep infrastructure may lock your Image 
  	If you are not sure what you are doing, close the debugger now." 
	<PharoGs> 

	^self @env0:newProcess
%

category: 'scheduling'
method: ExecBlock
newProcessWith: anArray  
	"Answer a Process running the code in the receiver. The receiver's block  
	arguments are bound to the contents of the argument, anArray. The  
	process is not scheduled. '" 
	<PharoGs> 

	^self @env0:newProcessWith: anArray
%

category: 'accessing'
method: ExecBlock
numArgs 
	"Answer the number of arguments that must be used to evaluate this block"
	
	<primitive: 458>
	<PharoGs>
	^0
%

category: 'error handling'
method: ExecBlock
numArgsError: numArgsForInvocation 
	
	<PharoGs>
	| printNArgs | 
	printNArgs := [:n| n printString, ' argument', (n = 1 ifTrue: [''] ifFalse:['s'])].  
	self error:  
			'This block accepts ', (printNArgs value: self numArgs),  
			', but was called with ', (printNArgs value: numArgsForInvocation), '.'
%

category: 'accessing'
method: ExecBlock
numCopiedValues 
	"Answer the number of copied values of the receiver.  Since these are 
	 stored in the receiver's indexable fields this is the receiver's basic size. 
	 Primitive. Answer the number of indexable variables in the receiver.  
	 This value is the same as the largest legal subscript." 
	<PharoGsError> 

    self @env0:error: 'GemStone implemtation may differ'
%

category: 'accessing'
method: ExecBlock
numLocalTemps 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ExecBlock
numTemps 
	
	<PharoGs>
	^self @env0:numberTemps
%

category: 'exceptions'
method: ExecBlock
on: exceptionSelector do: handlerBlock
	"Evaluate the receiver in the scope of an exception handler. 
	The following primitive is just a marker used to find the error handling context.  
	See MethodContext>>#isHandlerOrSignalingContext.  
	<primitive: 199>   
	^ self value" 

    "GemStone: 
    Try to evaluate the receiver, which should be a zero-argument block.
    If an exception occurs and the expression
        exceptionSelector handles: theExceptionInstance
    returns true, then evaluate the one argument block handlerBlock , 
    passing it the exception instance as its argument.

    Two forms are supported directly by the VM, and the 'fast path code'
    below is used.
        (1) on: anException do: handlerBlock
        (2) on: anExceptionSet do: handlerBlock
    A third form is handled by Smalltalk code in the body of this method,
    and for this form only, #handles is sent to anObject to determine
    whether an exception should be handled .
        (3) on: anObject     do: handlerBlock

    anException must be the class Exception  or a subclass thereof ;
    anExceptionSet must be a kind of ExceptionSet;
    handlerBlock must be an instance of ExecBloc otherwise an error is generated.

    For forms 1,2,3 if handlerBlock expects more than 1 argument,
    an error is generated if exception handling attempts to
    invoke that handlerBlock.  

    If handlerBlock is invoked to handle an Exception which occurs during
    execution of the receiver and handlerBlock completes normally , then
    the result of handlerBlock (value of last expression in handlerBlock)
    will be the result of the on:do: send .  Other-than-normal
    completion of handlerBlock is available by use of 
    Exception's instance methods such as
        #return  #return:  #retry #retryUsing: #resume #resume: #pass #outer 
    within handlerBlock 

    For forms 1 and 2, when searching for a handlerBlock to handle a signaled Exception, 
    the VM uses Behavior>>_subclassOf: semantics . classHistories of the 
    class of the signaled Exception and of anException or elements of anExceptionSet
    are ignored. '
"

    <primitive: 2030> "always fails"
    | fastPath | "fastPath := true by primitive if form 1 or 2 detected"
    "fastPath := nil  by primitive if form 3 detected."
    fastPath ifNotNil:[ "fast path code"
        ^ self value
    ].
    "Any changes to this method's code before this line may also
    require changes to code in comgen.c conditional on BcPrim_ENTER_onDo . '
    "
    ^ self onException: AbstractException do:[:ex |
        (exceptionSelector handles: ex) ifTrue:[
            handlerBlock argumentCount == 0 
            ifTrue:[ handlerBlock value ]
            ifFalse:[ handlerBlock value: ex ]
        ] ifFalse:[
            ex pass
        ]
    ]
%

category: 'exceptions'
method: ExecBlock
on: exception fork: handlerAction 

	<PharoGs>
	^self on: exception do: [:ex | handlerAction forkWith: ex]
%

category: '*UnifiedFFI'
method: ExecBlock
on: exception fork: handlerAction return: answerAction
	<PharoGsError>
	self _gsError "
	""This is the same as #on:fork: but instead just fork and letting the flow continues, in 
	 case of an error it also evaluates answerAction and returns its result.""
		
	^ self on: exception do: [:ex |
		| onDoCtx handler bottom thisCtx |
		
		onDoCtx := thisContext.
		thisCtx := onDoCtx home.
		""find the context on stack for which this method's is sender""
		[ onDoCtx sender == thisCtx] whileFalse: [ 
			onDoCtx := onDoCtx sender.
			onDoCtx ifNil: [ 
				""Can't find our home context. seems like we're already forked
				and handling another exception in new thread. In this case, just pass it through handler.""
				^ handlerAction cull: ex ] ].
		bottom := [ Processor terminateActive ] asContext.
		onDoCtx privSender: bottom.
		handler := [ handlerAction cull: ex ] asContext.
		handler privSender: thisContext sender.
		(Process forContext: handler priority: Processor activePriority) resume.
		""cut the stack of current process""
		thisContext privSender: thisCtx.
		answerAction cull: exception ]
"
%

category: 'exceptions'
method: ExecBlock
onDNU: selector do: handleBlock 
	"Catch MessageNotUnderstood exceptions but only those of the given selector 
	(DNU stands for doesNotUnderstand:)"
	
	<PharoGs>
	^ self on: MessageNotUnderstood do: [:exception | 
		exception message selector = self selector 
			ifTrue: [handleBlock cull: self exception] 
			ifFalse: [exception pass] 
	  ]
%

category: 'exceptions'
method: ExecBlock
onException: anException do: handlerBlock

	<primitive: 2018> "marks frame with Exception_Mark_NIL, always fails"
	<PharoGs>
	^ self value

%

category: 'accessing'
method: ExecBlock
outerContext 
	
	<PharoGsError>
	^self _gsError
%

category: 'initialization'
method: ExecBlock
outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil 
	
	<PharoGsError>
	^self _gsError
%

category: 'printing'
method: ExecBlock
printOn: aStream 
	
	<PharoGs>
	aStream nextPutAll: ((self method hasSourceCode 
		or: [ "There is a decompiler" 
			Smalltalk globals includesKey: #FBDDecompiler ]) 
				ifTrue: [ self sourceNode formattedCode ] 
				ifFalse: [ 'aBlockClosure(no source code or decompiler available)' ])
%

category: 'accessing'
method: ExecBlock
receiver 
	
	<PharoGsError>
	^self _gsError
%

category: 'private'
method: ExecBlock
reentrant 
	
	<PharoGsError>
	^self _gsError
%

category: 'controlling'
method: ExecBlock
repeat
	<PharoGs>
	"Evaluate the receiver repeatedly, ending only if the block explicitly returns."
	[self value. true] whileTrue
%

category: 'controlling'
method: ExecBlock
repeatWithGCIf: testBlock 
	
	<PharoGs>
	| result |
	result := self value.
	(testBlock value: result) ifTrue: [
		result := self value.
	].
	^result
%

category: '*Reflectivity'
method: ExecBlock
rfEnsure: aBlock 
	"same as #esure, carefully written to never have active meta-links as it is called in the code path that checks for recursion" 
	
	<PharoGsError>
	^self _gsError
%

category: '*Reflectivity'
method: ExecBlock
rfvalue 
	"same as value, for recursion stopping metalinks" 
	
	<PharoGsError>
	^self _gsError
%

category: 'debugger access'
method: ExecBlock
sender 
	
	<PharoGsError>
	^self _gsError
%

category: '*metacello-core'
method: ExecBlock
setAuthorInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setAuthorWithBlock: self
%

category: '*metacello-core'
method: ExecBlock
setBaseline: aString withInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setBaseline: aString withBlock: self
%

category: '*metacello-core'
method: ExecBlock
setBlessingInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setBlessingWithBlock: self
%

category: '*metacello-core'
method: ExecBlock
setConfiguration: aString withInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setConfiguration: aString withBlock: self
%

category: '*metacello-core'
method: ExecBlock
setDescriptionInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setDescriptionWithBlock: self
%

category: '*metacello-core'
method: ExecBlock
setPackage: aString withInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setPackage: aString withBlock: self
%

category: '*metacello-core'
method: ExecBlock
setProject: aString withInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setProject: aString withBlock: self
%

category: '*metacello-core'
method: ExecBlock
setTimestampInMetacelloConfig: aMetacelloConfig 
	
	<PharoGs>
	aMetacelloConfig setTimestampWithBlock: self
%

category: 'evaluating'
method: ExecBlock
simulateValueWithArguments: anArray caller: aContext 
	
	<PharoGsError>
	^self _gsError
%

category: '*OpalCompiler-Core'
method: ExecBlock
sourceNode 
	
	<PharoGsError>
	^self _gsError
%

category: '*Collections-Abstract-splitjoin'
method: ExecBlock
split: aSequenceableCollection indicesDo: aBlock 
	"Perform an action specified as aBlock (with a start and end argument) 
	to each of the indices of aSequenceableCollection that have been 
	identified by taking the receiver as a splitter."  
	 
	"(String streamContents: [:s | [:c | c isSeparator ] 
		split:  'Pharo is cool'  
		indicesDo: [ :start :end | 
			s << 's:' << start asString << ' ' << 'e:' << end asString << ' ' ]]) 
	>>>  's:1 e:5 s:7 e:8 s:10 e:13 '"

	<PharoGs>
	| position | 
	 
	position := 1. 
	 
	aSequenceableCollection withIndexDo: [:element :idx | 
		(self value: element)  
			ifTrue: [ 
				aBlock value: position value: idx - 1. 
				position := idx + 1 ]]. 
		 
	aBlock value: position value: aSequenceableCollection size
%

category: 'accessing'
method: ExecBlock
startpc 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: ExecBlock
tempNames 
	
	<PharoGs>
	| names |
	names := self @env0:argsAndTemps.
	^names @env0:copyFrom: self @env0:argumentCount + 1 to: names size
%

category: '*Slot-Core'
method: ExecBlock
temporaryVariableNamed: aName 
	
	<PharoGs>
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ]. 
	^TemporaryVariable  
		name: aName  
		block: self  
%

category: '*Slot-Core'
method: ExecBlock
temporaryVariables 
	
	<PharoGs>
	^self tempNames collect: [ :name | TemporaryVariable new name: name ] 
%

category: '*Kernel-Chronology-Extras'
method: ExecBlock
timeToRun 
	"Answer the number of milliseconds taken to execute this block."
	
	<PharoGs>
	^ Duration milliSeconds: (Time millisecondsToRun: self)
%

category: 'evaluating'
method: ExecBlock
value 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)." 

    "GemStone: 
    Return the value of the receiver evaluated with no arguments.
    If the block expects any arguments, an error is generated.

    #value is optimized by the compiler.
    This method is in the image for use by perform: and for
    failure paths from the optimized bytecode "
	<PharoGs> 

    ^ self valueWithArguments: #()
%

category: 'evaluating'
method: ExecBlock
value: anArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the argument and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)."	  

    "GemStone: 
    Return the value of the receiver evaluated with anArg as its argument.  If
    the block expects a different number of arguments, an error is generated.

    #value: is optimized by the compiler.
    This method is in the image for use by perform: and for
    failure paths from the optimized bytecode "
	<PharoGs> 

    ^ self valueWithArguments: { anArg }
%

category: 'evaluating'
method: ExecBlock
value: firstArg value: secondArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)." 

    "GemStone: 
    Return the value of the receiver evaluated with the two objects as its
    arguments.  If the block expects a different number of arguments, an error is
    generated.

    #value:value: is optimized by the compiler.
    This method is in the image for use by perform: and for
    failure paths from the optimized bytecode "
	<PharoGs> 

    ^ self valueWithArguments: {  firstArg . secondArg }
%

category: 'evaluating'
method: ExecBlock
value: firstArg value: secondArg value: thirdArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)." 

    "GemStone: 
    Return the value of the receiver evaluated with the three objects as its
    arguments.  If the block expects a different number of arguments, an error is
    generated.

    #value:value:value: is optimized by the compiler.
    This method is in the image for use by perform: and for
    failure paths from the optimized bytecode "
	<PharoGs> 

    ^ self valueWithArguments: {  firstArg . secondArg . thirdArg }
%

category: 'evaluating'
method: ExecBlock
value: firstArg value: secondArg value: thirdArg value: fourthArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)." 

    "GemStone: 
    Return the value of the receiver evaluated with the four objects as
    its arguments.  If the block expects a different number of arguments,
    an error is generated.

    #value:value:value:value: is optimized by the compiler.
    This method is in the image for use by perform: and for
    failure paths from the optimized bytecode "
	<PharoGs> 

    ^ self valueWithArguments: {  firstArg . secondArg . thirdArg . fourthArg }
%

category: 'evaluating'
method: ExecBlock
valueAfterWaiting: aDelay 
	"Waits for a delay, then executes the block. Answers the process so you can terminate it"
	
	<PharoGs>
	^ [ aDelay wait. self value ]  
		forkAt: Processor userBackgroundPriority 
		named: (String streamContents: [ :s | 
				s  
					<< 'After '; 
					print: aDelay; 
					<<' do: '; 
					print: self ] )
%

category: 'evaluating'
method: ExecBlock
valueAt: blockPriority  
	"Evaluate the receiver (block), with another priority as the actual one  
	and restore it afterwards. The caller should be careful with using  
	higher priorities."
	
	<PharoGs>
	| activeProcess result outsidePriority | 
	activeProcess := Processor activeProcess. 
	outsidePriority := activeProcess priority. 
	activeProcess priority: blockPriority. 
	result := self ensure: [activeProcess priority: outsidePriority]. 
	"Yield after restoring lower priority to give the preempted processes a   
	chance to run." 
	blockPriority > outsidePriority 
		ifTrue: [Processor yield]. 
	^ result
%

category: 'evaluating'
method: ExecBlock
valueNoContextSwitch 
	"An exact copy of ExecBlock>>value except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self _gsError.
%

category: 'accessing'
method: ExecBlock
valueNoContextSwitch: anArg 
	"An exact copy of ExecBlock>>value: except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self _gsError.
%

category: '*Reflectivity'
method: ExecBlock
valueSelector 
	
	<PharoGs>
	self numArgs = 0 ifTrue: [^#value]. 
	^(String streamContents: [:stream |  
		stream nextPutAll: 'value:'. 
		(self numArgs - 1) timesRepeat: [stream nextPutAll: 'value:']]) asSymbol.
%

category: 'evaluating'
method: ExecBlock
valueSupplyingAnswer: anObject 
	
	<PharoGs>
	^ (anObject isCollection and: [anObject isString not]) 
		ifTrue: [self valueSupplyingAnswers: {anObject}] 
		ifFalse: [self valueSupplyingAnswers: {{'*'. anObject}}]
%

category: 'evaluating'
method: ExecBlock
valueSupplyingAnswers: aListOfPairs 
	"evaluate the block using a list of questions / answers that might be called upon to 
	automatically respond to Object>>confirm: or FillInTheBlank requests"
	
	<PharoGs>
	^ self  
		on: ProvideAnswerNotification 
		do:  
			[:notify | | answer caption | 
			 
			caption := notify messageText withSeparatorsCompacted. "to remove new lines" 
			answer := aListOfPairs 
				detect:  
					[:each | caption = each first or: 
						[(caption includesSubstring: each first caseSensitive: false) or: 
						[(each first match: caption) or: 
						[(String includesSelector: #matchesRegex:) and:  
						[ [ caption matchesRegex: each first ] on: Error do: [:ignored | false ]]]]]] 
					ifNone: [nil]. 
			answer 
				ifNotNil: [notify resume: answer second] 
				ifNil:  
					[ | outerAnswer | 
					outerAnswer := ProvideAnswerNotification signal: notify messageText. 
					outerAnswer  
						ifNil: [notify resume]  
						ifNotNil: [notify resume: outerAnswer]]]
%

category: '*metacello-mc'
method: ExecBlock
valueSupplyingMetacelloAnswers: aListOfPairs 
	"evaluate the block using a list of questions / answers that might be called upon to 
	automatically respond to Object>>confirm: or FillInTheBlank requests"
	
	<PharoGs>
	^ [self value]  
		on: ProvideAnswerNotification 
		do:  
			[:notify | | answer caption | 
			 
			caption := notify messageText withSeparatorsCompacted. "to remove new lines" 
			answer := aListOfPairs 
				detect:  
					[:each | caption = each first or: 
						[(caption includesSubstring: each first caseSensitive: false) or: 
						[(each first match: caption) or: 
						[(String includesSelector: #matchesRegex:) and:  
						[ [ caption matchesRegex: each first ] on: Error do: [:ignored | false ]]]]]] 
					ifNone: [nil]. 
			answer 
				ifNotNil: [notify resume: answer second] 
				ifNil:  
					[ | outerAnswer | 
					outerAnswer := ProvideAnswerNotification signal: notify messageText. 
					outerAnswer  
						ifNil: [notify resume]  
						ifNotNil: [notify resume: outerAnswer]]]
%

category: 'evaluating'
method: ExecBlock
valueSuppressingAllMessages 
	
	<PharoGs>
	^ self valueSuppressingMessages: #('*')
%

category: 'evaluating'
method: ExecBlock
valueSuppressingMessages: aListOfStrings 
	
	<PharoGs>
	^ self 
		valueSuppressingMessages: aListOfStrings 
		supplyingAnswers: #()
%

category: 'evaluating'
method: ExecBlock
valueSuppressingMessages: aListOfStrings supplyingAnswers: aListOfPairs 
	
	<PharoGs>
	^ self valueSupplyingAnswers: aListOfPairs, (aListOfStrings collect: [:each | {each. true}])
%

category: 'evaluating'
method: ExecBlock
valueUninterruptably 
	"Prevent remote returns from escaping the sender.  
	Even attempts to terminate (unwind) this process 
	will be halted and the process will resume here.  
	A terminate message is needed for every one of 
	these in the sender chain to get the entire 
	process unwound."

	<PharoGs>
	^ self ifCurtailed: [^ self]
%

category: 'private'
method: ExecBlock
valueUnpreemptively 
	"Evaluate the receiver (block), without the possibility of preemption 
	by higher priority processes. Use this facility VERY sparingly!" 
	"Think about using Block>>valueUninterruptably first, and think about 
	using Semaphore>>critical: before that, and think about redesigning your application even before that!  
	After you have done all that thinking, go right ahead and use it..."

	<PharoGs>
	| activeProcess oldPriority result semaphore | 
	activeProcess := Processor activeProcess. 
	oldPriority := activeProcess priority. 
	activeProcess priority: Processor highestPriority. 
	result := self ensure: [activeProcess priority: oldPriority]. 
	 
	"Yield after restoring priority to give the preempted processes a chance to run. 
	We inline the code of Processor yield here, but without the primitive. 
	The reason: the yield primitive does not take into account a change of priority 
	as done above"  
	semaphore := Semaphore new. 
	[semaphore signal] fork. 
	semaphore wait. 
	^result
%

category: 'evaluating'
method: ExecBlock
valueWithArguments: anArray 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments in an anArray and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you are going to want this for performance)." 

    "Return the value of the receiver evaluated with the elements of the Array
    anArray as arguments.  If the block expects a different number of arguments,
    an error is generated."

    <primitive: 2003>  "compiler emits special bytecode"
    <PharoGs>
    ^ self @env0:_primitiveFailed: #valueWithArguments: args: { anArray }
%

category: 'evaluating'
method: ExecBlock
valueWithEnoughArguments: anArray 
	"call me with enough arguments from anArray"
	
	<PharoGs>
	| args | 
	(anArray size == self numArgs) 
		ifTrue: [ ^self valueWithArguments: anArray ]. 
	args := Array new: self numArgs. 
	args replaceFrom: 1 
		to: (anArray size min: args size) 
		with: anArray 
		startingAt: 1. 
	^ self valueWithArguments: args
%

category: 'evaluating'
method: ExecBlock
valueWithExit  
	
	<PharoGs>
	self value: [ ^nil ]
%

category: 'evaluating'
method: ExecBlock
valueWithin: aDuration onTimeout: timeoutBlock 
	"Evaluate the receiver. 
	If the evaluation does not complete in less than aDuration 
	evaluate the timeoutBlock instead"
	
	<PharoGs>
	<debuggerCompleteToSender> 
	| theProcess delay watchdog tag | 
	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ]. 
	"the block will be executed in the current process" 
	theProcess := Processor activeProcess. 
	delay := aDuration asDelay. 
	tag := self. 
	"make a watchdog process" 
	watchdog := [ 
		delay wait. 	"wait for timeout or completion" 
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)]  
	] newProcess. 
	"Watchdog needs to run at high priority to do its job (but not at timing priority)" 
	watchdog priority: Processor timingPriority-1. 
	"catch the timeout signal" 
	^ [	watchdog resume.					"start up the watchdog" 
		self ensure:[						"evaluate the receiver" 
			theProcess := nil.				"it has completed, so ..." 
			delay delaySemaphore signal.	"arrange for the watchdog to exit" 
		]] on: TimedOut do: [ :e |  
			e tag == tag  
				ifTrue:[ timeoutBlock value ] 
				ifFalse:[ e pass]].
%

category: 'evaluating'
method: ExecBlock
valueWithInterval: aDelay 
	"Executes the block every x milliseconds specified in arguments. 
	Answers the process, so you can terminate it"
	
	<PharoGs>
	^ [ [ self value. aDelay wait. ] repeat ]  
		forkAt: Processor userBackgroundPriority  
		named: (String streamContents: [ :s | 
				s  
					<< 'every '; 
					print: aDelay; 
					<<' do: '; 
					print: self ] ) 
%

category: '*system-announcements'
method: ExecBlock
valueWithoutNotifications  
	
	<PharoGs>
	^SystemAnnouncer uniqueInstance suspendAllWhile: self 
%

category: 'evaluating'
method: ExecBlock
valueWithPossibleArgs: anArray  
	"Execute the receiver with the correct number of arguments taken from the argument." 
	"([:x | x + 1] valueWithPossibleArgs: #( 13 12 15)) 
	>>> 14 
	" 
	"([:x :y | x + y] valueWithPossibleArgs: #( 13 12 15))  
	>>> 25 
	" 
	"([:x :y :z | x + y + z] valueWithPossibleArgs: #( 13 12 15))  
	>>> 40 
	" 
	"([:x :y :z | x + y + z] valueWithPossibleArgs: #( 13 12 15))  
	>>> 40 
	" 

	<PharoGs>
	| numArgs |
	numArgs := self numArgs.
	^numArgs = 0 
		ifTrue: [self value] 
		ifFalse: 
			[self valueWithArguments: 
				(numArgs = anArray size 
					ifTrue: [anArray] 
					ifFalse: 
						[numArgs > anArray size 
							ifTrue: [anArray, (Array new: numArgs - anArray size)] 
							ifFalse: [anArray copyFrom: 1 to: numArgs]])]
%

category: 'evaluating'
method: ExecBlock
valueWithPossibleArgument: anArg  
	"Evaluate the block represented by the receiver.  
	 If the block requires one argument, use anArg, if it requires more than one, 
	 fill up the rest with nils."
	 
	<PharoGs>
	| a numArgs |
	numArgs := self numArgs.
	numArgs = 0 ifTrue: [^self value]. 
	numArgs = 1 ifTrue: [^self value: anArg]. 
	a := Array new: numArgs. 
	a at: 1 put: anArg. 
	^self valueWithArguments: a
%

category: 'controlling'
method: ExecBlock
whileFalse
"(Reserved selector.)  Evaluate the receiver once and then repeatedly as long 
 as the value returned by the evaluation is false.

 The following is a control structure optimization, not a recursive send."

	<PharoGs>
	^ [ self value] whileFalse
%

category: 'controlling'
method: ExecBlock
whileFalse: aBlock  
	"(Reserved selector.)  Evaluates the zero-argument block aBlock repeatedly
	while the receiver evaluates to false.  Return nil.  Generates an error if the
	receiver is not a zero-argument block."

	"The following is a control structure optimization, not a recursive send."

	<PharoGs>
	^ [self value] whileFalse: [aBlock value]
%

category: 'controlling'
method: ExecBlock
whileNil: aBlock  
	"Unlike #whileTrue/False: this is not compiled inline."
	
	<PharoGs>
	^ [self value isNil] whileTrue: [aBlock value] 
%

category: 'controlling'
method: ExecBlock
whileNotNil: aBlock  
	"Unlike #whileTrue/False: this is not compiled inline."
	
	<PharoGs>
	^ [self value notNil] whileTrue: [aBlock value] 
%

category: 'controlling'
method: ExecBlock
whileTrue
	"(Reserved selector.)  Evaluate the receiver once and then repeatedly as long 
	as the value returned by the evaluation is true.

	The following is a control structure optimization, not a recursive send."

	<PharoGs>
	^ [ self value] whileTrue
%

category: 'controlling'
method: ExecBlock
whileTrue: aBlock

	"(Reserved selector.)  Evaluates the zero-argument block aBlock repeatedly
	while the receiver evaluates to true.  Return nil.  Generates an error if the
	receiver is not a zero-argument block."

	"The following is a control structure optimization, not a recursive send."

	<PharoGs>
	^ [self value] whileTrue: [aBlock value]
%

category: 'controlling'
method: ExecBlock
_gsReservedSelector_repeat 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: ExecBlock
_gsReservedSelector_whileFalse 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: ExecBlock
_gsReservedSelector_whileFalse: aBlock

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: ExecBlock
_gsReservedSelector_whileTrue 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: ExecBlock
_gsReservedSelector_whileTrue: aBlock

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

set compile_env: 0
