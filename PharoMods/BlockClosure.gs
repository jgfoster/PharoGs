set compile_env: 2

category: '*Debugging-Core'
method: BlockClosure
abstractBytecodeMessagesDo: aBlock 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: BlockClosure
argumentCount 
	"Answer the number of arguments that must be used to evaluate this block"
	
	<PharoGs>
	^self @env0:argumentCount
%

category: 'accessing'
method: BlockClosure
argumentNames 
	
	<PharoGsError>
	^self _gsError
%

category: 'scheduling'
method: BlockClosure
asContext 
	
	<PharoGsError>
	^self _gsError
%

category: 'private'
method: BlockClosure
asContextWithSender: aContext 
	
	<PharoGsError>
	^self _gsError
%

category: '*Jobs'
method: BlockClosure
asJob 
	
	<PharoGs>
	^ Job block: self
%

category: 'private'
method: BlockClosure
asMinimalRepresentation 
	
	<PharoGs>
	^self
%

category: 'exceptions'
method: BlockClosure
assert 
	
	<PharoGs>
	self value ifFalse: [AssertionFailure signal: 'Assertion failed'] 
%

category: 'exceptions'
method: BlockClosure
assertWithDescription: aStringOrABlock 
	
	<PharoGs>
	| value | 
	self value 
		ifTrue: [ ^ self ]. 
	value := aStringOrABlock value. 
	AssertionFailure signal: value
%

category: '*Text-Core'
method: BlockClosure
asText 
	
	<PharoGs>
	^ self asString asText
%

category: '*Kernel-Chronology-Extras'
method: BlockClosure
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
method: BlockClosure
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
method: BlockClosure
blockCreationBytecodeMessage 
	
	<PharoGsError>
	^self _gsError
%

category: 'private'
method: BlockClosure
copyForSaving 
	
	<PharoGsError>
	^self _gsError
%

category: 'evaluating'
method: BlockClosure
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
method: BlockClosure
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
method: BlockClosure
cull: firstArg cull: secondArg cull: thirdArg 
	
	<PharoGs>
	^self numArgs < 3  
		ifTrue: [self cull: firstArg cull: secondArg] 
		ifFalse: [self value: firstArg value: secondArg value: thirdArg] 
%

category: 'evaluating'
method: BlockClosure
cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg 
	
	<PharoGs>
	^self numArgs < 4  
		ifTrue: [self cull: firstArg cull: secondArg cull: thirdArg] 
		ifFalse: [self value: firstArg value: secondArg value: thirdArg value: fourthArg] 
%

category: 'controlling'
method: BlockClosure
doWhileFalse: conditionBlock 
	"Evaluate the receiver once, then again as long the value of conditionBlock is false."
	
	<PharoGs>
	| result | 
	[result := self value. 
	conditionBlock value] whileFalse. 
	^ result
%

category: 'controlling'
method: BlockClosure
doWhileTrue: conditionBlock 
	"Evaluate the receiver once, then again as long the value of conditionBlock is true."
	
	<PharoGs>
	| result | 
	[result := self value. 
	conditionBlock value] whileTrue. 
	^ result
%

category: 'evaluating'
method: BlockClosure
durationToRun 
	"Answer the duration taken to execute this block."
	
	<PharoGs>
	^ self timeToRun 
%

category: 'accessing'
method: BlockClosure
endPC 
	
	<PharoGsError>
	^self _gsError
%

category: '*metacello-core-scripting'
method: BlockClosure
execute: projectSpecBlock against: aScriptExecutor 
	
	<PharoGs>
    aScriptExecutor executeBlock: self do: projectSpecBlock
%

category: 'scheduling'
method: BlockClosure
fork 
	
	<PharoGs>
	^ self newProcess resume
%

category: 'scheduling'
method: BlockClosure
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
method: BlockClosure
forkAt: priority  
	"Create and schedule a Process running the code in the receiver 
	at the given priority. Answer the newly created process."

	<PharoGs>
	^ self newProcess 
		priority: priority; 
		resume
%

category: 'scheduling'
method: BlockClosure
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
method: BlockClosure
forkNamed: aString 
	"Create and schedule a Process running the code in the receiver and 
	having the given name."
	
	<PharoGs>
	^ self newProcess name: aString; resume
%

category: 'testing'
method: BlockClosure
hasMethodReturn 
	
	<PharoGsError>
	^self _gsError
%

category: '*Slot-Core'
method: BlockClosure
hasTemporaryVariableNamed: aName 
	
	<PharoGs>
	^(self tempNames includes: aName)
%

category: 'accessing'
method: BlockClosure
home 
	
	<PharoGsError>
	^self _gsError
%

category: 'evaluating'
method: BlockClosure
ifError: errorHandlerBlock 
	"Evaluate the block represented by the receiver, and normally return it's value.  
	If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead 
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
method: BlockClosure
isBlock 
	
	<PharoGs>
	^true
%

category: 'testing'
method: BlockClosure
isClean 
	
	<PharoGsError>
	^self _gsError
%

category: 'testing'
method: BlockClosure
isClosure 
	
	<PharoGs>
	^true
%

category: 'testing'
method: BlockClosure
isDead 
	
	<PharoGs>
	^false
%

category: 'private'
method: BlockClosure
isValid 
	
	<PharoGs>
	^true
%

category: 'accessing'
method: BlockClosure
method 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: BlockClosure
numArgs 
	"Answer the number of arguments that must be used to evaluate this block"
	
	<primitive: 458>
	<PharoGs>
	^0
%

category: 'error handling'
method: BlockClosure
numArgsError: numArgsForInvocation 
	
	<PharoGs>
	| printNArgs | 
	printNArgs := [:n| n printString, ' argument', (n = 1 ifTrue: [''] ifFalse:['s'])].  
	self error:  
			'This block accepts ', (printNArgs value: self numArgs),  
			', but was called with ', (printNArgs value: numArgsForInvocation), '.'
%

category: 'accessing'
method: BlockClosure
numLocalTemps 
	
	<PharoGsError>
	^self _gsError
%

category: 'accessing'
method: BlockClosure
numTemps 
	
	<PharoGs>
	^self @env0:numberTemps
%

category: 'exceptions'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%

category: 'accessing'
method: BlockClosure
foo
	
	<PharoGs>
	^self @env0:foo
%





category: 'accessing'
method: BlockClosure
copiedValueAt: i 

    <PharoGs>
	^self basicAt: i
%

category: 'exceptions'
method: BlockClosure
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

category: 'exceptions'
method: BlockClosure
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

category: 'scheduling'
method: BlockClosure
newProcess 
	"Answer a Process running the code in the receiver. The process is not  
	scheduled. 
	IMPORTANT! Debug stepping this deep infrastructure may lock your Image 
  	If you are not sure what you are doing, close the debugger now." 
	<PharoGs> 

	^Process 
		forContext:  
			[self value. 
			Processor terminateActive] asContext 
		priority: Processor activePriority
%

category: 'scheduling'
method: BlockClosure
newProcessWith: anArray  
	"Answer a Process running the code in the receiver. The receiver's block  
	arguments are bound to the contents of the argument, anArray. The  
	process is not scheduled." 
	<PharoGs> 

	^Process 
		forContext:  
			[self valueWithArguments: anArray. 
			Processor terminateActive] asContext 
		priority: Processor activePriority
%

category: 'accessing'
method: BlockClosure
numCopiedValues 
	"Answer the number of copied values of the receiver.  Since these are 
	 stored in the receiver's indexable fields this is the receiver's basic size. 
	 Primitive. Answer the number of indexable variables in the receiver.  
	 This value is the same as the largest legal subscript." 
	<PharoGsError> 

    self @env0:error: 'GemStone implemtation may differ'
%

category: 'exceptions'
method: BlockClosure
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
    are ignored.
"

    <primitive: 2030> "always fails"
    | fastPath | "fastPath := true by primitive if form 1 or 2 detected"
    "fastPath := nil  by primitive if form 3 detected."
    fastPath ifNotNil:[ "fast path code"
        ^ self value
    ].
    "Any changes to this method's code before this line may also
    require changes to code in comgen.c conditional on BcPrim_ENTER_onDo .
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

category: 'evaluating'
method: BlockClosure
value 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)." 

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
method: BlockClosure
value: anArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the argument and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)."	  

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
method: BlockClosure
value: firstArg value: secondArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)." 

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
method: BlockClosure
value: firstArg value: secondArg value: thirdArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)." 

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
method: BlockClosure
value: firstArg value: secondArg value: thirdArg value: fourthArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)." 

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
method: BlockClosure
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

category: '*Reflectivity'
method: BlockClosure
rfEnsure: aBlock 
	"same as #esure, carefully written to never have active meta-links as it is called in the code path that checks for recursion" 
    <PharoGsError>

    self _gsError.
%

category: '*Reflectivity'
method: BlockClosure
rfvalue 
	"same as value, for recursion stopping metalinks" 
    <PharoGsError>

    self _gsError.
%

category: 'evaluating'
method: BlockClosure
valueNoContextSwitch 
	"An exact copy of BlockClosure>>value except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self _gsError.
%

category: 'accessing'
method: BlockClosure
valueNoContextSwitch: anArg 
	"An exact copy of BlockClosure>>value: except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self _gsError.
%

category: 'controlling'
method: BlockClosure
_gsReservedSelector_repeat 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: BlockClosure
_gsReservedSelector_whileFalse 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: BlockClosure
_gsReservedSelector_whileFalse: aBlock

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: BlockClosure
_gsReservedSelector_whileTrue 

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

category: 'controlling'
method: BlockClosure
_gsReservedSelector_whileTrue: aBlock

    <PharoGs>
    self @env0:error: 'Reserved selector'.
%

set compile_env: 0
