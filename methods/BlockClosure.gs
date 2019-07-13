set compile_env: 2

category: 'accessing'
method: BlockClosure
copiedValueAt: i 

    <PharoGsDone>
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
    <PharoGsDone>
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
	 context's method as the mark for an ensure:/ifCurtailed: activation.
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
    <PharoGsDone>

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
	<PharoGsDone> 

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
	<PharoGsDone> 

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
	<PharoGsDone> 

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
	<PharoGsDone> 

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
	<PharoGsDone> 

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
	<PharoGsDone> 

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
	<PharoGsDone> 

    ^ self valueWithArguments: {  firstArg . secondArg . thirdArg . fourthArg }
%

category: 'evaluating'
method: BlockClosure
valueWithArguments: anArray 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this message. 
	 Supply the arguments in an anArray and copied values to the activation as its arguments and copied temps. 
	 Primitive. Optional (but you're going to want this for performance)." 

    "Return the value of the receiver evaluated with the elements of the Array
    anArray as arguments.  If the block expects a different number of arguments,
    an error is generated."

    <primitive: 2003>  "compiler emits special bytecode"
    <PharoGsDone>
    ^ self @env0:_primitiveFailed: #valueWithArguments: args: { anArray }
%

category: '*Reflectivity'
method: BlockClosure
rfEnsure: aBlock 
	"same as #esure, carefully written to never have active meta-links as it is called in the code path that checks for recursion" 
    <PharoGsError>

    self @env0:error: 'Not supported in GemStone'.
%

category: '*Reflectivity'
method: BlockClosure
rfvalue 
	"same as value, for recursion stopping metalinks" 
    <PharoGsError>

    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: BlockClosure
valueNoContextSwitch 
	"An exact copy of BlockClosure>>value except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self @env0:error: 'Not supported in GemStone'.
%

category: 'accessing'
method: BlockClosure
valueNoContextSwitch: anArg 
	"An exact copy of BlockClosure>>value: except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 
    <PharoGsError>

    self @env0:error: 'Not supported in GemStone'.
%

set compile_env: 0
