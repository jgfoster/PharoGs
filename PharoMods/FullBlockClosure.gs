set compile_env: 2

category: 'evaluating'
method: FullBlockClosure
value 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the copied values to the activation as its copied 
	 temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
value: firstArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the argument and copied values to the activation 
	 as its argument and copied temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
value: firstArg value: secondArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the arguments and copied values to the activation 
	 as its arguments and copied temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
value: firstArg value: secondArg value: thirdArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the arguments and copied values to the activation 
	 as its arguments and copied temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
value: firstArg value: secondArg value: thirdArg value: fourthArg 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the arguments and copied values to the activation 
	 as its arguments and copied temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
valueNoContextSwitch 
	"An exact copy of BlockClosure>>value except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
valueNoContextSwitch: anArg 
	"An exact copy of BlockClosure>>value: except that this version will not preempt 
	 the current process on block activation if a higher-priority process is runnable. 
	 Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

category: 'evaluating'
method: FullBlockClosure
valueWithArguments: anArray 
	"Activate the receiver, creating a closure activation (MethodContext) 
	 whose closure is the receiver and whose caller is the sender of this 
	 message. Supply the arguments in an anArray and copied values to 
	 the activation as its arguments and copied temps. Primitive. Essential." 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'.
%

set compile_env: 0
