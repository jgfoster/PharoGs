set compile_env: 2

category: 'accessing'
method: MessageNotUnderstood
defaultAction 
	<PharoGs> 

	self @env0:dynamicInstVarAt: #reachedDefaultHandler put: true. 
	super defaultAction.
%

category: 'initialization'
method: MessageNotUnderstood
initialize 
	<PharoGs> 
 
	super initialize. 
	self @env0:dynamicInstVarAt: #reachedDefaultHandler put: false. 
%

category: 'accessing'
method: MessageNotUnderstood
message 
	<PharoGs> 

	^Message selector: selector arguments: gsArgs
%

category: 'accessing'
method: MessageNotUnderstood
message: aMessage 
	<PharoGs> 

   gsArgs := aMessage arguments.
   selector := aMessage selector.
%

category: 'accessing'
method: MessageNotUnderstood
messageText
	<PharoGs> 

	^self @env0:messageText
%

category: 'accessing'
method: MessageNotUnderstood
reachedDefaultHandler 
	<PharoGs> 

	^self @env0:dynamicInstVarAt: #reachedDefaultHandler
%

category: 'accessing'
method: MessageNotUnderstood
smartDescription 
	<PharoGs> 
	 
	self message ifNil: [^self description]. 
	receiver == UndefinedObject ifTrue: [  
		^ selector printString, ' was sent to nil']. 
	^'Instance of ', receiver class printString 
		, ' did not understand ', selector printString
%

set compile_env: 0
