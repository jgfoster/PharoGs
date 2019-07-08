category: '*OpalCompiler-Core'
set compile_env: 2
method: Object
mustBeBooleanInMagic: context
	<PharoGs>
	"Permits to redefine methods inlined by compiler.
	Take the ast node corresponding to the mustBeBoolean error, compile it on the with Opal fly and executes it as a DoIt. Then resume the execution of the context."
	| sendNode methodNode method |
	"get the message send node that triggered mustBeBoolean"
	sendNode := context sourceNode sourceNodeForPC: context pc - 1.
	"Build node to perform send unoptimized"
	methodNode := RBMethodNode 
		selector: #ExecuteUnOptimizedIn: 
		arguments: { RBVariableNode named:'ThisContext' } 
		body: sendNode copy asSequenceNode.
	"Rewrite temps to access the values in context"
	methodNode rewriteTempsForContext: context.
	"Rewrite non-local returns to return to the correct context from send"
	RBParseTreeRewriter new 
		replace: '^ ``@value' with: 'ThisContext home return: ``@value';
		executeTree: methodNode.
	"Add a return to the method body"
	methodNode body transformLastToReturn.	
	"Keep same compilation context as the sender node's"
	methodNode compilationContext: sendNode methodNode compilationContext copy.
	"Disable inlining so the message send will be unoptimized"
	methodNode compilationContext compilerOptions: #(#- optionInlineIf optionInlineAndOr optionInlineWhile).
	"Generate the method"	
	method := methodNode generate.
	"resume the context at the instruction following the send when returning from deoptimized code"
	context pc: sendNode irInstruction nextBytecodeOffsetAfterJump.
	"Execute the generated method"
	^ context receiver withArgs: {context} executeMethod: method
%
set compile_env: 0
category: 'accessing'
set compile_env: 2
method: Object
at: index  
	"Primitive. Assumes receiver is indexable. Answer the value of an  
	indexable element in the receiver. Fail if the argument index is not an  
	Integer or is out of bounds. Essential. See Object documentation  
	whatIsAPrimitive. Read the class comment for a discussion about that the fact 
	that the index can be a float." 

	<primitive: 32>
	<PharoGs> 
"	<primitive: 60> "
	index isInteger ifTrue: 
		[self class isVariable 
			ifTrue: [self errorSubscriptBounds: index] 
			ifFalse: [self errorNotIndexable]]. 
	index isNumber 
		ifTrue: [^self at: index asInteger] 
		ifFalse: [self errorNonIntegerIndex]
%
set compile_env: 0
category: 'private'
set compile_env: 2
method: Object
species 
	"Answer the preferred class for reconstructing the receiver.  For example,  
	collections create new collections whenever enumeration messages such as  
	collect: or select: are invoked.  The new kind of collection is determined by  
	the species of the original collection.  Species and class are not always the  
	same.  For example, the species of Interval is Array." 

	<PharoGs> 
"	<primitive: 111> "
	^self class
%
set compile_env: 0
