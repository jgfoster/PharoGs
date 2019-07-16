set compile_env: 2

category: 'logical operations'
method: Boolean
& aBoolean
	"Evaluating conjunction. Evaluate the argument. Then answer true if 
	both the receiver and the argument are true."

	<PharoGsDone> 
	^self @env0:& aBoolean
%

category: 'controlling'
method: Boolean
and: alternativeBlock
	"Nonevaluating conjunction. If the receiver is true, answer the value of 
	the argument, alternativeBlock; otherwise answer false without 
	evaluating the argument."

	<PharoGsDone> 
	^self @env0:and: [alternativeBlock @env2: value]
%

category: 'converting'
method: Boolean
asBit
	"convert myself to an Integer representing 1 for true and 0 for false"

	<PharoGsDone> 
	^self ifTrue: [1] ifFalse: [0]
%

category: 'controlling'
method: Boolean
ifFalse: alternativeBlock
	"If the receiver is true (i.e., the condition is true), then the value is the 
	true alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."

	<PharoGsDone> 
	^self @env0:ifFalse: [alternativeBlock value]
%

category: 'controlling'
method: Boolean
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock

	<PharoGsDone> 
	^self ifTrue: [trueAlternativeBlock value] ifFalse: [falseAlternativeBlock value]
%

category: 'controlling'
method: Boolean
ifTrue: alternativeBlock
	"If the receiver is false (i.e., the condition is false), then the value is the 
	false alternative, which is nil. Otherwise answer the result of evaluating 
	the argument, alternativeBlock. Create an error notification if the 
	receiver is nonBoolean. Execution does not actually reach here because 
	the expression is compiled in-line."

	<PharoGsDone> 
	^self @env0:ifTrue: [alternativeBlock @env2:value]
%

category: 'controlling'
method: Boolean
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock

	<PharoGsDone> 
	^self ifTrue: [trueAlternativeBlock value] ifFalse: [falseAlternativeBlock value]
%

category: 'logical operations'
method: Boolean
not
	"Negation. Answer true if the receiver is false, answer false if the 
	receiver is true."

	<PharoGsDone> 
	^self @env0:not
%

category: 'controlling'
method: Boolean
or: alternativeBlock
	"Nonevaluating disjunction. If the receiver is false, answer the value of 
	the argument, alternativeBlock; otherwise answer true without 
	evaluating the argument."

	<PharoGsDone> 
	^self @env0:or: [alternativeBlock value]
%

category: 'controlling'
method: Boolean
xor: alternativeBlock

	<PharoGsDone> 
	^self ifTrue: [alternativeBlock not] ifFalse: [alternativeBlock]
%

category: 'logical operations'
method: Boolean
| aBoolean
	"Evaluating disjunction (OR). Evaluate the argument. Then answer true 
	if either the receiver or the argument is true."

	<PharoGsDone> 
	^self @env0:or: [aBoolean]
%

set compile_env: 0
