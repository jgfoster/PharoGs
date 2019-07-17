set compile_env: 2

category: 'handling'
method: Exception
freezeUpTo:  aContext 

	<PharoGsError>
    self _gsError
%

category: 'testing'
method: Exception
isNested 
    "Determine whether the current exception handler is within the scope of another handler for the same exception."

	<PharoGs>
    ^self @env0:isNested
%

category: 'handling'
method: Exception
outer

	<PharoGs>
    self @env0:outer
%

category: 'handling'
method: Exception
pass
    "Yield control to the enclosing exception action for the receiver."

	<PharoGs>
    self @env0:pass
%

category: 'private'
method: Exception
privHandlerContext

	<PharoGsError>
    self _gsError
%

category: 'private'
method: Exception
privHandlerContext: aContextTag 

	<PharoGsError>
    self _gsError
%

category: 'handling'
method: Exception
resignalAs: replacementException

	<PharoGs>
    self @env0:resignalAs: replacementException
%

category: 'handling'
method: Exception
resumeUnchecked: resumptionValue 

	<PharoGsError>
    self _gsError
%

category: '*Debugging-Core'
method: Exception
retry
    "Abort an exception handler and re-evaluate its protected block."

	<PharoGs>
    self @env0:retry
%

category: 'handling'
method: Exception
retryUsing: alternativeBlock
    "Abort an exception handler and evaluate a new block in place of the handler's protected block '"

	<PharoGs>
    self @env0:retryUsing: alternativeBlock
%

category: 'handling'
method: Exception
return: returnValue 
	"Return the argument as the value of the block protected by the active exception handler."

	<PharoGs>
    self @env0:return: returnValue
%

category: 'handling'
method: Exception
searchFrom: aContext

	<PharoGsError>
    self _gsError
%

category: 'signaling'
method: Exception
signal
    "Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	<PharoGs>
    self @env0:signal
%

category: 'accessing'
method: Exception
signaler

	<PharoGsError>
    self _gsError
%

category: 'accessing'
method: Exception
signaler: anObject 

	<PharoGsError>
    self _gsError
%

category: 'accessing'
method: Exception
signalerContext
    "Find the first sender of signal(:), the first context which is neither for an instance method nor for a class side method of Exception (or subclass). 
	This will make sure that the same context is found for both, `Error signal` and `Error new signal`"

	<PharoGsError>
    self _gsError
%

category: 'signaling'
method: Exception
signalIn: context
    "Ask ContextHandlers in the sender chain starting at the given context to handle this signal.  
	The default is to execute and return my defaultAction."

	<PharoGsError>
    self _gsError
%

set compile_env: 0
