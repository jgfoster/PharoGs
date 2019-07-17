set compile_env: 2

category: 'error handling'
classmethod: ExternalFunction
getLastError 
	"Return the last error from an external call. 
	Only valid immediately after the external call failed." 

	<PharoGsError>
    self _gsError
%

category: 'invoking'
method: ExternalFunction
invokeWithArguments: argArray 
	"Manually invoke the receiver, representing an external function." 

	<PharoGsError>
    self _gsError
%

category: 'invoking'
method: ExternalFunction
tryInvokeWithArguments: argArray 
	"Sent from the debugger to simulate an FFI call." 

	<PharoGsError>
    self _gsError
%

set compile_env: 0
