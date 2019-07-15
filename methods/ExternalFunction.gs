set compile_env: 2

category: 'error handling'
classmethod: ExternalFunction
getLastError 
	"Return the last error from an external call. 
	Only valid immediately after the external call failed." 

	<PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

category: 'invoking'
method: ExternalFunction
invokeWithArguments: argArray 
	"Manually invoke the receiver, representing an external function." 

	<PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

category: 'invoking'
method: ExternalFunction
tryInvokeWithArguments: argArray 
	"Sent from the debugger to simulate an FFI call." 

	<PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
