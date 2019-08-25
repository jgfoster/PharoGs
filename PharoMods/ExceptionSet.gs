set compile_env: 2

category: 'exceptionselector'
method: ExceptionSet
, anExceptionOrExceptionSet
	"Return an exception set that contains the receiver and the argument exception. This is commonly used to specify a set of exception selectors for an exception handler."
	
	<PharoGs>
	^self @env0:, anExceptionOrExceptionSet
%

category: 'private'
method: ExceptionSet
add: anExceptionOrExceptionSet

	<PharoGs>
    self @env0:, anExceptionOrExceptionSet.
    ^anExceptionOrExceptionSet
%

category: 'exceptionselector'
method: ExceptionSet
handles: anException 

	<PharoGs>
    ^self @env0:handles: anException
%

category: 'initialization'
method: ExceptionSet
initialize 

	<PharoGs>
    ^self @env0:initialize
%

set compile_env: 0
