set compile_env: 2

category: 'private'
method: ExceptionSet
add: anExceptionOrExceptionSet

	<PharoGs>
    ^ExceptionSet
        @env0:with: anExceptionOrExceptionSet
        with: self
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
