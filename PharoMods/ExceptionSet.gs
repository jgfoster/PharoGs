set compile_env: 2

category: 'private'
method: ExceptionSet
add: anException

	<PharoGs>
    self @env0:add: anException
%

category: 'exceptionselector'
method: ExceptionSet
handles: anException 

	<PharoGs>
    self @env0:handles: anException
%

category: 'initialization'
method: ExceptionSet
initialize 

	<PharoGs>
    super @env0:initialization
%

set compile_env: 0
