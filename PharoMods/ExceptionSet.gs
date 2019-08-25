set compile_env: 2

category: 'private'
method: ExceptionSet
add: anExceptionOrExceptionSet

	<PharoGs>
    (anExceptionOrExceptionSet @env0: isKindOf: ExceptionSet) 
        ifTrue: [anExceptionOrExceptionSet @env0:do: [:each | self @env0:add: each]]
        ifFalse: [self @env0:add: anExceptionOrExceptionSet].
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
