set compile_env: 2

category: 'accessing'
classmethod: SessionManager
default

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'SessionManager_Default' 
		ifAbsentPut: [self new installNewSession; yourself].
%

category: 'accessing'
classmethod: SessionManager
default: aSessionManager

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'SessionManager_Default' 
		put: aSessionManager
%

set compile_env: 0
