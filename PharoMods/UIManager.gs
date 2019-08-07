set compile_env: 2

category: 'initialization'
classmethod: UIManager
default

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'UIManager_Default' 
		ifAbsentPut: [NonInteractiveUIManager new].
%

set compile_env: 0
