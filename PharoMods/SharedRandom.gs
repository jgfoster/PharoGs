set compile_env: 2

category: 'instance creation'
classmethod: SharedRandom
globalGenerator

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'SharedRandom_global' 
		ifAbsentPut: [self new].
%

set compile_env: 0
