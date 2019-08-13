set compile_env: 2

category: 'accessing'
classmethod: UUIDGenerator
default
	"Return the default UUID generator.
	Sharing an instance is more efficient and correct."

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'UUIDGenerator_Default' 
		ifAbsentPut: [self new].
%

set compile_env: 0
