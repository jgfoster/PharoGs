set compile_env: 2

category: 'class initialization'
classmethod: ProcessSpecificVariable
resetSoleInstance

    <PharoGs>
	(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:removeKey: (self @env0:name @env0:, '_soleInstance') @env0:asSymbol
%

category: 'class initialization'
classmethod: ProcessSpecificVariable
soleInstance

    <PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: (self @env0:name @env0:, '_soleInstance') @env0:asSymbol
        ifAbsentPut: [self new]
%

set compile_env: 0
