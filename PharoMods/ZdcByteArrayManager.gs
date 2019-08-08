set compile_env: 2

category: 'accessing'
classmethod: ZdcByteArrayManager
current

    <PharoGs>
    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'ZdcByteArrayManager_Current' 
		ifAbsentPut: [self new].
%

set compile_env: 0
