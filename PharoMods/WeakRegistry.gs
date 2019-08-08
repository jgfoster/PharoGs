set compile_env: 2

category: 'accessing'
classmethod: WeakRegistry
default

    <PharoGs>
    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'WeakRegistry_Default' 
		ifAbsentPut: [self new].
%

set compile_env: 0
