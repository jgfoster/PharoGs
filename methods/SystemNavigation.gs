set compile_env: 2

category: 'query'
method: SystemNavigation
allObjects 
	"Answer an Array of all objects in the system.  
    Fail if there isn't enough memory to instantiate the result." 

	<PharoGsError> 
    self @env0:error: 'Not supported in GemStone'
%

category: 'query'
method: SystemNavigation
allObjectsOrNil 
	"Answer an Array of all objects in the system.  
    Fail if there isn't enough memory to instantiate the result and answer nil." 

	<PharoGsError> 
    self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
