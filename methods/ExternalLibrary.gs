set compile_env: 2

category: 'initialization'
method: ExternalLibrary
forceLoading 
	"Primitive. Force loading the given library. 
	The primitive will fail if the library is not available 
	or if anything is wrong with the receiver." 

	<PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
