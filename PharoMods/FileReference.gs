set compile_env: 2

category: 'instance creation'
classmethod: FileReference
primDropRequestFileName: dropIndex 
	"Primitive. Return the file name for some file that was just dropped onto the application. 
	Fail if dropIndex is out of range or the primitive is not supported." 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
