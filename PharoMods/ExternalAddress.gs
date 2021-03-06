set compile_env: 2

category: 'instance creation'
classmethod: ExternalAddress
allocate: byteSize 
	"Primitive. Allocate an object on the external heap." 

	<PharoGsError>
    self _gsError
%

category: 'instance creation'
classmethod: ExternalAddress
loadSymbol: moduleSymbol module: module  

	<PharoGsError>
    self _gsError
%

category: 'copying'
method: ExternalAddress
clone 
	<PharoGsError>
    self _gsError
%

category: 'initialization'
method: ExternalAddress
free 
	"Primitive. Free the object pointed to on the external heap. 
	Dangerous - may break your system if the receiver hasn't been 
	allocated by ExternalAddress class>>allocate:. No checks are done." 

	<PharoGsError>
    self _gsError
%

set compile_env: 0
