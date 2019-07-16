set compile_env: 2

category: 'instance creation'
method: Class
new
	"Answer a new initialized instance of the receiver (which is a class) with no indexable variables. Fail if the class is indexable."

	<PharoGs> 
	^ self basicNew initialize
%

category: 'instance creation'
method: Class
new: sizeRequested
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	<PharoGs> 
	^ (self basicNew: sizeRequested) initialize
%

set compile_env: 0
