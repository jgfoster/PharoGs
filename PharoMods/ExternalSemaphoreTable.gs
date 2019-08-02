set compile_env: 2

category: 'accessing'
classmethod: ExternalSemaphoreTable
clearExternalObjects
	"Clear the array of objects that have been registered for use in non-Smalltalk code."
	"Only lock additions, removals executing in parallel would have little effect on the resulting array"

	<PharoGs>
	"Ignored"
%

category: 'accessing'
classmethod: ExternalSemaphoreTable
externalObjects
	"Not really sure why this is protected, once called you are out of protection of the locks anyways, and any use of the object is dangerous...
	Only additions can potentially change the actual array in use though, so only lock that."

	<PharoGs>
	^#()
%

category: 'initialize'
classmethod: ExternalSemaphoreTable
initialize

	<PharoGs>
	"Ignored"
%

category: 'accessing'
classmethod: ExternalSemaphoreTable
registerExternalObject: anObject

	<PharoGs>
	"Ignored"
%

category: 'accessing'
classmethod: ExternalSemaphoreTable
unregisterExternalObject: anObject

	<PharoGs>
	"Ignored"
%

set compile_env: 0
