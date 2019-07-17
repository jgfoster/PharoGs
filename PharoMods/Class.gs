set compile_env: 2

category: 'accessing'
method: Class
basicCategory 

	<PharoGs> 
	^classCategory
%

category: 'accessing'
method: Class
basicCategory: aSymbol 

	<PharoGs> 
	classCategory := aSymbol.
%

category: 'accessing'
method: Class
classPool 

	<PharoGs> 
	^classVars
%

category: 'accessing'
method: Class
classPool: aDictionary

	<PharoGs> 
	classVars := aDictionary.
%

category: 'copying'
method: Class
copyForAnnouncement 

	<PharoGsError>
	self _gsError
%

category: 'organization'
method: Class
environment

	<PharoGs>
	^super environment
%

category: 'organization'
method: Class
environment: anEnvironment

	<PharoGsError>
	self _gsError
%

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

category: 'pool variables'
method: Class
sharedPools 
	"Answer an orderedCollection  of the shared pools declared in the receiver."

	<PharoGs> 
	^(poolDictionaries ifNil: [#()]) @env0:asOrderedCollection
%

category: 'pool variables'
method: Class
sharedPools: aCollection 
	
	<PharoGs> 
	poolDictionaries := aCollection
%

set compile_env: 0
