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
	| dict |
	dict := Dictionary new.
	classVars @env0:keysAndValuesDo: [:eachKey :eachValue |
		dict add: (ClassVariable key: eachKey value: eachValue).
	].
	^dict
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
	^Smalltalk globals
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

category: 'initialization'
method: Class
obsolete
	"In GemStone, we do nothing for now"
	
	<PharoGs>
%

category: 'pool variables'
method: Class
sharedPools 
	"Answer an orderedCollection of the shared pools declared in the receiver."

	<PharoGs> 
	^OrderedCollection withAll: (poolDictionaries ifNil: [#()])
%

category: 'pool variables'
method: Class
sharedPools: aCollection 
	
	<PharoGs> 
	poolDictionaries := aCollection
%

category: 'subclass creation - deprecated'
method: Class
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat

	<PharoGs>
	s @env0:notEmpty @env0:ifTrue: [self _gsError].
	^(self
		@env0:subclass: t
		instVarNames: ((f @env0:subStrings: Character @env0:space) @env0:reject: [:each | each @env0:isEmpty])
		classVars: ((d @env0:subStrings: Character @env0:space) @env0:reject: [:each | each @env0:isEmpty])
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: Pharo)
		category: cat;
		yourself
%

category: 'accessing'
method: Class
subclasses 

	<PharoGs>
	^self @env0:subclasses @env0:asArray
%

set compile_env: 0
