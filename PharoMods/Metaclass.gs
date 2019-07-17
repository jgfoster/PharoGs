set compile_env: 2

category: 'accessing'
method: Metaclass
environment 

	<PharoGs>
    ^destClass environment
%

category: '*CodeExport'
method: Metaclass
fileOutOn: aFileStream initializing: aBool 
	<PharoGs> 

	super fileOutOn: aFileStream. 
	(aBool and:[ self includesSelector: #initialize ]) ifTrue: [ 
		aFileStream cr. 
		aFileStream cr. 
		aFileStream nextChunkPut: destClass name , ' initialize'. 
		aFileStream cr ]
%

category: 'accessing'
method: Metaclass
name 
	<PharoGs> 

	"Answer a String that is the name of the receiver, either 'Metaclass' or  
	the name of the receiver's class followed by ' class'. '" 
	^ destClass isNil 
		ifTrue: [ 'a Metaclass' ] 
		ifFalse: [ destClass name asString, ' class' ]
%

category: 'instance creation'
method: Metaclass
new 
	<PharoGsError> 

	self _gsError
%

category: 'copying'
method: Metaclass
postCopy 
	<PharoGs> 
 
	"Do not share the reference to the sole instance."
	super postCopy. 
	self classLayout: (layout copy host: self). 
	destClass := nil.
%

category: 'accessing'
method: Metaclass
soleInstance 
	<PharoGs> 

	"The receiver has only one instance. Answer it." 
	^destClass
%

set compile_env: 0
