set compile_env: 2

category: 'organization'
method: ClassDescription
basicOrganization 
	"GemStone does not have this instance variable and even if it did
	 we would not want to use it (security and commit conflicts)."

	<PharoGs> 
	| dict |
	dict := (Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'ClassDescription_organization' 
		ifAbsentPut: [(Globals @env0:at: #'KeyValueDictionary') @env0:new].
	^dict
		@env0:at: self @env0:class @env0:name
		ifAbsent: [nil]
%

category: 'organization'
method: ClassDescription
basicOrganization: aClassOrg 
	"GemStone does not have this instance variable and even if it did
	 we would not want to use it (security and commit conflicts)."

	<PharoGs> 
	| dict |
	dict := (Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'ClassDescription_organization' 
		ifAbsentPut: [(Globals @env0:at: #'KeyValueDictionary') @env0:new].
	dict
		@env0:at: self @env0:class @env0:name
		put: aClassOrg
%

category: 'filein/out'
method: ClassDescription
definitionWithoutSlots 
	<PharoGs> 

	| poolString stream | 
	poolString := self sharedPoolsString. 
	stream := (String new: 800) writeStream. 
	self superclass 
		ifNotNil: [ stream nextPutAll: self superclass name ] 
		ifNil: [ stream nextPutAll: 'ProtoObject' ]. 
	 
	stream 
		nextPutAll: self kindOfSubclass; 
		store: self name. 
	self hasTraitComposition ifTrue: [  
		stream 
			crtab; 
			nextPutAll: 'uses: '; 
			nextPutAll: self traitCompositionString ]. 
	stream 
		crtab; 
		nextPutAll: 'instanceVariableNames: '; 
		store: self instanceVariablesString. 
	stream 
		crtab; 
		nextPutAll: 'classVariableNames: '; 
		store: self classVariablesString. 
	 
	poolString = '' ifFalse: [  
		stream  
			crtab; 
			nextPutAll: 'poolDictionaries: '; 
			store: poolString ]. 
	 
	stream 
		crtab; 
		nextPutAll: 'package: '; 
		store: self category asString. 
		 
	self superclass ifNil: [  
		stream 
			nextPutAll: '.'; 
			cr. 
		stream  
			nextPutAll: self name. 
		stream 
			space; 
			nextPutAll: 'superclass: nil' ]. 
	^ stream contents 
%

category: 'filein/out'
method: ClassDescription
definitionWithSlots 
	<PharoGs> 

	"The class definition with a way to specify slots. Shown when the class defines special Slot"
	 
	| stream poolString| 
	poolString := self sharedPoolsString. 
	stream := (String new: 800) writeStream. 
	self superclass  
		ifNotNil: [stream nextPutAll: self superclass name] 
		ifNil: [stream nextPutAll: 'ProtoObject']. 
	stream  
		nextPutAll: ' subclass: ';  
		store: self name. 
	self hasTraitComposition ifTrue: [ 
		stream  
			crtab;  
			nextPutAll: 'uses: '; 
			nextPutAll: self traitCompositionString ]. 
			 
	(self classLayout isKindOf: FixedLayout) ifFalse: [ 
		stream  
			crtab;  
			nextPutAll: 'layout: '; 
			nextPutAll: self classLayout class name ]. 
	 
	stream  
		crtab;  
		nextPutAll: 'slots: '; 
		nextPutAll: self slotDefinitionString. 
		 
	stream  
		crtab;  
		nextPutAll: 'classVariables: '; 
		nextPutAll: self classVariableDefinitionString. 
	 
	poolString = '' ifFalse: [ 
		stream  
			crtab;  
			nextPutAll: 'poolDictionaries: '; 
			store: poolString ]. 
		 
	stream  
		crtab;  
		nextPutAll: 'package: '; 
		store: self category asString. 
	self superclass ifNil: [  
		stream nextPutAll: '.'; cr. 
		stream nextPutAll: self name. 
		stream space; nextPutAll: 'superclass: nil'. ]. 
	^ stream contents 
%

category: 'filein/out'
method: ClassDescription
oldDefinition 
	<PharoGs> 

	"Answer a String that defines the receiver."
	| aStream | 
	aStream := (String new: 800) writeStream. 
	self superclass  
		ifNil: [aStream nextPutAll: 'ProtoObject'] 
		ifNotNil: [aStream nextPutAll: self superclass name]. 
	aStream nextPutAll: self kindOfSubclass; 
			store: self name. 
	(self hasTraitComposition) ifTrue: [ 
		aStream cr; tab; nextPutAll: 'uses: '; 
			nextPutAll: self traitCompositionString]. 
	aStream cr; tab; nextPutAll: 'instanceVariableNames: '; 
			store: self instanceVariablesString. 
	aStream cr; tab; nextPutAll: 'classVariableNames: '; 
			store: self classVariablesString. 
	aStream cr; tab; nextPutAll: 'poolDictionaries: '; 
			store: self sharedPoolsString. 
	aStream cr; tab; nextPutAll: 'category: '; 
			store: self category asString. 
	self superclass ifNil: [  
		aStream nextPutAll: '.'; cr. 
		aStream nextPutAll: self name. 
		aStream space; nextPutAll: 'superclass: nil'. ]. 
	^ aStream contents
%

category: 'slots'
method: ClassDescription
slots
    "Not yet supported in GemStone"

	<PharoGs>
	^#()
%

category: '*Slot-Core'
method: ClassDescription
superclass: aSuperclass layout: aLayout 

	<PharoGsError>
    self _gsError
%

set compile_env: 0
