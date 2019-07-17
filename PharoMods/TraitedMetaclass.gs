set compile_env: 2

category: 'testing'
method: TraitedMetaclass
isRejectedMethod: aSelector 
	<PharoGs> 

	"The method is not installed in method dictionary if: 
	 - It is in the localMethods. 
	 - It is from Class (we already have them) and they are not overriden in TraitedClass. 
	 - If it is in TraitedClass and it is in my superclass. 
	"
	| isFromClass isFromTraitedClass isMySuperclassTraitedClass | 
	(self isLocalSelector: aSelector) 
		ifTrue: [ ^ true ]. 
	isFromClass := Class canUnderstand: aSelector. 
	isFromTraitedClass := TraitedClass methodDict includesKey: aSelector. 
	isMySuperclassTraitedClass := (self superclass isKindOf: TraitedMetaclass) and: [self superclass isObsolete not]. 
	(isFromClass and: [ isFromTraitedClass not ]) 
		ifTrue: [ ^ true ]. 
	(isFromTraitedClass and: isMySuperclassTraitedClass) 
		ifTrue: [ ^ true ]. 
	^ false 
%

category: 'initialization'
method: TraitedMetaclass
rebuildMethodDictionary 
	<PharoGs> 

	| selectors removedSelectors modified | 
	 
		"During the creation of the class or after a change in the traitComposition, the whole method dictionary is calculated. 
	If I return true, my users should be updated. 
	Check the version in TraitedClass for more details." 
	 
	modified := false. 
	 
	self methodDict valuesDo: [ :m | m traitSource ifNil: [ localMethods at: m selector put: m ]]. 
	 
	selectors := self traitComposition selectors reject: [ :e | self isRejectedMethod: e ]. 
	selectors do: [ :e | modified := modified | (self traitComposition installSelector: e into: self replacing: false) ]. 
	 
	removedSelectors := self methodDict keys reject: [ :aSelector | (selectors includes: aSelector) or: [ self isSelectorToKeep: aSelector ] ]. 
	modified := modified | (removedSelectors isNotEmpty). 
	removedSelectors do: [ :aSelector | self methodDict removeKey: aSelector ]. 
	removedSelectors do: [ :aSelector | self organization removeElement: aSelector ].  
	 
	^ modified 
%

set compile_env: 0
