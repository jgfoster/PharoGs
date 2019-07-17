set compile_env: 2

category: 'accessing'
method: MetaclassForTraits
name 

    <PharoGs>
	^ self thisClass isNil 
		ifTrue: [ 'a MetaclassForTraits' ] 
		ifFalse: [ self thisClass name asString, ' classTrait' ]
%

set compile_env: 0
