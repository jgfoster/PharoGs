set compile_env: 2

category: 'accessing'
method: SystemDictionary
associationAt: aKey

	<PharoGs>
	^self associationAt: aKey ifAbsent: [self error: 'not found']
%

category: 'accessing'
method: SystemDictionary
associationAt: aKey ifAbsent: aBlock

	<PharoGs>
	(Pharo @env0:includesKey: aKey) @env0:ifTrue: [^Pharo @env0:associationAt: aKey].
	^UserGlobals @env0:associationAt: aKey ifAbsent: aBlock
%

category: 'accessing'
method: SystemDictionary
at: aKey

	<PharoGs>
	^self at: aKey ifAbsent: [self error: 'not found']
%

category: 'accessing'
method: SystemDictionary
at: aKey ifAbsent: aBlock

	<PharoGs>
	(Pharo @env0:includesKey: aKey) @env0:ifTrue: [^Pharo @env0:at: aKey].
	^UserGlobals @env0:at: aKey ifAbsent: aBlock
%

category: 'accessing'
method: SystemDictionary
at: aKey ifPresent: aBlock

	<PharoGs>
	| object |
	object := System @env0:myUserProfile @env0:symbolList @env0:objectNamed: aKey.
	^object @env0:ifNotNil: aBlock
%

category: 'accessing'
method: SystemDictionary
at: aKey ifPresent: presentBlock ifAbsent: absentBlock

	<PharoGs>
	| object |
	object := System @env0:myUserProfile @env0:symbolList @env0:objectNamed: aKey.
	^object @env0:ifNil: absentBlock ifNotNil: presentBlock
%

category: 'accessing'
method: SystemDictionary
at: aKey put: anObject

	<PharoGs>
	(Pharo @env0:includesKey: aKey) @env0:ifTrue: [^Pharo @env0:at: aKey put: anObject].
	^UserGlobals @env0:at: aKey put: anObject
%

category: 'testing'
method: SystemDictionary
includesKey: aKey

	<PharoGs>
	^(System @env0:myUserProfile @env0:symbolList @env0:objectNamed: aKey) notNil
%

category: 'system attributes'
method: SystemDictionary
maxIdentityHash 
	"Answer the maximum identityHash value supported by the VM." 

	<PharoGsError> 
    self _gsError
%

set compile_env: 0
