set compile_env: 2

category: 'accessing'
method: SystemDictionary
associationAt: aKey

	<PharoGs>
	^self associationAt: aKey ifAbsent: [NotFound signal]
%

category: 'accessing'
method: SystemDictionary
associationAt: aKey ifAbsent: aBlock

	<PharoGs>
	^Pharo @env0:associationAt: aKey ifAbsent: aBlock
%

category: 'accessing'
method: SystemDictionary
at: aKey

	<PharoGs>
	^self at: aKey ifAbsent: [NotFound signal]
%

category: 'accessing'
method: SystemDictionary
at: aKey ifAbsent: aBlock

	<PharoGs>
	^Pharo @env0:at: aKey ifAbsent: aBlock
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
	^Pharo @env0:at: aKey put: anObject
%

category: 'testing'
method: SystemDictionary
includesKey: aKey

	<PharoGs>
	^(System @env0:myUserProfile @env0:symbolList @env0:objectNamed: aKey) notNil
%

category: 'accessing'
method: SystemDictionary
keys

	<PharoGs>
	^(Globals @env0:at: #'IdentitySet') @env0:new
		@env0:addAll: Pharo @env0:keys @env0:asArray;
		@env0:asArray
%

category: 'system attributes'
method: SystemDictionary
maxIdentityHash 
	"Answer the maximum identityHash value supported by the VM." 

	<PharoGsError> 
    self _gsError
%

category: 'dictionary access'
method: SystemDictionary
removeKey: key ifAbsent: aBlock
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."
	
	self flushClassNameCache.
	(Pharo @env0:includesKey: key) @env0:ifFalse: [^aBlock value].
	^Pharo @env0:removeKey: key
%

set compile_env: 0
