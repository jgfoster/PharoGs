set compile_env: 2

category: 'accessing'
method: SystemDictionary
at: aKey ifPresent: aBlock

	<PharoGs>
	| object |
	object := System @env0:myUserProfile @env0:symbolList @env0:objectNamed: aKey.
	^object @env0:ifNotNil: aBlock
%

category: 'system attributes'
method: SystemDictionary
maxIdentityHash 
	"Answer the maximum identityHash value supported by the VM." 

	<PharoGsError> 
    self _gsError
%

set compile_env: 0
