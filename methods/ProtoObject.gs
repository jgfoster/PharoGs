category: '*Reflectivity'
set compile_env: 2
method: ProtoObject
metaLinkOptions
	<PharoGs>
	^{
	#isNil -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
category: 'class membership'
set compile_env: 2
method: ProtoObject
class 
	"Primitive. Answer the object which is the receiver's class. Essential. See  
	Object documentation whatIsAPrimitive." 

	<primitive: 610>
	<PharoGs> 
"	<primitive: 111> "
	self primitiveFailed
%
set compile_env: 0
