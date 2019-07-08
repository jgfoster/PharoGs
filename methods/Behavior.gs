category: '*Reflectivity'
set compile_env: 2
method: Behavior
metaLinkOptions
	<PharoGs>
	^{
	#methodDict -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
category: 'instance creation'
set compile_env: 2
method: Behavior
basicNew 
	"Primitive. Answer an instance of the receiver (which is a class) with no  
	 indexable variables. Fail if the class is indexable. Essential. See Object  
	 documentation whatIsAPrimitive. 
	 
	 If the primitive fails because space is low then the scavenger will run 
	 before the method is activated.  Check that space was low and retry 
	 via handleFailingBasicNew if so." 

	<primitive: 50>
	<PharoGs> 
"	<primitive: 70 error: ec> 
	ec == #'insufficient object memory' ifTrue: 
		[^self handleFailingBasicNew]. 
	self isVariable ifTrue: [^self basicNew: 0]. 
	self primitiveFailed"
%
set compile_env: 0
