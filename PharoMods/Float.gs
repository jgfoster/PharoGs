set compile_env: 2

category: 'converting'
method: Float
asTrueFraction
	" Answer a fraction that EXACTLY represents self, 
	  a double precision IEEE floating point number. 
	  Floats are stored in the same form on all platforms. 
	  (Does handle gradual underflow but not NANs.) 
	  By David N. Smith with significant performance 
	  improvements by Luciano Esteban Notarfrancesco. 
	  (Version of 11April97)" 

	<PharoGs>
	^self @env0:asFraction
%

category: 'accessing'
method: Float
basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an  
	indexable element in the receiver. Fail if the argument index is not an  
	Integer or is out of bounds. Essential. Do not override in a subclass. See  
	Object documentation whatIsAPrimitive. 
	This version of basicAt: is specifically for floats, answering the most significant 
	word for index 1 and the least significant word for index 2.  This alows the VM 
	to store floats in whatever order it chooses while it appears to the image that 
	they are always in big-endian/PowerPC order." 

	<PharoGsError>
	self @env0:error: 'Does GemStone store a Float in big-endian format?'
%

category: 'accessing'
method: Float
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Store the second argument  
	value in the indexable element of the receiver indicated by index. Fail  
	if the index is not an Integer or is out of bounds. Or fail if the value is  
	not of the right type for this kind of collection. Answer the value that  
	was stored. Essential. Do not override in a subclass. See Object  
	documentation whatIsAPrimitive. 
	This version of basicAt: is specifically for floats, answering the most significant 
	word for index 1 and the least significant word for index 2.  This alows the VM 
	to store floats in whatever order it chooses while it appears to the image that 
	they are always in big-endian/PowerPC order." 

	<PharoGsError>
	self @env0:error: 'Does GemStone store a Float in big-endian format?'
%

category: 'testing'
method: Float
negative

	<PharoGs>
	^self < 0.0
%

category: 'testing'
method: Float
positive

	<PharoGs>
	^self >= 0.0
%

set compile_env: 0
