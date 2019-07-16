set compile_env: 2

category: 'instance creation'
classmethod: Interval
new 
	"Primitive. Create and answer with a new instance of the receiver 
	(a class) with no indexable fields. Fail if the class is indexable. Override 
	SequenceableCollection new. Essential. See Object documentation 
	whatIsAPrimitive." 

    <primitive: 51>
	<PharoGs> 
	self isVariable ifTrue: [ ^ self new: 0 ]. 
%

set compile_env: 0
