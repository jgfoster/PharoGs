set compile_env: 2

category: 'bit manipulation'
method: Integer
bitAnd: arg  
	"Primitive. Answer an Integer whose bits are the logical AND of the 
	receiver's bits and those of the argument, arg. 
	Numbers are interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 740>
	<PharoGsDone> 
	^self @env0:bitAnd: arg
%

category: 'bit manipulation'
method: Integer
bitOr: n  
	"Answer an Integer whose bits are the logical OR of the receiver's bits   
	and those of the argument, n."

    <primitive: 741>
	| norm | 
	<PharoGsDone> 
	norm := n normalize. 
	^ self 
		digitLogic: norm 
		op: #bitOr: 
		length: (self digitLength max: norm digitLength)
%

category: 'bit manipulation'
method: Integer
bitXor: n  
	"Answer an Integer whose bits are the logical XOR of the receiver's bits   
	and those of the argument, n." 

    <primitive: 743>
	| norm | 
	<PharoGsDone> 
	norm := n normalize. 
	^ self 
		digitLogic: norm 
		op: #bitXor: 
		length: (self digitLength max: norm digitLength)
%

set compile_env: 0
