set compile_env: 2

category: 'instance creation'
classmethod: CompiledCode
newMethod: numberOfBytes header: headerWord 
	"Primitive. Answer an instance of me. The number of literals (and other  
	 information) is specified by the headerWord (see my class comment). 
	 The first argument specifies the number of fields for bytecodes in the 
	 method. Fail if either argument is not a SmallInteger, or if numberOfBytes 
	 is negative, or if memory is low. Once the header of a method is set by 
	 this primitive, it cannot be changed to change the number of literals. 
	 Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'literals'
method: CompiledCode
objectAt: index  
	"Primitive. Answer the method header (if index=1) or a literal (if index  
	>1) from the receiver. Essential. See Object documentation  
	whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'literals'
method: CompiledCode
objectAt: index put: value  
	"Primitive. Store the value argument into a literal in the receiver. An  
	index of 2 corresponds to the first literal. Fails if the index is less than 2  
	or greater than the number of literals. Answer the value as the result.  
	Normally only the compiler sends this message, because only the  
	compiler stores values in CompiledMethods. Essential. See Object  
	documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'cleaning'
method: CompiledCode
voidCogVMState 
	"Tell the VM to remove all references to any machine code form of the method. 
	 This primitive must be called whenever a method is in use and modified.  This is 
	 more aggressive (and *much* more costly) than flushCache since it must search 
	 through all context objects, making sure that none have a (hidden) machine code pc 
	 in the receiver.  Since modifying a method will likely change the generated machine code, 
	 modifying a method (rather than redefining it) requires this more aggressive flush." 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
