set compile_env: 2

category: 'xx'
method: VariableNotDeclared
smartDescription 
	<PharoGs> 
	 
	self message ifNil: [^self description]. 
	^self classSymbol printString 
		, ' is missing, and does not understand ', self message selector printString
%

set compile_env: 0
