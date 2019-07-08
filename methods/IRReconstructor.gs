category: 'initialize'
set compile_env: 2
method: IRReconstructor
createTempVectorNamed: name withVars: anArray
	<PharoGs>

	" Don't add the temp yet, we only know it's index at the end of the block or method "
	"self addVectorTemps: anArray"
	" Update the byte index to point before the pushing of the new vector "
	sourceMapByteIndex := sourceMapByteIndex - 2.
	self add: (IRInstruction createTempVectorNamed: name withVars: anArray)
%
set compile_env: 0
