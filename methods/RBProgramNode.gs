category: 'accessing'
set compile_env: 2
method: RBProgramNode
comments: aCollection
	<PharoGs>

	(aCollection ifNil: [#()])
		ifEmpty: [ self removeProperty: #comments ifAbsent: [] ] 
		ifNotEmpty: [ self propertyAt: #comments put: aCollection ].
%
set compile_env: 0
