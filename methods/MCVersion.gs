category: 'enumerating'
set compile_env: 2
method: MCVersion
withAllDependenciesDo: aBlock
	<PharoGs>

	self allDependenciesDo: aBlock ifUnresolved: [:ignored |].
	aBlock value: self
%
set compile_env: 0
