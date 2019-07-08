category: 'comparing'
set compile_env: 2
method: TemporaryVariable
hash
	<PharoGs>

	^ (name hash
		bitXor: method hash)
		bitXor: (startpc ifNil: [0])
%
set compile_env: 0
