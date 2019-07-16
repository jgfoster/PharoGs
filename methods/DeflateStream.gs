set compile_env: 2

category: 'private'
method: DeflateStream
updateHashTable: table delta: delta 
    <PharoGs>

	| pos | 
	1 to: table size do:[:i| 
		"Discard entries that are out of range" 
		(pos := table at: i) >= delta 
			ifTrue:[table at: i put: pos - delta] 
			ifFalse:[table at: i put: 0]].
%

set compile_env: 0
