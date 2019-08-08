set compile_env: 2

category: 'enumerating'
classmethod: ChangeSet
gatherChangeSets		"ChangeSet gatherChangeSets"
	"Collect any change sets created in other projects"

	<PharoGs>
	^ AllChangeSets
%

set compile_env: 0
