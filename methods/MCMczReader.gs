category: 'loading'
set compile_env: 2
method: MCMczReader
loadDefinitions
	<PharoGs>

	definitions := OrderedCollection new.
	(self zip memberNamed: 'snapshot.bin') ifNotNil:
		[:m | [^ definitions := (MCDataStream on: m contentStream) next definitions]
			on: Error do: [:fallThrough | ]].
	"otherwise"
	(self zip membersMatching: 'snapshot/*')
		do: [:m | self extractDefinitionsFrom: m]
%
set compile_env: 0
