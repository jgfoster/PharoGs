category: 'initialization'
set compile_env: 2
method: PharoChangesCondenser
reset
	<PharoGs>

	remoteStringMap := IdentityDictionary new.
	newChangesFile := self temporaryFile.
	
	"Keep a copy of the source streams for performance"
	sourceStreams := Array
			with: PharoFilesOpener default sourcesFileOrNil
			with: PharoFilesOpener default changesFileOrNil
%
set compile_env: 0
