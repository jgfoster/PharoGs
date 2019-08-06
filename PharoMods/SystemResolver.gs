set compile_env: 2

category: 'origins'
method: SystemResolver
imageDirectory

	<PharoGs>
	^self localDirectory
%

category: 'origins'
method: SystemResolver
localDirectory

	<PharoGs>
	^FileReference / GsFile @env0:serverCurrentDirectory
%

set compile_env: 0
