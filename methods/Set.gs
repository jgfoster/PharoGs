category: '*Reflectivity'
set compile_env: 2
method: Set
metaLinkOptions
	<PharoGs>
	^{
	#parseOptions: -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
