category: '*Reflectivity'
set compile_env: 2
method: MethodDictionary
metaLinkOptions
	<PharoGs>
	^{
	#scanFor: -> #( #+ optionCompileOnLinkInstallation).
	#at:put: -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
