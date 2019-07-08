category: '*Reflectivity'
set compile_env: 2
method: CompiledMethod
metaLinkOptions
	<PharoGs>
	^{
	#penultimateLiteral -> #( #+ optionCompileOnLinkInstallation).
	#selector -> #( #+ optionCompileOnLinkInstallation).
	#objectAt: -> #( #+ optionCompileOnLinkInstallation).
	#header -> #( #+ optionCompileOnLinkInstallation).
	#numLiterals -> #( #+ optionCompileOnLinkInstallation).
	#literalAt: -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
