category: 'invalidate'
set compile_env: 2
method: ReflectiveMethod
metaLinkOptions
	<PharoGs>
	^{
	#invalidate -> #( #+ optionCompileOnLinkInstallation).
	#increaseLinkCount -> #( #+ optionCompileOnLinkInstallation).
	#linkCount: -> #( #+ optionCompileOnLinkInstallation).
	#methodClass: -> #( #+ optionCompileOnLinkInstallation).
	#compiledMethod: -> #( #+ optionCompileOnLinkInstallation).
	#ast -> #( #+ optionCompileOnLinkInstallation).
	#installCompiledMethod -> #( #+ optionCompileOnLinkInstallation).
	#installReflectiveMethod -> #( #+ optionCompileOnLinkInstallation).
	#installMethod:-> #( #+ optionCompileOnLinkInstallation).
	#installLink: -> #( #+ optionCompileOnLinkInstallation).
	#reinstallASTInCache -> #( #+ optionCompileOnLinkInstallation).
	#removeLink: -> #( #+ optionCompileOnLinkInstallation).
	#selector: -> #( #+ optionCompileOnLinkInstallation).
	#run:with:in: -> #( #+ optionCompileOnLinkInstallation).
	#compiledMethod -> #( #+ optionCompileOnLinkInstallation).
	#reflectiveMethod -> #( #+ optionCompileOnLinkInstallation).
	#decreaseLinkCount -> #( #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( #+ optionCompileOnLinkInstallation)
	}
%
set compile_env: 0
