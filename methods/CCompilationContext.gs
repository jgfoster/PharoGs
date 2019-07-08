category: 'instance creation'
set compile_env: 2
classmethod: CCompilationContext
default 
	<PharoGs>
	| instance |
	instance := self new.
	instance parseOptions: OpalCompiler defaultOptions.
	AbstractCompiler compileWithErrors 
		ifTrue: [ instance parseOptions: #(#+ optionParseErrors) ].
	^instance.
%
set compile_env: 0
