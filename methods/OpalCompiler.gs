category: 'options'
set compile_env: 2
classmethod: OpalCompiler
defaultOptions
	<PharoGs>
	^ #(
	"by default we inline all optimized constructs"
	#+ optionInlineIf
	#+ optionInlineIfNil
	#+ optionInlineAndOr
	#+ optionInlineWhile
	#+ optionInlineToDo
	#+ optionInlineCase
	#- optionInlineTimesRepeat
	#- optionInlineRepeat
	
	#- optionInlineNone "to turn off all. Overrides the others" 	
	#- optionLongIvarAccessBytecodes "special for Contexts"
	#+ optionOptimizeIR 
	
	#- optionParseErrors "parse syntactically wrong code"
	)
%
set compile_env: 0
