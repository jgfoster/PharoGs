category: '*CodeExport'
set compile_env: 2
method: TraitDescription
fileOutLocalMethodsInCategory: aSymbol on: aFileStream
	<PharoGs>

	"File out only the local (non-trait) methods for this category."
	| selectors localSelectorsA |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aSymbol.
   localSelectorsA := selectors select:[:s | self isLocalSelector: s ].
	"Overridden to preserve author stamps in sources file regardless"
	localSelectorsA do: [:sel |
		self printMethodChunk: sel on: aFileStream ].
	^ self
%
set compile_env: 0
