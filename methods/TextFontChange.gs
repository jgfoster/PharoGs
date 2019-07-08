category: 'as yet unclassified'
set compile_env: 2
classmethod: TextFontChange
defaultFontChange
	<PharoGs>

  "Answer a TextFontChange that represents the default font"
  | defaultFontStyle fontIndex |
  defaultFontStyle := TextStyle default.
  fontIndex := defaultFontStyle ifNil: [1] ifNotNil: [defaultFontStyle defaultFontIndex].
  ^ self new fontNumber: fontIndex.
%
set compile_env: 0
