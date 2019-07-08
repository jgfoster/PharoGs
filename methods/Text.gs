category: 'initialization'
set compile_env: 2
classmethod: Text
initialize	"Text initialize"
	<PharoGs>

	"Initialize constants shared by classes associated with text display."
	TextSharedInformation at: #CaretForm put:
				(Form extent: 16@5
					fromArray: #(
						2r00110000000000000000000000000000 
						2r00110000000000000000000000000000
						2r01111000000000000000000000000000 
						2r11111100000000000000000000000000 
						2r11001100000000000000000000000000
					)
					offset: -3@0).
%
set compile_env: 0
category: 'instance creation'
set compile_env: 2
classmethod: Text
fromString: aString 
	<PharoGs>

  "Answer an instance of me whose characters are those of the argument, aString."
  | defaultFontStyle fontNumber |
  defaultFontStyle := TextStyle default.
  fontNumber := defaultFontStyle ifNil: [1] ifNotNil: [defaultFontStyle defaultFontIndex].
  ^ self
    string: aString
    attributes:{ "No default attributes" }
%
set compile_env: 0
