category: 'other'
set compile_env: 2
method: Form
fillAlpha: alphaValue
	<PharoGs>

	"Fill a 32bit form with a constant alpha value"
	| bb |
	self depth = 32 ifFalse:[^self error: 'Only valid for 32 bit forms'].
	bb := BitBlt toForm: self.
	bb combinationRule: 7. "bitOr:with:"
	bb fillColor: (Bitmap with: alphaValue << 24).
	bb copyBits.
%
set compile_env: 0
