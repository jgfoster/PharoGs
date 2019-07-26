set compile_env: 2

category: 'instance creation'
classmethod: ScaledDecimal
newFromNumber: aNumber scale: anInteger

	<PharoGs>
	^aNumber isFraction ifTrue: [
		self 
			@env0:numerator: aNumber @env0:numerator 
			denominator: aNumber @env0:denominator 
			scale: anInteger
	] ifFalse: [
		self 
			@env0:numerator: aNumber 
			denominator: 1
			scale: anInteger
	]
%

category: 'arithmetic'
method: ScaledDecimal
* aNumber

	<PharoGs>
	^self @env0:* aNumber
%

category: 'arithmetic'
method: ScaledDecimal
+ aNumber

	<PharoGs>
	^self @env0:+ aNumber
%

category: 'arithmetic'
method: ScaledDecimal
- aNumber

	<PharoGs>
	^self @env0:- aNumber
%

category: 'arithmetic'
method: ScaledDecimal
/ aNumber

	<PharoGs>
	^self @env0:/ aNumber
%

category: 'comparing'
method: ScaledDecimal
< aNumber

	<PharoGs>
	^self @env0:< aNumber
%

category: 'comparing'
method: ScaledDecimal
<= aNumber

	<PharoGs>
	^self @env0:<= aNumber
%

category: 'comparing'
method: ScaledDecimal
= aNumber

	<PharoGs>
	^self @env0:= aNumber
%

category: 'comparing'
method: ScaledDecimal
> aNumber

	<PharoGs>
	^self @env0:> aNumber
%

category: 'comparing'
method: ScaledDecimal
>= aNumber

	<PharoGs>
	^self @env0:>= aNumber
%

category: 'converting'
method: ScaledDecimal
adaptToFraction: rcvr andSend: selector
	"If I am involved in arithmetic with a Fraction, convert it to a ScaledDecimal."

	<PharoGs>
	^(rcvr asScaledDecimal: scale) perform: selector with: self
%

category: 'converting'
method: ScaledDecimal
adaptToInteger: rcvr andSend: selector
	"If I am involved in arithmetic with an Integer, convert it to a ScaledDecimal."

	<PharoGs>
	^(rcvr asScaledDecimal: scale) perform: selector with: self
%

category: 'converting'
method: ScaledDecimal
asFloat

	<PharoGs>
	^self @env0:asFloat
%

category: 'converting'
method: ScaledDecimal
asFraction

	<PharoGs>
	^self @env0:asFraction
%

category: 'private'
method: ScaledDecimal
coerce: aNumber
	"Note: this quick hack could be replaced by double dispatching"

	<PharoGs>
	aNumber class = self class ifTrue: [^self class newFromNumber: aNumber scale: (scale max: aNumber scale)].
	(aNumber isFraction or: [aNumber isInteger]) ifTrue: [^self class newFromNumber: aNumber scale: scale].
	^aNumber
%

category: 'testing'
method: ScaledDecimal
isFraction

	<PharoGs>
	^false
%

category: 'testing'
method: ScaledDecimal
isLiteral
	"Answer if this number could be a well behaved literal.
	Well, it would only if evaluating back to self.
	This is not the case of all ScaledDecimals.
	Some have an infinite precision and would need an infinite number of digits to print literally.
	Try for example (3.00s2 reciprocal)."

	<PharoGs>
	| denominator |
	denominator := self @env0:asFraction denominator.
	^denominator = 1 "first test trivial case before engaging arithmetic"
		or: ["Exactly we should test:
				(numerator * (10 raisedTo; scale)) \\ denominator = 0.
				But since we can assume fraction is reduced already this will be simply:"
			(10 raisedTo: scale) \\ denominator = 0]
%

category: 'accessing'
method: ScaledDecimal
isSelfEvaluating
    "Not all scaled decimal are self evaluating, because they print rounded digits."

	<PharoGs>
    ^self isLiteral
%

category: 'comparing'
method: ScaledDecimal
literalEqual: other
	"Testing equality is not enough.
	It is also necessary to test number of decimal places (scale).
	Otherwise we cannot compile both literals 0.5s1 and 0.50s2 in the same method"

	<PharoGs>
	^(super literalEqual: other) and: [self scale = other scale]
%

category: 'arithmetic'
method: ScaledDecimal
negated

	<PharoGs>
	^self class newFromNumber: super negated scale: scale
%

category: 'printing'
method: ScaledDecimal
printOn: aStream
	"Append an approximated representation of the receiver on aStream.
	Use prescribed number of digits after decimal point (the scale) using a rounding operation if not exact"

	<PharoGs>
	self printOn: aStream showingDecimalPlaces: scale.

	"Append a scale specification so that the number can be recognized as a ScaledDecimal"
	aStream nextPut: $s; print: scale.
%

category: 'printing'
method: ScaledDecimal
printOn: aStream base: base

	<PharoGs>
	base = 10 ifFalse: [self error: 'ScaledDecimals should be printed only in base 10'].
	self printOn: aStream
%

category: 'mathematical functions'
method: ScaledDecimal
raisedTo: aNumber

	<PharoGs>
	^self coerce: (super raisedTo: aNumber)
%

category: 'mathematical functions'
method: ScaledDecimal
raisedToFraction: aFraction

	<PharoGs>
	| result |
	result := self asFraction raisedToFraction: aFraction.
	^result isFloat
		ifTrue: [result]
		ifFalse: [result asScaledDecimal: scale]
%

category: 'mathematical functions'
method: ScaledDecimal
raisedToInteger: aNumber

	<PharoGs>
	^self class newFromNumber: (super raisedToInteger: aNumber) scale: scale
%

category: 'arithmetic'
method: ScaledDecimal
reciprocal

	<PharoGs>
	^self class newFromNumber: super reciprocal scale: scale
%

category: 'accessing'
method: ScaledDecimal
scale

	<PharoGs>
	^scale
%

category: 'private'
method: ScaledDecimal
setNumerator: n denominator: d scale: s

	<PharoGs>
	| sd |
	sd := ScaledDecimal @env0:numerator: n denominator: d scale: s.
	mantissa := sd @env0:mantissa.
	scale := sd @env0:scale.
%

category: 'arithmetic'
method: ScaledDecimal
squared

	<PharoGs>
	^self class newFromNumber: super squared scale: scale
%

category: 'printing'
method: ScaledDecimal
storeOn: aStream
	"ScaledDecimal sometimes have more digits than they print (potentially an infinity).
	In this case, do not use printOn: because it would loose some extra digits"

	<PharoGs>
	| fraction |
	fraction := self @env0:asFraction.
	self shouldBePrintedAsLiteral
		ifTrue: [self printOn: aStream]
		ifFalse: [aStream
			nextPut: $(;
		 	store: fraction @env0:numerator;
			nextPut: $/;
			store: fraction @env0:denominator;
			nextPut: $s;
			store: scale;
			nextPut: $)]
%

set compile_env: 0
