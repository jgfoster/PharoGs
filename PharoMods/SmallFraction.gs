set compile_env: 2

category: 'arithmetic'
method: SmallFraction
* aNumber  
	<PharoGs>
    ^self @env0:* aNumber
%

category: 'arithmetic'
method: SmallFraction 
+ aNumber  
	<PharoGs>
    ^self @env0:+ aNumber
%

category: 'arithmetic'
method: SmallFraction 
- aNumber 
	<PharoGs>
    ^self @env0:- aNumber
%

category: 'arithmetic'
method: SmallFraction 
/ aNumber 
	<PharoGs>
    ^self @env0:/ aNumber
%

category: 'comparing'
method: SmallFraction 
< aNumber 
	<PharoGs>
    ^self @env0:< aNumber
%

category: 'comparing'
method: SmallFraction 
<= aNumber 
	<PharoGs>
    ^self @env0:<= aNumber
%

category: 'comparing'
method: SmallFraction 
= aNumber 
	<PharoGs>
    ^self @env0:= aNumber
%

category: 'comparing'
method: SmallFraction 
> aNumber 
	<PharoGs>
    ^self @env0:> aNumber
%

category: 'comparing'
method: SmallFraction 
>= aNumber 
	<PharoGs>
    ^self @env0:>= aNumber
%

category: 'converting'
method: SmallFraction 
adaptToInteger: rcvr andSend: selector 
	"If I am involved in arithmetic with an Integer, convert it to a Fraction." 

    <PharoGs>
	^ (Fraction numerator: rcvr denominator: 1) perform: selector with: self 
%

category: 'converting'
method: SmallFraction 
asFloat 
	<PharoGs>
    ^self @env0:asFloat
%

category: 'converting'
method: SmallFraction 
asFraction	 
	<PharoGs>
    ^self
%

category: 'truncation and round off'
method: SmallFraction 
asLargerPowerOfTwo 
	"Convert the receiver into a power of two which is not less than the receiver" 
	<PharoGs>

	| numerator denominator quotient | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0]. 
	^(quotient := denominator // numerator) > 0  
		ifTrue: [Fraction numerator: 1 denominator:  (1 bitShift: (quotient highBit -1))] 
		ifFalse: [quotient := numerator // denominator. 
				"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not. 
				 (This is equivalent to Integer asLargerPowerOfTwo returning self when receiver is power of two) " 
				(quotient isPowerOfTwo and: [numerator \\ denominator = 0]) 
					ifTrue: [quotient] 
					ifFalse: [1 bitShift: (quotient highBit )]] 
%

category: 'converting'
method: SmallFraction 
asScaledDecimal 
	"Convert the receiver to a ScaledDecimal. 
	If there is a finite decimal representation of the receiver, then use the exact number of decimal places required. 
	Else, use a default number of decimals." 
	<PharoGs>
	 
	| pow2 pow5 q q5 numerator denominator | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	pow2 := denominator lowBit - 1. 
	q := denominator bitShift: pow2 negated. 
	pow5 := 0. 
	[q = 1] 
		whileFalse: [ 
			q5 := q // 5. 
			(q - (5 * q5)) = 0 ifFalse: [^super asScaledDecimal]. 
			q := q5. 
			pow5 := pow5 + 1]. 
	^self asScaledDecimal: (pow2 max: pow5) 
%

category: 'truncation and round off'
method: SmallFraction 
asSmallerPowerOfTwo 
	"Convert the receiver into a power of two which is not larger than the receiver" 
	<PharoGs>

	| numerator denominator quotient | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	(numerator = 0 or: [numerator sign ~= denominator sign]) ifTrue: [^DomainError signal: 'Value outside (0 , infinity)' from: 0]. 
	^(quotient := denominator // numerator) > 0  
		ifTrue: [	"If my quotient is a power of two, we, we need to check remainder, to see if we should shift by highbit or not. 
				 (This is equivalent to Integer asSmallerPowerOfTwo returning self when receiver is power of two) " 
				 (quotient isPowerOfTwo and: [denominator \\ numerator = 0])  
					ifTrue: [Fraction numerator: 1 denominator: quotient] 
					ifFalse:[Fraction numerator: 1 denominator:  (1 bitShift: quotient highBit)]] 
					 
		ifFalse: [1 bitShift: ((numerator // denominator) highBit -1)] 
%

category: 'private'
method: SmallFraction 
denominator 

	<PharoGs>
	^self @env0:denominator 
%

category: 'comparing'
method: SmallFraction 
hash 
	"Hash is reimplemented because = is implemented. 
	Care is taken that a Fraction equal to a Float also have an equal hash" 

	<PharoGs>
	| numerator denominator tmp | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	denominator isPowerOfTwo ifTrue: [ 
		"If denominator is a power of two, I can be exactly equal to a Float" 
		tmp := self asFloat. 
		tmp isFinite ifTrue: [^tmp hash]]. 
	 
	"Else, I cannot be exactly equal to a Float, use own hash algorithm. 
	(Assume the fraction is already reduced)" 
	^numerator hash bitXor: denominator hash 
%

category: 'converting'
method: SmallFraction 
isFraction 
	^ true 
%

category: 'testing'
method: SmallFraction 
isPowerOfTwo 

	<PharoGs>
	^ self @env0:numerator = 1 and: [ self @env0:denominator isPowerOfTwo ] 
%

category: 'self evaluating'
method: SmallFraction 
isSelfEvaluating  
	^ true 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
ln 
	"This function is defined because super ln might overflow." 

	<PharoGs>
	| numerator denominator res | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	self <= 0 ifTrue: [^DomainError signal: 'ln is only defined for x > 0' from: 0]. 
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow." 
	numerator < denominator ifTrue: [^self reciprocal ln negated]. 
	res := super ln. 
	res isFinite ifTrue: [^res]. 
	^numerator ln - denominator ln 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
log 
	"This function is defined because super log might overflow." 

	<PharoGs>
	| numerator denominator res | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	self <= 0 ifTrue: [^DomainError signal: 'log is only defined for x > 0' from: 0]. 
	"Test self < 1 before converting to float in order to avoid precision loss due to gradual underflow." 
	numerator < denominator ifTrue: [^self reciprocal log negated]. 
	res := super log. 
	res isFinite ifTrue: [^res]. 
	^numerator log - denominator log 
%

category: 'arithmetic'
method: SmallFraction 
negated  
	"Refer to the comment in Number|negated." 

	<PharoGs>
	^ Fraction 
		numerator: self @env0:nnumerator negated 
		denominator: self @env0:ndenominator 
%

category: 'testing'
method: SmallFraction 
negative 

	<PharoGs>
	^self @env0:nnumerator negative 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
nthRoot: aPositiveInteger 
	"Answer the nth root of the receiver." 

	<PharoGs>
	| numerator denominator guess | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	guess := (numerator nthRootTruncated: aPositiveInteger) / (denominator nthRootTruncated: aPositiveInteger). 
	(guess raisedTo: aPositiveInteger) = self ifTrue: [^guess]. 
	"There is no exact nth root, so answer a Float approximation" 
	^(self abs ln / aPositiveInteger) exp * self sign 
%

category: 'private'
method: SmallFraction 
numerator 

	<PharoGs>
	^self @env0:numerator 
%

category: 'printing'
method: SmallFraction 
printOn: aStream 

	<PharoGs>
	aStream nextPut: $(. 
	self @env0:numerator printOn: aStream. 
	aStream nextPut: $/. 
	self @env0:denominator printOn: aStream. 
	aStream nextPut: $). 
%

category: 'printing'
method: SmallFraction 
printOn: aStream base: base 

	<PharoGs>
	aStream nextPut: $(. 
	self @env0:numerator printOn: aStream base: base. 
	aStream nextPut: $/. 
	self @env0:denominator printOn: aStream base: base. 
	aStream nextPut: $). 
%

category: 'printing'
method: SmallFraction 
printOn: aStream showingDecimalPlaces: placesDesired 
	"Same as super, but provides a faster implementation by inlining some Fraction protocol thus avoiding intermediate Fraction creation." 
	 
	<PharoGs>
	| roundedFractionPart integerPart scaling numerator denominator | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	placesDesired <= 0 
		ifTrue: [self rounded printOn: aStream] 
		ifFalse: 
			[scaling := 10 raisedToInteger: placesDesired. 
			integerPart := numerator abs quo: denominator. 
			roundedFractionPart := (numerator abs - (integerPart * denominator)) * scaling * 2 + denominator quo: denominator * 2. 
			roundedFractionPart = scaling 
				ifTrue: 
					[integerPart := integerPart + 1. 
					roundedFractionPart := 0]. 
			"Don't print minus sign if result is rouded to zero '" 
			(numerator negative and: [integerPart > 0 or: [roundedFractionPart > 0]]) ifTrue: [aStream nextPut: $-]. 
			integerPart printOn: aStream. 
			aStream nextPut: $.. 
			roundedFractionPart printOn: aStream base: 10 length: placesDesired padded: true]. 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
raisedToFraction: aFraction 
	 
	<PharoGs>
	| numerator denominator roundedFractionPart root | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	root := (self numerator nthRootTruncated: aFraction denominator) / (self denominator nthRootTruncated: aFraction denominator). 
	(root raisedToInteger: aFraction denominator) = self ifTrue: [^root raisedToInteger: aFraction numerator]. 
	^super raisedToFraction: aFraction 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
raisedToInteger: anInteger  
	"See Number | raisedToInteger:" 
	 
	<PharoGs>
	anInteger = 0 ifTrue: [^ 1]. 
	anInteger < 0 ifTrue: [^ self reciprocal raisedToInteger: anInteger negated]. 
	^ Fraction numerator: (self @env0:numerator raisedToInteger: anInteger) 
		denominator: (self @env0:denominator raisedToInteger: anInteger) 
%

category: 'private'
method: SmallFraction 
reciprocal 
	 
	<PharoGs>
	| numerator denominator | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	numerator abs = 1 ifTrue: [^denominator * numerator]. 
	^self class numerator: denominator denominator: numerator 
%

category: 'private'
method: SmallFraction 
reduced 
	 
	<PharoGs>
	| numerator denominator gcd numer denom | 
    numerator := self @env0:numerator.
    denominator := self @env0:denominator.
	numerator = 0 ifTrue: [^0]. 
	gcd := numerator gcd: denominator. 
	numer := numerator // gcd. 
	denom := denominator // gcd. 
	denom = 1 ifTrue: [^numer]. 
	^Fraction numerator: numer denominator: denom 
%

category: 'truncation and round off'
method: SmallFraction 
round: numberOfWishedDecimal 
	"Round the decimal part of the receiver to be limited to the number of wished decimal. Only leave a fixed amount of decimal" 
   "(1/3 round: 2) >>> (33/100) "    
	"(111/100 round: 2) >>> (111/100) " 
	 
	<PharoGs>
	^self roundTo: (10 raisedTo: numberOfWishedDecimal negated) 
%

category: 'private'
method: SmallFraction 
setNumerator: n denominator: d 
	 
	<PharoGs>
    self _gsError
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
sqrt 
	 
	<PharoGs>
	| d n | 
	n := self @env0:numerator sqrt. 
	d := self @env0:denominator sqrt. 
	"The #sqrt method in integer will only answer a Float if there's no exact square root. '
	So, we need a float anyway." 
	(n isInfinite or: [ d isInfinite ]) ifTrue: [ 
		^self asFloat sqrt ]. 
	^n / d 
%

category: '*Math-Operations-Extensions'
method: SmallFraction 
squared 
	"See Fraction (Number) | squared" 
	 
	<PharoGs>
	^ Fraction 
        numerator: self @env0:numerator squared 
        denominator: self @env0:denominator squared 
%

category: 'printing'
method: SmallFraction 
storeOn: aStream base: base 
	 
	<PharoGs>
	aStream nextPut: $(. 
	self @env0:numerator storeOn: aStream base: base. 
	aStream nextPut: $/. 
	self @env0:denominator storeOn: aStream base: base. 
	aStream nextPut: $). 
%

category: 'truncation and round off'
method: SmallFraction 
truncated  
	"Refer to the comment in Number|truncated." 
	 
	<PharoGs>
	^self @env0:numerator quo: self @env0:denominator 
%

category: 'instance creation'
classmethod: SmallFraction 
numerator: numInteger denominator: denInteger  
	 
	<PharoGs>
    self _gsError
%

category: '*AST-Core-Parser'
classmethod: SmallFraction 
readFrom: stringOrStream  
	"Answer a Fraction as described on aStream with following rules: 
	- numerator can specify a different radix (like '16rABC'). 
	- fraction sign '/' is optional, and must immediately follow numerator without space. 
	- denominator must immediately follow fraction sign and can specify a different radix (like 16rABC). 
	If stringOrStream does not start with a valid number description, fail." 
	 
	 
	<PharoGs>
	^(NumberParser on: stringOrStream) nextFraction 
%

set compile_env: 0
