set compile_env: 0

run
"The Pharo ScaledDecimal class maps to the GemStone FixedPoint class, but
 the compiler generates a GemStone ScaledDecimal for literals such as 123.4s1
 so we need to map them over to the Pharo class."
Globals at: #ScaledDecimal
%
set class **

set compile_env: 2

category: 'forward to Pharo'
method: 
_asFixedPoint

	<PharoGs>
	| fixedPoint |
	fixedPoint := self @env0:asFraction.
	^FixedPoint
		@env0:numerator: fixedPoint @env0:numerator 
		denominator: fixedPoint @env0:denominator 
		scale: scale.
%

method: 
doesNotUnderstand: aMessage

	<PharoGs>
	^self _asFixedPoint
		perform: aMessage selector
		withArguments: aMessage arguments
%

method: 
* aNumber
	<PharoGs>
    ^self _asFixedPoint * aNumber
%

method: 
+ aNumber
	<PharoGs>
    ^self _asFixedPoint + aNumber
%

method: 
- aNumber
	<PharoGs>
    ^self _asFixedPoint - aNumber
%

method: 
/ aNumber
	<PharoGs>
    ^self _asFixedPoint / aNumber
%

method: 
< aNumber
	<PharoGs>
    ^self _asFixedPoint < aNumber
%

method: 
<= aNumber
	<PharoGs>
    ^self _asFixedPoint <= aNumber
%

method: 
= aNumber
	<PharoGs>
    ^self _asFixedPoint = aNumber
%

method: 
=> aNumber
	<PharoGs>
    ^self _asFixedPoint => aNumber
%

method: 
>= aNumber
	<PharoGs>
    ^self _asFixedPoint >= aNumber
%

method: 
adaptToFraction: rcvr andSend: selector
	<PharoGs>
    ^self _asFixedPoint adaptToFraction: rcvr andSend: selector
%

method: 
adaptToInteger: rcvr andSend: selector
	<PharoGs>
    ^self _asFixedPoint adaptToInteger: rcvr andSend: selector
%

method: 
asFraction
	<PharoGs>
    ^self _asFixedPoint asFraction
%

method: 
class
	<PharoGs>
    ^Pharo @env0:at: #'ScaledDecimal'
%

method: 
coerce: aNumber
	<PharoGs>
    ^self _asFixedPoint coerce: aNumber
%

method: 
isFraction
	<PharoGs>
    ^false
%

method: 
isLiteral
	<PharoGs>
    ^self _asFixedPoint isLiteral
%

method: 
isSelfEvaluating
	<PharoGs>
    ^self _asFixedPoint isSelfEvaluating
%

method: 
literalEqual: other
	<PharoGs>
    ^self _asFixedPoint literalEqual: other
%

method: 
negated
	<PharoGs>
    ^self _asFixedPoint negated
%

method: 
nthRoot: anInteger
	<PharoGs>
    ^self _asFixedPoint nthRoot: anInteger
%

method: 
perform: aSymbol with: anObject  
	<PharoGs>
    ^self _asFixedPoint perform: aSymbol with: anObject 
%

method: 
printOn: aStream
	<PharoGs>
    ^self _asFixedPoint printOn: aStream
%

method: 
printOn: aStream base: base
	<PharoGs>
    ^self _asFixedPoint printOn: aStream base: base
%

method: 
raisedTo: aNumber
	<PharoGs>
    ^self _asFixedPoint raisedTo: aNumber
%

method: 
raisedToFraction: aFraction
	<PharoGs>
    ^self _asFixedPoint raisedToFraction: aFraction
%

method: 
raisedToInteger: aNumber
	<PharoGs>
    ^self _asFixedPoint raisedToInteger: aNumber
%

method: 
reciprocal
 	<PharoGs>
   ^self _asFixedPoint reciprocal
%

method: 
round: numberOfWishedDecimal
 	<PharoGs>
   ^self _asFixedPoint round: numberOfWishedDecimal
%

method: 
scale
	<PharoGs>
    ^self _asFixedPoint scale
%

method: 
setNumerator: n denominator: d scale: s
	<PharoGs>
    ^self _asFixedPoint setNumerator: n denominator: d scale: s
%

method: 
sqrt
	<PharoGs>
    ^self _asFixedPoint sqrt
%

method: 
squared
	<PharoGs>
    ^self _asFixedPoint squared
%

method: 
storeOn: aStream 
	<PharoGs>
    ^self _asFixedPoint storeOn: aStream 
%

set compile_env: 0
