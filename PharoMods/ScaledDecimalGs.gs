set compile_env: 0

run
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
* aNumber
    ^self _asFixedPoint * aNumber
%

method: 
+ aNumber
    ^self _asFixedPoint + aNumber
%

method: 
- aNumber
    ^self _asFixedPoint - aNumber
%

method: 
/ aNumber
    ^self _asFixedPoint / aNumber
%

method: 
< aNumber
    ^self _asFixedPoint < aNumber
%

method: 
<= aNumber
    ^self _asFixedPoint <= aNumber
%

method: 
= aNumber
    ^self _asFixedPoint = aNumber
%

method: 
=> aNumber
    ^self _asFixedPoint => aNumber
%

method: 
>= aNumber
    ^self _asFixedPoint >= aNumber
%

method: 
adaptToFraction: rcvr andSend: selector
    ^self _asFixedPoint adaptToFraction: rcvr andSend: selector
%

method: 
adaptToInteger: rcvr andSend: selector
    ^self _asFixedPoint adaptToInteger: rcvr andSend: selector
%

method: 
asFraction
    ^self _asFixedPoint asFraction
%

method: 
class
    ^Pharo @env0:at: #'ScaledDecimal'
%

method: 
coerce: aNumber
    ^self _asFixedPoint coerce: aNumber
%

method: 
isFraction
    ^false
%

method: 
isLiteral
    ^self _asFixedPoint isLiteral
%

method: 
isSelfEvaluating
    ^self _asFixedPoint isSelfEvaluating
%

method: 
literalEqual: other
    ^self _asFixedPoint literalEqual: other
%

method: 
negated
    ^self _asFixedPoint negated
%

method: 
nthRoot: anInteger
    ^self _asFixedPoint nthRoot: anInteger
%

method: 
perform: aSymbol with: anObject  
    ^self _asFixedPoint perform: aSymbol with: anObject 
%

method: 
printOn: aStream
    ^self _asFixedPoint printOn: aStream
%

method: 
printOn: aStream base: base
    ^self _asFixedPoint printOn: aStream base: base
%

method: 
raisedTo: aNumber
    ^self _asFixedPoint raisedTo: aNumber
%

method: 
raisedToFraction: aFraction
    ^self _asFixedPoint raisedToFraction: aFraction
%

method: 
raisedToInteger: aNumber
    ^self _asFixedPoint raisedToInteger: aNumber
%

method: 
reciprocal
    ^self _asFixedPoint reciprocal
%

method: 
round: numberOfWishedDecimal
    ^self _asFixedPoint round: numberOfWishedDecimal
%

method: 
scale
    ^self _asFixedPoint scale
%

method: 
setNumerator: n denominator: d scale: s
    ^self _asFixedPoint setNumerator: n denominator: d scale: s
%

method: 
sqrt
    ^self _asFixedPoint sqrt
%

method: 
squared
    ^self _asFixedPoint squared
%

method: 
storeOn: aStream 
    ^self _asFixedPoint storeOn: aStream 
%

set compile_env: 0
