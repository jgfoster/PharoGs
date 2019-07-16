set compile_env: 2

category: 'bit manipulation'
method: Integer
anyBitOfMagnitudeFrom: start to: stopArg  
	"Tests for any magnitude bits in the interval from start to stopArg." 
	"Primitive fixed in LargeIntegers v1.2. If you have an earlier version  
	comment out the primitive call (using this ST method then)." 
	| magnitude firstDigitIx lastDigitIx rightShift leftShift stop | 
	<PharoGs>

	start < 1 | (stopArg < 1) 
		ifTrue: [^ self error: 'out of range']. 
	magnitude := self abs. 
	stop := stopArg min: magnitude highBit. 
	start > stop 
		ifTrue: [^ false]. 
	firstDigitIx := start - 1 // 8 + 1. 
	lastDigitIx := stop - 1 // 8 + 1. 
	rightShift := (start - 1 \\ 8) negated. 
	leftShift := 7 - (stop - 1 \\ 8). 
	firstDigitIx = lastDigitIx 
		ifTrue: [| digit mask |  
			mask := (255 bitShift: rightShift negated) 
						bitAnd: (255 bitShift: leftShift negated). 
			digit := magnitude digitAt: firstDigitIx. 
			^ (digit bitAnd: mask) 
				~= 0]. 
	((magnitude digitAt: firstDigitIx) 
			bitShift: rightShift) 
			~= 0 
		ifTrue: [^ true]. 
	firstDigitIx + 1 
		to: lastDigitIx - 1 
		do: [:ix | (magnitude digitAt: ix) 
					~= 0 
				ifTrue: [^ true]]. 
	(((magnitude digitAt: lastDigitIx) 
			bitShift: leftShift) 
			bitAnd: 255) 
			~= 0 
		ifTrue: [^ true]. 
	^ false
%

category: 'bit manipulation'
method: Integer
bitAnd: arg  
	"Primitive. Answer an Integer whose bits are the logical AND of the 
	receiver's bits and those of the argument, arg. 
	Numbers are interpreted as having 2's-complement representation. 
	Essential.  See Object documentation whatIsAPrimitive." 

	<primitive: 740>
	<PharoGs> 
	^self @env0:bitAnd: arg
%

category: 'bit manipulation'
method: Integer
bitOr: n  
	"Answer an Integer whose bits are the logical OR of the receiver's bits   
	and those of the argument, n."

    <primitive: 741>
	| norm | 
	<PharoGs> 
	norm := n normalize. 
	^ self 
		digitLogic: norm 
		op: #bitOr: 
		length: (self digitLength max: norm digitLength)
%

category: 'bit manipulation'
method: Integer
bitShiftMagnitude: shiftCount  
	"Answer an Integer whose value (in magnitude representation) is   
	the receiver's value (in magnitude representation) shifted left by   
	the number of bits indicated by the argument. Negative arguments 
	shift right. Zeros are shifted in from the right in left shifts." 
	<PharoGs>

	| rShift | 
	shiftCount >= 0 ifTrue: [^ self digitLshift: shiftCount]. 
	rShift := 0 - shiftCount. 
	^ (self 
		digitRshift: (rShift bitAnd: 7) 
		bytes: (rShift bitShift: -3) 
		lookfirst: self digitLength) normalize
%

category: 'bit manipulation'
method: Integer
bitXor: n  
	"Answer an Integer whose bits are the logical XOR of the receiver's bits   
	and those of the argument, n." 

    <primitive: 743>
	| norm | 
	<PharoGs> 
	norm := n normalize. 
	^ self 
		digitLogic: norm 
		op: #bitXor: 
		length: (self digitLength max: norm digitLength)
%

category: 'private'
method: Integer
digitAdd: arg  
	<PharoGs>

	| len arglen accum sum | 
	accum := 0. 
	(len := self digitLength) < (arglen := arg digitLength) ifTrue: [len := arglen]. 
	"Open code max: for speed"
	sum := Integer new: len neg: self negative. 
	1 to: len do:  
		[:i |  
		accum := (accum bitShift: -8) 
					+ (self digitAt: i) + (arg digitAt: i). 
		sum digitAt: i put: (accum bitAnd: 255)]. 
	accum > 255 
		ifTrue:  
			[sum := sum growby: 1. 
			sum at: sum digitLength put: (accum bitShift: -8)]. 
	^ sum
%

category: 'private'
method: Integer
digitCompare: arg  
	"Compare the magnitude of self with that of arg.    
	Return a code of 1, 0, -1 for self >, = , < arg" 
	<PharoGs>

	| len arglen argDigit selfDigit | 
	len := self digitLength. 
	(arglen := arg digitLength) ~= len 
		ifTrue: [arglen > len 
				ifTrue: [^ -1] 
				ifFalse: [^ 1]]. 
	[len > 0] 
		whileTrue:  
			[(argDigit := arg digitAt: len) ~= (selfDigit := self digitAt: len) 
				ifTrue: [argDigit < selfDigit 
						ifTrue: [^ 1] 
						ifFalse: [^ -1]]. 
			len := len - 1]. 
	^ 0
%

category: 'private'
method: Integer
digitDiv: arg neg: ng  
	"Answer with an array of (quotient, remainder)." 
	<PharoGs>

	| quo rem ql d div dh dnh dl qhi qlo j l hi lo r3 a t | 
	arg = 0 ifTrue: [^ (ZeroDivide dividend: self) signal]. 
	"TFEI added this line" 
	l := self digitLength - arg digitLength + 1. 
	l <= 0 ifTrue: [^ Array with: 0 with: self]. 
	"shortcut against #highBit" 
	d := 8 - arg lastDigit highBitOfPositiveReceiver. 
	div := arg digitLshift: d. 
	div := div growto: div digitLength + 1. 
	"shifts so high order word is >=128" 
	rem := self digitLshift: d. 
	rem digitLength = self digitLength ifTrue: [rem := rem growto: self digitLength + 1]. 
	"makes a copy and shifts"
	quo := Integer new: l neg: ng. 
	dl := div digitLength - 1. 
	"Last actual byte of data"
	ql := l. 
	dh := div digitAt: dl. 
	dnh := dl = 1 
				ifTrue: [0] 
				ifFalse: [div digitAt: dl - 1]. 
	1 to: ql do:  
		[:k |  
		"maintain quo*arg+rem=self" 
		"Estimate rem/div by dividing the leading to bytes of rem by dh." 
		"The estimate is q = qhi*16+qlo, where qhi and qlo are nibbles." 
		j := rem digitLength + 1 - k. 
		"r1 := rem digitAt: j." 
		(rem digitAt: j) 
			= dh 
			ifTrue: [qhi := qlo := 15 
				"i.e. q=255"] 
			ifFalse:  
				["Compute q = (r1,r2)//dh, t = (r1,r2)\\dh.   
				Note that r1,r2 are bytes, not nibbles.   
				Be careful not to generate intermediate results exceeding 13   
				bits."
				"r2 := (rem digitAt: j - 1)."
				t := ((rem digitAt: j) 
							bitShift: 4) 
							+ ((rem digitAt: j - 1) 
									bitShift: -4). 
				qhi := t // dh. 
				t := (t \\ dh bitShift: 4) 
							+ ((rem digitAt: j - 1) 
									bitAnd: 15). 
				qlo := t // dh. 
				t := t \\ dh. 
				"Next compute (hi,lo) := q*dnh" 
				hi := qhi * dnh. 
				lo := qlo * dnh + ((hi bitAnd: 15) 
								bitShift: 4). 
				hi := (hi bitShift: -4) 
							+ (lo bitShift: -8). 
				lo := lo bitAnd: 255. 
				"Correct overestimate of q.   
				Max of 2 iterations through loop -- see Knuth vol. 2"
				r3 := j < 3 
							ifTrue: [0] 
							ifFalse: [rem digitAt: j - 2]. 
				[(t < hi 
					or: [t = hi and: [r3 < lo]]) 
					and:  
						["i.e. (t,r3) < (hi,lo)" 
						qlo := qlo - 1. 
						lo := lo - dnh. 
						lo < 0 
							ifTrue:  
								[hi := hi - 1. 
								lo := lo + 256]. 
						hi >= dh]] 
					whileTrue: [hi := hi - dh]. 
				qlo < 0 
					ifTrue:  
						[qhi := qhi - 1. 
						qlo := qlo + 16]]. 
		"Subtract q*div from rem" 
		l := j - dl. 
		a := 0. 
		1 to: div digitLength do:  
			[:i |  
			hi := (div digitAt: i) 
						* qhi. 
			lo := a + (rem digitAt: l) - ((hi bitAnd: 15) 
							bitShift: 4) - ((div digitAt: i) 
							* qlo). 
			rem digitAt: l put: lo - (lo // 256 * 256). 
			"sign-tolerant form of (lo bitAnd: 255)" 
			a := lo // 256 - (hi bitShift: -4). 
			l := l + 1]. 
		a < 0 
			ifTrue:  
				["Add div back into rem, decrease q by 1" 
				qlo := qlo - 1. 
				l := j - dl. 
				a := 0. 
				1 to: div digitLength do:  
					[:i |  
					a := (a bitShift: -8) 
								+ (rem digitAt: l) + (div digitAt: i). 
					rem digitAt: l put: (a bitAnd: 255). 
					l := l + 1]]. 
		quo digitAt: quo digitLength + 1 - k put: (qhi bitShift: 4) 
				+ qlo]. 
	rem := rem 
				digitRshift: d 
				bytes: 0 
				lookfirst: dl. 
	^ Array with: quo with: rem
%

category: 'private'
method: Integer
digitMultiply: arg neg: ng  
	| prod prodLen carry digit k ab | 
	<PharoGs>

	(arg digitLength = 1 and: [(arg digitAt: 1) 
			= 0]) 
		ifTrue: [^ 0]. 
	(self digitLength = 1 and: [(self digitAt: 1) 
			= 0]) 
		ifTrue: [^ 0]. 
	prodLen := self digitLength + arg digitLength. 
	prod := Integer new: prodLen neg: ng. 
	"prod starts out all zero" 
	1 to: self digitLength do: [:i | (digit := self digitAt: i) ~= 0 
			ifTrue:  
				[k := i. 
				carry := 0. 
				"Loop invariant: 0<=carry<=0377, k=i+j-1" 
				1 to: arg digitLength do:  
					[:j |  
					ab := (arg digitAt: j) 
								* digit + carry + (prod digitAt: k). 
					carry := ab bitShift: -8. 
					prod digitAt: k put: (ab bitAnd: 255). 
					k := k + 1]. 
				prod digitAt: k put: carry]]. 
	^ prod normalize
%

category: 'private'
method: Integer
digitSubtract: arg  
	| smaller larger z sum sl al ng | 
	<PharoGs>

	sl := self digitLength. 
	al := arg digitLength. 
	(sl = al 
		ifTrue:  
			[[(self digitAt: sl) 
				= (arg digitAt: sl) and: [sl > 1]] 
				whileTrue: [sl := sl - 1]. 
			al := sl. 
			(self digitAt: sl) 
				< (arg digitAt: sl)] 
		ifFalse: [sl < al]) 
		ifTrue:  
			[larger := arg. 
			smaller := self. 
			ng := self negative == false. 
			sl := al] 
		ifFalse:  
			[larger := self. 
			smaller := arg. 
			ng := self negative]. 
	sum := Integer new: sl neg: ng. 
	z := 0. 
	"Loop invariant is -1<=z<=1" 
	1 to: sl do:  
		[:i |  
		z := z + (larger digitAt: i) - (smaller digitAt: i). 
		sum digitAt: i put: z - (z // 256 * 256). 
		"sign-tolerant form of (z bitAnd: 255)" 
		z := z // 256]. 
	^ sum normalize
%

category: 'private'
method: Integer
montgomeryDigitLength 
	"Answer the number of bits composing a digit in Montgomery algorithm. 
	Primitive use either 8 or 32 bits digits" 
	<PharoGs> 

	^8 "Legacy plugin which did not have this primitive did use 8 bits digits" 
%

category: 'private'
method: Integer
montgomeryTimes: a modulo: m mInvModB: mInv 
	"Answer the result of a Montgomery multiplication 
	self * a * (b raisedTo: m montgomeryNumberOfDigits) inv \\ m 
	NOTE: it is assumed that: 
	self montgomeryNumberOfDigits <= m montgomeryNumberOfDigits 
	a montgomeryNumberOfDigits <= m montgomeryNumberOfDigits 
	mInv * m \\ b = (-1 \\ b) = (b-1) (this implies m odd) 
	where b = self montgomeryDigitBase 
	 
	Answer nil in case of absent plugin or other failure." 
	<PharoGs> 

	^nil
%

set compile_env: 0
