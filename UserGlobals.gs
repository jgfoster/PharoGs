! ------- Create dictionary if it is not present
run
| aSymbol names userProfile |
aSymbol := #'UserGlobals'.
userProfile := System myUserProfile.
names := userProfile symbolList names.
(names includes: aSymbol) ifFalse: [
	| symbolDictionary |
	symbolDictionary := SymbolDictionary new name: aSymbol; yourself.
	userProfile insertDictionary: symbolDictionary at: names size + 1.
].
%
set compile_env: 0
! ------------------- Class definition for ArrayTestCase
expectvalue /Class
doit
TestCase subclass: 'ArrayTestCase'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
ArrayTestCase category: 'Kernel'
%
set compile_env: 0
! ------------------- Class definition for CharacterTestCase
expectvalue /Class
doit
TestCase subclass: 'CharacterTestCase'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
CharacterTestCase category: 'Kernel'
%
set compile_env: 0
! ------------------- Class definition for IntegerTestCase
expectvalue /Class
doit
TestCase subclass: 'IntegerTestCase'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
IntegerTestCase category: 'Kernel'
%
set compile_env: 0
! ------------------- Class definition for LargeIntegerTestCase
expectvalue /Class
doit
TestCase subclass: 'LargeIntegerTestCase'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
LargeIntegerTestCase category: 'Kernel'
%
set compile_env: 0
! ------------------- Class definition for SmallIntegerTestCase
expectvalue /Class
doit
TestCase subclass: 'SmallIntegerTestCase'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: UserGlobals
  options: #()

%
expectvalue /Class
doit
SmallIntegerTestCase category: 'Kernel'
%

! ------------------- Remove existing behavior from ArrayTestCase
expectvalue /Metaclass3       
doit
ArrayTestCase removeAllMethods.
ArrayTestCase class removeAllMethods.
%
! ------------------- Class methods for ArrayTestCase
! ------------------- Instance methods for ArrayTestCase
set compile_env: 0
category: 'other'
method: ArrayTestCase
test_atWrap_

	self
		assert: (#(11 22 33) @env2:atWrap: 2) == 22;
		assert: (#(11 22 33) @env2:atWrap: 3) == 33;
		assert: (#(11 22 33) @env2:atWrap: 4) == 11;
		yourself
%
category: 'other'
method: ArrayTestCase
test_atWrap_put_

	self
		assert: ((#(11 22 33) copy @env2:atWrap: 2 put: 2; yourself) at: 2) == 2;
		assert: ((#(11 22 33) copy @env2:atWrap: 3 put: 3; yourself) at: 3) == 3;
		assert: ((#(11 22 33) copy @env2:atWrap: 4 put: 4; yourself) at: 1) == 4;
		yourself
%
category: 'other'
method: ArrayTestCase
test_C_new_

	self
		assert: (Array @env2:new: 3) size == 3;
		yourself
%
category: 'other'
method: ArrayTestCase
test_printString

	self
		assert: #(1 2 3) @env0:printString = 'anArray( 1, 2, 3)';
		assert: #(1 2 3) @env2:printString = '#(1 2 3)';
		yourself
%
category: 'other'
method: ArrayTestCase
test_size

	self
		assert: #() @env2:size == 0;
		yourself
%

! ------------------- Remove existing behavior from CharacterTestCase
expectvalue /Metaclass3       
doit
CharacterTestCase removeAllMethods.
CharacterTestCase class removeAllMethods.
%
! ------------------- Class methods for CharacterTestCase
! ------------------- Instance methods for CharacterTestCase
set compile_env: 0
category: 'other'
method: CharacterTestCase
testEquals

	self
		assert: $a @env2:= $a;
		yourself
%
category: 'other'
method: CharacterTestCase
testGreaterThan

	self
		assert: $b @env2:> $a;
		yourself
%
category: 'other'
method: CharacterTestCase
testLessThan

	self
		assert: $a @env2:< $b;
		yourself
%
category: 'other'
method: CharacterTestCase
test_asCharacter

	self
		assert: $a @env2:asCharacter == $a;
		yourself
%
category: 'other'
method: CharacterTestCase
test_asciiValue

	self
		assert: $a @env2:asciiValue == 97;
		yourself
%
category: 'other'
method: CharacterTestCase
test_asHTMLString

	self
		assert: $< @env2:asHTMLString = '&lt;';
		yourself
%
category: 'other'
method: CharacterTestCase
test_asInteger

	self
		assert: $a @env2:asInteger == 97;
		yourself
%

! ------------------- Remove existing behavior from IntegerTestCase
expectvalue /Metaclass3       
doit
IntegerTestCase removeAllMethods.
IntegerTestCase class removeAllMethods.
%
! ------------------- Class methods for IntegerTestCase
! ------------------- Instance methods for IntegerTestCase
set compile_env: 0
category: 'other'
method: IntegerTestCase
test_bitAnd_

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (int + 3 @env2:bitAnd: 1) == 1;
		assert: (int + 3 perform: #bitAnd: env: 2 withArguments: #(1)) == 1;
		yourself
%
category: 'other'
method: IntegerTestCase
test_bitOr_

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (int + 4 @env2:bitOr: 1) = (int + 5);
		assert: (int + 4 perform: #bitOr: env: 2 withArguments: #(1)) = (int + 5);
		yourself
%

! ------------------- Remove existing behavior from LargeIntegerTestCase
expectvalue /Metaclass3       
doit
LargeIntegerTestCase removeAllMethods.
LargeIntegerTestCase class removeAllMethods.
%
! ------------------- Class methods for LargeIntegerTestCase
! ------------------- Instance methods for LargeIntegerTestCase
set compile_env: 0
category: 'other'
method: LargeIntegerTestCase
testAdd

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:+ 1) = (int + 1);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testDivide

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:/ 2) = (int / 2);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testGreaterThan

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		deny: int @env2:> (int + 1);
		deny: int @env2:> (int + 0);
		assert: int @env2:> (int - 1);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testGreaterThanOrEqualTo

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		deny: int @env2:>= (int + 1);
		assert: int @env2:>= (int + 0);
		assert: int @env2:>= (int - 1);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testIntegerDivide

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:// 3) = (int // 3);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testLessThan

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: int @env2:< (int + 1);
		deny: int @env2:< (int + 0);
		deny: int @env2:< (int - 1);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testLessThanOrEqualTo

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: int @env2:<= (int + 1);
		assert: int @env2:<= (int + 0);
		deny: int @env2:<= (int - 1);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testMultiply

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:* 2) = (2 * int);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testRemainder

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:\\ 3) = (int \\ 3);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
testSubtract

	| int x |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (x := int @env2:- int) == 0;
		yourself
%
category: 'other'
method: LargeIntegerTestCase
test_quo_

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (int @env2:quo: 3) == (int quo: 3);
		assert: (int @env2:quo: 7) == (int quo: 7);
		yourself
%
category: 'other'
method: LargeIntegerTestCase
test_rem_

	| int |
	int := 16rFFFFFFFFFFFFFFF + 1.
	self
		assert: (int isKindOf: LargeInteger);
		assert: (int @env2:rem: 3) == (int rem: 3);
		assert: (int @env2:rem: 7) == (int rem: 7);
		yourself
%

! ------------------- Remove existing behavior from SmallIntegerTestCase
expectvalue /Metaclass3       
doit
SmallIntegerTestCase removeAllMethods.
SmallIntegerTestCase class removeAllMethods.
%
! ------------------- Class methods for SmallIntegerTestCase
! ------------------- Instance methods for SmallIntegerTestCase
set compile_env: 0
category: 'other'
method: SmallIntegerTestCase
testDivide

	self
		assert: 6 @env2:/ 3 == 2;
		assert: (6 perform: #/ env: 2 withArguments: #(3)) == 2;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testEquals

	self
		assert: 0 @env2:= 0;
		assert: (0 perform: #= env: 2 withArguments: #(0));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testGreaterThan

	self
		assert: 4 @env2:> 3;
		assert: (4 perform: #> env: 2 withArguments: #(3));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testGreaterThanOrEqualTo

	self
		assert: 5 @env2:>= 4;
		assert: (5 perform: #>= env: 2 withArguments: #(4));
		assert: 4 @env2:>= 4;
		assert: (4 perform: #>= env: 2 withArguments: #(4));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testLessThan

	self
		assert: 3 @env2:< 4;
		assert: (3 perform: #< env: 2 withArguments: #(4));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testLessThanOrEqualTo

	self
		assert: 3 @env2:<= 4;
		assert: (3 perform: #<= env: 2 withArguments: #(4));
		assert: 4 @env2:<= 4;
		assert: (4 perform: #<= env: 2 withArguments: #(4));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testMinus

	self
		assert: 2 @env2:- 3 == -1;
		assert: (2 perform: #- env: 2 withArguments: #(3)) == -1;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testModulo

	self
		assert: 7 @env2:\\ 3 == 1;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testNotEquals

	self
		assert: 0 @env2:~= 1;
		assert: (0 perform: #~= env: 2 withArguments: #(1));
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testPlus

	self
		assert: 2 @env2:+ 3 == 5;
		assert: (2 perform: #+ env: 2 withArguments: #(3)) == 5;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testQuotient

	self
		assert: 7 @env2:// 3 == 2;
		assert: (7 perform: #// env: 2 withArguments: #(3)) == 2;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
testTimes

	self
		assert: 2 @env2:* 3 == 6;
		assert: (2 perform: #* env: 2 withArguments: #(3)) == 6;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_abs

	self
		assert: -3 @env2:abs == 3;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_asCharacter

	self
		assert: 97 @env2:asCharacter == $a;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_asFloat

	self
		assert: 42 @env2:asFloat == 42.0;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_basicIdentityHash

	self
		assert: 42 @env2:basicIdentityHash == 42;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_bitAnd_

	self
		assert: (3 @env2:bitAnd: 1) == 1;
		assert: (3 perform: #bitAnd: env: 2 withArguments: #(1)) == 1;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_bitOr_

	self
		assert: (3 @env2:bitOr: 1) == 3;
		assert: (3 perform: #bitOr: env: 2 withArguments: #(1)) == 3;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_bitXor_

	self
		assert: (3 @env2:bitXor: 1) == 2;
		assert: (3 perform: #bitXor: env: 2 withArguments: #(1)) == 2;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_gcd_

	self
		assert: (64 @env2:gcd: 40) == 8;
		yourself
%
category: 'other'
method: SmallIntegerTestCase
test_quo_

	self
		assert: (-9 @env2:quo: 4) == -2;
		yourself
%
