[
| count fileStream keepGs list noSubclasses |
keepGs := #(	"classes known to the VM that need to use the GemStone one"
	Array Behavior BlockClosure Boolean ByteArray CannotReturn Character 
	Class Collection Error Exception ExceptionSet Float FloatingPointException Halt 
	Integer LargeInteger Magnitude Message MessageNotUnderstood Notification 
	Number Object Pragma ProcessorScheduler SequenceableCollection SmallInteger String 
	Symbol UndefinedObject Warning ZeroDivide
) asOrderedCollection.
noSubclasses := #(
	BoxedFloat64 ByteSymbol ExceptionSetWithExclusions False
	FullBlockClosure LargeNegativeInteger LargePositiveInteger 
	SmallFloat64 True WideSymbol
).
fileStream := (FileStream forceNewFileNamed: 'classes.gs') text;
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output push classes.out only'; lf;
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'run'; lf;
	nextPutAll: 'Smalltalk'; lf;
	yourself.
keepGs do: [:each | 
	fileStream 
		tab; nextPutAll: 'at: #'; nextPutAll: each; 
		nextPutAll: ' put: (Globals at: #'; nextPutAll: each;
		nextPutAll: ');'; lf;
		yourself.
].
keepGs 
	add: #ArithmeticError; 
	add: #ClassDescription; 
	add: #Metaclass; 
	add: #ScaledDecimal; 
	yourself.
fileStream lf;
	tab; nextPutAll: 'at: #ArithmeticError	put: (Globals at: #NumericError);'; lf;
	tab; nextPutAll: 'at: #ClassDescription	put: (Globals at: #Module);'; lf;
	tab; nextPutAll: 'at: #Metaclass 		put: (Globals at: #Metaclass3);'; lf;
	tab; nextPutAll: 'at: #ScaledDecimal 	put: (Globals at: #FixedPoint);'; lf;	
	tab; nextPutAll: 'yourself.'; lf;
	nextPut: $%; lf;
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'run'; lf;
	nextPutAll: '(Object subclass: ''NoSubclasses'''; lf;
	tab; nextPutAll: 'instVarNames: #()'; lf;
	tab; nextPutAll: 'classVars: #()'; lf;
	tab; nextPutAll: 'classInstVars: #()'; lf;
	tab; nextPutAll: 'poolDictionaries: {}'; lf;
	tab; nextPutAll: 'inDictionary: Smalltalk'; lf;
	tab; nextPutAll: 'newVersionOf: (Smalltalk at: #NoSubclasses ifAbsent: [nil])'; lf;
	tab; nextPutAll: 'description: ''0'''; lf;
	tab; nextPutAll: 'options: #()'; lf;
	tab; nextPutAll: ') category: ''GemStone''.'; lf;
	nextPut: $%; lf;
	yourself.
list := OrderedCollection with: ProtoObject.
count := 0.
[ list notEmpty ] whileTrue: [ 
	| next |
	next := list removeFirst.
	next class == Metaclass ifFalse: [
		list addAll: next subclasses.
		(keepGs includes: next name) ifTrue: [
			fileStream
				nextPutAll: 'errorCount'; lf; 
				nextPutAll: 'run'; lf;
				nextPutAll: '| names |'; lf;
				nextPutAll: 'names := #(';
				yourself.
			next classVarNames do: [:each | 
				fileStream space; nextPutAll: each.
			].
			next class notNil ifTrue: [
				next class instVarNames do: [:each | 
					fileStream space; nextPutAll: each.
				].
			].
			fileStream 
				nextPutAll: ').'; lf;
				nextPutAll: 'names do: [:each | '; lf;
				tab; nextPut: $(; nextPutAll: next name;
					nextPutAll: ' classVarNames includes: each) ifFalse: ['; lf;
				tab; tab; nextPutAll: next name;
					nextPutAll: ' addClassVarName: each]].'; lf;
				nextPut: $%; lf;
				yourself.	
		] ifFalse: [
			| instSpec superclass message isBytes |
			instSpec := next instSpec.
			isBytes := false.
			((superclass := next superclass) notNil and: [superclass instSpec ~~ instSpec]) ifTrue: [
				(16 <= instSpec and: [instSpec <= 23]) ifTrue: [isBytes := true. message := ' byteSubclass: '''] ifFalse: [
				(2 <= instSpec and: [instSpec <= 4]) ifTrue: [message := ' indexableSubclass: '''] ifFalse: [
					message := ' subclass: '''.
				]].
			] ifFalse: [
				message := ' subclass: '''.
			].
			fileStream
				nextPutAll: 'errorCount'; lf; nextPutAll: 'run'; lf;
				nextPut: $(; nextPutAll: (
					superclass ifNil: ['nil'] 
					ifNotNil: [:class | 
						(noSubclasses includes: next name)
							ifTrue: ['NoSubclasses']
							ifFalse: [class name]
					]
				);
				nextPutAll: message; nextPutAll: next name; nextPut: $'; 
				lf.
			isBytes ifFalse: [
				fileStream tab; nextPutAll: 'instVarNames: #('.
				next instVarNames do: [ :each | fileStream space; nextPutAll: each ].
				fileStream nextPut: $); lf.
			].
			fileStream tab; nextPutAll: 'classVars: #('.
			next classVarNames do: [ :each | fileStream space; nextPutAll: each ].
			fileStream nextPut: $); lf; tab; nextPutAll: 'classInstVars: #('.
			next class instVarNames do: [ :each | fileStream space; nextPutAll: each ].
			fileStream nextPut: $); lf; tab; nextPutAll: 'poolDictionaries: {'.
			next sharedPools do: [ :each | fileStream space; nextPutAll: each name; nextPutAll: '. ' ].
			fileStream 
				nextPut: $}; lf; 
				tab; nextPutAll: 'inDictionary: Smalltalk'; lf;
				tab; nextPutAll: 'newVersionOf: (Smalltalk at: #';
					nextPutAll: next name;
					nextPutAll: ' ifAbsent: [nil])'; lf;
				tab; nextPutAll: 'description: '''; print: instSpec; nextPut: $'; lf;
				tab; nextPutAll: 'options: #()'; lf;
				nextPutAll: ') category: '''; nextPutAll: next category; nextPutAll: '''.'; lf;
				nextPut: $%; lf;
				yourself.
		].
		count := count + 1.
	].
].
fileStream 
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output pop'; lf;
	nextPutAll: 'errorCount'; lf;
	close.
Transcript show: 'classes count = ' , count printString; cr.
] on: Error do: [:ex | 
	Transcript show: ex description; cr.
].
SmalltalkImage current snapshot: false andQuit: true.
