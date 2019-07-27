PackageManifest subclass: #ManifestPharoGs
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'PharoGs'!
!ManifestPharoGs commentStamp: '<historical>' prior: 0!
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser!

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ManifestPharoGs class
	instanceVariableNames: ''!

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 08:20'!
ruleRBCascadedNextPutAllsRuleV1FalsePositive
	^ #(#(#(#RGPackageDefinition #(#PharoGs)) #'2019-07-16T08:20:25.643541-07:00') )! !

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 08:20'!
ruleRBUtilityMethodsRuleV1FalsePositive
	^ #(#(#(#RGMethodDefinition #(#PharoGsExport #createMissingInstVarsAsGlobalsIn: #false)) #'2019-07-16T08:20:52.291696-07:00') )! !

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 08:12'!
ruleRBCodeCruftLeftInMethodsRuleV1FalsePositive
	^ #(#(#(#RGPackageDefinition #(#PharoGs)) #'2019-07-16T08:12:30.933457-07:00') )! !

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 08:19'!
ruleRBYourselfNotUsedRuleV1FalsePositive
	^ #(#(#(#RGPackageDefinition #(#PharoGs)) #'2019-07-16T08:19:15.840946-07:00') )! !

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 09:30'!
ruleRBUsesAddRuleV1FalsePositive
	^ #(#(#(#RGMethodDefinition #(#PharoGsExport #listOfClasses #false)) #'2019-07-16T09:30:48.65773-07:00') )! !

!ManifestPharoGs class methodsFor: 'code-critics' stamp: 'JamesFoster 7/16/2019 08:48'!
ruleFloatReferencesRuleV1FalsePositive
	^ #(#(#(#RGMethodDefinition #(#PharoGsExport #pharoToGsClassMap #false)) #'2019-07-16T08:48:17.324327-07:00') )! !


Object subclass: #PharoGsExport
	instanceVariableNames: 'fileStream'
	classVariableNames: ''
	package: 'PharoGs'!
!PharoGsExport commentStamp: 'JamesFoster 7/16/2019 08:08' prior: 0!
I export Pharo code in a format that can be loaded into GemStone/S.!


!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/17/2019 14:51'!
createLinksToIdenticalClasses

	fileStream
		nextPutAll: '!! - create links to identical classes'; lf;
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: 'Pharo'; lf;
		yourself.
	self pharoToGsClassMap keysAndValuesDo: [:eachKey :eachValue | 
		fileStream 
			tab; nextPutAll: 'at: #'; nextPutAll: eachKey name; 
			nextPutAll: ' put: (Globals at: #'; nextPutAll: eachValue;
			nextPutAll: ');'; lf;
			yourself.
	].
	fileStream lf;
		tab; nextPutAll: 'yourself.'; lf;
		nextPut: $%; lf;
		nextPutAll: 'errorCount'; lf;
		yourself.
! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/18/2019 08:30'!
listOfClasses

	| list queue |
	queue := OrderedCollection with: ProtoObject with: SharedPool with: Object.
	list := OrderedCollection with: ProtoObject with: Object with: SharedPool.
	[ queue notEmpty ] whileTrue: [ 
		| eachClass subclasses |
		eachClass := queue removeFirst.
		subclasses := eachClass subclasses asSortedCollection: [:a :b | a name <= b name].
		queue addAll: subclasses.
		list addAll: (subclasses reject: [:each | list includes: each]).
	].
	list := list reject: [:each | each isMeta or: [#(ManifestPharoGs PharoGsExport) includes: each name]].
	^list! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/17/2019 16:24'!
nameForSuperclassOf: aClass

	| superclass |
	superclass := aClass superclass.
	superclass ifNil: [ ^'nil' ].
	"some of the Pharo classes are subclasses of GemStone classes that don't allow subclassing"
	^(#(Behavior BlockClosure Boolean Class ExceptionSet LargeInteger Symbol) includes: superclass name)
		ifTrue: [ 'NoSubclasses' ]
		ifFalse: [ superclass name ]
! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/16/2019 09:49'!
addClassVarsFor: aClass
	"We have an identical class in GemStone but it might not have the desired class variables"

	aClass class classVarNames isEmpty ifTrue: [ ^self ].
	fileStream
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: '| names |'; lf;
		nextPutAll: 'names := #(';
		yourself.
	"add any Pharo class variable names to the GemStone class"
	aClass classVarNames do: [ :each | 
			fileStream space; nextPutAll: each.
	].
	aClass class notNil ifTrue: [ 
		aClass class instVarNames do: [ :each | 
			fileStream space; nextPutAll: each. 
		]
	].
	fileStream
		nextPutAll: ').'; lf;
		nextPutAll: 'names do: [:each | '; lf;
		tab; nextPut: $(;
		nextPutAll: aClass name;
		nextPutAll: ' classVarNames includes: each) ifFalse: ['; lf;
		tab; tab; nextPutAll: aClass name; nextPutAll: ' addClassVarName: each]].'; lf;
		nextPut: $%; lf;
		yourself.! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/16/2019 11:50'!
defineInstVarsFor: aClass

	fileStream tab; nextPutAll: 'instVarNames: #('.
	aClass instVarNames do: [ :each | 
		fileStream space; nextPutAll: each.
	].
	fileStream nextPut: $); lf.! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/23/2019 18:00'!
pharoToGsClassMap

	^Dictionary new 
		"These have the same name"
		at: Array 						put: #Array;
		at: Behavior 					put: #Behavior;
		at: Boolean 						put: #Boolean;
		at: ByteArray 					put: #ByteArray;
		at: CannotReturn 				put: #CannotReturn;
		at: Character 					put: #Character;
		at: Class 						put: #Class;
		at: Collection 					put: #Collection;
		at: Error 						put: #Error;
		at: Exception					put: #Exception;
		at: ExceptionSet 				put: #ExceptionSet;
		at: Fraction 						put: #Fraction;
		at: Integer 						put: #Integer;
		at: LargeInteger 				put: #LargeInteger;
		at: Magnitude 					put: #Magnitude;
		at: Message 						put: #Message;
		at: MessageNotUnderstood 	put: #MessageNotUnderstood;
		at: Notification 				put: #Notification;
		at: Number 						put: #Number;
		at: Object 						put: #Object;
		at: Pragma 						put: #Pragma;
		at: ProcessorScheduler 		put: #ProcessorScheduler;
		at: ScaledDecimal 				put: #ScaledDecimal;
		at: Semaphore 					put: #Semaphore;
		at: SequenceableCollection 	put: #SequenceableCollection;
		at: SmallInteger 				put: #SmallInteger;
		at: Symbol 						put: #Symbol;
		at: UndefinedObject 			put: #UndefinedObject;
		at: Warning 						put: #Warning;
		at: ZeroDivide 					put: #ZeroDivide;

		"These have different names"
		at: ArithmeticError 			put: #NumericError;
		at: BlockClosure 				put: #ExecBlock;
		at: BoxedFloat64 				put: #Float;
		at: ByteString 					put: #String;
		at: ByteSymbol 					put: #Symbol;
		at: ClassDescription 			put: #Module;
		at: CompiledMethod 			put: #GsNMethod;
		at: Float 						put: #BinaryFloat;
		at: Metaclass 					put: #Metaclass3;
		at: MethodDictionary 			put: #GsMethodDictionary;
		at: Process 						put: #GsProcess;
		at: SmallFloat64 				put: #SmallDouble;
		at: String 						put: #CharacterCollection;
		at: WideString 					put: #QuadByteString;
		at: WideSymbol 					put: #QuadByteSymbol;

		yourself! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/18/2019 08:32'!
addClassFor: aClass
	"create a new class to represent the Pharo class"

	| instSpec isBytes message superclass |
	instSpec := aClass instSpec.
	isBytes := false.
	"determine the subclass type (byte, indexable, or objects"
	message := ((superclass := aClass superclass) notNil
		and: [ superclass instSpec ~~ instSpec ])
		ifTrue: [ 
			(instSpec between: 16 and: 23) ifTrue: [ 
				isBytes := true.
				' byteSubclass: ''' 
			] ifFalse: [
				(instSpec between: 2 and: 4)
					ifTrue: [ ' indexableSubclass: ''' ]
					ifFalse: [ ' subclass: ''' ].
			]
		] ifFalse: [ 
			' subclass: ''' 
		].
	fileStream
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'run'; lf;
		nextPut: $(;
		nextPutAll: (self nameForSuperclassOf: aClass);
		nextPutAll: message;
		nextPutAll: aClass name;
		nextPut: $';
		lf.
	isBytes ifFalse: [ 
		self defineInstVarsFor: aClass.
	].
	self 
		defineClassVarsFor: aClass;
		defineClassInstVarsFor: aClass;
		definePoolsFor: aClass;
		yourself.
	fileStream
		tab; nextPutAll: 'inDictionary: Pharo'; lf;
		tab; nextPutAll: 'newVersionOf: (Pharo at: #';
			nextPutAll: aClass name;
			nextPutAll: ' ifAbsent: [nil])'; lf;
		tab; nextPutAll: 'description: ''Pharo instSpec: '; print: aClass instSpec; nextPut: $'; lf;
		tab; nextPutAll: 'options: #()'; lf;
		nextPutAll: ') category: '''; nextPutAll: aClass category; nextPutAll: '''.'; lf;
		nextPut: $%; lf;
		yourself.
		! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/18/2019 08:36'!
definePoolsFor: aClass

	fileStream
		tab; nextPutAll: 'poolDictionaries: {'.
	aClass sharedPools do: [ :each | 
		fileStream space; nextPutAll: each name; nextPutAll: ' _classVars. '.
	].
	fileStream nextPut: $}; lf.
	! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/18/2019 07:57'!
createClasses

	| list |
	self
		createLinksToIdenticalClasses;
		createNoSubclassesClass;
		yourself.
	list := self listOfClasses.
	list do: [ :aClass | 
		(self pharoToGsClassMap keys includes: aClass) ifTrue: [ 
			"we are using the GemStone class, so should not create a new class"
			self addClassVarsFor: aClass.
		] ifFalse: [ 
			"create a new class"
			self addClassFor: aClass.
		].
		fileStream flush.
	].
	fileStream nextPutAll: 'errorCount'; lf.
	Transcript show: 'classes count = ' , list size printString; cr.! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/17/2019 14:51'!
createNoSubclassesClass

	fileStream
		nextPutAll: 'run'; lf;
		nextPutAll: '(Object subclass: ''NoSubclasses'''; lf;
		tab; nextPutAll: 'instVarNames: #()'; lf;
		tab; nextPutAll: 'classVars: #()'; lf;
		tab; nextPutAll: 'classInstVars: #()'; lf;
		tab; nextPutAll: 'poolDictionaries: {}'; lf;
		tab; nextPutAll: 'inDictionary: Pharo'; lf;
		tab; nextPutAll: 'newVersionOf: (Pharo at: #NoSubclasses ifAbsent: [nil])'; lf;
		tab; nextPutAll: 'description: ''0'''; lf;
		tab; nextPutAll: 'options: #()'; lf;
		tab; nextPutAll: ') category: ''GemStone''.'; lf;
		nextPut: $%; lf;
		yourself.
! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/16/2019 10:11'!
defineClassVarsFor: aClass

	fileStream tab; nextPutAll: 'classVars: #('.
	aClass classVarNames do: [ :each | 
		fileStream space; nextPutAll: each.
	].
	fileStream nextPut: $); lf.
! !

!PharoGsExport methodsFor: 'classes' stamp: 'JamesFoster 7/16/2019 12:07'!
defineClassInstVarsFor: aClass

	fileStream tab; nextPutAll: 'classInstVars: #('.
	aClass class instVarNames do: [ :each | 
		fileStream space; nextPutAll: each.
	].
	fileStream nextPut: $); lf.
	! !


!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 10:27'!
reservedSelectors 

	^#(
		__inProtectedMode _and: _downTo:by:do: _downTo:do: _gsReturnNothingEnableEvents
		_gsReturnNoResult _isArray _isExceptionClass _isExecBlock _isFloat _isInteger
		_isNumber _isOneByteString _isRange _isRegexp _isRubyHash _isScaledDecimal
		_isSmallInteger _isSymbol _leaveProtectedMode _or: _stringCharSize ~~
		and: == ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: ifTrue:
		ifTrue:ifFalse: isKindOf: or: timesRepeat: to:by:do: to:do: untilFalse untilFalse: untilTrue
		untilTrue: whileFalse whileFalse: whileTrue whileTrue: repeat
	)! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/23/2019 18:04'!
compileErrors

	^#(
		#(#Behavior #basicSuperclass:)
		#(#Behavior #classLayout)
		#(#Behavior #classLayout:)
		#(#Behavior #localSelectors)
		#(#Behavior #methodDict)
		#(#Behavior #methodDict:)
		#(#Behavior #superclass)
		#(#BlockClosure $*) 
		#(#Class #copyForAnnouncement) "superclass category classPool"
		#(#Class #basicCategory)
		#(#Class #basicCategory:)
		#(#Class #classPool) 
		#(#Class #classPool:) 
		#(#Class #environment) 
		#(#Class #environment:) 
		#(#Class #sharedPools) 
		#(#Class #sharedPools:) 
		#(#ClassDescription #basicOrganization)
		#(#ClassDescription #basicOrganization:)
		#(#ClassDescription #definitionWithSlots)
		#(#ClassDescription #definitionWithoutSlots)
		#(#ClassDescription #oldDefinition)
		#(#ClassDescription #superclass:layout:)
		#(#CannotReturn #result)
		#(#CannotReturn #result:)
		#(#CompiledMethod $*)
		#('CompiledMethod class' $*)
		#(#Delay $*)
		#('Delay class' $*)
		#(#DelayWaitTimeout $*)
		#('DelayWaitTimeout class' $*)
		#(#Exception freezeUpTo:)	"handlerContext outerContext signalContext signaler"
		#(#Exception isNested)
		#(#Exception outer)
		#(#Exception pass)
		#(#Exception privHandlerContext)
		#(#Exception privHandlerContext:)
		#(#Exception resignalAs:)
		#(#Exception resumeUnchecked:)
		#(#Exception retry)
		#(#Exception retryUsing:)
		#(#Exception return:)
		#(#Exception searchFrom:)
		#(#Exception signal)
		#(#Exception signalIn:)
		#(#Exception signaler)
		#(#Exception signaler:)
		#(#Exception signalerContext)
		#(#ExceptionSet #add:)	
		#(#ExceptionSet #handles:)
		#(#ExceptionSet #initialize)
		#(#FullBlockClosure $*)
		#(#Message #analogousCodeTo:)	"lookupClass"
		#(#Message #lookupClass)
		#(#Message #lookupClass:)
		#(#Message #sentTo:)
		#(#MessageNotUnderstood #defaultAction)	"message reachedDefaultHandler"
		#(#MessageNotUnderstood #initialize)
		#(#MessageNotUnderstood #message)
		#(#MessageNotUnderstood #message:)
		#(#MessageNotUnderstood #messageText)
		#(#MessageNotUnderstood #smartDescription)
		#(#MessageNotUnderstood #reachedDefaultHandler)
		#(#Metaclass #adoptInstanceFrom:)	"thisClass"
		#(#Metaclass #environment)
		#(#Metaclass #adoptInstance:from:)
		#(#Metaclass #fileOutOn:initializing:)
		#(#Metaclass #name)
		#(#Metaclass #new)
		#(#Metaclass #postCopy)
		#(#Metaclass #soleInstance)
		#(#MetaclassForTraits #name)
		#(#MethodDictionary $*)
		#(#MonitorDelay $*)
		#('MonitorDelay class' $*)
		#(#Pragma #selector)
		#(#Pragma #selector:)
		#(#Process $*)
		#(#ProcessorScheduler $*)
		#(#ProtoObjectTest $*)
		#(#ScaledDecimal $*)
		#('SmallInteger class' #maxVal)
		#('SmallInteger class' #minVal)
		#('SmallInteger class' #startUp:)
		#(#TraitedMetaclass #isRejectedMethod:)
		#(#TraitedMetaclass #rebuildMethodDictionary)
		#(#VariableNotDeclared #smartDescription)
	)! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/17/2019 15:02'!
createMethods

	| count |
	Transcript show: 'method count = '.
	count := 0.

	fileStream
		nextPutAll: '!! - generate methods'; lf;
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'set compile_env: 0'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: '| symbolList |'; lf;
		nextPutAll: 'symbolList := GsSession currentSession symbolList.'; lf;
		nextPutAll: 'symbolList add: (symbolList remove: Pools) after: Pharo.'; lf;
		nextPut: $%; lf.
	self listOfClasses do: [:aClass | 
		count := count + (self createMethodsFor: aClass).
	].
	fileStream  nextPutAll: 'errorCount'; lf.
	Transcript show: count printString; cr.
! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 11:09'!
rewriteReservedSelectorsIn: aString selector: eachSelector

	| source |
	(self reservedSelectors includes: eachSelector) ifFalse: [ ^aString ].
	source := aString.
	source := eachSelector == #~~ ifTrue: [
		'_gsReservedSelector_tilde_tilde_:' , (source copyFrom: 3 to: source size)
	] ifFalse: [eachSelector == #== ifTrue: [
		'_gsReservedSelector_equal_equal_:' , (source copyFrom: 3 to: source size)
	] ifFalse: [
		'_gsReservedSelector_' , source.
	]].
	source := source , Character lf asString , Character tab asString , 
		'"The above Pharo code attempts to compile a reserved selector so we place it here for reference"' , 
		Character lf asString.
	^source
	! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 10:24'!
replaceCrWithLfIn: aString

	^aString collect: [:each | 
		each == Character cr ifTrue: [Character lf] ifFalse: [each].
	]
! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 10:29'!
fixBinarySelectorsInLiteralArrayIn: aString

	| literalArrayDepth prev readStream writeStream |
	prev := nil.
	literalArrayDepth := 0.
	readStream := ReadStream on: aString.
	writeStream := WriteStream on: String new.
	[
		readStream atEnd.
	] whileFalse: [
		| next |
		next := readStream next.
		
		"deal with comments"
		(next == $" and: [prev ~~ $$]) ifTrue: [
			writeStream nextPut: $"; nextPutAll: (readStream upTo: $"); nextPut: $".
			prev := $".
		] ifFalse: [
			"deal with strings"
		(next == $' and: [prev ~~ $$]) ifTrue: [
			writeStream nextPut: $'; nextPutAll: (readStream upTo: $'); nextPut: $'.
			prev := $'.
		] ifFalse: [
		
		"deal with start of literal Array"
		(next == $# and: [readStream peekFor: $(]) ifTrue: [
			writeStream nextPutAll: '#('.
			prev := $(.
			literalArrayDepth := literalArrayDepth + 1.
		] ifFalse: [
			"here we are inside a literal array"
		literalArrayDepth > 0 ifTrue: [
				"a nested literal array"
			(next == $( and: [prev ~~ $# and: [prev ~~ $$]]) ifTrue: [
				writeStream nextPutAll: '#('.
				prev := next.
				literalArrayDepth := literalArrayDepth + 1.
			] ifFalse: [
				"end of a literal array"
			next == $) ifTrue: [
				writeStream nextPut: next.
				prev := next.
				literalArrayDepth := literalArrayDepth - 1.
			] ifFalse: [
			
			"handle binary selectors (this is why we are doing the work)"
			(next isSpecial and: [prev ~~ $$]) ifTrue: [
				prev ~~ $# ifTrue: [
					writeStream nextPutAll: ' #'.
					prev := $#.
				].
				writeStream nextPut: next.
				prev := next.
				[
					readStream peek isSpecial.
				] whileTrue: [
					writeStream nextPut: (prev := readStream next).
				].
			
			"other characters inside a literal array"
			] ifFalse: [
				writeStream nextPut: next.
				prev := next.
			]]]
			"other characters outside of a literal array"
		] ifFalse: [
			writeStream nextPut: next.
			prev := next.
		]]]]
	].
	^writeStream contents.
! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/17/2019 15:02'!
addClassCommentFor: aClass

	"for GemStone classes, add a class-side #'pharoComment' method"
	(self pharoToGsClassMap keys includes: aClass) ifTrue: [
		fileStream
			nextPutAll: 'set compile_env: 2'; lf;
			nextPutAll: 'category: ''GemStone'''; lf;
			nextPutAll: 'classMethod:'; lf;
			nextPutAll: 'pharoComment'; lf;
			nextPut: $^; 
			print: (self replaceCrWithLfIn: aClass comment); lf;
			nextPut: $%; lf;
			yourself.
		] ifFalse: [
		
		"for Pharo classes, add the comment"
		fileStream
			nextPutAll: 'run'; lf;
			nextPutAll: '(Pharo at: #'; nextPutAll: aClass name;
			nextPutAll: ') comment: '; lf;
			print: (self replaceCrWithLfIn: aClass comment); lf;
			nextPut: $%; lf;
			nextPutAll: 'set compile_env: 2'; lf;
			yourself.
	].

	! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/17/2019 15:02'!
addMethodsHeaderFor: aClass

	fileStream
		nextPutAll: 'output push output/';
		nextPutAll: aClass name;
		nextPutAll: '.out only'; lf;
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'set compile_env: 0'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: 'Pharo at: #'; nextPutAll: aClass name; lf;
		nextPut: $%; lf;
		nextPutAll: 'set class **'; lf;
		yourself.
! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/23/2019 18:08'!
fixMethodsThatWillNotCompileInBehavior: behavior selector: selector source: source

	| flag i |
	flag := self compileErrors anySatisfy: [:pair | 
		(pair first == $* or: [pair first = behavior name]) 
		and: [pair last == $* or: [pair last == selector]]].
	flag ifFalse: [ ^source ].
	i :=  source indexOf: Character lf.
	^(source copyFrom: 1 to: i - 1), Character lf asString , 
		'	<PharoCompileError>' , Character lf asString , 
		'	self _gsError "' , 
		((source copyFrom: i to: source size) copyReplaceAll: '"' with: '""') , '"'
! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 12:02'!
addMethodsFor: eachBehavior

	| count methodDict organization  |
	count := 0.
	methodDict := eachBehavior methodDict.
	organization := eachBehavior organization.
	methodDict keys asSortedCollection do: [ :eachSelector | 
		| category source |
		category := organization categoryOfElement: eachSelector.
		source := self replaceCrWithLfIn: (methodDict at: eachSelector) sourceCode.
		source := self rewritePrimitiveIn: source.
		source := self rewriteReservedSelectorsIn: source selector: eachSelector.
		source := self fixBinarySelectorsInLiteralArrayIn: source.
		source := self 
			fixMethodsThatWillNotCompileInBehavior: eachBehavior
			selector: eachSelector 
			source: source.
			
		"special handling for Semaphore"
		eachBehavior == Semaphore ifTrue: [
			source := source 
				copyReplaceAll: 'excessSignals'
				with: 'signalCount'.
		].
			
		"method category and instance or class side"
		fileStream lf;
			nextPutAll: 'category: '''; nextPutAll: category; nextPut: $'; lf;
			nextPutAll: (eachBehavior isMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']); lf.
			"put out the method"
		(source substrings: (Array with: Character lf)) do: [:eachLine | 
			| line |
			line := eachLine , ' '.
			(eachLine notEmpty and: [eachLine first == $%]) ifTrue: [fileStream space].
			eachBehavior == Class ifTrue: [	"can't use instVarNames as method argument"
				line := line copyReplaceAll: ' instVarNames ' with: ' instVarNameList '.
			].
			fileStream nextPutAll: line; lf.
		].
		fileStream nextPut: $%; lf.
		count := count + 1.
	].
	^count! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 11:04'!
rewritePrimitiveIn: aString

	| i source |
	source := aString.
	"special handling of primitives"
	i := source findString: '<primitive:'.
	0 < i ifTrue: [ | string inComment inString |
		inComment := false.
		inString := false.
		(string := source copyFrom: 1 to: i - 1) do: [:char | 
			char == $" ifTrue: [inComment := inComment not].
			(inComment not and: [char == $']) ifTrue: [inString := inString not].
		].
		(inComment not and: [inString not]) ifTrue: [
			source := string , '<PharoPrimitive>' , Character lf asString , 
				'self @env0:error: ''PharoPrimitive''.' , Character lf asString ,
				'"' , Character tab asString , 
				((source copyFrom: i to: source size) copyReplaceAll: '"' with: '""') , 
				'"'.
		].
	].
	^source! !

!PharoGsExport methodsFor: 'methods' stamp: 'JamesFoster 7/16/2019 11:01'!
createMethodsFor: aClass

	| count path savedStream |
	count := 0.
	aClass isMeta ifTrue: [^count].
	"generate file-in for methods"
	path := 'classes/' , aClass name , '.gs'.
	fileStream nextPutAll: 'input '; nextPutAll: path; lf.
	savedStream := fileStream.
	[ 
		fileStream := path asFileReference writeStream.
		self 
			addMethodsHeaderFor: aClass;
			addClassCommentFor: aClass;
			yourself.
		count := count + (self addMethodsFor: aClass).
		count := count + (self addMethodsFor: aClass class).
		fileStream 
			nextPutAll: 'set compile_env: 0'; lf;
			nextPutAll: 'errorCount'; lf;
			nextPutAll: 'output pop'; lf;
			nextPutAll: 'errorCount'; lf;
			close.
	] ensure: [ 
		fileStream := savedStream.
	].
	^count
	! !


!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/16/2019 10:40'!
openFile

	fileStream := 'PharoGs.tpz' asFileReference writeStream
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'output push PharoGs.out only'; lf;
		nextPutAll: 'errorCount'; lf;
		yourself.
! !

!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/23/2019 11:13'!
doExport

	[ 
		self 
			installPackages;
			openFile;
			createGlobals;
			createPools;
			createClasses;
			createMethods;
			closeFile;
			yourself.
	] on: Error do: [:ex | 
		Transcript show: ex description; cr.
		ex signalerContext stack do: [:each |
			Transcript show: each; cr.
		].
	].
! !

!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/17/2019 17:25'!
createGlobals

	| dict |
	dict := Smalltalk globals reject: [ :each | each isBehavior].
	fileStream
		nextPutAll: '!! - create Pharo globals'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: 'Pharo'; lf.
	dict keys asSortedCollection do: [:eachKey | 
		fileStream 
			tab; nextPutAll: 'at: #'; 
			nextPutAll: eachKey; 
			nextPutAll: ' put: nil;';
			lf.
	].
	fileStream
		tab; nextPutAll: 'yourself.'; lf;
		nextPut: $%; lf;
		nextPutAll: 'errorCount'; lf;
		yourself.
	Transcript show: 'globals count = ' , dict size printString; cr.
! !

!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/23/2019 11:40'!
installPackages
	"Extra (or replacement) packages we want to be in GemStone"
	
	| list |
	list := OrderedCollection new
		"Extra packages to add to GemStone"
		add: '/Users/jfoster/code/pharo/src/' -> 'Monticello';
		add: '/Users/jfoster/code/pharo/src/' -> 'SUnit-Core';
		add: '/Users/jfoster/code/pharo/src/' -> 'Kernel-Tests';
		add: '/Users/jfoster/code/pharo/src/' -> 'Files-Tests';
		"Edited packages to load from the checkout (not yet in a bootstrap image) can be added as well"
		yourself.
	list do: [:each | | reader |
	    Transcript nextPutAll: 'Installing '; nextPutAll: each value; cr.
	    reader := TonelReader 
	        on: each key asFileReference
	        fileName: each value.
	    (MCSnapshot fromDefinitions: reader definitions) install.
	].
! !

!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/18/2019 08:25'!
createPools

	| list |
	list := SharedPool allSubclasses.
	fileStream
		nextPutAll: '!! - create Pharo SharedPool globals'; lf;
		nextPutAll: 'run'; lf;
		nextPutAll: 'Pharo'; lf.
	(list asSortedCollection: [ :a :b | a name <= b name ]) do: [:each | 
		fileStream 
			tab; nextPutAll: 'at: #'; 
			nextPutAll: each name; 
			nextPutAll: ' put: nil;';
			lf.
	].
	fileStream
		tab; nextPutAll: 'yourself.'; lf;
		nextPut: $%; lf;
		nextPutAll: 'errorCount'; lf;
		yourself.
	Transcript show: 'pools count = ' , list size printString; cr.
! !

!PharoGsExport methodsFor: 'export' stamp: 'JamesFoster 7/16/2019 12:53'!
closeFile

	fileStream 
		nextPutAll: 'errorCount'; lf;
		nextPutAll: 'output pop'; lf;
		nextPutAll: 'errorCount'; lf;
		close.
		! !
