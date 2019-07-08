Transcript show: 'method count = '.
[
| cr lf tab block stream1 reservedSelectors list count compileErrors keepGs |
cr := Character cr.
lf := Character lf.
tab := Character tab.
count := 0.
block := [:aString | aString collect: [:each | each == cr ifTrue: [lf] ifFalse: [each]]].
reservedSelectors := #(
	__inProtectedMode _and: _downTo:by:do: _downTo:do: _gsReturnNothingEnableEvents
	_gsReturnNoResult _isArray _isExceptionClass _isExecBlock _isFloat _isInteger
	_isNumber _isOneByteString _isRange _isRegexp _isRubyHash _isScaledDecimal
	_isSmallInteger _isSymbol _leaveProtectedMode _or: _stringCharSize ~~
	and: == ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: ifTrue:
	ifTrue:ifFalse: isKindOf: or: timesRepeat: to:by:do: to:do: untilFalse untilFalse: untilTrue
	untilTrue: whileFalse whileFalse: whileTrue whileTrue: repeat
).
compileErrors := 
#(
	#(#AdditionalMethodState #metaLinkOptions)
	#(#Behavior #metaLinkOptions)
	#('CCompilationContext class' #default)
	#(#ChunkReadStream #parseLangTagFor:)
	#(#Class #category)
	#('CompilationContext class' #default)
	#(#CompiledMethod #metaLinkOptions)
	#(#ConfigurationCommandLineHandler #printConfigurations:)
	#('Context class' #runSimulated:)
	#(#Form #fillAlpha:)
	#(#HashedCollection #metaLinkOptions)
	#('InstructionStream class' #compiler)
	#(#IRBytecodeDecompiler #jump:)
	#(#IRPrinterV2 #visitPopIntoRemoteTemp:)
	#(#IRPrinterV2 #visitPushRemoteTemp:)
	#(#IRPrinterV2 #visitStoreRemoteTemp:)
	#(#IRReconstructor #createTempVectorNamed:withVars:)
	#('Job class' #exampleCurrentValue)
	#(#MCMcdReader #loadPatch)
	#(#MCMczReader #loadDefinitions)
	#(#MCPatch #initializeWithBase:target:)
	#(#MCVersion #withAllDependenciesDo:)
	#('MetaLink class' #defaultOptions)
	#(#MethodDictionary #metaLinkOptions)
	#(#MultiByteFileStream #nextChunk)
	#(#MultiByteFileStream #nextPreamble)
	#(#Object #mustBeBooleanInMagic:)
	#('OpalCompiler class' #defaultOptions)
	#(#PharoChangesCondenser #reset)
	#(#Protocol #initialize)
	#(#ProtoObject #metaLinkOptions)
	#(#RBMethodNode #metaLinkOptions)
	#(#RBProgramNode #comments:)
	#(#ReflectiveMethod #metaLinkOptions)
	#(#Set #metaLinkOptions)
	#(#SmalltalkImage #metaLinkOptions)
	#(#SourceFileArray #readOnlyCopy)
	#(#StdioStream #next:)
	#(#String #withNoLineLongerThan:)
	#(#TemporaryVariable #hash)
	#('Text class' #fromString:)
	#('Text class' #initialize)
	#('TextFontChange class' #defaultFontChange)
	#(#Trait #category)
	#(#TraitDescription fileOutLocalMethodsInCategory:on:)
	#(#UIManager #spawnNewProcess)
	#(#UTF16TextConverter #nextFromStream:)
	#(#VirtualMachine #metaLinkOptions)
	#(#ZdcPluginSSLSession #serverName:)
	#(#ZnImageExampleDelegate #html)
).
keepGs := #(	"classes known to the VM that need to use the GemStone one"
	Array ArithmeticError Behavior BlockClosure Boolean ByteArray CannotReturn 
	Character Class ClassDescription Collection Error Exception ExceptionSet 
	Float FloatingPointException Halt Integer LargeInteger Magnitude Message 
	MessageNotUnderstood Metaclass Notification Number Object Pragma 
	ProcessorScheduler ScaledDecimal SequenceableCollection SmallInteger String 
	Symbol UndefinedObject Warning ZeroDivide
).
stream1 := (FileStream forceNewFileNamed: 'methods.gs') text
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output push methods.out only'; lf;
	nextPutAll: 'set compile_env: 0'; lf;
	nextPutAll: 'run'; lf;
	nextPutAll: '| symbolList |'; lf;
	nextPutAll: 'symbolList := GsSession currentSession symbolList.'; lf;
	nextPutAll: 'symbolList add: (symbolList remove: PharoPools) after: Smalltalk.'; lf;
	nextPut: $%; lf.
list := true
	ifTrue: [ProtoObject withAllSubclasses]
	ifFalse: [Array with: Object].
list do: [ :eachClass | 
	eachClass isMeta ifFalse: [ | path stream2 |
		path := 'classes/' , eachClass name , '.gs'.
		stream1 
			nextPutAll: 'input '; nextPutAll: path; lf.
		stream2 := (FileStream forceNewFileNamed: path) 
			text; 
			nextPutAll: 'output push output/';
			nextPutAll: eachClass name;
			nextPutAll: '.out only'; lf;
			nextPutAll: 'errorCount'; lf;
			nextPutAll: 'set compile_env: 0'; lf;
			nextPutAll: 'run'; lf;
			nextPutAll: 'Smalltalk at: #'; nextPutAll: eachClass name; lf;
			nextPut: $%; lf;
			nextPutAll: 'set class **'; lf;
			yourself.
		(keepGs includes: eachClass name) ifTrue: [
			stream2
				nextPutAll: 'set compile_env: 2'; lf;
				nextPutAll: 'category: ''GemStone'''; lf;
				nextPutAll: 'classMethod:'; lf;
				nextPutAll: 'pharoComment'; lf;
				nextPut: $^; 
				print: (block value: eachClass comment); lf;
				nextPut: $%; lf;
				yourself.
		] ifFalse: [
			stream2
				nextPutAll: 'run'; lf;
				nextPutAll: '(Smalltalk at: #'; nextPutAll: eachClass name;
				nextPutAll: ') comment: ';
				print: (block value: eachClass comment); lf;
				nextPutAll: '; yourself.'; lf;
				nextPut: $%; lf;
				nextPutAll: 'set compile_env: 2'; lf;
				yourself.
		].
		(Array with: eachClass class with: eachClass) do: [ :eachBehavior |
			| methodDict organization  |
			methodDict := eachBehavior methodDict.
			organization := eachBehavior organization.
			methodDict keys asSortedCollection do: [ :eachSelector | | category source i |
				category := organization categoryOfElement: eachSelector.
				source := block value: (methodDict at: eachSelector) sourceCode.
				i := source findString: '<primitive:'.
				0 < i ifTrue: [ | string inComment inString |
					inComment := false.
					inString := false.
					(string := source copyFrom: 1 to: i - 1) do: [:char | 
						char == $" ifTrue: [inComment := inComment not].
						(inComment not and: [char == $']) ifTrue: [inString := inString not].
					].
					(inComment not and: [inString not]) ifTrue: [
						source := string , '<pharoPrimitive>' , lf asString , 
							'self error: ''pharoPrimitive''.' , lf asString ,
							'"' , tab asString , 
							((source copyFrom: i to: source size) copyReplaceAll: '"' with: '""') , 
							'"'.
					].
				].
				(reservedSelectors includes: eachSelector) ifTrue: [
					eachSelector == #~~ ifTrue: [
						source := '_reservedSelector_tilde_tilde_:' , (source copyFrom: 3 to: source size)
					] ifFalse: [eachSelector == #== ifTrue: [
						source := '_reservedSelector_equal_equal_:' , (source copyFrom: 3 to: source size)
					] ifFalse: [
						source := '_reservedSelector_' , source.
					]].
				].
				(compileErrors anySatisfy: [:pair | 
					pair first = eachBehavior name and: [pair last == eachSelector]]) ifTrue: [
						| i |
						i :=  source indexOf: lf.
						(source at: i - 1) == $. ifTrue: [i := i - 1].
						source := (source copyFrom: 1 to: i - 1) , lf asString , 
							'	<compileError>' , lf asString , '"' , 
							((source copyFrom: i to: source size) copyReplaceAll: '"' with: '""') , '"'.
					].
				stream2 lf;
					nextPutAll: 'category: '''; nextPutAll: category; nextPut: $'; lf;
					nextPutAll: (eachBehavior isMeta ifTrue: ['classMethod: '] ifFalse: ['method: ']); lf.
				(source substrings: (Array with: lf)) do: [:eachLine | 
					| line |
					line := eachLine , ' '.
					(eachLine notEmpty and: [eachLine first == $%]) ifTrue: [stream2 space].
					eachClass == Class ifTrue: [	"can't use instVarNames as method argument"
						line := line copyReplaceAll: 'self instVarNames' with: '(self instVarNames)'.
						line := line copyReplaceAll: 'self class instVarNames' with: '(self class instVarNames)'.
						line := line copyReplaceAll: ' instVarNames ' with: ' instVarNameList '.
					].
					stream2 nextPutAll: line; lf.
				].
				stream2 nextPut: $%; lf.
				count := count + 1.
			].
		].
		stream2 
			nextPutAll: 'errorCount'; lf;
			nextPutAll: 'output pop'; lf;
			nextPutAll: 'errorCount'; lf;
			close.
	].
].
stream1 
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output pop'; lf;
	nextPutAll: 'errorCount'; lf;
	close.
Transcript show: count printString; cr.
] on: Error do: [:ex | 
	Transcript show: ex description; cr.
].
SmalltalkImage current snapshot: false andQuit: true.
