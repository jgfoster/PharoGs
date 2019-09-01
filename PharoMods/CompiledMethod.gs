set compile_env: 2

! copied from export of Pharo

category: 'source code management'
method: CompiledMethod
argumentNames
	<PharoGsError>
	self _gsError "
	^ self propertyAt: #argumentNames ifAbsent: [ super argumentNames ]"
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
asActiveRingDefinition
	<PharoGsError>
	self _gsError "
	""Retrieves an active RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the compiled method""
	
	^ RGMethodDefinition new 
			name: self selector;
			parentName: self methodClass name;
			isMetaSide: self methodClass isMeta;
			asActive.
"
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
asFullRingDefinition
	<PharoGsError>
	self _gsError "
	""Retrieves an active RGMethodDefinition which knows its parent <class>.
	Note that the full conversion does not happen at the level of the class. If you need that request asFullRingDefinition to the class""
	| rgClass rgMethod |
	rgClass := self realClass asRingDefinition.
	rgMethod := self asActiveRingDefinition.
	rgClass addMethod: rgMethod.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	^ rgMethod"
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
asHistoricalRingDefinition
	<PharoGsError>
	self _gsError "
	""Retrieves a historical RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the source file method""
	| ring |
	ring := (RGMethodDefinition named: self selector)
				parentName: self methodClass name;
				isMetaSide: self methodClass isMeta.
	self sourcePointer isZero
		ifTrue: [ ""this should not happen but sometimes the system looks corrupted""
			ring protocol: self category;
				sourceCode: self sourceCode;
				stamp: self timeStamp ]
		ifFalse: [ 
			ring sourcePointer: self sourcePointer ].
	ring asHistorical.	
	
	^ ring"
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
asPassiveRingDefinition
	<PharoGsError>
	self _gsError "
	""Retrieves a passive RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from value assigned in creation""
	
	^RGMethodDefinition new 
		 	name: self selector;
			parentName: self methodClass name;
			isMetaSide: self methodClass isMeta;
			protocol: self category;
			sourceCode: self sourceCode;
			stamp: self timeStamp;
			asPassive."
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
asRingDefinition
	<PharoGsError>
	self _gsError "
	""Retrieves an active RGMethodDefinition object based on the receiver.
	Note that its class is not converted.""
	
	^ self asActiveRingDefinition"
%

category: '*Reflectivity'
method: CompiledMethod
assignmentNodes
	<PharoGsError>
	self _gsError "
	^self ast assignmentNodes"
%

category: '*AST-Core'
method: CompiledMethod
ast
	<PharoGsError>
	self _gsError "
	""return an AST for this method. The AST is cached. see class comment of ASTCache""
	^ ASTCache at: self"
%

category: '*Reflectivity'
method: CompiledMethod
blockNodes
	<PharoGsError>
	self _gsError "
	^self ast blockNodes"
%

category: 'accessing-backward compatible'
method: CompiledMethod
category
	<PharoGsError>
	self _gsError "
	""Please favor protocol instead of category. We want to have method protocol and class package and tag = a category""
	^ self methodClass organization categoryOfElement: self selector"
%

category: 'accessing'
method: CompiledMethod
classBinding
	<PharoGsError>
	self _gsError "
	""answer the association to the class that I am installed in, or nil if none.""
	^self literalAt: self numLiterals"
%

category: 'accessing'
method: CompiledMethod
classBinding: aBinding
	<PharoGsError>
	self _gsError "
       ""sets the association to the class that I am installed in""
       ^self literalAt: self numLiterals put: aBinding."
%

category: 'accessing'
method: CompiledMethod
codeForNoSource
	<PharoGsError>
	self _gsError "
	""if everything fails, decompile the bytecode""
	""If there is no compiler, we cannot decompile it""	
	Smalltalk hasCompiler ifFalse: [ ^ nil ].
	
	 ^(self compiler decompileMethod: self) formattedCode"
%

category: '*AST-Core'
method: CompiledMethod
comments
	<PharoGsError>
	self _gsError "
	""Answer a string representing the comments in the method. Return an empty collection if the method's source code does not contain a comment.""
	^ self ast comments collect: [ :comment | comment contents ]"
%

category: '*Reflectivity'
method: CompiledMethod
compiledMethod
	<PharoGsError>
	self _gsError "
	^self"
%

category: 'source code management'
method: CompiledMethod
copyWithSource: aString
	<PharoGsError>
	self _gsError "
	^self copyWithTrailerBytes: (CompiledMethodTrailer new sourceCode: aString) "
%

category: 'initialization'
method: CompiledMethod
copyWithTrailerBytes: trailer
	<PharoGsError>
	self _gsError "
""Testing:
	(CompiledMethod compiledMethodAt: #copyWithTrailerBytes:)
		tempNamesPut: 'copy end '
""
	| copy end start penultimateLiteral |
	start := self initialPC.
	end := self endPC.
	copy := trailer createMethod: end - start + 1 class: self class header: self header.
	1 to: self numLiterals do: [:i | copy literalAt: i put: (self literalAt: i)].
	(penultimateLiteral := self penultimateLiteral) isMethodProperties ifTrue:
		[copy penultimateLiteral: (penultimateLiteral copy
									setMethod: copy;
									yourself)].
	start to: end do: [:i | copy at: i put: (self at: i)].
	^copy"
%

category: '*Reflectivity'
method: CompiledMethod
createTwin
	<PharoGsError>
	self _gsError "
	self reflectiveMethod: (ReflectiveMethod on: self)"
%

category: '*Debugging-Core'
method: CompiledMethod
debuggerMap
	<PharoGsError>
	self _gsError "
	^self compilerClass debuggerMethodMapForMethod: self."
%

category: '*opalcompiler-core'
method: CompiledMethod
decompile
	<PharoGsError>
	self _gsError "
	^Smalltalk globals 
		at: #FBDDecompiler
		ifPresent: [ :decompiler | decompiler new  decompile: self]  
		ifAbsent: [ RBParser parseMethod: self selector asMethodPreamble, '
	self noSourceAvailable' ]"
%

category: '*opalcompiler-core'
method: CompiledMethod
decompileIR
	<PharoGsError>
	self _gsError "
	
	^ IRBytecodeDecompiler new decompile: self"
%

category: 'accessing'
method: CompiledMethod
defaultSelector 
	<PharoGsError>
	self _gsError "
	""Invent and answer an appropriate message selector (a Symbol) for me, 
	that is, one that will parse with the correct number of arguments.""
	^ #DoIt numArgs: self numArgs"
%

category: '*Reflectivity'
method: CompiledMethod
destroyTwin
	<PharoGsError>
	self _gsError "
	self reflectiveMethod: nil"
%

category: 'source code management'
method: CompiledMethod
embeddSourceInTrailer
	<PharoGsError>
	self _gsError "
	""When receiver is deinstalled from its class, its not managed anymore by development tools
	and it's hard to predict, how long a method could stay in the image, because if it contains blocks,
	they could still reference it. 
	Therefore we trying to preserve as much as we can , actually by embedding the method's source code into its trailer
	""
	self trailer hasSourcePointer ifTrue: [
		^self becomeForward: (self copyWithSource: self sourceCode)]
"
%

category: '*UnifiedFFI'
method: CompiledMethod
ffiArgumentNames
	<PharoGsError>
	self _gsError "
	""Answer the method's argument names. We using a separate method, 
	to get arg names not from source code directly, but from method properties, collected at
	compile time. Useful, when there is no source code available (for some reason)""
	
	^ self 
		propertyValueAt: #ffiArgumentNames 
		ifAbsent: [ self  propertyValueAt: #ffiArgumentNames put: self argumentNames ]
"
%

category: '*AST-Core'
method: CompiledMethod
firstComment
	<PharoGsError>
	self _gsError "
	""Answer a string representing the first comment in the method associated with selector. Return an empty string if the method's source code does not contain a comment.""
	^ self comments ifEmpty: [ #() ] ifNotEmpty: [ :comments | comments first ]"
%

category: 'accessing'
method: CompiledMethod
flushCache
	<PharoGsError>
	self _gsError "
	""Tell the virtual machine to remove all references to this method from its method lookup caches, and to discard any optimized version 	of the method, if it has any of these.  This must be done whenever a method is modified in place, such as modifying its literals or 	machine code, to reflect the revised code.  c.f. Behavior>>flushCache & Symbol>>flushCache.  Essential.	 See MethodDictionary class 	comment.""
	<PharoPrimitive>
self @env0:error: 'PharoPrimitive'.
""	<primitive: 116>
"""
%

category: 'source code management'
method: CompiledMethod
getPreambleFrom: aFileStream at: position
	<PharoGsError>
	self _gsError "
	^ SourceFiles getPreambleFrom: aFileStream at: position"
%

category: 'source code management'
method: CompiledMethod
getSourceFromFile
	<PharoGsError>
	self _gsError "
	""PLEASE Note: clients should always call #sourceCode""
	""Read the source code from file, determining source file index and
	file position from the last 3 bytes of this method.""
	^ [ SourceFiles sourceCodeAt: self sourcePointer ] on: Error do: [ '' ]"
%

category: 'source code management'
method: CompiledMethod
getSourceReplacingSelectorWith: newSelector
	<PharoGsError>
	self _gsError "
	| oldKeywords newKeywords source newSource oldSelector start |
	source := self sourceCode.
	
	source ifNil: [ ^ nil ].
	
	oldSelector := self selector.
	oldSelector = newSelector ifTrue: [ ^ source ].
	
	oldKeywords := oldSelector keywords.
	newKeywords := (newSelector ifNil: [self defaultSelector]) keywords.
	[oldKeywords size = newKeywords size] assert.
	
	newSource := source.
	start := 1.
	oldKeywords with: newKeywords do: [:oldKey :newKey| |pos|
		pos := newSource findString: oldKey startingAt: start .
		newSource := newSource copyReplaceFrom: pos to: (pos + oldKey size -1) with: newKey.
		start := pos + newKey size ].
	^newSource"
%

category: '*Reflectivity'
method: CompiledMethod
hasBreakpoint
	<PharoGsError>
	self _gsError "
	^ Breakpoint isInstalledIn: self
	
	"
%

category: '*Reflectivity'
method: CompiledMethod
hasLinks
	<PharoGsError>
	self _gsError "
	self 
		deprecated: 'use #hasMetaLinks'
		transformWith:  '`@receiver hasLinks' -> '`@receiver hasMetaLinks'.
	^self hasMetaLinks"
%

category: '*Reflectivity'
method: CompiledMethod
hasMetaLinks
	<PharoGsError>
	self _gsError "
	self reflectiveMethod ifNil: [ ^false ].
	^self reflectiveMethod hasMetaLinks. "
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
hasPragmaNamed: aSymbol
	<PharoGsError>
	self _gsError "
	^ self pragmas anySatisfy: [ :pragma | pragma keyword = aSymbol ]"
%

category: 'testing'
method: CompiledMethod
hasProperties
	<PharoGsError>
	self _gsError "
	^ self penultimateLiteral isMethodProperties"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
hasProperty: aKey
	<PharoGsError>
	self _gsError "
	self propertyAt: aKey ifAbsent: [ ^false ].
	^true."
%

category: 'accessing'
method: CompiledMethod
hasSourceCode
	<PharoGsError>
	self _gsError "
	""Retrieve or reconstruct the source code for this method.""
	| trailer source |
	trailer := self trailer.
	trailer sourceCode ifNotNil: [:code | ^ true ].
	trailer hasSourcePointer ifFalse: [^ false].
	""Situation normal;  read the sourceCode from the file""
	source := [self getSourceFromFile]
				on: Error
				do: [ :ex | ex return: nil].
	source isEmptyOrNil ifTrue: [^ false].			
	^ true"
%

category: 'testing'
method: CompiledMethod
hasSourcePointer
	<PharoGsError>
	self _gsError "
	^ self trailer hasSourcePointer"
%

category: '*Slot-Core'
method: CompiledMethod
hasTemporaryVariableNamed: aName
	<PharoGsError>
	self _gsError "
	^(self tempNames includes: aName)
	"
%

category: '*System-Support'
method: CompiledMethod
implementors
	<PharoGsError>
	self _gsError "
	^ SystemNavigation default allImplementorsOf: self selector"
%

category: '*Reflectivity'
method: CompiledMethod
installLink: aMetaLink
	<PharoGsError>
	self _gsError "
	self reflectiveMethod ifNil: [ self createTwin ].
	self reflectiveMethod increaseLinkCount.
	(aMetaLink optionCompileOnLinkInstallation or: [ self isRealPrimitive ])
		ifTrue: [ self reflectiveMethod compileAndInstallCompiledMethod ]
		ifFalse: [ self invalidate ].
	aMetaLink announceChange.
	"
%

category: '*Reflectivity'
method: CompiledMethod
instanceVariableReadNodes
	<PharoGsError>
	self _gsError "
	^self ast instanceVariableReadNodes"
%

category: '*Reflectivity'
method: CompiledMethod
instanceVariableWriteNodes
	<PharoGsError>
	self _gsError "
	^self ast instanceVariableWriteNodes"
%

category: '*Reflectivity'
method: CompiledMethod
invalidate
	<PharoGsError>
	self _gsError "
	| reflectiveMethod |
	self reflectivityDisabled ifTrue: [ ^self ].
	
	reflectiveMethod := self reflectiveMethod.
	reflectiveMethod ifNil: [^self ""do nothing""].
	(self isRealPrimitive or: (reflectiveMethod ast metaLinkOptionsFromClassAndMethod includes: #optionCompileOnLinkInstallation))
					ifTrue: [reflectiveMethod compileAndInstallCompiledMethod ] 
					ifFalse: [reflectiveMethod installReflectiveMethod]
	"
%

category: '*opalcompiler-core'
method: CompiledMethod
ir
	<PharoGsError>
	self _gsError "
	""We as the AST for the IR... for decompiling ir from bytecode, look at IRBytecodeDecompiler""
	^ self ast ir"
%

category: '*opalcompiler-core'
method: CompiledMethod
irPrimitive
	<PharoGsError>
	self _gsError "
	| primNode n |
	primNode := IRPrimitive new num: (n := self primitive).
	(n = 117 or: [n = 120]) ifTrue: [
		primNode spec: (self literalAt: 1)].
	^ primNode"
%

category: 'testing'
method: CompiledMethod
isAbstract
	<PharoGsError>
	self _gsError "
	""Answer true if I am abstract""
	
	| marker |
	marker := self markerOrNil ifNil: [^false].
	
	^marker == self class subclassResponsibilityMarker 
		or: [ marker == self class explicitRequirementMarker ]
"
%

category: 'testing'
method: CompiledMethod
isBinarySelector
	<PharoGsError>
	self _gsError "
	^self selector
		allSatisfy: [:each | each isSpecial]"
%

category: 'testing'
method: CompiledMethod
isCompiledMethod
	<PharoGsError>
	self _gsError "
	^ true"
%

category: 'testing'
method: CompiledMethod
isConflict
	<PharoGsError>
	self _gsError "
	^ self markerOrNil == self class conflictMarker"
%

category: '*rpackage-core'
method: CompiledMethod
isDefinedInPackage: anRPackage
	<PharoGsError>
	self _gsError "
	^ anRPackage includesDefinedSelector: self selector ofClass: self methodClass"
%

category: 'testing'
method: CompiledMethod
isDeprecated
	<PharoGsError>
	self _gsError "
	""Object selectorsInProtocol: #deprecation""
	(self
		sendsAnySelectorOf:
			#(#deprecated: #deprecated:on:in: #deprecated:on:in:transformWith: #deprecated:transformWith:))
		ifTrue: [ ^ true ].
	$-
		split: self protocol asString
		do: [ :each | 
			each withBlanksCondensed = 'deprecated'
				ifTrue: [ ^ true ] ].
	^ false"
%

category: 'testing'
method: CompiledMethod
isDisabled
	<PharoGsError>
	self _gsError "
	^ self isDisabled: self markerOrNil"
%

category: 'testing'
method: CompiledMethod
isDisabled: marker
	<PharoGsError>
	self _gsError "
	^ marker == self class disabledMarker"
%

category: 'testing'
method: CompiledMethod
isDoIt
	<PharoGsError>
	self _gsError "
	^self selector isDoIt."
%

category: '*SUnit-Core'
method: CompiledMethod
isErrorTest
	<PharoGsError>
	self _gsError "
	""Is the receiver a test method that raised an error?""
	^ self methodClass isTestCase
		and: [ self methodClass methodRaisedError: self selector ]"
%

category: 'testing'
method: CompiledMethod
isExplicitlyRequired
	<PharoGsError>
	self _gsError "
	^ self isExplicitlyRequired: self markerOrNil"
%

category: 'testing'
method: CompiledMethod
isExplicitlyRequired: marker
	<PharoGsError>
	self _gsError "
	^ marker == self class explicitRequirementMarker"
%

category: '*rpackage-core'
method: CompiledMethod
isExtension
	<PharoGsError>
	self _gsError "
	^ self origin package ~= self package"
%

category: '*rpackage-core'
method: CompiledMethod
isExtensionInPackage: anRPackage
	<PharoGsError>
	self _gsError "
	^ anRPackage includesExtensionSelector: self selector ofClass: self methodClass"
%

category: '*UnifiedFFI'
method: CompiledMethod
isFFIMethod
	<PharoGsError>
	self _gsError "
	
	^ self hasProperty: #isFFIMethod"
%

category: '*SUnit-Core'
method: CompiledMethod
isFailedTest
	<PharoGsError>
	self _gsError "
	""Is the receiver a test method that failed?""
	^ self methodClass isTestCase
		and: [ self methodClass methodFailed: self selector ]"
%

category: 'testing'
method: CompiledMethod
isFromTrait
	<PharoGsError>
	self _gsError "
	""Return true for methods that have been included from Traits""
	^ self origin isTrait and: [ self origin ~= self methodClass ]"
%

category: 'testing'
method: CompiledMethod
isInstalled
	<PharoGsError>
	self _gsError "
	self methodClass ifNotNil:
		[:class|
		self selector ifNotNil:
			[:selector|
			^self == (class compiledMethodAt: selector ifAbsent: [])]].
	^false"
%

category: 'testing'
method: CompiledMethod
isOverridden
	<PharoGsError>
	self _gsError "
	| selector| 
	selector := self selector.
	self methodClass allSubclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
	"
%

category: '*SUnit-Core'
method: CompiledMethod
isPassedTest
	<PharoGsError>
	self _gsError "
	""Is the receiver a test method that passed?""
	^ self methodClass isTestCase
		and: [ self methodClass methodPassed: self selector ]"
%

category: 'testing'
method: CompiledMethod
isProvided
	<PharoGsError>
	self _gsError "
	^ self isProvided: self markerOrNil"
%

category: 'testing'
method: CompiledMethod
isProvided: marker
	<PharoGsError>
	self _gsError "
	marker ifNil: [^ true].
	^ (self isRequired: marker) not and: [(self isDisabled: marker) not]"
%

category: 'testing'
method: CompiledMethod
isRequired
	<PharoGsError>
	self _gsError "
	^ self isRequired: self markerOrNil"
%

category: 'testing'
method: CompiledMethod
isRequired: marker
	<PharoGsError>
	self _gsError "
	marker ifNil: [^ false].
	(self isExplicitlyRequired: marker) ifTrue: [^ true]. 
	(self isSubclassResponsibility: marker) ifTrue: [^ true]. 
	^ false"
%

category: 'testing'
method: CompiledMethod
isReturnField
	<PharoGsError>
	self _gsError "
	""Answer whether the receiver is a quick return of an instance variable.""
	^ self primitive between: 264 and: 519"
%

category: 'testing'
method: CompiledMethod
isReturnSelf
	<PharoGsError>
	self _gsError "
	""Answer whether the receiver is a quick return of self.""
	^ self primitive = 256"
%

category: 'testing'
method: CompiledMethod
isReturnSpecial
	<PharoGsError>
	self _gsError "
	""Answer whether the receiver is a quick return of self or constant.""
	^ self primitive between: 256 and: 263"
%

category: 'printing'
method: CompiledMethod
isSelfEvaluating
	<PharoGsError>
	self _gsError "
	^self methodClass notNil and: [self isDoIt not]"
%

category: 'testing'
method: CompiledMethod
isSubclassResponsibility
	<PharoGsError>
	self _gsError "
	^ self isSubclassResponsibility: self markerOrNil"
%

category: 'testing'
method: CompiledMethod
isSubclassResponsibility: marker
	<PharoGsError>
	self _gsError "
	^ marker == self class subclassResponsibilityMarker"
%

category: 'testing'
method: CompiledMethod
isTaggedWith: aSymbol
	<PharoGsError>
	self _gsError "
	""Not methods tags implemented by protocols. Look #tags comment""
	^self protocol == aSymbol"
%

category: '*SUnit-Core'
method: CompiledMethod
isTestMethod
	<PharoGsError>
	self _gsError "
	self numArgs isZero
		ifFalse: [ ^ false ].
	""unary selectors starting with 'should' are supposed to be treated as test methods too""
	((self selector beginsWith: 'test') or: [ self selector beginsWith: 'should' ])
		ifFalse: [ ^ false ].
	""Is the receiver a TestCase test method?""
	self methodClass isTestCase
		ifFalse: [ ^ false ].
	^true"
%

category: 'source code management'
method: CompiledMethod
linesOfCode
	<PharoGsError>
	self _gsError "
	""An approximate measure of lines of code.
	Includes comments, but excludes empty lines.""
	| lines |
	lines := 0.
	self sourceCode lineIndicesDo: [:start :endWithoutDelimiters :end |
		endWithoutDelimiters > start ifTrue: [lines := lines+1]].
	^lines"
%

category: 'private'
method: CompiledMethod
markerOrNil
	<PharoGsError>
	self _gsError "
	^ self encoderClass markerOrNilFor: self"
%

category: '*Reflectivity'
method: CompiledMethod
metaLinkOptions
	<PharoGsError>
	self _gsError "
	^{
	#penultimateLiteral -> #(  #+ optionCompileOnLinkInstallation).
	#selector -> #(  #+ optionCompileOnLinkInstallation).
	#objectAt: -> #(  #+ optionCompileOnLinkInstallation).
	#header -> #(  #+ optionCompileOnLinkInstallation).
	#numLiterals -> #(  #+ optionCompileOnLinkInstallation).
	#literalAt: -> #(  #+ optionCompileOnLinkInstallation).
	#metaLinkOptions -> #(  #+ optionCompileOnLinkInstallation)
	}"
%

category: 'accessing'
method: CompiledMethod
method
	<PharoGsError>
	self _gsError "
	""polymorphic with closure""
	^ self"
%

category: 'accessing'
method: CompiledMethod
methodClass
	<PharoGsError>
	self _gsError "
	""answer the class that I am installed in""
	^self classBinding value"
%

category: 'accessing'
method: CompiledMethod
methodClass: aClass
	<PharoGsError>
	self _gsError "
	""set the class binding in the last literal to aClass""
	^self numLiterals > 0
		ifTrue: [ self literalAt: self numLiterals put: aClass binding ]	"
%

category: '*OpalCompiler-Core'
method: CompiledMethod
methodNode
	<PharoGsError>
	self _gsError "
	""returns an AST for this method, do not cache it. (see #ast for the cached alternative)""
	^self parseTree"
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
methodReference
	<PharoGsError>
	self _gsError "
	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^(RGMethodDefinition realClass: class selector: selector) 
		package: self package asRingDefinition; yourself
	"
%

category: 'accessing'
method: CompiledMethod
name
	<PharoGsError>
	self _gsError "
	^ self printString"
%

category: 'accessing'
method: CompiledMethod
numberOfReservedLiterals
	<PharoGsError>
	self _gsError "
	^ 2"
%

category: 'accessing'
method: CompiledMethod
origin
	<PharoGsError>
	self _gsError "
	^ self methodClass findOriginClassOf: self"
%

category: 'accessing'
method: CompiledMethod
originMethod
	<PharoGsError>
	self _gsError "
	^ self methodClass findOriginMethodOf: self.
	
"
%

category: '*rpackage-core'
method: CompiledMethod
package
	<PharoGsError>
	self _gsError "
	^ self packageFromOrganizer: RPackage organizer"
%

category: '*rpackage-core'
method: CompiledMethod
packageFromOrganizer: anRPackageOrganizer
	<PharoGsError>
	self _gsError "
	| originSelector |
	""This method returns the package this method belongs to.  
	It takes into account classes and traits.  
	If the method is in no package, returns nil by now""
	self flag: 'TODO: use anRPackageOrganizer, or better delegate to anRPackageOrganizer'.
	originSelector := self originMethod selector.
	
	^self origin packages 
		detect: [ :each | 
			self origin isMeta
				ifFalse: [ each includesSelector: originSelector ofClassName: self origin instanceSide originalName]
				ifTrue: [ each includesSelector: originSelector ofMetaclassName: self origin instanceSide originalName]] 
		ifNone: [ nil ]"
%

category: '*AST-Core'
method: CompiledMethod
parseTree
	<PharoGsError>
	self _gsError "
	""returns an AST for this method, do not cache it. (see #ast for the cached alternative)""
	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | 
			^ self decompile ]) methodClass: self methodClass."
%

category: 'private'
method: CompiledMethod
penultimateLiteral
	<PharoGsError>
	self _gsError "
	""Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector).""
	| pIndex |
	^(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex]
		ifFalse: [nil]"
%

category: 'private'
method: CompiledMethod
penultimateLiteral: anObject
	<PharoGsError>
	self _gsError "
	""Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector).""
	| pIndex |
	(pIndex := self numLiterals - 1) > 0 
		ifTrue: [self literalAt: pIndex put: anObject]
		ifFalse: [self error: 'insufficient literals']"
%

category: 'copying'
method: CompiledMethod
postCopy
	<PharoGsError>
	self _gsError "
	| penultimateLiteral |
	(penultimateLiteral := self penultimateLiteral) isMethodProperties ifTrue:
		[self penultimateLiteral: (penultimateLiteral copy
									setMethod: self;
									yourself).
		 self penultimateLiteral pragmas do:
			[:p| p setMethod: self]]
"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
pragmaAt: aKey
	<PharoGsError>
	self _gsError "
	""Answer the pragma with selector aKey, or nil if none.""
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector at: aKey ifAbsent: [nil]]
		ifFalse: [nil]"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
pragmas
	<PharoGsError>
	self _gsError "
	| selectorOrProperties |
	^(selectorOrProperties := self penultimateLiteral) isMethodProperties
		ifTrue: [selectorOrProperties pragmas]
		ifFalse: [#()]"
%

category: 'debugger support'
method: CompiledMethod
prepareForSimulationWith: numArgs
	<PharoGsError>
	self _gsError "
	""This method changes the argument count of a CompiledMethod header to numArgs, its temporary count to numArgs + 1 and change the code handling primitive error to store the error code in the unique temporary of the method""
			
	| newHeader |
	newHeader := (((self header bitAnd: 2r01110000000000111111111111111111) 
			bitOr: (numArgs bitShift: 24))
			bitOr: (numArgs + 1 bitShift: 18)).
	newHeader := newHeader + (self class headerFlagForEncoder: self encoderClass).
	self objectAt: 1 put: newHeader.
	
	self encoderClass prepareMethod: self forSimulationWith: numArgs"
%

category: 'accessing'
method: CompiledMethod
primitive
	<PharoGsError>
	self _gsError "
	""Answer the primitive index associated with the receiver.
	 Zero indicates that this is not a primitive method.""
	| initialPC |
	^(self header anyMask: 65536) ""Is the hasPrimitive? flag set?""
		ifTrue: [(self at: (initialPC := self initialPC) + 1) + ((self at: initialPC + 2) bitShift: 8)]
		ifFalse: [0]"
%

category: 'printing'
method: CompiledMethod
printOn: aStream 
	<PharoGsError>
	self _gsError "
	""Overrides method inherited from the byte arrayed collection.""
	aStream print: self methodClass; nextPutAll: '>>'; print: self selector."
%

category: 'printing'
method: CompiledMethod
printPrimitiveOn: aStream
	<PharoGsError>
	self _gsError "
	""Print the primitive on aStream""
	| primDecl |
	self isPrimitive ifFalse: [ ^self ].
	self isExternalCallPrimitive ifTrue:
		[^aStream print: (self literalAt: 1); cr].
	aStream nextPutAll: '<primitive: '.
	self isNamedPrimitive
		ifTrue:
			[primDecl := self literalAt: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: self primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; cr"
%

category: 'accessing'
method: CompiledMethod
properties
	<PharoGsError>
	self _gsError "
	""Answer the method properties of the receiver.""
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector]
		ifFalse: [self class methodPropertiesClass forMethod: self selector: propertiesOrSelector]"
%

category: 'accessing'
method: CompiledMethod
properties: aMethodProperties
	<PharoGsError>
	self _gsError "
	""Set the method-properties of the receiver to aMethodProperties.""
	self literalAt: self numLiterals - 1
		put: (aMethodProperties isEmpty
				ifTrue: [aMethodProperties selector]
				ifFalse: [aMethodProperties
							setMethod: self;
							yourself])"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
propertyAt: propName
	<PharoGsError>
	self _gsError "
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: [nil]]
		ifFalse: [nil]"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
propertyAt: propName ifAbsent: aBlock
	<PharoGsError>
	self _gsError "
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: aBlock]
		ifFalse: [aBlock value]"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
propertyAt: aKey ifAbsentPut: aBlock
	<PharoGsError>
	self _gsError "
	""Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value.""
	
	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
propertyAt: propName put: propValue
	<PharoGsError>
	self _gsError "
	""Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property.""
	| propertiesOrSelector |
	(propertiesOrSelector := self penultimateLiteral) isMethodProperties ifFalse:
		[self penultimateLiteral: ((self class methodPropertiesClass
									selector: propertiesOrSelector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
		^propValue].
	(propertiesOrSelector includesProperty: propName) ifTrue:
		[^propertiesOrSelector at: propName put: propValue].
	self penultimateLiteral: (propertiesOrSelector
								copyWith: (Association
												key: propName asSymbol
												value: propValue)).
	^propValue"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
propertyKeysAndValuesDo: aBlock
	<PharoGsError>
	self _gsError "
	""Enumerate the receiver with all the keys and values.""
	| propertiesOrSelector |
	(propertiesOrSelector := self penultimateLiteral) isMethodProperties ifTrue:
		[propertiesOrSelector propertyKeysAndValuesDo: aBlock]"
%

category: 'accessing-properties - compatibility'
method: CompiledMethod
propertyValueAt: propName
	<PharoGsError>
	self _gsError "
	""use the version without ..Value, this methid is retained for compatibility""
	^self propertyAt: propName"
%

category: 'accessing-properties - compatibility'
method: CompiledMethod
propertyValueAt: propName ifAbsent: aBlock
	<PharoGsError>
	self _gsError "
	""use the version without ..Value, this method is retained for compatibility""
	^self propertyAt: propName ifAbsent: aBlock"
%

category: 'accessing-properties - compatibility'
method: CompiledMethod
propertyValueAt: propName put: propValue
	<PharoGsError>
	self _gsError "
	""use the version without ..Value, this method is retained for compatibility""
	^self propertyAt: propName put: propValue"
%

category: 'accessing'
method: CompiledMethod
protocol
	<PharoGsError>
	self _gsError "
	""Return in which protocol (conceptual groups of methods) the receiver is grouped into.""
	^ self category"
%

category: 'accessing'
method: CompiledMethod
protocol: aString
	<PharoGsError>
	self _gsError "
	^ self methodClass organization classify: self selector under: aString"
%

category: '*System-Sources'
method: CompiledMethod
putSource: sourceStr class: class category: catName withStamp: changeStamp priorMethod: priorMethod
	<PharoGsError>
	self _gsError "
	^ self
		putSource: sourceStr
		withPreamble: [ :file | 
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod.
			file cr ]"
%

category: '*System-Sources'
method: CompiledMethod
putSource: source withPreamble: preambleBlock
	<PharoGsError>
	self _gsError "
	""Store the source code for the receiver on an external file.""
	SourceFiles 
		writeSource: source 
		preamble: (String streamContents: preambleBlock)
		onSuccess: [ :newSourcePointer | 
			""Method chunk needs a final ! !""
			SourceFiles changesWriteStreamDo: [ :file | 
				(ChunkWriteStream on: file) nextPut: ' ' ].
			""Update with new source pointer""
			self setSourcePointer: newSourcePointer ]
		onFail: [ 
			self becomeForward: (self copyWithSource: source) ]
"
%

category: 'scanning'
method: CompiledMethod
readsField: varIndex 
	<PharoGsError>
	self _gsError "
	""Answer whether the receiver loads the instance variable indexed by the argument.""
	self isReturnField ifTrue: [^self returnField = (varIndex - 1)].
	^ super readsField: varIndex "
%

category: '*Ring-Deprecated-Core-Kernel'
method: CompiledMethod
realClass
	<PharoGsError>
	self _gsError "
	""answer the class that I am installed in""
	
	^ self methodClass"
%

category: '*opalcompiler-core'
method: CompiledMethod
recompile
	<PharoGsError>
	self _gsError "
	^ self methodClass recompile: self selector"
%

category: '*System-Support'
method: CompiledMethod
referencedClasses
	<PharoGsError>
	self _gsError "
	""Return classes that are directly referenced by this method""
	^ self literals
		select: [ :l | l value isClass ] 
		thenCollect: [:v | v value ]."
%

category: '*Reflectivity'
method: CompiledMethod
reflectiveMethod
	<PharoGsError>
	self _gsError "
	^self propertyAt: #reflectiveMethod ifAbsent: nil"
%

category: '*Reflectivity'
method: CompiledMethod
reflectiveMethod: aReflectiveMethod
	<PharoGsError>
	self _gsError "
	self propertyAt: #reflectiveMethod put: aReflectiveMethod"
%

category: '*Reflectivity'
method: CompiledMethod
reflectivityDisabled
	<PharoGsError>
	self _gsError "
	^self hasPragmaNamed: #disableReflectivity"
%

category: '*opalcompiler-core'
method: CompiledMethod
reformat
	<PharoGsError>
	self _gsError "
	self methodClass compile: self ast formattedCode classified:  self category."
%

category: 'initialization'
method: CompiledMethod
removeFromSystem
	<PharoGsError>
	self _gsError "
	^ self methodClass removeSelector: self selector"
%

category: '*Reflectivity'
method: CompiledMethod
removeLink: aLink
	<PharoGsError>
	self _gsError "
	self reflectiveMethod ifNotNil: [:rm | rm removeLink: aLink]."
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
removeProperty: propName
	<PharoGsError>
	self _gsError "
	""Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing.""
	| value |
	value := self propertyAt: propName ifAbsent: [^nil].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value"
%

category: 'accessing-pragmas & properties'
method: CompiledMethod
removeProperty: propName ifAbsent: aBlock
	<PharoGsError>
	self _gsError "
	""Remove the property propName if it exists.
	 Answer the evaluation of aBlock if the property is missing.""
	| value |
	value := self propertyAt: propName ifAbsent: [^aBlock value].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value"
%

category: '*Slot-Core'
method: CompiledMethod
removeSavedTemp: aTemp
	<PharoGsError>
	self _gsError "
	self savedTemps ifNotNil: [ :saved | 
			saved remove: aTemp.
			saved ifEmpty: [ self removeProperty: #savedTemps ] ]"
%

category: 'accessing'
method: CompiledMethod
returnField
	<PharoGsError>
	self _gsError "
	""Answer the index of the instance variable returned by a quick return 
	method.""
	| prim |
	prim := self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]"
%

category: '*Slot-Core'
method: CompiledMethod
saveTemp: aTemp
	<PharoGsError>
	self _gsError "
	self savedTemps ifNil: [ self propertyAt: #savedTemps put: Set new].
	self savedTemps add: aTemp."
%

category: '*Slot-Core'
method: CompiledMethod
savedTemps
	<PharoGsError>
	self _gsError "
	^self propertyAt: #savedTemps ifAbsent: nil"
%

category: 'accessing'
method: CompiledMethod
selector
	<PharoGsError>
	self _gsError "
	""Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal.""
	| penultimateLiteral | 
	^(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector]
		ifFalse: [penultimateLiteral]"
%

category: 'accessing'
method: CompiledMethod
selector: aSelector
	<PharoGsError>
	self _gsError "
	""Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal.""
	| penultimateLiteral nl | 
	Symbol internSelector:  aSelector.
	(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector: aSelector]
		ifFalse: [(nl := self numLiterals) < 2 ifTrue:
					[self error: 'insufficient literals to hold selector'].
				self literalAt: nl - 1 put: aSelector]"
%

category: '*Reflectivity'
method: CompiledMethod
sendNodes
	<PharoGsError>
	self _gsError "
	^self ast sendNodes"
%

category: '*System-Support'
method: CompiledMethod
senders
	<PharoGsError>
	self _gsError "
	^ SystemNavigation default allSendersOf: self selector"
%

category: 'source code management'
method: CompiledMethod
setSourcePointer: srcPointer
	<PharoGsError>
	self _gsError "
	""We can't change the trailer of existing method, since it could have completely different format. 	
	Therefore we need to generate a copy with new trailer, containing scrPointer, and then become it.""
	| trailer copy |
	trailer := CompiledMethodTrailer new sourcePointer: srcPointer.
	copy := self copyWithTrailerBytes: trailer.
	""If possible do a replace in place as an optimization""
	(self trailer class == trailer class and: [ self size = copy size ])
		ifTrue: [ 
			| start |
			start := self endPC + 1.
			self replaceFrom: start to: self size with: copy startingAt: start ]
		ifFalse: [ self becomeForward: copy ].
	^ self
"
%

category: 'accessing'
method: CompiledMethod
sourceCode
	<PharoGsError>
	self _gsError "
	""Retrieve or reconstruct the source code for this method.""
	| trailer |
	trailer := self trailer.
	trailer sourceCode ifNotNil: [:code | ^ code ].
	trailer hasSourcePointer ifFalse: [ ^ self codeForNoSource ].
	^ self getSourceFromFile ifEmpty: [ self codeForNoSource ]"
%

category: '*opalcompiler-core'
method: CompiledMethod
sourceNode
	<PharoGsError>
	self _gsError "
	^self ast"
%

category: '*opalcompiler-core'
method: CompiledMethod
sourceNodeExecutedForPC: aPC
	<PharoGsError>
	self _gsError "
	^self sourceNode sourceNodeExecutedForPC: aPC"
%

category: '*opalcompiler-core'
method: CompiledMethod
sourceNodeForPC: aPC
	<PharoGsError>
	self _gsError "
	^self sourceNode sourceNodeForPC: aPC"
%

category: 'source code management'
method: CompiledMethod
sourcePointer
	<PharoGsError>
	self _gsError "
	""Answer the integer which can be used to find the source file and position for this method.
	The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles.""
	^ self trailer sourcePointer
"
%

category: 'printing'
method: CompiledMethod
storeOn: aStream
	<PharoGsError>
	self _gsError "
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' newMethod: '.
	aStream store: self size - self initialPC + 1.
	aStream nextPutAll: ' header: '.
	aStream store: self header.
	aStream nextPut: $).
	noneYet := self storeElementsFrom: self initialPC to: self endPC on: aStream.
	1 to: self numLiterals do:
		[:index |
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' literalAt: '.
		aStream store: index.
		aStream nextPutAll: ' put: '.
		aStream store: (self literalAt: index)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)"
%

category: 'accessing-tags'
method: CompiledMethod
tagWith: aSymbol
	<PharoGsError>
	self _gsError "
	""Any method could be tagged with multiple symbols for user purpose. 
	This method should apply new tag. All existing tags should not be changed. 
	But now we could only implemented tags with protocols. 	
	So tagging method with tag removes all existing tags and add new one. 
	It should not be problem with single tag scenario which are now defined by single protocol""
	
	self protocol: aSymbol"
%

category: 'accessing-tags'
method: CompiledMethod
tags
	<PharoGsError>
	self _gsError "
	""Any method could be tagged with multiple symbols for user purpose. 
	For now we only define API to manage them implemented on top of method protocols.
	Protocol unclassified means that method is not tagged by anything""
	| protocol |
	protocol := self protocol.
	protocol ifNil: [ ^#() ].
	protocol = Protocol unclassified ifTrue: [ ^#() ].
	^{protocol}"
%

category: 'source code management'
method: CompiledMethod
tempNames
	<PharoGsError>
	self _gsError "
	""on the level of the compiled method, tempNames includes argument names""
	^self ast argumentNames, self ast temporaryNames"
%

category: '*Slot-Core'
method: CompiledMethod
temporaryVariableNamed: aName
	<PharoGsError>
	self _gsError "
	(self hasTemporaryVariableNamed: aName) ifFalse: [ ^nil ].
	^TemporaryVariable 
		name: aName 
		method: CompiledMethodCompiledMethodCompiledMethodself 
"
%

category: '*Slot-Core'
method: CompiledMethod
temporaryVariables
	<PharoGsError>
	self _gsError "
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]
"
%

category: '*TraitsV2'
method: CompiledMethod
traitSource
	<PharoGsError>
	self _gsError "
	^ self propertyAt: #traitSource"
%

category: 'accessing-tags'
method: CompiledMethod
untagFrom: aSymbol
	<PharoGsError>
	self _gsError "
	""Any method could be tagged with multiple symbols for user purpose. 
	This method should remove given tag from it. All other tags should not be changed. 
	But now we could implemented tags with protocols which allow only tag for method.
	And to remove tag from method we must change it protocol to Protocol unclassified""
	self protocol = aSymbol ifTrue: [ 
		self protocol: Protocol unclassified]"
%

category: 'evaluating'
method: CompiledMethod
valueWithReceiver: aReceiver arguments: anArray 
	<PharoGsError>
	self _gsError "
	""This should be changed when all the VM will support passign of extra arguments
		^self receiver: aReceiver withArguments: anArray executeMethod: self""
	
	^ aReceiver withArgs: anArray executeMethod: self"
%

category: '*Reflectivity'
method: CompiledMethod
variableNodes
	<PharoGsError>
	self _gsError "
	^self ast variableNodes"
%

category: '*Reflectivity'
method: CompiledMethod
variableReadNodes
	<PharoGsError>
	self _gsError "
	^self ast variableReadNodes"
%

category: '*Reflectivity'
method: CompiledMethod
variableWriteNodes
	<PharoGsError>
	self _gsError "
	^self ast variableWriteNodes"
%

category: 'copying'
method: CompiledMethod
veryDeepCopyWith: deepCopier
	<PharoGsError>
	self _gsError "
	""Return self.  I am always shared.  Do not record me.  Only use this for blocks.  Normally methodDictionaries should not be copied this way."""
%

category: 'scanning'
method: CompiledMethod
writesField: varIndex
	<PharoGsError>
	self _gsError "
	""Answer whether the receiver stores into the instance variable indexed by the argument.""
	self isQuick ifTrue: [^false].
	^ super writesField: varIndex"
%

category: 'constants'
classMethod: CompiledMethod
abstractMarker
	<PharoGsError>
	self _gsError "
	^ #subclassResponsibility"
%

category: 'instance creation'
classMethod: CompiledMethod
basicNew
	<PharoGsError>
	self _gsError "
	self error: 'CompiledMethods may only be created with newMethod:header:' "
%

category: 'instance creation'
classMethod: CompiledMethod
basicNew: size
	<PharoGsError>
	self _gsError "
	self error: 'CompiledMethods may only be created with newMethod:header:' "
%

category: 'class initialization'
classMethod: CompiledMethod
checkBytecodeSetConflictsInMethodsWith: aBlock
	<PharoGsError>
	self _gsError "
	self allSubInstances
		detect: aBlock
		ifFound: [ Warning signal: 'There are existing CompiledMethods with a different encoderClass.' ]"
%

category: 'class initialization'
classMethod: CompiledMethod
checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass
	<PharoGsError>
	self _gsError "
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder']."
%

category: 'instance creation'
classMethod: CompiledMethod
cleanUpSourceInTrailers
	<PharoGsError>
	self _gsError "
	self allInstances do: [:e | e isInstalled ifFalse: [e embeddSourceInTrailer]].  
	""pay attention since embeddSourceInTrailer creates a new compiled method. So iterating while
	changing it is a bad idea. This is why we use allInstances do and not allInstancesDo:"""
%

category: 'constants'
classMethod: CompiledMethod
conflictMarker
	<PharoGsError>
	self _gsError "
	^ #traitConflict"
%

category: 'constants'
classMethod: CompiledMethod
disabledMarker
	<PharoGsError>
	self _gsError "
	^ #shouldNotImplement"
%

category: 'constants'
classMethod: CompiledMethod
explicitRequirementMarker
	<PharoGsError>
	self _gsError "
	^ #explicitRequirement"
%

category: 'class initialization'
classMethod: CompiledMethod
fullFrameSize  "CompiledMethod fullFrameSize"
	<PharoGsError>
	self _gsError "
	^ LargeFrame"
%

category: 'private'
classMethod: CompiledMethod
handleFailingFailingNewMethod: numberOfBytes header: headerWord
	<PharoGsError>
	self _gsError "
	""This newMethod:header: gets sent after handleFailingBasicNew: has done a full
	 garbage collection and possibly grown memory.  If this basicNew: fails then the
	 system really is low on space, so raise the OutOfMemory signal.
	 Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, headerWord, and the number of bytecodes specified
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough
	 memory available. Essential. See Object documentation whatIsAPrimitive.""
	<PharoPrimitive>
self @env0:error: 'PharoPrimitive'.
""	<primitive: 79>
	""""space must be low.""""
	OutOfMemory signal.
	""""retry if user proceeds""""
	^self newMethod: numberOfBytes header: headerWord"""
%

category: 'private'
classMethod: CompiledMethod
handleFailingNewMethod: numberOfBytes header: headerWord
	<PharoGsError>
	self _gsError "
	""This newMethod:header: gets sent after newMethod:header: has failed
	 and allowed a scavenging garbage collection to occur.  The scavenging
	 collection will have happened as the VM is activating the (failing) basicNew:.
	 If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient
	 space and a global garbage collection is required.  Retry after garbage
	 collecting and growing memory if necessary.
	 Primitive. Answer an instance of this class with the number of indexable variables
	 specified by the argument, headerWord, and the number of bytecodes specified
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough
	 memory available. Essential. See Object documentation whatIsAPrimitive.""
	<PharoPrimitive>
self @env0:error: 'PharoPrimitive'.
""	<primitive: 79>
	| bytesRequested |
	bytesRequested := (headerWord bitAnd: 16rFFFF) + 1 * Smalltalk wordSize + numberOfBytes + 16.
	Smalltalk garbageCollect < bytesRequested ifTrue:
		[Smalltalk growMemoryByAtLeast: bytesRequested].
	""""retry after global garbage collect and possible grow""""
	^self handleFailingFailingNewMethod: numberOfBytes header: headerWord"""
%

category: 'instance creation'
classMethod: CompiledMethod
headerFlagForEncoder: anEncoderClass
	<PharoGsError>
	self _gsError "
	anEncoderClass == PrimaryBytecodeSetEncoderClass ifTrue: [ ^ 0 ].
	anEncoderClass == SecondaryBytecodeSetEncoderClass ifTrue: [ ^ SmallInteger minVal ].
	
	""The following is a hack to support the old compiler... We can remove it once the old compiler is removed""
	(PrimaryBytecodeSetEncoderClass name endsWith: (anEncoderClass name withoutPrefix: 'Legacy')) ifTrue: [ ^ 0 ].
	(SecondaryBytecodeSetEncoderClass name endsWith: (anEncoderClass name withoutPrefix: 'Legacy')) ifTrue: [ ^ SmallInteger minVal ].
	
	self error: 'The encoder is not one of the two installed bytecode sets'.
	^ 0"
%

category: 'class initialization'
classMethod: CompiledMethod
initialize    "CompiledMethod initialize"
	<PharoGsError>
	self _gsError "
	""Initialize class variables specifying the size of the temporary frame
	needed to run instances of me.""
	SmallFrame := 16.	""Context range for temps+stack""
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass := EncoderForV3PlusClosures.
	SecondaryBytecodeSetEncoderClass := EncoderForSistaV1"
%

category: 'class initialization'
classMethod: CompiledMethod
installPrimaryBytecodeSet: aBytecodeEncoderSubclass
	<PharoGsError>
	self _gsError "
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [:m| 
		m usesPrimaryBytecodeSet and: [m encoderClass ~~ aBytecodeEncoderSubclass]].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass"
%

category: 'class initialization'
classMethod: CompiledMethod
installSecondaryBytecodeSet: aBytecodeEncoderSubclass
	<PharoGsError>
	self _gsError "
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^ self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [ :m | 
		m usesSecondaryBytecodeSet and: [ m encoderClass ~~ aBytecodeEncoderSubclass ] ].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass"
%

category: 'accessing class hierarchy'
classMethod: CompiledMethod
methodPropertiesClass
	<PharoGsError>
	self _gsError "
	""Answer the class to use to create a method's properties, which can be a poor man's way
	 to add instance variables to subclassses of CompiledMethod.  Subclasses of CompiledMethod
	 should define a corresponding subclass of AdditionalMethodState that adds any instance variables
	 required, and override this method to answer that class.""
	^AdditionalMethodState"
%

category: 'instance creation'
classMethod: CompiledMethod
new
	<PharoGsError>
	self _gsError "
	""This will not make a meaningful method, but it could be used
	to invoke some otherwise useful method in this class.""
	^self newMethod: 2 header: 1024"
%

category: 'instance creation'
classMethod: CompiledMethod
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	<PharoGsError>
	self _gsError "
	""Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined.""
	| method pc |
	nArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	method := trailer
				createMethod: numberOfBytes
				class: self
				header:    (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue:
		[pc := method initialPC.
		 method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method"
%

category: 'instance creation'
classMethod: CompiledMethod
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex flag: flag
	<PharoGsError>
	self _gsError "
	""Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined.""
	| method pc |
	nArgs > 15 ifTrue:
		[^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63 ifTrue:
		[^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 65535 ifTrue:
		[^self error: 'Cannot compile -- too many literals'].
	method := trailer
				createMethod: numberOfBytes
				class: self
				header:    (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])
						+ (flag ifTrue: [1 bitShift: 29] ifFalse: [0]).
	primitiveIndex > 0 ifTrue:
		[pc := method initialPC.
		 method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method"
%

category: 'instance creation'
classMethod: CompiledMethod
newFrom: aCompiledMethod
	<PharoGsError>
	self _gsError "
	| inst |
	inst := super basicNew: aCompiledMethod size.
	1 to: aCompiledMethod size do: [:index |
		inst at: index put: (aCompiledMethod at: index)].
	^ inst."
%

category: 'instance creation'
classMethod: CompiledMethod
newInstanceFrom: oldInstance variable: variable size: instSize map: map
	<PharoGsError>
	self _gsError "
	""Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars""
	| new |
	new := self newFrom: oldInstance.
	1 to: instSize do: 
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	^new"
%

category: 'instance creation'
classMethod: CompiledMethod
primitive: primNum numArgs: numArgs numTemps: numTemps stackSize: stackSize literals: literals bytecodes: bytecodes trailer: trailerBytes
	<PharoGsError>
	self _gsError "
	""Create method with given attributes.  numTemps includes numArgs.  stackSize does not include numTemps.""
	| compiledMethod |
	compiledMethod := self
		newBytes: bytecodes size
		trailerBytes: trailerBytes 
		nArgs: numArgs
		nTemps: numTemps
		nStack: 0
		nLits: literals size
		primitive: primNum.
	(WriteStream with: compiledMethod)
		position: compiledMethod initialPC - 1;
		nextPutAll: bytecodes.
	literals withIndexDo: [:obj :i | compiledMethod literalAt: i put: obj].
	compiledMethod needsFrameSize: stackSize.
	^ compiledMethod"
%

category: 'class initialization'
classMethod: CompiledMethod
smallFrameSize
	<PharoGsError>
	self _gsError "
	^ SmallFrame"
%

category: '*Collections-Abstract'
classMethod: CompiledMethod
sortBlock
	<PharoGsError>
	self _gsError "
	""Return a sort block that orders methods by class name and then by selector""
	^  [ :a :b| 
			a methodClass = b methodClass
				ifTrue: [ a selector <= b selector ]
				ifFalse: [ a methodClass name <= b methodClass name ]]"
%

category: 'constants'
classMethod: CompiledMethod
subclassResponsibilityMarker
	<PharoGsError>
	self _gsError "
	^ #subclassResponsibility"
%

category: 'instance creation'
classMethod: CompiledMethod
toReturnConstant: index trailerBytes: trailer
	<PharoGsError>
	self _gsError "
	""Answer an instance of me that is a quick return of the constant
	indexed in (true false nil -1 0 1 2).""
	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256 + index"
%

category: 'instance creation'
classMethod: CompiledMethod
toReturnField: field trailerBytes: trailer
	<PharoGsError>
	self _gsError "
	""Answer an instance of me that is a quick return of the instance variable 
	indexed by the argument, field.""
	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 264 + field"
%

category: 'instance creation'
classMethod: CompiledMethod
toReturnSelf
	<PharoGsError>
	self _gsError "
	""Answer an instance of me that is a quick return of the instance (^self).""
	^ self toReturnSelfTrailerBytes: CompiledMethodTrailer empty"
%

category: 'instance creation'
classmethod: CompiledMethod
toReturnSelfTrailerBytes: trailer
	<PharoGsError>
	self _gsError "
	""Answer an instance of me that is a quick return of the instance (^self).""
	^self newBytes: 3 trailerBytes: trailer nArgs: 0 nTemps: 0 nStack: 0 nLits: 2 primitive: 256"
%

! hand-edited methods 

category: 'accessing'
method: CompiledMethod
methodClass
	"answer the class that I am installed in"

	<PharoGs>
	^self @env0:inClass
%

category: 'accessing'
method: CompiledMethod
origin
	<PharoGs>
	^ self methodClass findOriginClassOf: self
%

category: 'accessing'
method: CompiledMethod
originMethod

	<PharoGs>
	^ self methodClass findOriginMethodOf: self.
%

category: '*RPackage-Copre'
method: CompiledMethod
package 
	<PharoGs> 
	^ self packageFromOrganizer: RPackage organizer
%

category: '*RPackage-Copre'
method: CompiledMethod
packageFromOrganizer: anRPackageOrganizer

	<PharoGs> 
	| originSelector |
	"This method returns the package this method belongs to.  
	It takes into account classes and traits.  
	If the method is in no package, returns nil by now"
	self flag: 'TODO: use anRPackageOrganizer, or better delegate to anRPackageOrganizer'.
	originSelector := self originMethod selector.
	
	^self origin packages 
		detect: [ :each | 
			self origin isMeta
				ifFalse: [ each 
					includesSelector: originSelector 
					ofClassName: self origin instanceSide originalName]
				ifTrue: [ each 
					includesSelector: originSelector 
					ofMetaclassName: self origin instanceSide originalName]] 
		ifNone: [ nil ]
%

category: 'accessing'
method: CompiledMethod
pragmas 
	<PharoGs> 
	^self @env0:pragmas
%

category: 'accessing'
method: CompiledMethod
selector

	<PharoGs>
	^self @env0:selector
%

set compile_env: 0
