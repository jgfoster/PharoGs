set compile_env: 2

category: 'accessing'
method: Object
at: index  
	"Primitive. Assumes receiver is indexable. Answer the value of an  
	indexable element in the receiver. Fail if the argument index is not an  
	Integer or is out of bounds. Essential. See Object documentation  
	whatIsAPrimitive. Read the class comment for a discussion about that the fact 
	that the index can be a float." 

	<primitive: 32>
	<PharoGs> 
	index isInteger ifTrue: 
		[self class isVariable 
			ifTrue: [self errorSubscriptBounds: index] 
			ifFalse: [self errorNotIndexable]]. 
	index isNumber 
		ifTrue: [^self at: index asInteger] 
		ifFalse: [self errorNonIntegerIndex]
%

category: 'accessing'
method: Object
at: index put: value  
	"Primitive. Assumes receiver is indexable. Store the argument value in  
	the indexable element of the receiver indicated by index. Fail if the  
	index is not an Integer or is out of bounds. Or fail if the value is not of  
	the right type for this kind of collection. Answer the value that was  
	stored. Essential. See Object documentation whatIsAPrimitive." 

	<primitive: 268>
	<PharoGs> 
	index isInteger  
		ifTrue: [self class isVariable 
			ifTrue: [(index between: 1 and: self size) 
				ifFalse: [ ^ self errorSubscriptBounds: index]] 
			ifFalse: [ ^ self errorNotIndexable ]] 
		ifFalse: [   
			^ index isNumber 
				ifTrue: [ self at: index asInteger put: value] 
				ifFalse: [ self errorNonIntegerIndex] ]. 
	self isReadOnlyObject  
		ifTrue: [ ^ self modificationForbiddenFor: #at:put: index: index value: value ]. 
	self errorImproperStore
%

category: 'accessing'
method: Object
basicAt: index  
	"Primitive. Assumes receiver is indexable. Answer the value of an  
	indexable element in the receiver. Fail if the argument index is not an  
	Integer or is out of bounds. Essential. Do not override in a subclass. See  
	Object documentation whatIsAPrimitive." 

	<primitive: 32>
	<PharoGs> 
	index isInteger ifTrue: [self errorSubscriptBounds: index]. 
	index isNumber 
		ifTrue: [^self basicAt: index asInteger] 
		ifFalse: [self errorNonIntegerIndex]
%

category: 'accessing'
method: Object
basicAt: index put: value  
	"Primitive. Assumes receiver is indexable. Store the second argument  
	value in the indexable element of the receiver indicated by index. Fail  
	if the index is not an Integer or is out of bounds. Or fail if the value is  
	not of the right type for this kind of collection. Answer the value that  
	was stored. Essential. Do not override in a subclass. See Object  
	documentation whatIsAPrimitive." 

	<primitive: 268>
	<PharoGs> 
	index isInteger  
		ifTrue: [self class isVariable 
			ifTrue: [(index between: 1 and: self size) 
				ifFalse: [ ^ self errorSubscriptBounds: index ]] 
			ifFalse: [ ^ self errorNotIndexable ]] 
		ifFalse: [   
			^ index isNumber 
				ifTrue: [ self basicAt: index asInteger put: value ] 
				ifFalse: [ self errorNonIntegerIndex ] ]. 
	self isReadOnlyObject  
		ifTrue: [ ^ self modificationForbiddenFor: #basicAt:put: index: index value: value ]. 
	self errorImproperStore 
%

category: 'accessing'
method: Object
basicSize 
	"Primitive. Answer the number of indexable variables in the receiver.  
	This value is the same as the largest legal subscript. Essential. Do not  
	override in any subclass. See Object documentation whatIsAPrimitive." 

	<primitive: 32>
	<PharoGs> 
	^self @env0:basicSize
%

category: 'copying'
method: Object
clone 
	"Answer a shallow copy of the receiver." 

	<primitive: 885>
	<PharoGs> 
	^self @env0:shallowCopy
%

category: 'copying'
method: Object
copyFrom: anotherObject 
	"Copy to myself all instance variables I have in common with anotherObject.  This is dangerous because it ignores an object's control over its own inst vars.  " 

	| mine his | 
	<PharoGs> 
	mine := self class allInstVarNames. 
	his := anotherObject class allInstVarNames. 
	1 to: (mine size min: his size) do: [:ind | 
		(mine at: ind) = (his at: ind) ifTrue: [ 
			self instVarAt: ind put: (anotherObject instVarAt: ind)]]. 
	self class isVariable & anotherObject class isVariable ifTrue: [ 
		1 to: (self basicSize min: anotherObject basicSize) do: [:ind | 
			self basicAt: ind put: (anotherObject basicAt: ind)]].
%

category: 'introspection'
method: Object
instVarAt: index 
	"Primitive. Answer a fixed variable in an object. The numbering of the variables 
	 corresponds to the named instance variables, followed by the indexed instance 
	 variables. Fail if the index is not an Integer or is not the index of a fixed  
	 variable or indexed variable. Essential. See Object documentation whatIsAPrimitive." 

	<primitive: 611>
	<PharoGs> 
	^self @env0:instVarAt: index
%

category: 'introspection'
method: Object
instVarAt: index put: anObject 
	"Primitive. Store a value into a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables, followed by the indexed 
	 instance variables. Fail if the index is not an Integer or is not the index of a fixed 
	 variable or indexed variable. Essential. See Object documentation whatIsAPrimitive." 

	<primitive: 604>
	<PharoGs> 
	 (index isInteger  
		and: [ index between: 1 and: self class instSize + self basicSize])  
			ifFalse: [ ^ self errorSubscriptBounds: index ]. 
	self isReadOnlyObject  
		ifTrue: [ ^ self modificationForbiddenFor: #instVarAt:put: index: index value: anObject ]
%

category: 'pinning'
method: Object
isPinnedInMemory 
	"Answer if the receiver is pinned.  The VM's garbage collector routinely moves 
	 objects as it reclaims and compacts memory.  But it can also pin an object so 
	 that it will not be moved, which can make it easier to pass objects out through 
	 the FFI." 

	<PharoGs> 
	^false "GemStone does not 'pin' objects in memory"
%

category: 'write barrier'
method: Object
isReadOnlyObject 
	"Answer if the receiver is read-only. 
	 If the VM supports read-only objects it will not write to read-only objects. 
	 An attempt to write to an instance variable of a read-only object will 
	 cause the VM to send attemptToAssign:withIndex: to the read-only object. 
	 An attempt to modify a read-only object in a primitive will cause the 
	 primitive to fail with a #'no modification' error code." 

	<primitive: 36>
	<PharoGs> 
	^self class isImmediateClass
%

category: 'message performing'
method: Object
perform: aSymbol  
	"Send the unary selector, aSymbol, to the receiver. 
	Fail if the number of arguments expected by the selector is not zero. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 
	 
	<primitive: 2004>
	<reflective: #object:performMessageWith:> 
	<PharoGs> 
	^ self perform: aSymbol withArguments: (Array new: 0)
%

category: 'message performing'
method: Object
perform: aSymbol with: anObject  
	"Send the selector, aSymbol, to the receiver with anObject as its argument. 
	Fail if the number of arguments expected by the selector is not one. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 
	 
	<primitive: 2004>
	<reflective: #object:performMessageWith:> 
	<PharoGs> 
	^ self perform: aSymbol withArguments: (Array with: anObject)
%

category: 'message performing'
method: Object
perform: aSymbol with: firstObject with: secondObject  
	"Send the selector, aSymbol, to the receiver with the given arguments. 
	Fail if the number of arguments expected by the selector is not two. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 2004>
	<reflective: #object:performMessageWith:> 
	<PharoGs> 
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)
%

category: 'message performing'
method: Object
perform: aSymbol with: firstObject with: secondObject with: thirdObject  
	"Send the selector, aSymbol, to the receiver with the given arguments. 
	Fail if the number of arguments expected by the selector is not three. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 2004>
	<reflective: #object:performMessageWith:> 
	<PharoGs> 
	^ self perform: aSymbol 
		withArguments: {firstObject . secondObject . thirdObject}
%

category: 'message performing'
method: Object
perform: aSymbol with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Send the selector, aSymbol, to the receiver with the given arguments. 
	Fail if the number of arguments expected by the selector is not four. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 2004>
	<reflective: #object:performMessageWith:> 
	<PharoGs> 
	^ self perform: aSymbol 
		withArguments: (Array with: firstObject with: secondObject with: thirdObject with: fourthObject)
%

category: 'message performing'
method: Object
perform: selector withArguments: argArray  
	"Send the selector, aSymbol, to the receiver with arguments in argArray. 
	Fail if the number of arguments expected by the selector  
	does not match the size of argArray. 
	Primitive. Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 2005>
	<reflective: #object:performMessageWithArgs:> 
	<PharoGs> 
	^ self perform: selector withArguments: argArray inSuperclass: self class
%

category: 'message performing'
method: Object
perform: selector withArguments: argArray inSuperclass: lookupClass 
	"NOTE:  This is just like perform:withArguments:, except that 
	the message lookup process begins, not with the receivers's class, 
	but with the supplied superclass instead.  It will fail if lookupClass 
	cannot be found among the receiver's superclasses. 
	Primitive. Essential. See Object documentation whatIsAPrimitive." 
	 
	<reflective: #object:performMessageInSuperclass:> 
	<PharoGsError> 
	self @env0:error: 'Not supported in GemStone'
%

category: 'reflective operations'
method: Object
primitiveChangeClassTo: anObject 
	"Primitive. Change the class of the receiver into the class of the argument given that the format of the receiver matches the format of the argument's class. Fail if receiver or argument are SmallIntegers, or the receiver is an instance of a compact class and the argument isn't, or when the argument's class is compact and the receiver isn't, or when the format of the receiver is different from the format of the argument's class, or when the arguments class is fixed and the receiver's size differs from the size that an instance of the argument's class should have. 
	Note: The primitive will fail in most cases that you think might work. This is mostly because of a) the difference between compact and non-compact classes, and b) because of differences in the format. As an example, '(Array new: 3) primitiveChangeClassTo: Morph basicNew' would fail for three of the reasons mentioned above. Array is compact, Morph is not (failure #1). Array is variable and Morph is fixed (different format - failure #2). Morph is a fixed-field-only object and the array is too short (failure #3). 
	The facility is really provided for certain, very specific applications (mostly related to classes changing shape) and not for casual use." 

	<PharoGs> 
	self @env0:changeClassTo: anObject class.
%

category: 'pinning'
method: Object
setPinnedInMemory: aBoolean 
	"The VM's garbage collector routinely moves objects as it reclaims and compacts 
	 memory. But it can also pin an object so that it will not be moved around in memory, 
    while still being reclamable by the garbage collector. This can make 
	 it easier to pass objects out through the FFI. Objects are unpinnned when created. 
	 This primitive either pins or unpins an object, and answers if it was already pinned." 

	<PharoGsError> 
	aBoolean ifTrue: [
		self @env0:error: 'GemStone does not support pinning in memory'.
	].
%

category: 'copying'
method: Object
shallowCopy 
	"Answer a copy of the receiver which shares the receiver's instance variables. It should never be overridden. I'm invoked from the copy template method. Subclasses that need to specialize the copy should specialize the postCopy hook method." 
	 
	<primitive: 885>
	<PharoGs> 
	^self @env0:shallowCopy
%

category: 'write barrier'
method: Object
setIsReadOnlyObject: aBoolean 
	"If the VM supports read-only objects it will not write to read-only objects. 
	 An attempt to write to an instance variable of a read-only object will 
	 cause the VM to send attemptToAssign:withIndex: to the read-only object. 
	 An attempt to modify a read-only object in a primitive will cause the 
	 primitive to fail with a #'no modification' error code. 
	 This primitive sets the read-only flag of the receiver to the given 
	 value and answers the previous vaue of the flag. 
	 Note: Some objects can't be read-only, currently contexts and objects related 
	 to process scheduling (Processor, Process instances, Semaphore instances, ...)" 

	<PharoGsError> 
	aBoolean
		ifTrue: [self @env0:immediateInvariant]
		ifFalse: [self @env0:error: 'VM does not support removing read-only state']
%

category: 'accessing'
method: Object
size 
	"Primitive. Answer the number of indexable variables in the receiver.  
	This value is the same as the largest legal subscript. Essential. See Object  
	documentation whatIsAPrimitive." 

	<primitive: 0>
	<PharoGs> 
	self class isVariable ifFalse: [self errorNotIndexable]. 
	^ 0
%

category: 'accessing'
method: Object
sizeInMemory 
	"self class isImmediateClass 
		ifTrue: [ ^ 0 ]. 
	^ self class isVariable 
		ifTrue: [ self class byteSizeOfInstanceOfSize: self basicSize ] 
		ifFalse: [ self class byteSizeOfInstance ] "
	^self @env0:physicalSize
%

category: 'reflective operations'
method: Object
someObject 
	"Primitive. Answer the first object in the enumeration of all 
	 objects." 

	<PharoGs> 
	^self
%

category: 'private'
method: Object
species 
	"Answer the preferred class for reconstructing the receiver.  For example,  
	collections create new collections whenever enumeration messages such as  
	collect: or select: are invoked.  The new kind of collection is determined by  
	the species of the original collection.  Species and class are not always the  
	same.  For example, the species of Interval is Array." 

	<PharoGs> 
	^self class
%

set compile_env: 0
