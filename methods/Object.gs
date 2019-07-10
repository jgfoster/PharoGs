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
