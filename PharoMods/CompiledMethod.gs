set compile_env: 2

category: 'private'
classmethod: CompiledMethod
handleFailingFailingNewMethod: numberOfBytes header: headerWord 
	"This newMethod:header: gets sent after handleFailingBasicNew: has done a full 
	 garbage collection and possibly grown memory.  If this basicNew: fails then the 
	 system really is low on space, so raise the OutOfMemory signal. 
	 Primitive. Answer an instance of this class with the number of indexable variables 
	 specified by the argument, headerWord, and the number of bytecodes specified 
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes 
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough 
	 memory available. Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'private'
classmethod: CompiledMethod
handleFailingNewMethod: numberOfBytes header: headerWord 
	"This newMethod:header: gets sent after newMethod:header: has failed 
	 and allowed a scavenging garbage collection to occur.  The scavenging 
	 collection will have happened as the VM is activating the (failing) basicNew:. 
	 If handleFailingBasicNew: fails then the scavenge failed to reclaim sufficient 
	 space and a global garbage collection is required.  Retry after garbage 
	 collecting and growing memory if necessary. 
	 Primitive. Answer an instance of this class with the number of indexable variables 
	 specified by the argument, headerWord, and the number of bytecodes specified 
	 by numberOfBytes.  Fail if this if the arguments are not Integers, or if numberOfBytes 
	 is negative, or if the receiver is not a CompiledMethod class, or if there is not enough 
	 memory available. Essential. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: CompiledMethod
flushCache 
	"Tell the virtual machine to remove all references to this method from its 
    method lookup caches, and to discard any optimized version of the method, 
    if it has any of these. This must be done whenever a method is modified in 
    place, such as modifying its literals or machine code, to reflect the 
    revised code.  c.f. Behavior>>flushCache & Symbol>>flushCache.  
    Essential. See MethodDictionary class comment." 

    <PharoGsError>
    self _gsError
%

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
