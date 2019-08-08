set compile_env: 2

category: 'byte based hash'
classmethod: ByteArray
hashBytes: aByteArray startingWith: speciesHash 
	"Answer the hash of a byte-indexed collection, 
	using speciesHash as the initial value. 
	See SmallInteger>>hashMultiply. 
	The primitive should be renamed at a 
	suitable point in the future" 
    <PharoGs>

	| byteArraySize hash low | 
	byteArraySize := aByteArray size. 
	hash := speciesHash bitAnd: 16rFFFFFFF. 
	1 to: byteArraySize do: [:pos | 
		hash := hash + (aByteArray basicAt: pos). 
		"Begin hashMultiply"
		low := hash bitAnd: 16383. 
		hash := (16r260D * low + ((16r260D * (hash bitShift: -14) + (16r0065 * low) bitAnd: 16383) * 16384)) bitAnd: 16r0FFFFFFF. 
	]. 
	^ hash
%

category: 'accessing'
method: ByteArray
at: anIndex

    <primitive: 974>
    <PharoGs>
    ^self @env0:at: anIndex
%

category: 'accessing'
method: ByteArray
at: anIndex put: value

	<primitive: 268>
	<PharoGs> 
    (value @env0:isKindOf: Character) @env0:ifTrue: [^self at: anIndex put: value codePoint].
    ^self @env0:at: anIndex
%

category: 'accessing'
method: ByteArray
atAllPut: value 
	"Fill the receiver with the given value" 
    <PharoGs>

    1 to: self size do: [ :index | self at: index put: value].
    ^self   "not value!"
%

category: 'accessing'
method: ByteArray
byteAt: index 

    <primitive: 974>
    <PharoGs>
    ^self @env0:byteAt: index
%

category: 'accessing'
method: ByteArray
byteAt: index put: value 

    <primitive: 1002>
    <PharoGs>
    ^self @env0:byteAt: index put: value
%

category: '*FFI-Kernel'
method: ByteArray
doubleAt: byteOffset 
    "extract an 8 byte float from the receiver, the result will 
    be either a SmallDouble or a Float. "
	<PharoGs>

    ^self @env0:at: byteOffset signed: false width: -8
%

category: '*FFI-Kernel'
method: ByteArray
doubleAt: byteOffset put: value 
    "value must be a kind of BinaryFloat."
    <PharoGs>

    ^self @env0:at: byteOffset put: value signed: false width: 8
%

category: '*FFI-Kernel'
method: ByteArray
floatAt: byteOffset 
    "extract a 4 byte float from the receiver, the result will 
    be a SmallDouble."
    <PharoGs>

    ^self @env0:at: byteOffset signed: false width: -4
%

category: '*FFI-Kernel'
method: ByteArray
floatAt: byteOffset put: value 
    "value must be a kind of BinaryFloat, representable as
    a 4 byte IEEE float without loss of exponent bits. 
    If coercion of a BinaryFloat to a 4 byte float would produce
    loss of exponent bits, the primitive will fail.   
    If coercion of a BinaryFloat to a 4 byte float would cause loss
    of precision that would convert a non-zero value to zero , 
    the primitive will fail ."
    <PharoGs>

    ^self @env0:at: byteOffset put: value signed: false width: 4
%

category: '*FFI-Kernel'
method: ByteArray
integerAt: byteOffset put: value size: nBytes signed: aBoolean 
	"Primitive. Store the given value as integer of nBytes size 
	in the receiver. Fail if the value is out of range. 
	Note: This primitive will access memory in the outer space if 
	invoked from ExternalAddress." 

    "GemStone:
    Store the big-endian represention of an aNumber into
    the specified position in the receiver.
    aWidthInBytes is allowed to be 1,2,3,4 or 8 if aNumber is
    an Integer .
    aWidthInBytes is allowed to be 4 , or 8 if aNumber is
    a BinaryFloat  , in which case the absolute value of aWidthInBytes
    specifies the width to be stored and aBool is ignored.
    If representation of an Integer requires more than aWidthInBytes ,
    the primitive will fail. 
    If coercion of a BinaryFloat to a 4 byte C float would produce
    loss of exponent bits, the primitive will fail.   
    If coercion of a BinaryFloat to a 4 byte C float would cause loss
    of precision that would convert a non-zero value to zero , 
    the primitive will fail ."
    <PharoGs>

    ^self @env0:at: byteOffset put: value signed: aBoolean width: nBytes
%

category: '*FFI-Kernel'
method: ByteArray
integerAt: byteOffset size: nBytes signed: aBoolean 
	"Primitive. Return an integer of nBytes size from the receiver. 
	Note: This primitive will access memory in the outer space if 
	invoked from ExternalAddress." 

    "GemStone:
    Retrieve a Number stored in  big-endian represention ,
    from the specified position and width in the receiver.
    aWidthInBytes of 1,2,3,4 or 8  retrieves an Integer.
    aWidthInBytes of -4 or -8 retrieves a SmallDouble or Float,
    and aBool is ignored.  "

    ^self @env0:at: byteOffset signed: aBoolean width: nBytes
%

category: 'private'
method: ByteArray
replaceFrom: start to: stop with: replacement startingAt: repStart  
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive." 

	<PharoGs> 
	super replaceFrom: start to: stop with: replacement startingAt: repStart
%

set compile_env: 0
