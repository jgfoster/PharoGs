set compile_env: 2

category: 'primitives'
method: SHA1
primExpandBlock: aByteArray into: wordBitmap 
	"Expand the given 64-byte buffer into the given Bitmap of length 80." 

    <PharoGsError>
    self _gsError
%

category: 'primitives'
method: SHA1
primHashBlock: blockBitmap using: workingTotalsBitmap 
	"Hash the given block (a Bitmap) of 80 32-bit words, using the given workingTotals." 

    <PharoGsError>
    self _gsError
%

category: 'primitives'
method: SHA1
primHasSecureHashPrimitive 
	"Answer true if this platform has primitive support for the Secure Hash Algorithm." 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
