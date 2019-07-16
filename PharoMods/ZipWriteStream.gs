set compile_env: 2

category: 'deflating'
method: ZipWriteStream
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch 
	"^DeflatePlugin doPrimitive:#primitiveDeflateBlock" 

    <PharoGs>
	^super deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
%

set compile_env: 0
