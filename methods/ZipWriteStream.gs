set compile_env: 2

category: 'deflating'
method: ZipWriteStream
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch 
	"^DeflatePlugin doPrimitive:#primitiveDeflateBlock" 

    <PharoGsDone>
	^super deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
%

set compile_env: 0
