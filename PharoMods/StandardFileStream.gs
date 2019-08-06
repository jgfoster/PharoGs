set compile_env: 2

category: 'registry'
classmethod: StandardFileStream
retryWithGC: execBlock until: testBlock forFileNamed: fullName
    
    <PharoGs>
    ^execBlock value
%

category: 'registry'
classmethod: StandardFileStream
register: anObject
    "Ignore this for now; GemStone does it own finalization in the VM."

    <PharoGs>
%

category: 'registry'
classmethod: StandardFileStream
unregister: anObject
    "Ignore this for now; GemStone does it own finalization in the VM."

    <PharoGs>
%

category: 'private - primitives'
method: StandardFileStream
primAtEnd: id 
	"Answer true if the file position is at the end of the file." 
    
    <PharoGs>
    ^id @env0:atEnd
%

category: 'private - primitives'
method: StandardFileStream
primClose: id 
	"Close this file." 
    
    <PharoGs>
    ^id @env0:close
%

category: 'private - primitives'
method: StandardFileStream
primCloseNoError: id 
	"Close this file. Don't raise an error if the primitive fails. '" 
    
    <PharoGs>
    ^id @env0:close
%

category: 'private'
method: StandardFileStream
primDropRequestFileHandle: dropIndex 
	"Primitive. Return the (read-only) file handle for some file that was just dropped onto the application. 
	Fail if dropIndex is out of range or the primitive is not supported." 
    
    <PharoGsError>
    ^self _gsError
%

category: 'private'
method: StandardFileStream
primDropRequestFileName: dropIndex 
	"Primitive. Return the file name for some file that was just dropped onto the application. 
	Fail if dropIndex is out of range or the primitive is not supported." 
    
    <PharoGsError>
    ^self _gsError
%

category: 'private - primitives'
method: StandardFileStream
primFlush: id 
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their 
	own internal buffer to minimize write() syscalls. This flushes that buffer. 
	On Windows this and primSync: do the same thing." 
    
    <PharoGs>
    ^id @env0:flush
%

category: 'private - primitives'
method: StandardFileStream
primGetPosition: id 
	"Get this files current position." 
    
    <PharoGs>
    ^id @env0:position
%

category: 'private - primitives'
method: StandardFileStream
primOpen: fileName writable: writableFlag 
	"Open a file of the given name, and return the file ID obtained. 
	If writableFlag is true, then 
		if there is none with this name, then create one 
		else prepare to overwrite the existing from the beginning 
	otherwise 
		if the file exists, open it read-only 
		else return nil" 
    
    <PharoGs>
    | nameString gsFile |
    nameString := fileName @env0:bytesIntoString.
    writableFlag ifTrue: [
        gsFile := GsFile @env0:open: nameString mode: 'w+' onClient: false.
    ] ifFalse: [
        (GsFile @env0:existsOnServer: nameString) ifTrue: [
            gsFile := GsFile @env0:open: nameString mode: 'r' onClient: false.
        ].
    ].
    ^gsFile 
%

category: 'private - primitives'
method: StandardFileStream
primRead: id into: byteArray startingAt: startIndex count: count 
	"Read up to count bytes of data from this file into the given string or byte 
    array starting at the given index. Answer the number of bytes actually read." 
    
    <PharoGs>
    | bytes actualCount |
    id @env0:position @env0:>= id @env0:fileSize @env0:ifTrue: [^0].
    bytes := ByteArray new: count.
    actualCount := id @env0:next: count into: bytes.
    actualCount == nil ifTrue: [self @env0:error: id @env0:lastErrorString].
    byteArray 
        @env0:replaceFrom: startIndex
        to: startIndex + actualCount - 1
        with: bytes.
    ^actualCount
%

category: 'private - primitives'
method: StandardFileStream
primSetPosition: id to: anInteger 
	"Set this file to the given position." 
    
    <PharoGs>
    ^id @env0:position: anInteger
%

category: 'private - primitives'
method: StandardFileStream
primSize: id 
	"Answer the size of this file." 
    
    <PharoGs>
    | size |
    size := id @env0:fileSize.
    size == nil ifTrue: [self @env0:error: GsFile @env0:lastErrorString].
    ^size
%

category: 'private - primitives'
method: StandardFileStream
primSizeNoError: id 
	"Answer the size of this file. Answer nil if the primitive fails; 
    this indicates that the file handle has become stale." 
    
    <PharoGs>
    ^id @env0:fileSize.
%

category: 'private - primitives'
method: StandardFileStream
primSync: id 
	"On Unix, this syncs any written or flushed data still in the kernel file 
	system buffers to disk. On Windows this and primFlush: do the same thing" 
    
    <PharoGs>
    (id @env0:sync) == nil ifTrue: [self @env0:error: id @env0:lastErrorString].
%

category: 'private - primitives'
method: StandardFileStream
primTruncate: id to: anInteger 
	"Truncate this file to the given position." 

    <PharoGsError>
    self _gsError
%

category: 'private - primitives'
method: StandardFileStream
primWrite: id from: stringOrByteArray startingAt: startIndex count: count 
	"Write count bytes onto this file from the given string or byte array 
    starting at the given index. Answer the number of bytes written." 

    <PharoGs>
    | bytes |
    bytes := stringOrByteArray @env0:copyFrom: startIndex to: startIndex + count - 1.
    ^id @env0:write: count from: bytes
%

set compile_env: 0
