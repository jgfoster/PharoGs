set compile_env: 2

category: 'primitives'
method: AsyncFile
primClose: fHandle 
	"Close this file. Do nothing if primitive fails." 

	<PharoGsError>
    self _gsError
%

category: 'primitives'
method: AsyncFile
primOpen: fileName forWrite: openForWrite semaIndex: semaIndex 
	"Open a file of the given name, and return a handle for that file. Answer the receiver if the primitive succeeds, nil otherwise." 

	<PharoGsError>
    self _gsError
%

category: 'primitives'
method: AsyncFile
primReadResult: fHandle intoBuffer: buffer at: startIndex count: count 
	"Copy the result of the last read operation into the given buffer starting at the given index. The buffer may be any sort of bytes or words object, excluding CompiledMethods. Answer the number of bytes read. A negative result means: 
		-1 the last operation is still in progress 
		-2 the last operation encountered an error" 

	<PharoGsError>
    self _gsError
%

category: 'primitives'
method: AsyncFile
primReadStart: fHandle fPosition: fPosition count: count 
	"Start a read operation of count bytes starting at the given offset in the given file." 

	<PharoGsError>
    self _gsError
%

category: 'primitives'
method: AsyncFile
primWriteResult: fHandle 
	"Answer the number of bytes written. A negative result means: 
		-1 the last operation is still in progress 
		-2 the last operation encountered an error" 

	<PharoGsError>
    self _gsError
%

category: 'primitives'
method: AsyncFile
primWriteStart: fHandle fPosition: fPosition fromBuffer: buffer at: startIndex count: count 
	"Start a write operation of count bytes starting at the given index in the given buffer. The buffer may be any sort of bytes or words object, excluding CompiledMethods. The contents of the buffer are copied into an internal buffer immediately, so the buffer can be reused after the write operation has been started. Fail if there is insufficient C heap to allocate an internal buffer of the requested size." 

	<PharoGsError>
    self _gsError
%

set compile_env: 0
