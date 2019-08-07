set compile_env: 2

category: 'primitives-file'
classmethod: File
atEnd: id 
	"Answer true if the file position is at the end of the file." 

    <PharoGs>
    ^id @env0:atEnd
%

category: 'primitives-file'
classmethod: File
close: id 
	"Close this file." 

    <PharoGs>
    ^id @env0:close
%

category: 'primitives-file'
classmethod: File
connectToFile: filePointer writable: writableFlag 
	"Open the file with the supplied FILE* pointer, and return the file ID obtained. 
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened. 
	It is the responsibility of the caller to coordinate closing the file." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
connectToFileDescriptor: fileDescriptor writable: writableFlag 
	"Connect to the file with fileDescriptor number, and return the file ID obtained. 
	writeableFlag indicates whether to allow write operations and must be compatible with the way the file was opened. 
	It is the responsibility of the caller to coordinate closing the file." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-path'
classmethod: File
createDirectory: fullPath 
	"Create a directory named by the given path.  
	Fail if the path is bad or if a file or directory by that name already exists." 

    <PharoGs>
    ^GsFile @env0:createServerDirectory: fullPath
%

category: 'primitives-path'
classmethod: File
deleteDirectory: fullPath 
	"Delete the directory named by the given path.  
	Fail if the path is bad or if a directory by that name does not exist." 

    <PharoGs>
    ^GsFile @env0:removeServerDirectory: fullPath
%

category: 'primitives-path'
classmethod: File
delimiter 
	"Return the path delimiter for the underlying platform's file system. '" 

    <PharoGs>
    ^$/
%

category: 'primitives-version'
classmethod: File
fileAttributesVersionString 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
fileDescriptorType: fdNum 
	"Allow to test if the standard input/output files are from a console or not 
	Return values: 
	* -1 - Error 
	* 0 - no console (windows only) 
	* 1 - normal terminal (unix terminal / windows console) 
	* 2 - pipe 
	* 3 - file 
	* 4 - cygwin terminal (windows only)" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
flush: id 
	"On Unix, the FilePlugin uses stdio FILE* structs which maintain their 
	own internal buffer to minimize write() syscalls. This flushes that buffer. 
	On Windows this and primSync: do the same thing." 

    <PharoGs>
    ^id @env0:flush
%

category: 'primitives-path'
classmethod: File
getMacFile: fileName type: typeString creator: creatorString 
	"Get the Macintosh file type and creator info for the file with the given name. 
    Fails if the file does not exist or if the type and creator type arguments are 
    not strings of length 4. 
    This primitive is Mac specific; it is a noop on other platforms." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
getPosition: id 
	"Get this files current position." 

    <PharoGs>
    ^id @env0:position
%

category: 'primitives-file modes'
classmethod: File
isDirectory: aPath

    <PharoGs>
    ^(GsFile @env0:isServerDirectory: aPath) == true
%

category: 'primitives-path'
classmethod: File
lookupDirectory: fullPath filename: fileName 
	"Look up <fileName> (a simple file name) in the directory identified by <fullPath> and return an array containing: 
	<fileName> 			 
	<creationTime> 		(in seconds since the start of the Smalltalk time epoch) 
	<modificationTime> 	(in seconds since the start of the Smalltalk time epoch) 
	<dirFlag> 			DirFlag is true if the entry is a directory 
	<fileSize> 			FileSize the file size in bytes or zero for directories 
	<posixPermissions> 	Numeric Notation  
	<symLinkFlag>		seemingly, symLinkFlag is true if the entry is a symLink 
	On Unix, the empty path denotes '/'.  
    On Macs and PCs, it is the container of the system volumes." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-path'
classmethod: File
lookupEntryIn: fullPath index: index 
	"Look up the index-th entry of the directory with the given fully-qualified path  
	(i.e., starting from the root of the file hierarchy) and return an array containing: 
	<name> <creationTime> <modificationTime> <dirFlag> <fileSize> 
	The empty string enumerates the top-level files or drives. (For example, on Unix, the empty  
	path enumerates the contents of '/'. On Macs and PCs, it enumerates the mounted volumes/drives.) 
	The creation and modification times are in seconds since the start of the Smalltalk time epoch.  
	DirFlag is true if the entry is a directory. FileSize the file size in bytes or zero for directories.  
	The primitive returns nil when index is past the end of the directory. It fails if the given path  
	is bad." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
open: fileName writable: writableFlag 
	"Open a file of the given name, and return the file ID obtained. 
	If writableFlag is true, then 
		if there is none with this name, then create one 
		else prepare to overwrite the existing from the beginning 
	otherwise 
		if the file exists, open it read-only 
		else return nil" 

    <PharoGs>
    | gsFile |
    writableFlag ifTrue: [
        gsFile := GsFile @env0:openAppendOnServer: fileName.
        gsFile @env0:seekFromBeginning: 0.
    ] ifFalse: [
        (GsFile @env0:existsOnServer: fileName @env0:bytesIntoUnicode) ifTrue: [
            gsFile := GsFile @env0:openReadOnServer: fileName.
        ].
    ].
    ^gsFile 
%

category: 'primitives-directory'
classmethod: File
primClosedir: directoryPointerBytes 
	"Close the directory stream associated with directoryPointerBytes. 
	Caution: do not call this twice on the same externalAddress." 
	"self primClosedir: (self primOpendir: '/etc')" 
	"self primClosedir: (self primOpendir: '/no/such/directory')" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-path'
classmethod: File
primDeleteFile: aFileName 
	"Delete the file of the given name.  
	Return self if the primitive succeeds, nil otherwise." 

    <PharoGs>
    ^GsFile @env0:removeServerFile: aFileName
%

category: 'primitives-file attributes'
classmethod: File
primExists: aByteArray 
	"Answer a boolean indicating whether the supplied file exists." 

    <PharoGs>
    ^GsFile @env0:existsOnServer: aByteArray @env0:bytesIntoUnicode
%

category: 'primitives-file attributes'
classmethod: File
primFile: pathByteArray posixPermissions: anInteger 
	"Set the mode of pathByateArray to anInteger (as defined by chmod())" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file attributes'
classmethod: File
primFile: pathByteArray symlinkUid: uidInteger gid: gidInteger 
	"Set the owner and group of path by numeric id." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file attributes'
classmethod: File
primFile: pathByteArray uid: uidInteger gid: gidInteger 
	"Set the owner and group of path by numeric id." 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file attributes'
classmethod: File
primFileAttribute: aByteArray number: attributeNumber 
	"Answer a single attribute for the supplied file. 
	For backward compatibility (on Unix) with FileReference if the file does not exist, 
    and the specified path is a (broken) symbolic link, answer the requested attribute 
    for the symbolic link. 
    stat() information: 
        1: name 
        2: mode 
        3: ino 
        4: dev 
        5: nlink 
        6: uid 
        7: gid 
        8: size 
        9: accessDate 
        10: modifiedDate 
        11: changeDate 
        12: creationDate 
    access() information 
        13: is readable 
        14: is writeable 
        15: is executable 
    symbolic link information 
        16: is symbolic link 
	" 

    <PharoGs>
    attributeNumber @env0:== 16 @env0:ifTrue: [
        ^((self primFileAttributes: aByteArray mask: 2r001) at: 1) notNil
    ].
    attributeNumber @env0:<= 12 @env0:ifTrue: [
        ^(self primFileAttributes: aByteArray mask: 2r001) at: attributeNumber
    ].
    ^self _gsError
%

category: 'primitives-file attributes'
classmethod: File
primFileAttributes: aByteArray mask: attributeMask 
	"Answer an array of attributes for the supplied file.  
    The size and contents of the array are determined by the attributeMask: 
    Bit 0: stat() information 
    Bit 1: access() information 
    Bit 2: use lstat() (instead of stat()) 
    On error, answer an error code (Integer). 
    stat() information: 
        1: name 
        2: mode 
        3: ino 
        4: dev 
        5: nlink 
        6: uid 
        7: gid 
        8: size 
        9: accessDate 
        10: modifiedDate 
        11: creationDate 
    access() information 
        1: is readable 
        2: is writeable 
        3: is executable 
        " 

    <PharoGs>
    | string stat |
    string := aByteArray @env0:bytesIntoString.
    stat := GsFile @env0:_stat: string isLstat: (attributeMask @env0:bitAnd: 2r100) @env0:~~ 0.
    (attributeMask @env0:bitAnd: 2r001) @env0:~~ 0 "bit 0" ifTrue: [
        | link |
        link := String new.
        GsFile @env0:_prim763: 9 with: string with: link with: nil.
        link @env0:= '' @env0:ifTrue: [link := nil].
        ^(Array new: 13)
            at:  1 put: link;
            at:  2 put: stat @env0:mode;
            at:  3 put: stat @env0:ino;
            at:  4 put: stat @env0:dev;
            at:  5 put: stat @env0:nlink;
            at:  6 put: stat @env0:uid;
            at:  7 put: stat @env0:gid;
            at:  8 put: stat @env0:size;
            at:  9 put: stat @env0:atimeUtcSeconds; "time of last access"
            at: 10 put: stat @env0:mtimeUtcSeconds; "time of last modification"
            at: 11 put: stat @env0:ctimeUtcSeconds; "time of last status change"
            at: 12 put: nil;    "creation time?"
            at: 13 put: nil;
            yourself
    ].
    ^self _gsError
%

category: 'private'
classmethod: File
primFileMasks 
	"Answer an array of well known masks 
	For more information, see: http://man7.org/linux/man-pages/man2/stat.2.html 
    https://refspecs.linuxfoundation.org/LSB_2.1.0/LSB-Core-generic/LSB-Core-generic/libc-ddefs.html
	" 

    <PharoGs>
    ^#(
        "1: S_IFMT"     16rF000
        "2: S_IFSOCK"   16rC000
        "3: S_IFLNK"    16rA000
        "4: S_IFREG"    16r8000
        "5: S_IFBLK"    16r6000
        "6: S_IFDIR"    16r4000
        "7: S_IFCHR"    16r2000
        "8: S_IFIFO"    16r1000
    )
%

category: 'primitives-file attributes'
classmethod: File
primFromPlatformPath: aByteArray 
	"Convert the supplied platform encoded string to the native (UTF8) equivalent" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-windows'
classmethod: File
primLogicalDrives 
	"Answer the windows logical drive mask" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-directory'
classmethod: File
primOpendir: pathString 
	"Answer an ExternalAddress for a directory stream on pathString, or nil if 
	the directory cannot be opened" 
	"self primOpendir: '/etc'" 
	"self primOpendir: '.'" 
	"self primOpendir: '/no/such/directory'" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-directory'
classmethod: File
primPathMax 
	"Answer the VMs PATH_MAX value" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-directory'
classmethod: File
primReaddir: directoryPointerBytes 
	"Read the next directory entry from the directory stream associated with 
	directoryPointerBytes. Answer the name of the entry, ornil for end of directory stream." 
	"self primReaddir: (self primOpendir: '/etc')" 
	"self primReaddir: (self primOpendir: '/no/such/directory')" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-directory'
classmethod: File
primRewinddir: directoryPointerBytes 
	"Rewind the directory stream associated with directoryPointerBytes. Answer 
	anExternalAddress on success, or nil on failure." 
	"self primRewinddir: (self primOpendir: '/etc')" 
	"self primRewinddir: (self primOpendir: '/no/such/directory')" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file attributes'
classmethod: File
primToPlatformPath: aByteArray 
	"Convert the supplied UTF8 encoded string to the platform encoded equivalent" 

    <PharoGsError>
    ^self _gsError
%

category: 'primitives-file'
classmethod: File
read: id into: byteArray startingAt: startIndex count: count 
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read." 

    <PharoGs>
    | bytes actualCount |
    bytes := ByteArray new: count.
    actualCount := id @env0:next: count into: bytes.
    actualCount == nil ifTrue: [self @env0:error: id @env0:lastErrorString].
    byteArray 
        @env0:replaceFrom: startIndex
        to: startIndex + actualCount - 1
        with: bytes.
    ^actualCount
%

category: 'registry'
classmethod: File
register: anObject
    "Ignore this for now; GemStone does it own finalization in the VM."

    <PharoGs>
%

category: 'primitives-path'
classmethod: File
rename: oldFileFullName to: newFileFullName  
	"Rename the file of the given name to the new name. Fail if there is no file of the old name  
	or if there is an existing file with the new name." 

    <PharoGs>
    ^GsFile @env0:renameFileOnServer: oldFileFullName to: newFileFullName
%

category: 'registry'
classmethod: File
retryWithGC: execBlock until: testBlock forFileNamed: fullName
    
    <PharoGs>
    ^execBlock value
%

category: 'primitives-file'
classmethod: File
setPosition: id to: anInteger 
	"Set this file to the given position." 

    <PharoGs>
    ^id @env0:position: anInteger
%

category: 'primitives-file'
classmethod: File
sizeOf: id 
	"Answer the size of this file." 

    <PharoGs>
    ^id @env0:fileSize
%

category: 'primitives-file'
classmethod: File
sizeOrNil: id 
	"Answer the size of this file." 

    <PharoGs>
    ^[id @env0:fileSize] 
        @env0:on: (Globals @env0:at: #'Error') 
        do: [:ex | ex @env0:return: nil]
%

category: 'primitives-file'
classmethod: File
stdioHandles 

    <PharoGs>
    ^Array @env0:with: GsFile stdin with: GsFile stdout with: GsFile stderr
%

category: 'primitives-file'
classmethod: File
sync: id 
	"On Unix, this syncs any written or flushed data still in the kernel file 
	system buffers to disk. On Windows this and primFlush: do the same thing" 

    <PharoGs>
    ^id @env0:sync
%

category: 'primitives-file'
classmethod: File
truncate: id to: anInteger 
	"Truncate this file to the given position." 

    <PharoGs>
	anInteger < id @env0:fileSize ifTrue: [
		| gsFileClass mode pathName tempFile tempName |
		gsFileClass := Globals @env0:at: #'GsFile'.
		pathName := id @env0:pathName.
		mode := id @env0:mode.
		id @env0:close.
		tempName := pathName @env0:, id @env0:asOop @env0:printString.
		gsFileClass @env0:renameFileOnServer: pathName to: tempName.
		tempFile := gsFileClass @env0:open: tempName mode: 'r' onClient: false.
		id @env0:open: pathName mode: 'a'.
		id @env0:write: anInteger from: tempFile @env0:contents.
		id @env0:close.
        tempFile @env0:close.
        gsFileClass @env0:removeServerFile: tempName.
		id @env0:open: pathName mode: mode.
	] ifFalse: [
		| size |
		id @env0:setToEnd.
		id @env0:write: (size := anInteger @env0:- id @env0:fileSize) from: (ByteArray new: size).
	].
%

category: 'registry'
classmethod: File
unregister: anObject
    "Ignore this for now; GemStone does it own finalization in the VM."

    <PharoGs>
%

category: 'primitives-file'
classmethod: File
write: id from: stringOrByteArray startingAt: startIndex count: count 
	"Write count bytes onto this file from the given string or byte array 
    starting at the given index. 	
    Answer the number of bytes written." 

    <PharoGs>
    | bytes |
    bytes := stringOrByteArray @env0:copyFrom: startIndex to: startIndex + count - 1.
    ^id @env0:write: count from: bytes
%

set compile_env: 0
