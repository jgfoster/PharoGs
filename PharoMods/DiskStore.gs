set compile_env: 2

category: 'private'
method: DiskStore
directoryAt: aPath nodesDo: aBlock

	<PharoGs>
    | dirPath entries |
    dirPath := File encodePathString: (self stringFromPath: aPath).
	(self isDirectory: aPath) ifFalse:
		[ ^self signalDirectoryDoesNotExist: aPath ].
    entries := GsFile @env0:contentsOfDirectory: dirPath onClient: false.
    (entries copyFrom: 3 to: entries size) do: [:each | 
        | array |
        array := (Array new: 12)
            at:  1 put: nil; "symbolic link target"
            at:  2 put: nil; "mode"
            at:  3 put: nil; "inode"
            at:  4 put: nil; "device ID"
            at:  5 put: nil; "number of hard links"
            at:  6 put: nil; "UID"
            at:  7 put: nil; "GID"
            at:  8 put: nil; "size"
            at:  9 put: nil; "access time"
            at: 10 put: (GsFile @env0:lastModificationOfServerFile: each) @env0:asSeconds; "modification time"
            at: 11 put: nil; "change time"
            at: 12 put: nil; "creation time"
            yourself.
        aBlock value: (Array with: each with: array).
    ].
%

set compile_env: 0
