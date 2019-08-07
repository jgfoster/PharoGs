set compile_env: 0

category: 'MagLev'
classmethod: GsFile
_prim763: opcode with: fdOrString with: argOne with: argTwoArray

"opcode 
     0  delete(path)
     1  chmod(path, mode)
     2  truncate(path, length)
     3  chown(path, owner, group)
     4  lchown(path, owner, group)
     5  utime(path, acctime, modtime)
     6  symlink(path1, path2)
     7  rename(path1, path2)
     8  link(path1, path2)
     9  readlink(path, destString)
    10  fchmod(fd, mode)
    11  flock(fd, operation)
    12  fchown(fd, owner, group)
    13  fetch_flock_constants(fdIgnored, destArray)
        result of above is a SmallInt, 0 for success, or the C errno 
    14  dirname(path)
    15  ftruncate(fd, length)
    16  fsync(fd)
 result is typically 0 if successful or an errno value"

<primitive: 763>
opcode _validateClass: SmallInteger .
fdOrString _validateClasses: { SmallInteger . String }.
argOne ifNotNil:[ argOne _validateClasses: { SmallInteger . String }].
argTwoArray ifNotNil:[ | argTwo |
  argTwo := argTwoArray at: 1 .
  argTwo ifNotNil:[ argTwo _validateClasses: { SmallInteger . String }].
].
self _primitiveFailed: #_modifyFile:fdPath:with:with:
     args: { opcode . fdOrString . argOne . argTwoArray }
%

category: 'MagLev'
classmethod: GsFile
_prim765: opcode with: arg with: arg2

"opcode 5    umask()"

<primitive: 765>
^ self _primitiveFailed: #_twoArgPrim:with:with: args: { opcode. arg . arg2 }
%

classmethod: GsFile
_umask
" return current umask without changing it"
^ self _prim765: 5 with: nil with: -1
%

classmethod: GsFile
_umask: newMaskInt
" anInt == -1 means return current umask without changing it .
  otherwise set process' file creation mask to newMaskInt
  and return previous value.
"
^ self _prim765: 5 with: nil with: newMaskInt
%
