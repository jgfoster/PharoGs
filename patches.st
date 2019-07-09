
Metacello new
  baseline: 'Tonel';
  repository: 'github://pharo-vcs/tonel:v1.0.12';
  load.
!
| list reader |
list := OrderedCollection new
    add: '/Users/jfoster/code/pharo/src/' -> 'Collections-Sequenceable';
    add: '/Users/jfoster/code/pharo/src/' -> 'DeprecatedFileStream';
    add: '/Users/jfoster/code/pharo/src/' -> 'FFI-Kernel';
    add: '/Users/jfoster/code/pharo/src/' -> 'Hermes';
    add: '/Users/jfoster/code/pharo/src/' -> 'Hermes-Extensions';
    add: '/Users/jfoster/code/pharo/src/' -> 'Kernel';
    add: '/Users/jfoster/code/pharo/src/' -> 'Metacello-Bitbucket';
    add: '/Users/jfoster/code/pharo/src/' -> 'Metacello-GitHub';
    add: '/Users/jfoster/code/pharo/src/' -> 'MonticelloFileTree-Core';
    add: '/Users/jfoster/code/pharo/src/' -> 'Multilingual-Languages';
    add: '/Users/jfoster/code/pharo/src/' -> 'OpalCompiler-Core';
    add: '/Users/jfoster/code/pharo/src/' -> 'Reflectivity';
    add: '/Users/jfoster/code/pharo/src/' -> 'RPackage-Core';
    add: '/Users/jfoster/code/pharo/src/' -> 'Shift-ClassInstaller';
    add: '/Users/jfoster/code/pharo/src/' -> 'Slot-Core';
    add: '/Users/jfoster/code/pharo/src/' -> 'System-Finalization';
    add: '/Users/jfoster/code/pharo/src/' -> 'System-Platforms';
    add: '/Users/jfoster/code/pharo/src/' -> 'System-Sources';
    add: '/Users/jfoster/code/pharo/src/' -> 'System-Support';
"
    add: '/Users/jfoster/code/pharo/src/' -> 'XXX';
"
    yourself.
list do: [:each | 
    Transcript show: each value; cr.
    reader := TonelReader 
        on: each key asFileReference
        fileName: each value.
    reader loadPackage.
    reader definitions do: [:each | each load].
].
SmalltalkImage current snapshot: true andQuit: true.
!
