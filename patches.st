"
While our goal is to work with an officially-distributed Pharo minimal image,
there may be some things that need to be modified in order to load in GemStone.

One approach is to build our own minimal image (modify the Git checkout and 
use bootstrap to create the needed image). This takes about 15 minutes, so
is a bit of a barrier to rapid development.

This script provides a way to load modified packages into the otherwise 
minimal image. Each time a new minimal image is created with bootstrap, the
list of reloaded packages can be cleared.
"
Metacello new
  repository: 'github://pharo-vcs/tonel';
  baseline: 'Tonel';
  load: 'core'.
Transcript cr.
!
| list |
list := OrderedCollection new
    add: '/Users/jfoster/code/pharo/src/' -> 'Monticello';
    add: '/Users/jfoster/code/pharo/src/' -> 'SUnit-Core';
    add: '/Users/jfoster/code/pharo/src/' -> 'Kernel-Tests';
"
    add: '/Users/jfoster/code/pharo/src/' -> 'XXX';
"
    yourself.
list do: [:each | | reader |
    Transcript show: each value; cr.
    reader := TonelReader 
        on: each key asFileReference
        fileName: each value.
    (MCSnapshot fromDefinitions: reader definitions) install.
].
!
SmalltalkImage current snapshot: true andQuit: true.
!
