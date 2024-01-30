Metacello new
  repository: 'github://pharo-vcs/tonel:Pharo12';
  baseline: 'Tonel';
  load: 'core'.
Transcript cr.
!
Transcript show: 'installing PharoGs class ...'.
CodeImporter evaluateFileStream: 'PharoGs.st' asFileReference readStream.
Transcript show: 'Completed PharoGs install'; cr.
!
PharoGsExport new doExport.
!
SmalltalkImage current snapshot: false andQuit: true.
!
