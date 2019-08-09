set compile_env: 2

category: 'TTranscript'
method: TranscriptStreamPortable
<< anObject
	"Write anObject to the receiver, dispatching using #putOn:
	This is a shortcut for both nextPut: and nextPutAll: since anObject can be both
	the element type of the receiver as well as a collection of those elements.
	No further conversions of anObject are applied.
	Return self to accomodate chaining."

    <PharoGs>
 	anObject putOn: self
%

category: 'TTranscript'
method: TranscriptStreamPortable
clear
	"Clear the receiver, removing all existing output"

    <PharoGs>
%

category: 'TTranscript'
method: TranscriptStreamPortable
close
	"Close the receiver, indicating it is not longer needed"

    <PharoGs>
%

category: 'TTranscript'
method: TranscriptStreamPortable
cr
	"Output a cr on the receiver, buffered and not yet shown"
	
    <PharoGs>
	self nextPut: Character cr
%

category: 'TTranscript'
method: TranscriptStreamPortable
crShow: anObject
	"Output anObject asString on the receiver preceded by a cr and show the output"
	
    <PharoGs>
	self critical: [ self cr; show: anObject ]
%

category: 'explicitRequirement'
method: TranscriptStreamPortable
critical: block
	"Execute block making sure only one thread accesses the receiver"
	
    <PharoGs>
   	self @env0:mutex @env0:critical: [ block value ]
%

category: 'TTranscript'
method: TranscriptStreamPortable
endEntry
	"Show the currently buffered output"

    <PharoGs>
	self flush
%

category: 'explicitRequirement'
method: TranscriptStreamPortable
flush
	"Show the currently buffered output"

    <PharoGs>
    self @env0:flush
%

category: 'explicitRequirement'
method: TranscriptStreamPortable
nextPut: character
	"Output character on the receiver, buffered, not yet shown"

    <PharoGs>
    self @env0:stream @env0:nextPut: character
%

category: 'explicitRequirement'
method: TranscriptStreamPortable
nextPutAll: string
	"Output string on the receiver, buffered, not yet shown"

    <PharoGs>
    self @env0:stream @env0:nextPutAll: string
%

category: 'TTranscript'
method: TranscriptStreamPortable
print: anObject
	"Output anObject asString on the receiver, buffered, not yet shown"
	
    <PharoGs>
	self nextPutAll: anObject asString
%

category: 'TTranscript'
method: TranscriptStreamPortable
show: anObject
	"Output anObject asString on the receiver and show the output"
	
    <PharoGs>
	self print: anObject; endEntry
%

category: 'TTranscript'
method: TranscriptStreamPortable
space
	"Output a space on the receiver, buffered and not yet shown"

    <PharoGs>
	self nextPut: Character space
%

category: 'TTranscript'
method: TranscriptStreamPortable
tab
	"Output a tab on the receiver, buffered and not yet shown"

    <PharoGs>
	self nextPut: Character tab
%

set compile_env: 0
