set compile_env: 2

category: 'instance creation'
classmethod: DateAndTime
now
	"Answer the current date and time expressed in local time.
	[ 10000 timesRepeat: [ self now. ] ] timeToRun / 10000.0 . "

	<PharoGs>
	| nanoTicks |
	"Multiply by Integer rather than Float to avoid conversion in GemStone"
	nanoTicks := self clock microsecondClockValue * 1000.
	^ self basicNew
		setJdn: SqueakEpoch 
		seconds: 0
		nano: nanoTicks
		offset: self localOffset
%

set compile_env: 0
