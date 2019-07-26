set compile_env: 2

category: 'accessing'
classmethod: TestCase
historyAnnouncer

	<PharoGs>
	^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'TestCase_HistoryAnnouncer' 
		ifAbsentPut: [Announcer new].
%

set compile_env: 0
