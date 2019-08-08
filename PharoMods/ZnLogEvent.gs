set compile_env: 2

category: 'accessing'
classmethod: ZnLogEvent
announcer

    <PharoGs>
    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'ZnLogEvent_LogEventAnnouncer' 
		ifAbsentPut: [Announcer new].
%

set compile_env: 0
