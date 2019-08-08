set compile_env: 2

category: 'accessing'
classmethod: Job
jobAnnouncer
	"Answers the announcer for job announcements"

    <PharoGs>
    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Job_jobAnnouncer' 
		ifAbsentPut: [Announcer new].
%

set compile_env: 0
