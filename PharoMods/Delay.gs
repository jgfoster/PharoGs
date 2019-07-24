set compile_env: 2

category: 'sempahore'
classmethod: Delay
scheduler
    "Because we can not commit a Semaphore, it can not be in a class variable.
     Instead we have a SessionTemp with the instance."
    
    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Delay_scheduler' 
        ifAbsent: [     "copied from initialize"
            | scheduler |
            self scheduler: (scheduler := DelaySemaphoreScheduler new).
            scheduler startTimerEventLoop. 
            SessionManager default 
                registerSystemClassNamed: self name 
                atPriority: 20.
            scheduler
        ]
%

category: 'sempahore'
classmethod: Delay
scheduler: anObject

    ^(Globals @env0:at: #'SessionTemps') @env0:current 
        @env0:at: #'Delay_scheduler' 
        put: anObject
%

set compile_env: 0
