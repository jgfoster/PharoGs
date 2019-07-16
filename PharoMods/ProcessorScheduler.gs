set compile_env: 2

category: 'background process'
classmethod: ProcessorScheduler
relinquishProcessorForMicroseconds: anInteger 
	"Platform specific. This primitive is used to return processor cycles 
    to the host operating system when Pharo's idle process is running 
    (i.e., when no other Pharo process is runnable). On some platforms, 
    this primitive causes the entire Pharo application to sleep for 
    approximately the given number of microseconds. No Pharo process can 
    run while the Pharo application is sleeping, even if some external 
    event makes it runnable. On the Macintosh, this primitive simply 
    calls GetNextEvent() to give other applications a chance to run. 
    On platforms without a host operating system, it does nothing. 
    This primitive should not be used to add pauses to a Pharo process; 
    use a Delay instead." 
    <PharoGs>

    "don't fail if primitive is not implemented, just do nothing"
%

category: 'process state change'
method: ProcessorScheduler
yield 
	"Give other Processes at the current priority a chance to run." 
	<PharoGs>

    ^self @env0:yield
%

set compile_env: 0
