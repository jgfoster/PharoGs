set compile_env: 2

category: 'changing process state'
method: Process
primitiveResume 
	"Primitive. Allow the process that the receiver represents to continue. Put  
	the receiver in line to become the activeProcess. Fail if the receiver is  
	already waiting in a queue (in a Semaphore or ProcessScheduler).  
	Essential. See Object documentation whatIsAPrimitive." 
	<PharoGs>

    ^self @env0:resume 
%

category: 'changing process state'
method: Process
suspend 
	"Primitive. Stop the process that the receiver represents in such a way  
	that it can be restarted at a later time (by sending the receiver the  
	message resume). If the receiver represents the activeProcess, suspend it.  
	Otherwise remove the receiver from the list of waiting processes. 
	The return value of this method is the list the receiver was previously on (if any)."
	<PharoGs>

    ^self @env0:suspend 
%

set compile_env: 0
