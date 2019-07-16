set compile_env: 2

category: 'communication'
method: Semaphore
signal 
	"Primitive. Send a signal through the receiver. If one or more processes  
	have been suspended trying to receive a signal, allow the first one to  
	proceed. If no process is waiting, remember the excess signal. Essential.  
	See Object documentation whatIsAPrimitive." 

	<PharoGs> 
    ^self @env0:signal
%

category: 'communication'
method: Semaphore
wait 
	"Primitive. The active Process must receive a signal through the receiver  
	before proceeding. If no signal has been sent, the active Process will be  
	suspended until one is sent. Essential. See Object documentation  
	whatIsAPrimitive." 

	<PharoGs> 
    ^self @env0:wait
%

set compile_env: 0
