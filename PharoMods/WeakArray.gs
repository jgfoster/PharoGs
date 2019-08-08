set compile_env: 2

category: 'accessing'
classmethod: WeakArray
addWeakDependent: anObject

	<PharoGs>
%

category: '*System-Finalization'
classmethod: WeakArray
primitiveFetchMourner 
	"Answer the next mourner in the VM's queue of objects to be finalized.  '
	 The queue contains weak arrays and ephemerons.  If the primitive is 
	 not implemented, raise an error telling people to upgrade the VM.  If 
	 implemented, the primitive fails if the queue is empty, with the error 
	 code #'not found'. Primitive.  Essential." 
    
    <PharoGsError>
    ^self _gsError
%

category: 'accessing'
classmethod: WeakArray
removeWeakDependent: anObject

	<PharoGs>
%

set compile_env: 0
