set compile_env: 0
! ------------------- Class definition for MutexWrapper
expectvalue /Class
doit
SemaphoreWrapper subclass: 'MutexWrapper'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #()
  inDictionary: Pharo
  options: #( dbTransient)

%
expectvalue /Class
doit
MutexWrapper category: 'Kernel'
%

set compile_env: 2

category: 'private'
method: MutexWrapper
_semaphore

	^semaphore ifNil: [semaphore := super _semaphore signal].
%

set compile_env: 0
