set compile_env: 2

category: 'comparing'
method: Message
analogousCodeTo: anObject 

	<PharoGsError>
    self _gsError
%

category: 'accessing'
method: Message
lookupClass

	<PharoGsError>
    self _gsError
%

category: 'private'
method: Message
lookupClass: aClass 

	<PharoGsError>
    self _gsError
%

set compile_env: 0
