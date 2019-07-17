set compile_env: 2

category: 'comparing'
classmethod: Message
analogousCodeTo: anObject 

	<PharoGsError>
    self _gsError
%

category: 'accessing'
classmethod: Message
lookupClass

	<PharoGsError>
    self _gsError
%

category: 'private'
classmethod: Message
lookupClass: aClass 

	<PharoGsError>
    self _gsError
%

category: 'sending'
classmethod: Message
sentTo: receiver 

	<PharoGs>
    self @env0:add: anException
%

set compile_env: 0
