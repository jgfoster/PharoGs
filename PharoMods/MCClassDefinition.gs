set compile_env: 2

category: 'installing'
method: MCClassDefinition
createClass

	<PharoGs>
	type == #'normal' ifTrue: [
		^((System @env0:myUserProfile @env0:symbolList @env0:objectNamed: superclassName) 
			@env0:subclass: name
			instVarNames: (self instanceVariables collect: [:each | each name])
			classVars: (self classVariables collect: [:each | each name])
			classInstVars: (self classInstanceVariables collect: [:each | each name])
			poolDictionaries: (self sharedPoolsString collect: [:each | each name]) asArray
			inDictionary: UserGlobals)
			@env0:category: category;
			@env0:comment: comment;
			@env0:commentStamp: commentStamp;
			yourself
	].
	self _gsError.
%

set compile_env: 0
