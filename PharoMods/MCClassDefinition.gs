set compile_env: 2

category: 'installing'
method: MCClassDefinition
createClass

	<PharoGs>
	| superClass |
	superClass := System @env0:myUserProfile @env0:symbolList @env0:objectNamed: superclassName.
	superClass @env0:subclassesDisallowed ifTrue: [superClass := NoSubclasses].
	type == #'normal' ifTrue: [
		^(superClass 
			@env0:subclass: name
			instVarNames: (self instanceVariables collect: [:each | each name])
			classVars: (self classVariables collect: [:each | each name])
			classInstVars: (self classInstanceVariables collect: [:each | each name])
			poolDictionaries: (self sharedPoolsString collect: [:each | each name]) asArray
			inDictionary: Pharo)
			@env0:category: category;
			@env0:comment: comment;
			@env0:commentStamp: commentStamp;
			yourself
	].
	type == #'variable' ifTrue: [
		^(superClass
			@env0:indexableSubclass: name
			instVarNames: (self instanceVariables collect: [:each | each name])
			classVars: (self classVariables collect: [:each | each name])
			classInstVars: (self classInstanceVariables collect: [:each | each name])
			poolDictionaries: (self sharedPoolsString collect: [:each | each name]) asArray
			inDictionary: Pharo)
			@env0:category: category;
			@env0:comment: comment;
			@env0:commentStamp: commentStamp;
			yourself
	].
	self _gsError.
%

set compile_env: 0
