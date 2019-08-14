set compile_env: 2

category: 'installing'
method: MCTraitDefinition
createClass

	<PharoGs>
    ^(ProtoObject
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
%

set compile_env: 0
