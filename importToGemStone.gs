ifErr 1 stk
ifErr 2 stack
ifErr 3 abort
ifErr 4 exit
run
| symbolList |
symbolList := (Globals at: #System) myUserProfile symbolList.
(symbolList objectNamed: #'Smalltalk') ifNil: [
	symbolList 
		createDictionaryNamed: #'Smalltalk' at: 0;
		createDictionaryNamed: #'PharoPools' at: 0;
		yourself.
].
%
errorCount
commit
logout
login
input globals.gs
input pools.gs
input classes.gs
input methods.gs
input importGsEdits.tpz

output push UserGlobals.out only
errorCount
input UserGlobals.gs
errorCount
output pop

errorCount
run
Object superclassForEnv: 2 put: ProtoObject.
Object class superclassForEnv: 2 put: ProtoObject class.
Array superclassForEnv: 2 put: ArrayedCollection.
Array class superclassForEnv: 2 put: ArrayedCollection class.
String superclassForEnv: 2 put: ArrayedCollection.
String class superclassForEnv: 2 put: ArrayedCollection class.
true
%
errorCount
commit
logout
exit
