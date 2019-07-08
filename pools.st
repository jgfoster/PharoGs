| fileStream pools |
fileStream := (FileStream forceNewFileNamed: 'pools.gs')
	text; yourself.
pools := IdentitySet new.
ProtoObject withAllSubclasses do: [:eachClass | 
	pools addAll: eachClass sharedPools.
].
fileStream 
	nextPutAll: 'output push pools.out only'; lf;
	nextPutAll: 'errorCount'; lf.
pools := pools asSortedCollection: [:a :b | a name <= b name].
pools do: [:eachSharedPool | 
	fileStream 
		nextPutAll: 'run'; lf;
		nextPutAll: '(PharoPools at: #';
		nextPutAll: eachSharedPool name;
		nextPutAll: ' ifAbsentPut: [SymbolDictionary new name: #';
		nextPutAll: eachSharedPool name;
		nextPutAll: '])'; 
		lf.
	eachSharedPool classPool keys asSortedCollection do: [:eachKey | 
		fileStream
			tab; nextPutAll: 'at: #'; 
			nextPutAll: eachKey; 
			nextPutAll: ' ifAbsentPut: [nil];'; lf.
	].
	fileStream tab; nextPutAll: 'yourself.'; lf; nextPut: $%; lf.
].
fileStream 
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output pop'; lf;
	nextPutAll: 'errorCount'; lf;
	close.

Transcript show: 'pools count = ' , pools size printString; cr.
SmalltalkImage current snapshot: false andQuit: true.
