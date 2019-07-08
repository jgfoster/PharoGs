| fileStream dict |
fileStream := (FileStream forceNewFileNamed: 'globals.gs') text;
	nextPutAll: 'output push globals.out only'; lf;
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'run'; lf;
	nextPutAll: 'Smalltalk'; lf.
dict := Smalltalk globals reject: [ :each | each isBehavior or: [each == Smalltalk]].

dict keys asSortedCollection do: [:eachKey | 
	fileStream 
		tab; nextPutAll: 'at: #'; 
		nextPutAll: eachKey; 
		nextPutAll: ' put: nil;';
		lf.
].
fileStream 
	tab; nextPutAll: 'at: #superclass put: nil;'; lf;
	tab; nextPutAll: 'at: #thisContext put: nil;'; lf;
	tab; nextPutAll: 'at: #Color put: nil;'; lf;
	tab; nextPutAll: 'at: #ColorMap put: nil;'; lf;
	tab; nextPutAll: 'at: #ImageReadWriter put: nil;'; lf;
	tab; nextPutAll: 'at: #MailAddressParser put: nil;'; lf;
	tab; nextPutAll: 'at: #MailMessage put: nil;'; lf;
	tab; nextPutAll: 'at: #STON put: nil;'; lf;
	
	tab; nextPutAll: 'at: #lookupClass put: #Message;'; lf;
	tab; nextPutAll: 'at: #quiescentProcessLists put: #ProcessorScheduler;'; lf;
	tab; nextPutAll: 'at: #exceptions put: #ExceptionSet;'; lf;
	tab; nextPutAll: 'at: #startpc put: #BlockClosure;'; lf;
	tab; nextPutAll: 'at: #numArgs put: #BlockClosure;'; lf;
	tab; nextPutAll: 'at: #outerContext put: #BlockClosure;'; lf;
	tab; nextPutAll: 'at: #signalContext put: #Exception;'; lf;
	tab; nextPutAll: 'at: #handlerContext put: #Exception;'; lf;
	tab; nextPutAll: 'at: #outerContext put: #Exception;'; lf;
	tab; nextPutAll: 'at: #signaler put: #Exception;'; lf;
	tab; nextPutAll: 'at: #layout put: #Behavior;'; lf;
	tab; nextPutAll: 'at: #methodDict put: #Exception;'; lf;
	tab; nextPutAll: 'at: #startpc put: #FullBlockClosure;'; lf;
	tab; nextPutAll: 'at: #outerContext put: #FullBlockClosure;'; lf;
	tab; nextPutAll: 'at: #numArgs put: #FullBlockClosure;'; lf;
	tab; nextPutAll: 'at: #signalContext put: #Halt;'; lf;
	tab; nextPutAll: 'at: #organization put: #ClassDescription;'; lf;
	tab; nextPutAll: 'at: #layout put: #ClassDescription;'; lf;
	tab; nextPutAll: 'at: #reachedDefaultHandler put: #MessageNotUnderstood;'; lf;
	tab; nextPutAll: 'at: #message put: #MessageNotUnderstood;'; lf;
	tab; nextPutAll: 'at: #result put: #CannotReturn;'; lf;
	tab; nextPutAll: 'at: #category put: #Class;'; lf;
	tab; nextPutAll: 'at: #localSelectors put: #Class;'; lf;
	tab; nextPutAll: 'at: #classPool put: #Class;'; lf;
	tab; nextPutAll: 'at: #sharedPools put: #Class;'; lf;
	tab; nextPutAll: 'at: #environment put: #Class;'; lf;
	tab; nextPutAll: 'at: #traitComposition put: #Class;'; lf;
	tab; nextPutAll: 'at: #thisClass put: #Metaclass;'; lf;
	tab; nextPutAll: 'at: #localSelectors put: #Metaclass;'; lf;
	tab; nextPutAll: 'at: #traitComposition put: #Metaclass;'; lf;
	tab; nextPutAll: 'at: #CaseSensitiveOrder put: #ByteSymbol;'; lf;
	tab; nextPutAll: 'yourself.'; lf;
	nextPut: $%; lf;
	nextPutAll: 'errorCount'; lf;
	nextPutAll: 'output pop'; lf;
	nextPutAll: 'errorCount'; lf;
	close.

Transcript show: 'globals count = ' , (dict size + 8) printString; cr.
SmalltalkImage current snapshot: false andQuit: true.
