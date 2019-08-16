
"How many methods have a particular pragma?"
| environment methods symbol |
environment := 2.
symbol := #'PharoPrimitive'.
methods := (Globals at: #IdentitySet) new.
(Globals at: #System) myUserProfile symbolList do: [:eachDict | 
	eachDict do: [:eachGlobal | 
		eachGlobal isBehavior ifTrue: [
			((Globals at: #Array) with: eachGlobal class with: eachGlobal) do: [:eachBehavior | 
				(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
					(eachMethod pragmas anySatisfy: [:eachPragma | symbol == eachPragma keyword]) 
						ifTrue: [methods add: eachMethod].
				].
			].
		].
	].
].
methods size.
%

"Load Seaside"
[
	System @env0:abortTransaction. GsSocket @env0:closeAll.
	Metacello new
		baseline:'Seaside3';
		repository: 'github://SeasideSt/Seaside:master/repository';
		repositoryOverrides: (Array 
			with: 'filetree:///Users/jfoster/Library/GemStone/db1/github-cache/SeasideSt/Grease/v1.4.x/SeasideSt-Grease-762ec80/repository'
			with: 'filetree:///Users/jfoster/Library/GemStone/db1/github-cache/SeasideSt/Seaside/master/SeasideSt-Seaside-91ae092/repository');
		onWarningLog;
		load.
] on: Error do: [:ex | 
	ex halt.
].

"Find references to an object (e.g., a Semaphore preventing commit)"
(SystemRepository listReferencesInMemory:
	(Array with: (Object objectForOop: 100681217))) first.
%