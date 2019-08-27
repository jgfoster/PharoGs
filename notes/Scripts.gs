
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
"
With Seaside, we have:
classes			   2802
methods			  39348
PharoCompileError	225
PharoGs				809
PharoGsError		320
PharoPrimitive		 70
"
%

"Find references to an object (e.g., a Semaphore preventing commit)"
(SystemRepository listReferencesInMemory:
	(Array with: (Object objectForOop: 100681217))) first.
%