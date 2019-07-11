

	| environment methods symbol |
	environment := 2.
	symbol := #'PharoGs'.
	methods := (Globals at: #IdentitySet) new.
	(Globals at: #System) myUserProfile symbolList do: [:eachDict | 
		eachDict do: [:eachGlobal | 
			eachGlobal isBehavior ifTrue: [
				((Globals at: #Array) with: eachGlobal class with: eachGlobal) do: [:eachBehavior | 
					(eachBehavior methodDictForEnv: environment) do: [:eachMethod | 
						(eachMethod pragmas anySatisfy: [:eachPragma | symbol == eachPragma keyword]) ifTrue: [methods add: eachMethod].
					].
				].
			].
		].
	].
	methods size.
