set compile_env: 2

category: 'operations'
method: MethodAddition
createCompiledMethod

   	<PharoGs>
    [[
        compiledMethod := myClass 
            @env0:compileMethod: text asString
            dictionaries: System @env0:myUserProfile @env0:symbolList
            category: category @env0:asSymbol
            intoMethodDict: false
            intoCategories: nil
            environmentId: 2.
    ] @env0:on: (Globals @env0:at: #'CompileError') do: [:ex | ^nil].
    ] @env0:on: (Globals @env0:at: #'CompileWarning') do: [:ex | ex @env0:resume].
    selector := compiledMethod selector.
	priorMethodOrNil := myClass compiledMethodAt: selector ifAbsent: [ nil ].
	priorCategoryOrNil := myClass organization categoryOfElement: selector.
%

category: 'operations'
method: MethodAddition
installMethod

   	<PharoGs>
    [
        compiledMethod := myClass 
            @env0:compileMethod: text asString
            dictionaries: System @env0:myUserProfile @env0:symbolList
            category: category @env0:asSymbol
            intoMethodDict: nil
            intoCategories: nil
            environmentId: 2.
    ] @env0:on: (Globals @env0:at: #'CompileWarning') do: [:ex | ex @env0:resume].
%

set compile_env: 0
