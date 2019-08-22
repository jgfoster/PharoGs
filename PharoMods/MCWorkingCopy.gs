set compile_env: 2

category: 'private'
method: MCWorkingCopy
ancestorsFromArray: anArray cache: cacheDictionary
	"While using recursion is simpler, it runs a risk of stack overflow for packages with many ancestors,
     so we use a local stack to pre-load the cache in reverse order. 
     The original code remains on the last line, so the intermediate code 
     does not change the external behavior. "

    <PharoGs>
    | index stack |
    anArray ifNil: [^nil].
    stack := OrderedCollection withAll: anArray.
    index := 1.
    [ index <= stack size ] whileTrue: [
        | dict id |
        dict := stack at: index.
        id := (dict at: #id) asString.
        (cacheDictionary includesKey: id) ifFalse: [
            stack addAll: (dict at: #ancestors ifAbsent: [#()]).
        ].
        index := index + 1.
    ].
    stack reverseDo: [:each | 
        self infoFromDictionary: each cache: cacheDictionary.
    ].
    ^ anArray collect: [:dict | self infoFromDictionary: dict cache: cacheDictionary]
%

set compile_env: 0
