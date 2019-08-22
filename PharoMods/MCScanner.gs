set compile_env: 2

category: 'actions'
method: MCScanner
next
	"This would be slightly simpler using recursion but it risks a stack overflow 
     for some packages on some platforms, so we implement it using a local stack."

	<PharoGs>
	
    | stack |
    stack := Stack with: Stack new.
    [
        | c |
        stream skipSeparators.
        c := stream peek.
        "We essentially ignore the $# symbol and expect what follows to be a symbol or array"
        c = $# ifTrue: [ c := stream next; peek ].
        "Note that #'foo' will be treated as a String, not a Symbol"
        c = $' ifTrue: [ stack top push: self nextString ] ifFalse: [   "'"
        "Any alphanumeric, including an integer, is treated as a Symbol even if not preceeded by a $#"
        c isAlphaNumeric ifTrue: [ stack top push: self nextSymbol ] ifFalse: [
        "For an array, start a new level on the stack"
        c = $( ifTrue: [ stream next. stack push: Stack new ] ifFalse: [
        "At the end of an array, so add it to the previous level"
        c = $) ifTrue: [ | x | stream next. x := stack pop. stack top push: x asArray reverse ] ifFalse: [ 
        "Unexpected token"
        self error: 'Unknown token type' ]]]].
        "Keep looping while we are in an array"
        1 < stack size.
    ] whileTrue: [].
    ^stack top top
%

set compile_env: 0
