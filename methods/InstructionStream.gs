category: '*OpalCompiler-Core'
set compile_env: 2
classmethod: InstructionStream
compiler
	<PharoGs>
	"The JIT compiler needs to trap all reads to instance variables of contexts. As this check is costly, it is only done
	in the long form of the bytecodes, which are not used often. In this hierarchy we force the compiler to alwasy generate
	long bytecodes"
	^super compiler options: #(#+ optionLongIvarAccessBytecodes)
%
set compile_env: 0
