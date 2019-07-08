category: 'examples'
set compile_env: 2
classmethod: Job
exampleCurrentValue
	<PharoGs>

	[ :job| 
		job title: 'Simulating some progress for 1 Second'. 
		1 second wait. "simulate some work"
		job currentValue: 50.
		1 second wait. "simulate some more work"
		job currentValue: 100.
		1 second wait. "simulate some more work"
	] asJob run
%
set compile_env: 0
