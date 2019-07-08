category: 'initialization'
set compile_env: 2
method: Protocol
initialize
	<PharoGs>

	super initialize.
	methods := IdentitySet new.
	name := self class defaultName.
%
set compile_env: 0
