set compile_env: 2

category: 'initialization'
method: ClassOrganization
initializeClass: aClass

    <PharoGs>
	self initialize.
	organizedClass := aClass.
	organizedClass selectors do: [ :each | 
		self 
            classify: each 
            under: ((aClass @env0:categoryOfSelector: each environmentId: 2) 
                ifNil: [Protocol unclassified])
    ].
%

set compile_env: 0
