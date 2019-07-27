set compile_env: 2

category: 'accessing'
classmethod: OSPlatform
current 

    <PharoGs>
    ^Current ifNil: [self initialize. Current]
%

category: 'accessing'
method: OSPlatform
currentWorkingDirectoryPath 

    <PharoGs>
    ^GsFile @env0:serverCurrentDirectory
%

category: 'accessing'
method: OSPlatform
currentWorkingDirectoryPathWithBuffer: aByteString 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
