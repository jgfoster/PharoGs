set compile_env: 2

category: 'testing'
classmethod: MD5
isPluginAvailable 

    <PharoGsDone>
    ^false
%

category: 'private-buffers'
method: MD5
primProcessBuffer: aByteArray withState: s 

    <PharoGsError>
    self @env0:error: 'Not supported in GemStone'
%

set compile_env: 0
