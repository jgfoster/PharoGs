set compile_env: 2

category: 'testing'
classmethod: MD5
isPluginAvailable 

    <PharoGs>
    ^false
%

category: 'private-buffers'
method: MD5
primProcessBuffer: aByteArray withState: s 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
