set compile_env: 2

category: 'ssl'
method: ZdcSecureSocketStream
connect
    
    <PharoGs>
    socket connectSecureSocket.
%

category: 'testing'
method: ZdcSecureSocketStream
isConnected
    
    <PharoGs>
	^super isConnected @env0:and: [socket hasSecureConnection]
%

set compile_env: 0
