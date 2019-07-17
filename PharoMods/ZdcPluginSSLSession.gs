set compile_env: 2

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle accept: srcbuf startingAt: start count: length into: dstbuf 
	"Primitive. Starts or continues a server handshake using the provided data. 
	Will eventually produce output to be sent to the client. 
	Returns: 
		> 0   Number of bytes to be sent to the server 
		= 0   Success. The connection is established 
		= -1  More input is required 
		< -1  Other errors" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle connect: srcbuf startingAt: start count: length into: dstbuf 
	"Primitive. Starts or continues a client handshake using the provided data. 
	Will eventually produce output to be sent to the server. 
	Returns: 
		> 0   Number of bytes to be sent to the server 
		= 0   Success. The connection is established 
		= -1  More input is required 
		< -1  Other errors" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle decrypt: srcbuf startingAt: start count: length into: dstbuf 
	"Primitive. Takes incoming data for decryption and continues to decrypt data. 
	Returns the number of bytes produced in the output" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle encrypt: srcbuf startingAt: start count: length into: dstbuf 
	"Primitive. Encrypts the incoming buffer into the result buffer. 
	Returns the number of bytes produced as a result" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle getIntProperty: propID 
	"Primitive. Returns a string property from an SSL session" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle getStringProperty: propID 
	"Primitive. Returns a string property from an SSL session" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle setIntProperty: propID toValue: anInteger 
	"Primitive. Sets an integer property in an SSL session" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSL: sslHandle setStringProperty: propID toValue: aString 
	"Primitive. Sets a string property in an SSL session" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSLCreate 
	"Primitive. Creates and returns a new SSL handle in the VM plugin" 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: ZdcPluginSSLSession
primitiveSSLDestroy: sslHandle 
	"Primitive. Destroys the SSL session handle in the VM plugin" 

	<PharoGsError> 
    ^self _gsError
%

set compile_env: 0
