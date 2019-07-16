! use Pharo instance variable 'socketHandle' to hold an instance of GsSocket

set compile_env: 2

category: 'primitives'
method: Socket
primAcceptFrom: aHandle receiveBufferSize: rcvBufSize sendBufSize: sndBufSize semaIndex: semaIndex 
	"Create and return a new socket handle based on accepting the connection from the given listening socket" 

	<PharoGs> 
    ^aHandle @env0:accept
%

category: 'primitives'
method: Socket
primAcceptFrom: aHandle receiveBufferSize: rcvBufSize sendBufSize: sndBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema 
	"Create and return a new socket handle based on accepting the connection from the given listening socket" 

	<PharoGs> 
    ^aHandle @env0:accept
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID bindTo: socketAddress 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocket: socketID bindTo: anAddress port: aPort  
	"Bind socket to provided IPv4 address and port" 

	<PharoGs> 
    ^socketID @env0:bindTo: aPort toAddress: anAddress
%

category: 'primitives'
method: Socket
primSocket: socketID connectTo: socketAddress 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocket: socketID connectTo: hostAddress port: port 
	"Attempt to establish a connection to the given port of the given host. 
    This is an asynchronous call; query the socket status to discover if and when 
    the connection is actually completed." 

	<PharoGs> 
    ^socketID @env0:connectTo: port on: hostAddress
%

category: 'primitives'
method: Socket
primSocket: socketID getOption: aString  
	"Get some option information on this socket. Refer to the UNIX  
	man pages for valid SO, TCP, IP, UDP options. In case of doubt 
	refer to the source code. 
	TCP:=NODELAY, SO:=KEEPALIVE are valid options for example 
	returns an array containing the error code and the option value" 

	<PharoGs> 
    ^socketID @env0:option: aString
%

category: 'primitives'
method: Socket
primSocket: socketID listenOn: port 
	"Listen for a connection on the given port. This is an asynchronous call; 
    query the socket status to discover if and when the connection is actually completed." 

	<PharoGs> 
    ^socketID @env0:makeServer: 5 atPort: port
%

category: 'primitives'
method: Socket
primSocket: aHandle listenOn: portNumber backlogSize: backlog 
	"Primitive. Set up the socket to listen on the given port. 
	Will be used in conjunction with #accept only." 

	<PharoGs> 
    ^aHandle @env0:makeServer: backlog atPort: portNumber
%

category: 'primitives'
method: Socket
primSocket: aHandle listenOn: portNumber backlogSize: backlog interface: ifAddr 
	"Primitive. Set up the socket to listen on the given port. 
	Will be used in conjunction with #accept only." 

	<PharoGs> 
    ^aHandle @env0:makeServer: backlog atPort: portNumber atAddress: ifAddr
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID listenWithBacklog: backlogSize 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID localAddressResult: socketAddress 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocket: socketID receiveDataInto: aStringOrByteArray startingAt: startIndex count: count 
	"Receive data from the given socket into the given array starting at the given index. Return the number of bytes read or zero if no data is available." 

	<PharoGs> 
    ^socketID @env0:read: count into: aStringOrByteArray startingAt: startIndex
%

category: 'primitives'
method: Socket
primSocket: socketID receiveUDPDataInto: aStringOrByteArray startingAt: startIndex count: count 
	"Receive data from the given socket into the given array starting at the given index.  
	Return an Array containing the amount read, the host address byte array, the host port, and the more flag" 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID remoteAddressResult: socketAddress 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocket: socketID sendData: aStringOrByteArray startIndex: startIndex count: count 
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed." 
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created." 

	<PharoGs> 
    ^socketID @env0:write: count from: aStringOrByteArray startingAt: startIndex
%

category: 'primitives'
method: Socket
primSocket: socketID sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber startIndex: startIndex count: count 
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed." 
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created." 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocket: socketID setOption: aString value: aStringValue 
	"Set some option information on this socket. Refer to the UNIX  
	man pages for valid SO, TCP, IP, UDP options. In case of doubt 
	refer to the source code. 
	TCP:=NODELAY, SO:=KEEPALIVE are valid options for example 
	returns an array containing the error code and the negotiated value" 

	<PharoGs> 
    ^socketID @env0:option: aString put: aStringValue
%

category: 'primitives'
method: Socket
primSocket: socketID setPort: port 
	"Set the local port associated with a UDP socket. 
	Note: this primitive is overloaded.  The primitive will not fail on a TCP socket, but 
	the effects will not be what was desired.  Best solution would be to split Socket into 
	two subclasses, TCPSocket and UDPSocket." 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketAbortConnection: socketID 
	"Terminate the connection on the given port immediately without going through the normal close sequence. This is an asynchronous call; query the socket status to discover if and when the connection is actually terminated." 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketCloseConnection: socketID 
	"Close the connection on the given port. The remote end is informed that this end has closed and will do no further sends. This is an asynchronous call; query the socket status to discover if and when the connection is actually closed." 

	<PharoGs> 
    ^socketID @env0:close
%

category: 'primitives'
method: Socket
primSocketConnectionStatus: socketID 
	"Return an integer reflecting the connection status of this socket. For a list of possible values, see the comment in the 'initialize' method of this class. If the primitive fails, return a status indicating that the socket handle is no longer valid, perhaps because the Pharo image was saved and restored since the socket was created. (Sockets do not survive snapshots.)" 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex 
	"Return a new socket handle for a socket of the given type and buffer sizes. Return nil if socket creation fails. 
	The netType parameter is platform dependent and can be used to encode both the protocol type (IP, Xerox XNS, etc.) and/or the physical network interface to use if this host is connected to multiple networks. A zero netType means to use IP protocols and the primary (or only) network interface. 
	The socketType parameter specifies: 
		0	reliable stream socket (TCP if the protocol is IP) 
		1	unreliable datagram socket (UDP if the protocol is IP) 
	The buffer size parameters allow performance to be tuned to the application. For example, a larger receive buffer should be used when the application expects to be receiving large amounts of data, especially from a host that is far away. These values are considered requests only; the underlying implementation will ensure that the buffer sizes actually used are within allowable bounds. Note that memory may be limited, so an application that keeps many sockets open should use smaller buffer sizes. Note the macintosh implementation ignores this buffer size. Also see setOption to get/set socket buffer sizes which allows you to set/get the current buffer sizes for reading and writing. 
 	If semaIndex is > 0, it is taken to be the index of a Semaphore in the external objects array to be associated with this socket. This semaphore will be signalled when the socket status changes, such as when data arrives or a send completes. All processes waiting on the semaphore will be awoken for each such event; each process must then query the socket state to figure out if the conditions they are waiting for have been met. For example, a process waiting to send some data can see if the last send has completed." 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema 
	"See comment in primSocketCreateNetwork: with one semaIndex. However you should know that some implementations 
	ignore the buffer size and this interface supports three semaphores,  one for open/close/listen and the other two for 
	reading and writing" 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketDestroy: socketID 
	"Release the resources associated with this socket. If a connection is open, it is aborted." 

	<PharoGs> 
    ^socketID @env0:close
%

category: 'primitives'
method: Socket
primSocketDestroyGently: socketID 
	"Release the resources associated with this socket. If a connection is open, it is aborted. 
	Do not fail if the receiver is already closed." 

	<PharoGs> 
    ^socketID @env0:close
%

category: 'primitives'
method: Socket
primSocketError: socketID 
	"Return an integer encoding the most recent error on this socket. Zero means no error." 

	<PharoGs> 
    ^socketID @env0:lastErrorCode
%

category: 'primitives'
method: Socket
primSocketLocalAddress: socketID 
	"Return the local host address for this socket." 

	<PharoGs> 
    ^socketID @env0:address
%

category: 'primitive-ipv6'
method: Socket
primSocketLocalAddressSize: handle 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketLocalPort: socketID 
	"Return the local port for this socket, or zero if no port has yet been assigned." 

	<PharoGs> 
    ^socketID @env0:port
%

category: 'primitives'
method: Socket
primSocketReceiveDataAvailable: socketID 
	"Return true if data may be available for reading from the current socket." 

	<PharoGs> 
    ^socketID @env0:readWillNotBlock
%

category: 'primitives'
method: Socket
primSocketRemoteAddress: socketID 
	"Return the remote host address for this socket, or zero if no connection has been made." 

	<PharoGs> 
    ^socketID @env0:peerAddress
%

category: 'primitives-ipv6'
method: Socket
primSocketRemoteAddressSize: handle 

	<PharoGsError> 
    ^self @env0:error: 'Not supported in GemStone'
%

category: 'primitives'
method: Socket
primSocketRemotePort: socketID 
	"Return the remote port for this socket, or zero if no connection has been made." 

	<PharoGs> 
    ^socketID @env0:peerPort
%

category: 'primitives'
method: Socket
primSocketSendDone: socketID 
	"Return true if there is no send in progress on the current socket." 

	<PharoGs> 
    ^true "GemStone socket operations are synchronous"
%

set compile_env: 0
