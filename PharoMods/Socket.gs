! use Pharo instance variable 'socketHandle' to hold an instance of GsSocket

set compile_env: 2

category: 'primitives'
classmethod: Socket
newTCP
	"Create a socket and initialise it for TCP" 

	<PharoGs>
	^self basicNew 
		initialize: GsSocket @env0:new;
		yourself
%

category: 'primitives'
classmethod: Socket
newUDP
	"Create a socket and initialise it for TCP" 

	<PharoGs>
	^self basicNew 
		initialize: GsSocket @env0:newUdp;
		yourself
%

category: 'registry'
classmethod: Socket
register: anObject

	<PharoGs>
	"ignored"
%

category: 'registry'
classmethod: Socket
unregister: anObject

	<PharoGs>
	"ignored"
%

category: 'connection open/close'
method: Socket
connectNonBlockingTo: hostAddress port: port 
	"PHARO: 
	Initiate a connection to the given port at the given host address. 
	This operation will return immediately; follow it with waitForConnectionUntil: 
	to wait until the connection is established." 

	<PharoGs>
	"GEMSTONE:
	Connect the receiver to the server socket identified by port and hostAddress.
	hostAddress may be the name of the host or its numeric address,
	or hostAddress == -1 for <broadcast> , or hostAddress == nil for IN6ADDR_ANY_INIT .
	port maybe either a SmallInteger, or the String name of a service.
	If hostAddress is the name of a host, connect is attempted on IPv4 first 
	if getaddrinfo returns any IPv4 addresses for hostAddress, then attempted
	on IPv6."

	| addrList canonName host |
	(port @env0:_isSmallInteger @env0:and:[ port @env0:< 0]) @env0:ifTrue:[
		OutOfRange @env0:new @env0:name:'port' min: 0 actual: port ; @env0:signal
	].
	host := (hostAddress at: 1) printString , '.' , 
		(hostAddress at: 2) printString , '.' , 
		(hostAddress at: 3) printString , '.' , 
		(hostAddress at: 4) printString.
	addrList := socketHandle 
		@env0:_twoArgPrim: 25 
		with: host
		with: port. "calls getaddrinfo"
	(addrList @env0:isNil @env0:or: [addrList @env0:isEmpty]) @env0:ifTrue: [
		self error: 'host not found'.
	].
	"addrList is Array of Strings representing list returned by getaddrinfo
	first element is the entry->ai_canonname if first address.
	subequent elements are addresses, each a struct sockaddr"
	canonName := addrList @env0:at: 1 . "for debugging"
	2 @env0:to: addrList @env0:size do:[:j | | status |
		status := socketHandle 
			@env0:_twoArgPrim: 2 
			with: (addrList @env0:at: j) 
			with: nil. "non-blocking connect"
		status @env0:== socketHandle ifTrue:[ ^self ].
		status @env0:== false ifTrue: [ ^self ].
	].
%

category: 'initialization'
method: Socket
connectSecureSocket

	<PharoGs>
	socketHandle := GsSecureSocket @env0:newClientFromGsSocket: socketHandle.
	socketHandle @env0:secureConnect.
%

category: 'testing'
method: Socket
hasSecureConnection
    
    <PharoGs>
	^socketHandle @env0:hasSecureConnection
%

category: 'initialization'
method: Socket
initialize: aGsSocket

	<PharoGs>
	socketHandle := aGsSocket.
	semaphore := Semaphore new.
	readSemaphore := Semaphore new.
	writeSemaphore := Semaphore new.
%

category: 'testing'
method: Socket
isConnected

	<PharoGs>
	^socketHandle @env0:isConnected
%

category: 'connection open/close'
method: Socket
listenOn: portNumber backlogSize: backlog interface: ifAddr 
	"Listen for a connection on the given port. 
	If this method succeeds, #accept may be used to establish a new connection" 

	<PharoGs>
	socketHandle @env0:bindTo: portNumber toAddress: ifAddr printString.
	socketHandle @env0:makeListener: backlog.
%

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
    ^self _gsError
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
    ^self _gsError
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
    ^aHandle @env0:makeServer: backlog atPort: (portNumber == 0 ifTrue: [nil] ifFalse: [portNumber])
%

category: 'primitives'
method: Socket
primSocket: aHandle listenOn: portNumber backlogSize: backlog interface: ifAddr 
	"Primitive. Set up the socket to listen on the given port. 
	Will be used in conjunction with #accept only." 

	<PharoGs> 
    ^aHandle @env0:makeServer: backlog atPort: (portNumber == 0 ifTrue: [nil] ifFalse: [portNumber]) atAddress: ifAddr
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID listenWithBacklog: backlogSize 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID localAddressResult: socketAddress 

	<PharoGsError> 
    ^self _gsError
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
    ^self _gsError
%

category: 'primitives-ipv6'
method: Socket
primSocket: socketID remoteAddressResult: socketAddress 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: Socket
primSocket: socketID sendData: aStringOrByteArray startIndex: startIndex count: count 
	"Send data to the remote host through the given socket starting with the given byte index of the given byte array. The data sent is 'pushed' immediately. Return the number of bytes of data actually sent; any remaining data should be re-submitted for sending after the current send operation has completed." 
	"Note: In general, it many take several sendData calls to transmit a large data array since the data is sent in send-buffer-sized chunks. The size of the send buffer is determined when the socket is created." 

	<PharoGs> 
	| sent |
	sent := socketID @env0:write: count from: aStringOrByteArray startingAt: startIndex.
	sent @env0:ifNil: [self error: socketID @env0:lastErrorString].
	^sent
%

category: 'primitives'
method: Socket
primSocket: socketID sendUDPData: aStringOrByteArray toHost: hostAddress port: portNumber startIndex: startIndex count: count 
	"Send data to the remote host through the given socket starting with the given 
	 byte index of the given byte array. The data sent is 'pushed' immediately. 
	 Return the number of bytes of data actually sent; any remaining data should be 
	 re-submitted for sending after the current send operation has completed." 
	"Note: In general, it many take several sendData calls to transmit a large data 
	 array since the data is sent in send-buffer-sized chunks. The size of the send 
	 buffer is determined when the socket is created." 

	<PharoGs> 
	| bytes |
	bytes := aStringOrByteArray.
	startIndex @env0:== 1 @env0:ifFalse: [
		bytes := bytes @env0:copyFrom: startIndex to: aStringOrByteArray @env0:size.
	].
	bytes @env0:size == count @env0:ifFalse: [
		bytes := bytes @env0:copyFrom: 1 to: count.
	].
	[
		socketID
			@env0:sendUdp: bytes @env0:bytesIntoString
			flags: 0 
			toHost: 
				(hostAddress at: 1) printString , '.' , 
				(hostAddress at: 2) printString , '.' , 
				(hostAddress at: 3) printString , '.' , 
				(hostAddress at: 4) printString
			port: portNumber.
	] @env0:on: (Globals @env0:at: #'SocketError') do: [:ex | 
		NoBroadcastAllowed signal.
	].
    ^bytes @env0:size
%

category: 'primitives'
method: Socket
primSocket: socketID setOption: aString value: aValue
	"Set some option information on this socket. Refer to the UNIX  
	man pages for valid SO, TCP, IP, UDP options. In case of doubt 
	refer to the source code. 
	TCP:=NODELAY, SO:=KEEPALIVE are valid options for example 
	returns an array containing the error code and the negotiated value" 

	<PharoGs> 
	| index key value |
	index := aString @env0:indexOf: $_.
	key := aString @env0:copyFrom: index @env0:+ 1 to: aString @env0: size.
	value := (key @env0:= 'RCVBUF' @env0:or: [key @env0:= 'SNDBUF'])
		@env0:ifTrue: [(aValue @env0:isKindOf: String) @env0:ifTrue: [aValue @env0:asNumber] ifFalse: [aValue]]
		ifFalse: [(aValue @env0:isKindOf: Boolean) @env0:ifTrue: [aValue] ifFalse: [aValue @env0:= '1']].
    ^socketID 
		@env0:option: key
		put: value
%

category: 'primitives'
method: Socket
primSocket: socketID setPort: port 
	"Set the local port associated with a UDP socket. 
	Note: this primitive is overloaded.  The primitive will not fail on a TCP socket, but 
	the effects will not be what was desired.  Best solution would be to split Socket into 
	two subclasses, TCPSocket and UDPSocket." 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: Socket
primSocketAbortConnection: socketID 
	"Terminate the connection on the given port immediately without going through the normal close sequence. This is an asynchronous call; query the socket status to discover if and when the connection is actually terminated." 

	<PharoGsError> 
    ^self _gsError
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
	"Return an integer reflecting the connection status of this socket. 
	 For a list of possible values, see the comment in the 'initialize' 
	 method of this class. If the primitive fails, return a status 
	 indicating that the socket handle is no longer valid, perhaps 
	 because the Pharo image was saved and restored since the socket 
	 was created. (Sockets do not survive snapshots.)" 

	<PharoGs> 
	socketID @env0:readWillNotBlock @env0:ifTrue: [^Connected].
	socketID @env0:peerAddress @env0:ifNil: [^Unconnected].
	socketID @env0:isConnected @env0:ifTrue: [^Connected].
	^WaitingForConnection
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
    ^self _gsError
%

category: 'primitives'
method: Socket
primSocketCreateNetwork: netType type: socketType receiveBufferSize: rcvBufSize sendBufSize: sendBufSize semaIndex: semaIndex readSemaIndex: aReadSema writeSemaIndex: aWriteSema 
	"See comment in primSocketCreateNetwork: with one semaIndex. However you should know that some implementations 
	ignore the buffer size and this interface supports three semaphores,  one for open/close/listen and the other two for 
	reading and writing" 

	<PharoGsError> 
    ^self _gsError
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
	| string |
	string := socketID @env0:address @env0:ifNil: [^#[0 0 0 0]].
	^(Globals @env0:at: #'ByteArray') 
		@env0:withAll: ((string @env0:subStrings: $.) 
			@env0:collect: [:each | each @env0:asNumber])
%

category: 'primitive-ipv6'
method: Socket
primSocketLocalAddressSize: handle 

	<PharoGsError> 
    ^self _gsError
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
	| string |
	string := socketID @env0:peerAddress @env0:ifNil: [^#[0 0 0 0]].
	^(Globals @env0:at: #'ByteArray') 
		@env0:withAll: ((string @env0:subStrings: $.) 
			@env0:collect: [:each | each @env0:asNumber])
%

category: 'primitives-ipv6'
method: Socket
primSocketRemoteAddressSize: handle 

	<PharoGsError> 
    ^self _gsError
%

category: 'primitives'
method: Socket
primSocketRemotePort: socketID 
	"Return the remote port for this socket, or zero if no connection has been made." 

	<PharoGs> 
    ^socketID @env0:peerPort @env0:ifNil: [0]
%

category: 'primitives'
method: Socket
primSocketSendDone: socketID 
	"Return true if there is no send in progress on the current socket." 

	<PharoGs> 
    ^true "GemStone socket operations are synchronous"
%

category: 'primitives'
method: Socket
waitForAcceptFor: timeout
	"Wait and accept an incoming connection. Return nil if it fails"

	<PharoGs>
	^ self waitForAcceptFor: timeout ifTimedOut: [nil].
%

category: 'primitives'
method: Socket
waitForAcceptFor: timeout ifTimedOut: timeoutBlock
	"Wait and accept an incoming connection"

	<PharoGs>
	| flag |
    flag := socketHandle @env0:readWillNotBlockWithin: timeout * 1000.
	flag @env0:== true @env0:ifTrue: [^self accept].
	flag @env0:== false @env0:ifTrue: [^timeoutBlock value].
	self error: socketHandle @env0:lastErrorString
%

category: 'primitives'
method: Socket
waitForConnectionFor: timeout ifTimedOut: timeoutBlock

	<PharoGs>
	| flag |
    flag := socketHandle @env0:writeWillNotBlockWithin: timeout * 1000.
	flag @env0:== true @env0:ifTrue: [^self accept].
	flag @env0:== false @env0:ifTrue: [^timeoutBlock value].
	self error: socketHandle @env0:lastErrorString
%

category: 'waiting'
method: Socket
waitForDataFor: timeout ifClosed: closedBlock ifTimedOut: timedOutBlock
	"Wait for the given nr of seconds for data to arrive."
	
	<PharoGs> 
	| flag |
	"GemStone returns true on a closed socket!"
	socketHandle @env0:isConnected @env0:ifFalse: [closedBlock value].
    flag := socketHandle @env0:readWillNotBlockWithin: timeout * 1000.
	flag @env0:== true @env0:ifTrue: [^self].
	flag @env0:== false @env0:ifTrue: [^timedOutBlock value].
	self error: socketHandle @env0:lastErrorString
%

category: 'waiting'
method: Socket
waitForDataIfClosed: closedBlock
	"Wait indefinitely for data to arrive.  This method will block until
	data is available or the socket is closed."

	<PharoGs>
	^socketHandle @env0:readWillNotBlockWithin: -1
%

set compile_env: 0
