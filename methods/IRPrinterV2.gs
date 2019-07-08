category: 'visiting'
set compile_env: 2
method: IRPrinterV2
visitPopIntoRemoteTemp: remoteTemp
	<PharoGs>

	stream nextPutAll: 'popIntoRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
%
set compile_env: 0
category: 'visiting'
set compile_env: 2
method: IRPrinterV2
visitPushRemoteTemp: remoteTemp
	<PharoGs>

	stream nextPutAll: 'pushRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream
%
set compile_env: 0
category: 'visiting'
set compile_env: 2
method: IRPrinterV2
visitStoreRemoteTemp: remoteTemp
	<PharoGs>

	stream nextPutAll: 'storeRemoteTemp: '.
	remoteTemp name printOn: stream.
	stream nextPutAll: ' inVector: '.
	remoteTemp tempVectorName printOn: stream.
%
set compile_env: 0
