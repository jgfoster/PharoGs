# Seaside in PharoGs

## Load Seaside

```
! - Load Seaside
set comp 2
run
System @env0:abortTransaction. 
GsSocket @env0:closeAll.
(Globals @env0:at:#'Time') @env0:millisecondsElapsedTime: [
	[
		| cache |
		cache := 'filetree:///Users/jfoster/Library/GemStone/db1/github-cache/SeasideSt/'.
		Metacello new
			baseline:'Seaside3';
			repository: 'github://SeasideSt/Seaside:master/repository';
			"repositoryOverrides: (Array 
				with: cache , 'Grease/v1.4.x/SeasideSt-Grease-762ec80/repository'
				with: cache , 'Seaside/master/SeasideSt-Seaside-91ae092/repository');"
			onWarningLog;
			load.
	] on: Error do: [:ex | 
		ex halt.
	].
].
%
run
WAServerManager default stopAll.	"do not leave a server running"
Pharo at: #'PragmaSetting' put: nil.	"needed for some Seaside tests"
%
```

## Tests

### Passing Tests

```
WAAcceptTest suite run printString.                     "  7 ran,   7 passed"
WABacktrackingTest suite run printString.               "  7 ran,   7 passed"
WACacheTest suite run printString.                      " 36 ran,  36 passed"
WACallbackTest suite run printString.                   " 12 ran,  12 passed"
WACollectionMimeDocumentTest suite run printString.     "  3 ran,   3 passed"
WAComponentTest suite run printString.                  " 15 ran,  15 passed"
WAConfigurationTest suite run printString.              " 12 ran,  12 passed"
WAContextTest suite run printString.                    "733 ran, 733 passed"
WACurrentRequestContextTest suite run printString.      "  2 ran,   2 passed"
WADynamicVariableTest suite run printString.            "  4 ran,   4 passed"
WAEmailAddressTest suite run printString.               "  6 ran,   6 passed"
WAExceptionHandlerTest suite run printString.           "  7 ran,   7 passed"
WAEnvironmentDefaultHandlersTest suite run printString. "  3 ran,   3 passed"
WAFileTest suite run printString.                       "  6 ran,   6 passed"
WAHttpVersionTest suite run printString.                " 14 ran,  14 passed"
WAKeyGeneratorTest suite run printString.               "  1 ran,   1 passed"
WALocaleTest suite run printString.                     "  7 ran,   7 passed"
WAMergedRequestFieldsTest suite run printString.        " 16 ran,  16 passed"
WAMimeDocumentTest suite run printString.               "  6 ran,   6 passed"
WAMimeTypeTest suite run printString.                   " 12 ran,  12 passed"
WAMutexTest suite run printString.                      "  6 ran,   6 passed"
WAObjectTest suite run printString.                     "  2 ran,   2 passed"
WAPathConsumerTest suite run printString.               "  7 ran,   7 passed"
WARegistryKeyHandlingTest suite run printString.        " 12 ran,  12 passed"
WARenderContextTest suite run printString.              "  2 ran,   2 passed"
WARenderLoopContinuationTest suite run printString.     "  1 ran,   1 passed"
WARequestContextTest suite run printString.             "  1 ran,   1 passed"
WARequestTest suite run printString.                    "  6 ran,   6 passed"
WAResponseTest suite run printString.                   " 90 ran,  90 passed"
WAServerManagerTest suite run printString.              "  3 ran,   3 passed"
WATestsFunctionalPlatformTest suite run printString.    "  1 ran,   1 passed"
```

### Failing Tests

```
GRCollectionTest suite run printString.             "345 ran, 344 passed, 1 failure, 0 errors"
    failure: #testCapitalizedUmlauts.           "requires Unicode to be initialized"

GRPlatformTest suite run printString.               "78 ran, 75 passed, 1 failure, 2 errors"
    errors and failures                         "relies on thisContext"
    
WAComponentCallTest suite run printString.          "1 ran, 0 passed, 0 failures, 1 error"
    error: #'testCallSelf'                      "relies on thisContext"
    
WAContinuationTest suite run printString.           "9 ran, 1 passed, 0 failures, 8 errors"
    errors                                      "relies on thisContext"

WAFlowPlatformTest suite run printString.           "1 ran, 0 passed, 0 failures, 1 error"
    error: #'testSuspendCallbackDo'             "relies on thisContext"

WAFormTestCase suite run printString.               "14 ran, 12 passed, 0 failures, 2 errors"
    errors                                      "EllipseMorph is missing"

WAPartialContinuationTest suite run printString.    "7 ran, 1 passed, 2 failures, 4 errors"
    errors                                      "relies on thisContext"
```

### Summary

1473 passed, 4 failures, and 18 errors.

## Running Seaside

We need to modify a few methods in order to get an initial page from Seaside. The methods are in `../PharoMods/Seaside.gs` and can be installed after Seaside is loaded.

Next, you may wish to modify `ZnServer class>>defaultServerClass` to use the `ZnSingleThreadedServer`. Also, you may wish to put a breakpoint in `WAResponseGenerator>>internalError:` so you can debug errors.

Finally, the following scripts have been helpful for debugging:

```
WAServerManager default stopAll. 
GsSocket @env0:closeAll.
System @env0:abort.
WAServerManager default restartAll.
(Delay forSeconds: 60) wait.
```
