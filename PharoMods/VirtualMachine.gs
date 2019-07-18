set compile_env: 2

category: 'modules'
method: VirtualMachine
disableModuleLoading 
	"Primitive. Disable a new module loading mechanism for the rest of current session. 
	This operation is not reversable. 
	Any subsequent attempts to load either external or internal module(s) will fail" 

    <PharoGsError>
    self _gsError.
%

category: 'modules'
method: VirtualMachine
forgetModule: aString 
	"Primitive. If the module named aString is loaded, unloaded. 
    If not, and it is marked an unloadable, unmark it so the VM 
    will try to load it again next time. See comment for #unloadModule:." 

    <PharoGsError>
    self _gsError.
%

category: 'gc'
method: VirtualMachine
gcBiasToGrowLimit: arg 
	"Tell the VM the grow limit if the GC logic has bias to grow." 

    <PharoGsError>
    self _gsError.
%

category: 'parameters'
method: VirtualMachine
getParameters	 
	"Answer an Array containing the current values of the VM's internal 
	parameter/metric registers.  Each value is stored in the array at the 
	index corresponding to its VM register.  (See #vmParameterAt: and 
	#vmParameterAt:put:.) '" 
	"Smalltalk vm getParameters" 

    <PharoGsError>
    self _gsError.
%

category: 'attributes'
method: VirtualMachine
getSystemAttribute: attributeID  
	"Optional. Answer the string for the system attribute with the given  
	integer ID. Answer nil if the given attribute is not defined on this  
	platform. On platforms that support invoking programs from command  
	lines (e.g., Unix), this mechanism can be used to pass command line  
	arguments to programs written in Pharo. 
	By convention, the first command line argument that is not a VM 
	configuration option is considered a 'document' to be filed in. Such a 
	document can add methods and classes, can contain a serialized object, 
	can include code to be executed, or any combination of these. 
	Currently defined attributes include:  
	-1000   1000th command line argument that specify VM options 
   	... 
	-1              first command line argument that specify VM options 
      0               the full path name for currently executing VM 
                       (or, on some platforms, just the path name of the VM's directory) '
       1               full path name of this image (better use primImageName instead) 
       2               a Squeak document to open, if any 
       3               first command line argument for Squeak programs 
       ... 
       1000    1000th command line argument for Squeak programs 
       1001    this platform's operating system 'Mac OS', 'Win32', 'unix', ... '
       1002    operating system version 
       1003    this platform's processor type '
       1004    vm version 
       1005    window system name 
       1006    vm build id 
       1007    Interpreter class (Cog VM only) 
       1008    Cogit class (Cog VM only) 
       1201    max filename length (Mac OS only) 
       1202    file last error (Mac OS only) 
       10001   hardware details (Win32 only) 
       10002   operating system details (Win32 only) 
       10003   graphics hardware details (Win32 only)" 

	attributeID == 1001 ifTrue: [
		^#('Mac OS') at: (#('Darwin') indexOf: (System gemVersionAt: 'osName'))
	].
    <PharoGs>
    self @env0:error: 'Attribute not yet supported'.
%

category: 'accessing'
method: VirtualMachine
interpreterSourceVersion 
	"The use of this primitive not recommended. Not all VMs providing that" 
	"Answer a string corresponding to the version of the interpreter source. 
	This represents the version level of the Smalltalk source code (interpreter 
	and various plugins) that is translated to C by a CCodeGenerator, as distinct 
	from the external platform source code, typically written in C and managed 
	separately for each platform. An optional primitive is invoked that may not 
	be available on all virtual machines." 
	"Smalltalk vm interpreterSourceVersion" 

    <PharoGsError>
    self _gsError.
%

category: 'modules'
method: VirtualMachine
listBuiltinModule: index 
	"Return the name of the n-th builtin module. 
	This list is not sorted!" 

    <PharoGsError>
    self _gsError.
%

category: 'modules'
method: VirtualMachine
listLoadedModule: index 
	"Return the name of the n-th loaded module. 
	This list is not sorted!" 

    <PharoGsError>
    self _gsError.
%

category: 'parameters'
method: VirtualMachine
parameterAt: parameterIndex 
	"parameterIndex is a positive integer corresponding to one of the VM's internal 
	parameter/metric registers.  Answer with the current value of that register. 
	Fail if parameterIndex has no corresponding register. 
	VM parameters are numbered as follows: 
	1	end (v3)/size(Spur) of old-space (0-based, read-only) 
	2	end (v3)/size(Spur) of young/new-space (read-only) 
	3	end (v3)/size(Spur) of heap (read-only) 
	4	nil (was allocationCount (read-only)) 
	5	nil (was allocations between GCs (read-write) 
	6	survivor count tenuring threshold (read-write) 
	7	full GCs since startup (read-only) 
	8	total milliseconds in full GCs since startup (read-only) 
	9	incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only) 
	10	total milliseconds in incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only) 
	11	tenures of surving objects since startup (read-only) 
	12-20 were specific to ikp's JITTER VM, now 12-19 are open for use 
	20	utc microseconds at VM start-up (actually at time initialization, which precedes image load). 
	21	root table size (read-only) 
	22	root table overflows since startup (read-only) 
	23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored 
	in image file header). 
	24	memory threshold above which shrinking object memory (rw) 
	25	memory headroom when growing object memory (rw) 
	26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds	(rw) 27	number of times mark loop iterated for current IGC/FGC (read-only)	includes ALL marking 
	28	number of times sweep loop iterated for current IGC/FGC (read-only) 
	29	number of times make forward loop iterated for current IGC/FGC	(read-only) 30	number of times compact move loop iterated for current	IGC/FGC (read-only) 
	31	number of grow memory requests (read-only) 
	32	number of shrink memory requests (read-only) 
	33	number of root table entries used for current IGC/FGC (read-only) 
	34	number of allocations done before current IGC/FGC (read-only) 
	35	number of survivor objects after current IGC/FGC (read-only) 
	36	millisecond clock when current IGC/FGC completed (read-only) 
	37	number of marked objects for Roots of the world, not including Root	Table entries for current IGC/FGC (read-only) 
	38	milliseconds taken by current IGC (read-only) 
	39	Number of finalization signals for Weak Objects pending when current	IGC/FGC completed (read-only) 
	40	BytesPerOop for this image 
	41	imageFormatVersion for the VM 
	42	number of stack pages in use 
	43	desired number of stack pages (stored in image file header, max 65535) 
	44	size of eden, in bytes 
	45	desired size of eden, in bytes (stored in image file header) 
	46	machine code zone size, in bytes (Cog only; otherwise nil) 
	47	desired machine code zone size (stored in image file header; Cog only;	otherwise nil) 
	48	various header flags. See getCogVMFlags. 
	49	max size the image promises to grow the external semaphore table to (0	sets to default, which is 256 as of writing) 
	50-51 nil; reserved for VM parameters that persist in the image (such as	eden above) 
	52	root table capacity 
	53	number of segments (Spur only; otherwise nil) 
	54	total size of free old space (Spur only, otherwise nil) 
	55	ratio of growth and image size at or above which a GC will be performed	post scavenge 
	56	number of process switches since startup (read-only) 
	57	number of ioProcessEvents calls since startup (read-only) 
	58	number of ForceInterruptCheck calls since startup (read-only) 
	59	number of check event calls since startup (read-only) 
	60	number of stack page overflows since startup (read-only) 
	61	number of stack page divorces since startup (read-only)	62	compiled code compactions since startup (read-only; Cog only; otherwise nil) 
	63	total milliseconds in compiled code compactions since startup	(read-only; Cog only; otherwise nil) 
	64	the number of methods that currently have jitted machine-code 
	65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMTABILITY is bit 1 
	66	the byte size of a stack page 
	67	the max allowed size of old space (Spur only; nil otherwise; 0 implies	no limit except that of the underlying platform) 
	68	the average number of live stack pages when scanned by GC (at	scavenge/gc/become et al) 
	69	the maximum number of live stack pages when scanned by GC (at	scavenge/gc/become et al) 
	70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION) 
	71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)" 

    <PharoGsError>
    self _gsError.
%

category: 'parameters'
method: VirtualMachine
parameterAt: parameterIndex put: newValue 
	"parameterIndex is a positive integer corresponding to one of the VM's internal 
	parameter/metric registers.  Store newValue (a positive integer) into that 
	register and answer with the previous value that was stored there. 
	Fail if newValue is out of range, if parameterIndex has no corresponding 
	register, or if the corresponding register is read-only." 

    <PharoGsError>
    self _gsError.
%

category: 'accessing'
method: VirtualMachine
platformSourceVersion 
	"The use of this primitive not recommended. Not all VMs providing that" 
	"Answer a string corresponding to the version of the external platform source 
	code, typically written in C and managed separately for each platform. This 
	invokes an optional primitive that may not be available on all virtual machines." 
	"Smalltalk vm platformSourceVersion" 

    <PharoGsError>
    self _gsError.
%

category: 'primitives'
method: VirtualMachine
primitiveGCBiasToGrow: arg 
	"Tell the VM to grow after tenures instead of running in a tight loop 
    where it does IGCs over and over. For some weird reason the primitive 
    takes an integer not a bool but oh well..." 

    <PharoGsError>
    self _gsError.
%

category: 'primitives'
method: VirtualMachine
primVmPath 
	"Answer the path for the directory containing the Smalltalk virtual machine. 
    Return the empty string if this primitive is not implemented." 
	"Smalltalk vmPath" 

    <PharoGs>
    ^''
%

category: 'gc'
method: VirtualMachine
setGCBiasToGrow: aNumber 
	"Primitive. Indicate that the GC logic should be bias to grow" 

    <PharoGsError>
    self _gsError.
%

category: 'gc'
method: VirtualMachine
setGCBiasToGrowGCLimit: aNumber 
	"Primitive. Indicate that the bias to grow logic should do a GC after aNumber Bytes" 

    <PharoGsError>
    self _gsError.
%

category: 'gc'
method: VirtualMachine
setGCSemaphore: semaIndex 
	"Primitive. Indicate the GC semaphore index to be signaled on GC occurance." 

    <PharoGsError>
    self _gsError.
%

category: 'modules'
method: VirtualMachine
unloadModule: aString 
	"Primitive. Unload the given module. 
	This primitive is intended for development only since some 
	platform do not implement unloading of DLL's accordingly. 
	Also, the mechanism for unloading may not be supported 
	on all platforms." 

    <PharoGsError>
    self _gsError.
%

category: 'accessing'
method: VirtualMachine
versionLabel 
	"The use of this primitive not recommended. Not all VMs providing that" 
	"Answer a string corresponding to the version of virtual machine. This 
	represents the version level of the Smalltalk source code (interpreter 
	and various plugins) that is translated to C by a CCodeGenerator,  in 
	addition to the external platform source code, typically written in C and 
	managed separately for each platform. 
	 
	This invokes an optional primitive that may not be available on all virtual 
	machines. See also vmVersion, which answers a string identifying the image 
	from which virtual machine sources were generated." 
	"Smalltalk vm versionLabel" 

    <PharoGsError>
    self _gsError.
%

category: 'gc'
method: VirtualMachine
voidCogVMState 
	"Void any internal caches the VM maintains other than the method lookup caches. 
	 These comprise 
		- the stack zone, where method activations are stored, and 
		- the machine code zone, where the machine code form of CompiledMethods is held." 

    <PharoGsError>
    self _gsError.
%

set compile_env: 0
