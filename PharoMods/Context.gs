set compile_env: 2

category: 'enumerating'
classmethod: Context
allInstances 
	"Answer all instances of the receiver." 

    <PharoGs>
    ^super allInstances
%

category: 'accessing'
method: Context
at: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	 indexable element in the receiver. Fail if the argument index is not an 
	 Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
at: index put: value 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	 indexable element in the receiver. Fail if the argument index is not 
	 an Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
basicAt: index 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	 indexable element in the receiver. Fail if the argument index is not an 
	 Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
basicAt: index put: value 
	"Primitive. Assumes receiver is indexable. Answer the value of an 
	 indexable element in the receiver. Fail if the argument index is not 
	 an Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
basicSize 
	"Primitive. Answer the number of indexable variables in the receiver.  
	This value is the same as the largest legal subscript. Essential. Do not  
	override in any subclass. See Object documentation whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'controlling'
method: Context
closureCopy: numArgs copiedValues: anArray 
	"Distinguish a block of code from its enclosing method by  
	creating a BlockClosure for that block. The compiler inserts into all  
	methods that contain blocks the bytecodes to send the message  
	closureCopy:copiedValues:. Do not use closureCopy:copiedValues: in code that you write! Only the  
	compiler can decide to send the message closureCopy:copiedValues:. Fail if numArgs is  
	not a SmallInteger. Optional. No Lookup. See Object documentation  
	whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'private-exceptions'
method: Context
evaluateSignal: exception 
	"The following primitive is just a marker used to find the evaluation context.  
	See MethodContext>>#isHandlerOrSignalingContext. " 

    <PharoGsError>
    self _gsError
%

category: 'private-exceptions'
method: Context
findNextHandlerOrSignalingContext 
	"Return the next handler/signaling marked context, answering nil if there is none.  
	Search starts with self and proceeds up to nil." 

    <PharoGsError>
    self _gsError
%

category: 'private-exceptions'
method: Context
findNextUnwindContextUpTo: aContext 
	"Return the next unwind marked above the receiver, returning nil if there is none.  
    Search proceeds up to but not including aContext." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject basicAt: index  
	"Answer the value of an indexable element in the argument anObject without sending 
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if 
	 anObject is not indexable. This mimics the action of the VM when it indexes an object. 
	 Used to simulate the execution machinery by, for example, the debugger. 
	 Primitive.  See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject basicAt: index put: value  
	"Store the last argument  
	 value in the indexable element of the argument anObject indicated by index without sending 
	 anObject a message. Fail if the argument index is not an Integer or is out of bounds, or if 
	 anObject is not indexable, or if value is an inappropriate value for anObject's indexable slots. 
	 This mimics the action of the VM when it indexes an object. 
	 Used to simulate the execution machinery by, for example, the debugger. 
	 Primitive.  See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject eqeq: anOtherObject  
	"Answer whether the first and second arguments are the same object (have the 
	 same object pointer) without sending a message to the first argument.  This 
	 mimics the action of the VM when it compares two object pointers.  Used to 
	 simulate the execution machinery by, for example, the debugger. 
	 Primitive.  See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject instVarAt: anIndex 
	"Primitive. Answer a fixed variable in an object. The numbering of the  
	 variables corresponds to the named instance variables. Fail if the index  
	 is not an Integer or is not the index of a fixed variable. Essential for the 
	 debugger. See  Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject instVarAt: anIndex put: aValue  
	"Primitive. Store a value into a fixed variable in the argument anObject. 
	 The numbering of the variables corresponds to the named instance 
	 variables.  Fail if the index is not an Integer or is not the index of a 
	 fixed variable.  Answer the value stored as the result. Using this 
	 message violates the  principle that each object has sovereign control 
	 over the storing of values into its instance variables. Essential for the 
	 debugger. See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
object: anObject perform: selector withArguments: argArray inClass: lookupClass 
	"Send the selector, aSymbol, to anObject with arguments in argArray. 
	 Fail if the number of arguments expected by the selector  
	 does not match the size of argArray, or if lookupClass 
	 cannot be found among the anObject's superclasses. 
	 Primitive. Essential for the debugger." 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
objectClass: aReceiver 

    <PharoGsError>
    self _gsError
%

category: 'mirror primitives'
method: Context
objectSize: anObject 
	"Answer the number of indexable variables in the argument anObject without sending 
	 it a message. This mimics the action of the VM when it fetches an object's variable size. 
	 Used to simulate the execution machinery by, for example, the debugger. 
	 Primitive.  See Object documentation whatIsAPrimitive." 

    <PharoGsError>
    self _gsError
%

category: '*Reflectivity'
method: Context
rftempAt: index put: value  
	"same as #tempAt:put:, for recursion stopping metalinks" 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
size 
	"Primitive. Answer the number of indexable variables in the receiver.  
	This value is the same as the largest legal subscript. Essential. See Object  
	documentation whatIsAPrimitive.  Override the default primitive to give latitude to 
	 the VM in context management." 

    <PharoGs>
    ^super size
%

category: 'private'
method: Context
stackp: newStackp 
	"Storing into the stack pointer is a potentially dangerous thing. 
	This primitive stores nil into any cells that become accessible as a result, 
	and it performs the entire operation atomically." 
	"Once this primitive is implemented, failure code should cause an error" 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
tempAt: index  
	"Answer the value of the temporary variable whose index is the  
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the 
	 value of an indexable element in the receiver. Fail if the argument index 
	 is not an Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default at: primitive to give latitude to the 
	 VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'accessing'
method: Context
tempAt: index put: value  
	"Store the argument, value, as the temporary variable whose index is the  
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the 
	 value of an indexable element in the receiver. Fail if the argument index 
	 is not an Integer or is out of bounds. Essential. See Object documentation 
	 whatIsAPrimitive.  Override the default at:put: primitive to give latitude to 
	 the VM in context management." 

    <PharoGsError>
    self _gsError
%

category: 'controlling'
method: Context
terminateTo: previousContext 
	"Terminate all the Contexts between me and previousContext, if previousContext 
    is on my Context stack. Make previousContext my sender." 

    <PharoGsError>
    self _gsError
%

category: 'private'
method: Context
tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments 

    <PharoGsError>
    self _gsError
%

set compile_env: 0
