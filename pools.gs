output push pools.out only
errorCount
run
(PharoPools at: #ChronologyConstants ifAbsentPut: [SymbolDictionary new name: #ChronologyConstants])
	at: #DayNames ifAbsentPut: [nil];
	at: #DaysInMonth ifAbsentPut: [nil];
	at: #MicrosecondsInDay ifAbsentPut: [nil];
	at: #MonthNames ifAbsentPut: [nil];
	at: #NanosInMillisecond ifAbsentPut: [nil];
	at: #NanosInSecond ifAbsentPut: [nil];
	at: #SecondsInDay ifAbsentPut: [nil];
	at: #SecondsInHour ifAbsentPut: [nil];
	at: #SecondsInMinute ifAbsentPut: [nil];
	at: #SqueakEpoch ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #FFIConstants ifAbsentPut: [SymbolDictionary new name: #FFIConstants])
	at: #FFIAtomicTypeMask ifAbsentPut: [nil];
	at: #FFIAtomicTypeShift ifAbsentPut: [nil];
	at: #FFICallFlagThreaded ifAbsentPut: [nil];
	at: #FFICallTypeApi ifAbsentPut: [nil];
	at: #FFICallTypeCDecl ifAbsentPut: [nil];
	at: #FFICallTypesMask ifAbsentPut: [nil];
	at: #FFIErrorAddressNotFound ifAbsentPut: [nil];
	at: #FFIErrorAttemptToPassVoid ifAbsentPut: [nil];
	at: #FFIErrorBadAddress ifAbsentPut: [nil];
	at: #FFIErrorBadArg ifAbsentPut: [nil];
	at: #FFIErrorBadArgs ifAbsentPut: [nil];
	at: #FFIErrorBadAtomicType ifAbsentPut: [nil];
	at: #FFIErrorBadExternalFunction ifAbsentPut: [nil];
	at: #FFIErrorBadExternalLibrary ifAbsentPut: [nil];
	at: #FFIErrorBadReturn ifAbsentPut: [nil];
	at: #FFIErrorCallFrameTooBig ifAbsentPut: [nil];
	at: #FFIErrorCallType ifAbsentPut: [nil];
	at: #FFIErrorCoercionFailed ifAbsentPut: [nil];
	at: #FFIErrorGenericError ifAbsentPut: [nil];
	at: #FFIErrorIntAsPointer ifAbsentPut: [nil];
	at: #FFIErrorInvalidPointer ifAbsentPut: [nil];
	at: #FFIErrorModuleNotFound ifAbsentPut: [nil];
	at: #FFIErrorNoModule ifAbsentPut: [nil];
	at: #FFIErrorNotFunction ifAbsentPut: [nil];
	at: #FFIErrorStructSize ifAbsentPut: [nil];
	at: #FFIErrorWrongType ifAbsentPut: [nil];
	at: #FFIFlagAtomic ifAbsentPut: [nil];
	at: #FFIFlagPointer ifAbsentPut: [nil];
	at: #FFIFlagStructure ifAbsentPut: [nil];
	at: #FFINoCalloutAvailable ifAbsentPut: [nil];
	at: #FFIStructSizeMask ifAbsentPut: [nil];
	at: #FFITypeBool ifAbsentPut: [nil];
	at: #FFITypeDoubleFloat ifAbsentPut: [nil];
	at: #FFITypeSignedByte ifAbsentPut: [nil];
	at: #FFITypeSignedChar ifAbsentPut: [nil];
	at: #FFITypeSignedInt ifAbsentPut: [nil];
	at: #FFITypeSignedLongLong ifAbsentPut: [nil];
	at: #FFITypeSignedShort ifAbsentPut: [nil];
	at: #FFITypeSingleFloat ifAbsentPut: [nil];
	at: #FFITypeUnsignedByte ifAbsentPut: [nil];
	at: #FFITypeUnsignedChar ifAbsentPut: [nil];
	at: #FFITypeUnsignedInt ifAbsentPut: [nil];
	at: #FFITypeUnsignedLongLong ifAbsentPut: [nil];
	at: #FFITypeUnsignedShort ifAbsentPut: [nil];
	at: #FFITypeVoid ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #GZipConstants ifAbsentPut: [SymbolDictionary new name: #GZipConstants])
	at: #GZipAsciiFlag ifAbsentPut: [nil];
	at: #GZipCommentFlag ifAbsentPut: [nil];
	at: #GZipContinueFlag ifAbsentPut: [nil];
	at: #GZipDeflated ifAbsentPut: [nil];
	at: #GZipEncryptFlag ifAbsentPut: [nil];
	at: #GZipExtraField ifAbsentPut: [nil];
	at: #GZipMagic ifAbsentPut: [nil];
	at: #GZipNameFlag ifAbsentPut: [nil];
	at: #GZipReservedFlags ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #TextConstants ifAbsentPut: [SymbolDictionary new name: #TextConstants])
	at: #BS ifAbsentPut: [nil];
	at: #BS2 ifAbsentPut: [nil];
	at: #Basal ifAbsentPut: [nil];
	at: #Bold ifAbsentPut: [nil];
	at: #CR ifAbsentPut: [nil];
	at: #Centered ifAbsentPut: [nil];
	at: #Clear ifAbsentPut: [nil];
	at: #CrossedX ifAbsentPut: [nil];
	at: #CtrlA ifAbsentPut: [nil];
	at: #CtrlB ifAbsentPut: [nil];
	at: #CtrlC ifAbsentPut: [nil];
	at: #CtrlD ifAbsentPut: [nil];
	at: #CtrlDigits ifAbsentPut: [nil];
	at: #CtrlE ifAbsentPut: [nil];
	at: #CtrlF ifAbsentPut: [nil];
	at: #CtrlG ifAbsentPut: [nil];
	at: #CtrlH ifAbsentPut: [nil];
	at: #CtrlI ifAbsentPut: [nil];
	at: #CtrlJ ifAbsentPut: [nil];
	at: #CtrlK ifAbsentPut: [nil];
	at: #CtrlL ifAbsentPut: [nil];
	at: #CtrlM ifAbsentPut: [nil];
	at: #CtrlN ifAbsentPut: [nil];
	at: #CtrlO ifAbsentPut: [nil];
	at: #CtrlOpenBrackets ifAbsentPut: [nil];
	at: #CtrlP ifAbsentPut: [nil];
	at: #CtrlQ ifAbsentPut: [nil];
	at: #CtrlR ifAbsentPut: [nil];
	at: #CtrlS ifAbsentPut: [nil];
	at: #CtrlT ifAbsentPut: [nil];
	at: #CtrlU ifAbsentPut: [nil];
	at: #CtrlV ifAbsentPut: [nil];
	at: #CtrlW ifAbsentPut: [nil];
	at: #CtrlX ifAbsentPut: [nil];
	at: #CtrlY ifAbsentPut: [nil];
	at: #CtrlZ ifAbsentPut: [nil];
	at: #Ctrla ifAbsentPut: [nil];
	at: #Ctrlb ifAbsentPut: [nil];
	at: #Ctrlc ifAbsentPut: [nil];
	at: #Ctrld ifAbsentPut: [nil];
	at: #Ctrle ifAbsentPut: [nil];
	at: #Ctrlf ifAbsentPut: [nil];
	at: #Ctrlg ifAbsentPut: [nil];
	at: #Ctrlh ifAbsentPut: [nil];
	at: #Ctrli ifAbsentPut: [nil];
	at: #Ctrlj ifAbsentPut: [nil];
	at: #Ctrlk ifAbsentPut: [nil];
	at: #Ctrll ifAbsentPut: [nil];
	at: #Ctrlm ifAbsentPut: [nil];
	at: #Ctrln ifAbsentPut: [nil];
	at: #Ctrlo ifAbsentPut: [nil];
	at: #Ctrlp ifAbsentPut: [nil];
	at: #Ctrlq ifAbsentPut: [nil];
	at: #Ctrlr ifAbsentPut: [nil];
	at: #Ctrls ifAbsentPut: [nil];
	at: #Ctrlt ifAbsentPut: [nil];
	at: #Ctrlu ifAbsentPut: [nil];
	at: #Ctrlv ifAbsentPut: [nil];
	at: #Ctrlw ifAbsentPut: [nil];
	at: #Ctrlx ifAbsentPut: [nil];
	at: #Ctrly ifAbsentPut: [nil];
	at: #Ctrlz ifAbsentPut: [nil];
	at: #DefaultBaseline ifAbsentPut: [nil];
	at: #DefaultFontFamilySize ifAbsentPut: [nil];
	at: #DefaultLineGrid ifAbsentPut: [nil];
	at: #DefaultMarginTabsArray ifAbsentPut: [nil];
	at: #DefaultMask ifAbsentPut: [nil];
	at: #DefaultRule ifAbsentPut: [nil];
	at: #DefaultSpace ifAbsentPut: [nil];
	at: #DefaultTab ifAbsentPut: [nil];
	at: #DefaultTabsArray ifAbsentPut: [nil];
	at: #ESC ifAbsentPut: [nil];
	at: #EndOfRun ifAbsentPut: [nil];
	at: #Enter ifAbsentPut: [nil];
	at: #Italic ifAbsentPut: [nil];
	at: #Justified ifAbsentPut: [nil];
	at: #LeftFlush ifAbsentPut: [nil];
	at: #LeftMarginTab ifAbsentPut: [nil];
	at: #RightFlush ifAbsentPut: [nil];
	at: #RightMarginTab ifAbsentPut: [nil];
	at: #Space ifAbsentPut: [nil];
	at: #Tab ifAbsentPut: [nil];
	at: #TextSharedInformation ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #VTermOutputStyles ifAbsentPut: [SymbolDictionary new name: #VTermOutputStyles])
	at: #Blink ifAbsentPut: [nil];
	at: #Bold ifAbsentPut: [nil];
	at: #InvisibleFont ifAbsentPut: [nil];
	at: #ResetStyle ifAbsentPut: [nil];
	at: #Reverse ifAbsentPut: [nil];
	at: #Standout ifAbsentPut: [nil];
	at: #Underline ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #ZipConstants ifAbsentPut: [SymbolDictionary new name: #ZipConstants])
	at: #BaseDistance ifAbsentPut: [nil];
	at: #BaseLength ifAbsentPut: [nil];
	at: #BitLengthOrder ifAbsentPut: [nil];
	at: #DistanceCodes ifAbsentPut: [nil];
	at: #DynamicBlock ifAbsentPut: [nil];
	at: #EndBlock ifAbsentPut: [nil];
	at: #ExtraBitLengthBits ifAbsentPut: [nil];
	at: #ExtraDistanceBits ifAbsentPut: [nil];
	at: #ExtraLengthBits ifAbsentPut: [nil];
	at: #FixedBlock ifAbsentPut: [nil];
	at: #FixedDistanceTree ifAbsentPut: [nil];
	at: #FixedLiteralTree ifAbsentPut: [nil];
	at: #HashBits ifAbsentPut: [nil];
	at: #HashMask ifAbsentPut: [nil];
	at: #HashShift ifAbsentPut: [nil];
	at: #MatchLengthCodes ifAbsentPut: [nil];
	at: #MaxBitLengthBits ifAbsentPut: [nil];
	at: #MaxBitLengthCodes ifAbsentPut: [nil];
	at: #MaxBits ifAbsentPut: [nil];
	at: #MaxDistCodes ifAbsentPut: [nil];
	at: #MaxDistance ifAbsentPut: [nil];
	at: #MaxLengthCodes ifAbsentPut: [nil];
	at: #MaxLiteralCodes ifAbsentPut: [nil];
	at: #MaxMatch ifAbsentPut: [nil];
	at: #MinMatch ifAbsentPut: [nil];
	at: #NumLiterals ifAbsentPut: [nil];
	at: #Repeat11To138 ifAbsentPut: [nil];
	at: #Repeat3To10 ifAbsentPut: [nil];
	at: #Repeat3To6 ifAbsentPut: [nil];
	at: #StoredBlock ifAbsentPut: [nil];
	at: #WindowMask ifAbsentPut: [nil];
	at: #WindowSize ifAbsentPut: [nil];
	yourself.
%
run
(PharoPools at: #ZipFileConstants ifAbsentPut: [SymbolDictionary new name: #ZipFileConstants])
	at: #CentralDirectoryFileHeaderSignature ifAbsentPut: [nil];
	at: #CompressionDeflated ifAbsentPut: [nil];
	at: #CompressionLevelDefault ifAbsentPut: [nil];
	at: #CompressionLevelNone ifAbsentPut: [nil];
	at: #CompressionStored ifAbsentPut: [nil];
	at: #DataDescriptorLength ifAbsentPut: [nil];
	at: #DefaultDirectoryPermissions ifAbsentPut: [nil];
	at: #DefaultFilePermissions ifAbsentPut: [nil];
	at: #DeflatingCompressionFast ifAbsentPut: [nil];
	at: #DeflatingCompressionMaximum ifAbsentPut: [nil];
	at: #DeflatingCompressionNormal ifAbsentPut: [nil];
	at: #DeflatingCompressionSuperFast ifAbsentPut: [nil];
	at: #DirectoryAttrib ifAbsentPut: [nil];
	at: #EndOfCentralDirectorySignature ifAbsentPut: [nil];
	at: #FaMsdos ifAbsentPut: [nil];
	at: #FaUnix ifAbsentPut: [nil];
	at: #FileAttrib ifAbsentPut: [nil];
	at: #IfaBinaryFile ifAbsentPut: [nil];
	at: #IfaTextFile ifAbsentPut: [nil];
	at: #LocalFileHeaderSignature ifAbsentPut: [nil];
	yourself.
%
errorCount
output pop
errorCount
