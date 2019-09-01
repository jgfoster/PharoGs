set compile_env: 2

category: 'system primitives'
method: Locale
primCountry

	<PharoGs>
	^'US'
%

category: 'system primitives'
method: Locale
primCurrencyNotation
	"Returns boolean if symbol is pre- (true) or post-fix (false)"
	<PharoGs>
	^(Globals at: #'Locale') @env0:nCsPrecedes
%

category: 'system primitives'
method: Locale
primCurrencySymbol
	"Returns string with currency symbol"
	<PharoGs>
	^(Globals at: #'Locale') @env0:currencySymbol 
%

category: 'system primitives'
method: Locale
primDecimalSymbol
	"Returns string with e.g. '.' or ','"
	<PharoGs>
	^(Globals at: #'Locale') @env0:decimalPoint 
%

category: 'system primitives'
method: Locale
primDigitGrouping
	"Returns string with e.g. '.' or ',' (thousands etc)"
	<PharoGs>
	^(Globals at: #'Locale') @env0:grouping 
%

category: 'system primitives'
method: Locale
primDST
	"Returns boolean if DST  (daylight saving time) is active or not"
	<PharoGs>
	^(Globals @env0:at: #'DateAndTime') @env0:now @env0:offset @env0:asSeconds ~~ 
	 (Globals @env0:at: #'TimeZone') @env0:current @env0:secondsFromGmt
%

category: 'system primitives'
method: Locale
primLanguage

	<PharoGs>
	^'en'
%

category: 'system primitives'
method: Locale
primLongDateFormat
	"Returns the long date format
	d day, m month, y year,
	double symbol is null padded, single not padded (m=6, mm=06)
	dddd weekday
	mmmm month name"
	<PharoGs>
	^'dddd, mmmm d, yyyy'
%

category: 'system primitives'
method: Locale
primMeasurement
	"Returns boolean denoting metric(true) or imperial(false)."
	<PharoGs>
	^true
%

category: 'system primitives'
method: Locale
primShortDateFormat
	"Returns the short date format
	d day, m month, y year,
	double symbol is null padded, single not padded (m=6, mm=06)
	dddd weekday
	mmmm month name"
	<PharoGs>
	^'m/d/yy'
%

category: 'system primitives'
method: Locale
primTimeFormat
	"Returns string time format
	Format is made up of 
	h hour (h 12, H 24), m minute, s seconds, x (am/pm String)
	double symbol is null padded, single not padded (h=6, hh=06)"
	<PharoGs>
	^'h:mmx'
%

category: 'system primitives'
method: Locale
primTimezone
	"The offset from UTC in minutes, with positive offsets being towards the east.
	(San Francisco is in UTC -07*60 and Paris is in UTC +02*60 (daylight savings is not in effect)."
	<PharoGs>
	^(Globals @env0:at: #'TimeZone') @env0:current @env0:secondsFromGmt @env0:// 60
%

category: 'system primitives'
method: Locale
primVMOffsetToUTC
	"Returns the offset in minutes between the VM and UTC.
	If the VM does not support UTC times, this is 0.
	Also gives us backward compatibility with old VMs as the primitive will fail and we then can return 0."
	<PharoGs>
	^(Globals @env0:at: #'TimeZone') @env0:current @env0:secondsFromGmt @env0:// 60
%

set compile_env: 0
