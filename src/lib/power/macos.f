\ power/macos.f - POWER:KEEP-AWAKE and ALLOW-SLEEP through IOKit power
\ assertions, loaded on macOS.
require src/lib/power.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/lib/log.f

package POWER
private

\ https://developer.apple.com/documentation/iokit/iopmlib_h
LIBRARY /System/Library/Frameworks/CoreFoundation.framework/CoreFoundation
FUNCTION: CF-STRING CFStringCreateWithBytes ( n ptr u8 n n n -- n ) ;FUNCTION
FUNCTION: CF-RELEASE CFRelease ( n -- ) ;FUNCTION

4 constant ID-BYTES          \ IOPMAssertionID, a uint32_t

LIBRARY /System/Library/Frameworks/IOKit.framework/IOKit
FUNCTION: PM-CREATE IOPMAssertionCreateWithName ( n u32 n ptr u8 -- i32 )
   3 ID-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: PM-RELEASE IOPMAssertionRelease ( u32 -- i32 ) ;FUNCTION

$08000100 constant UTF8      \ kCFStringEncodingUTF8
255 constant LEVEL-ON        \ kIOPMAssertionLevelOn

\ An owned CFString of the bytes, 0 when they are not UTF-8.
: CF-TEXT ( ptr u8 n -- n )
   {: p:ptr u:n :}
   0 p u UTF8 0 CF-STRING ;

: CF-FREE ( n -- )
   dup 0 <> if CF-RELEASE else drop then ;

: CHECK ( n ptr u8 n -- )
   {: status:n verb:ptr verbu:n :}
   status 0= if exit then
   LOG:ERROR s" power" LOG:LINE verb verbu LOG:TEXT+ s"  refused, IOReturn " LOG:TEXT+
      status LOG:INT+ LOG:;LINE
   E-ASSERTION throw ;


\ Nothing that can throw runs between creating the strings and releasing them.
: CREATE-ID ( ptr u8 n ptr u8 NUM:alloc-byte-len -- n )
   drop {: name:ptr len:n id:ptr :}
   s" PreventUserIdleSystemSleep" CF-TEXT {: kind:n :}
   name len CF-TEXT {: label:n :}
   kind 0= label 0= or if kind CF-FREE label CF-FREE E-ASSERTION throw then
   kind LEVEL-ON label id PM-CREATE
   kind CF-RELEASE label CF-RELEASE
   s" IOPMAssertionCreateWithName" CHECK
   id LE:U32@ ;


: MAC-KEEP ( ptr u8 n -- n )
   ID-BYTES MEM:BYTES-ALLOC-LEN [: CREATE-ID ;] MEM:WITH-BYTES ;

: MAC-ALLOW ( n -- )
   PM-RELEASE s" IOPMAssertionRelease" CHECK ;

: INSTALL-MAC ( -- )
   ['] MAC-KEEP is KEEP-HOOK
   ['] MAC-ALLOW is ALLOW-HOOK ;
INSTALL-MAC
;package
