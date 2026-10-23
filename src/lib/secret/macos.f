\ secret/macos.f - SECRET:READ, WRITE and ERASE through the Security framework's
\ generic passwords, loaded by src/lib/secret.f on macOS. Contract:
\ src/lib/secret.f.
require src/lib/secret.f

package SECRET
private

\ https://developer.apple.com/documentation/security/keychain-items
\ CF values and the returned data remain owned through each native call.
LIBRARY /System/Library/Frameworks/CoreFoundation.framework/CoreFoundation
FUNCTION: CF-STRING CFStringCreateWithBytes ( n ptr u8 n n n -- n ) ;FUNCTION
FUNCTION: CF-DICT CFDictionaryCreateMutable ( n n n n -- n ) ;FUNCTION
FUNCTION: CF-SET CFDictionarySetValue ( n n n -- ) ;FUNCTION
FUNCTION: CF-REMOVE CFDictionaryRemoveValue ( n n -- ) ;FUNCTION
FUNCTION: CF-RELEASE CFRelease ( n -- ) ;FUNCTION
FUNCTION: CF-SIZE CFDataGetLength ( n -- n ) ;FUNCTION
FUNCTION: CF-BYTES CFDataGetBytePtr ( n -- ptr u8 ) ;FUNCTION
FUNCTION: CF-DATA-BORROW CFDataCreateWithBytesNoCopy ( n ptr u8 n n -- n ) ;FUNCTION

LIBRARY /System/Library/Frameworks/Security.framework/Security
FUNCTION: SEC-READ SecItemCopyMatching ( n ptr u8 -- i32 )
   1 8 WRITES-BYTES
;FUNCTION
FUNCTION: SEC-UPDATE SecItemUpdate ( n n -- i32 ) ;FUNCTION
FUNCTION: SEC-ADD SecItemAdd ( n n -- i32 ) ;FUNCTION
FUNCTION: SEC-DELETE SecItemDelete ( n -- i32 ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: LIB-CLOSE dlclose ( n -- i32 ) ;FUNCTION

-25300 constant SEC-NOT-FOUND    \ errSecItemNotFound
-25299 constant SEC-DUPLICATE    \ errSecDuplicateItem
-25291 constant SEC-UNAVAILABLE  \ errSecNotAvailable

: OWNED ( n -- n )
   dup 0= if drop E-UNAVAILABLE throw then ;

: STRING-OWNED ( n -- n )
   dup 0= if drop E-CONFIG throw then ;

\ Any status but success is a failure; the caller handles SEC-NOT-FOUND first.
: SEC-STATUS ( n -- )
   dup SEC-UNAVAILABLE = if drop E-UNAVAILABLE throw then
   0 <> if E-ACCESS throw then ;

: SYMBOL ( n ptr u8 -- n )
   FFI:DLSYM OWNED >BYTES CELL-VIEW @ ;

: SEC-SYMBOL ( ptr request ptr u8 -- n )
   {: req:ptr name:ptr :}
   req REQUEST-LIBRARY @ name SYMBOL ;

: CF-SYMBOL ( ptr request ptr u8 -- n )
   {: req:ptr name:ptr :}
   req REQUEST-LIBRARY2 @ name SYMBOL ;


\ The query selects one generic password by service and account.
: MAC-OPEN ( ptr request -- )
   {: req:ptr :}
   s\" /System/Library/Frameworks/Security.framework/Security\z" drop FFI:NOW FFI:DLOPEN
      OWNED req REQUEST-LIBRARY !
   s\" /System/Library/Frameworks/CoreFoundation.framework/CoreFoundation\z" drop FFI:NOW FFI:DLOPEN
      OWNED req REQUEST-LIBRARY2 !
   \ NULL callbacks borrow exact exported key identities and the owned CF values below.
   0 0 0 0 CF-DICT OWNED req REQUEST-QUERY !
   0 req REQUEST-SERVICE @ req REQUEST-SERVICE-LEN @ $08000100 0 CF-STRING
      STRING-OWNED req REQUEST-SERVICE-OBJ !
   0 req REQUEST-ACCOUNT @ req REQUEST-ACCOUNT-LEN @ $08000100 0 CF-STRING
      STRING-OWNED req REQUEST-ACCOUNT-OBJ !
   req REQUEST-QUERY @ req s\" kSecClass\z" drop SEC-SYMBOL
      req s\" kSecClassGenericPassword\z" drop SEC-SYMBOL CF-SET
   req REQUEST-QUERY @ req s\" kSecAttrService\z" drop SEC-SYMBOL req REQUEST-SERVICE-OBJ @ CF-SET
   req REQUEST-QUERY @ req s\" kSecAttrAccount\z" drop SEC-SYMBOL req REQUEST-ACCOUNT-OBJ @ CF-SET ;


: MAC-READ ( ptr request -- )
   {: req:ptr :}
   req MAC-OPEN
   req REQUEST-QUERY @ req s\" kSecReturnData\z" drop SEC-SYMBOL
      req s\" kCFBooleanTrue\z" drop CF-SYMBOL CF-SET
   req REQUEST-QUERY @ req REQUEST-VALUE BYTE-VIEW SEC-READ {: status:n :}
   status SEC-NOT-FOUND = if exit then
   status SEC-STATUS
   req REQUEST-VALUE @ OWNED {: value:n :}
   value CF-BYTES value CF-SIZE req COPY-VALUE ;


\ Updating in place keeps an existing item's access control; only a missing
\ item is added. An add that meets a duplicate lost a race with a concurrent
\ creator, so the value-only update runs once more against that item. The
\ CFData borrows the caller's bytes (kCFAllocatorNull), so Habu allocates no
\ copy of the value.
: MAC-WRITE ( ptr request -- )
   {: req:ptr :}
   req MAC-OPEN
   0 req REQUEST-DATA @ req REQUEST-CAPACITY @ req s\" kCFAllocatorNull\z" drop CF-SYMBOL
      CF-DATA-BORROW OWNED req REQUEST-VALUE !
   req s\" kSecValueData\z" drop SEC-SYMBOL {: data-key:n :}
   0 0 0 0 CF-DICT OWNED req REQUEST-ATTRS !
   req REQUEST-ATTRS @ data-key req REQUEST-VALUE @ CF-SET
   req REQUEST-QUERY @ req REQUEST-ATTRS @ SEC-UPDATE {: status:n :}
   status SEC-NOT-FOUND <> if status SEC-STATUS exit then
   req REQUEST-QUERY @ data-key req REQUEST-VALUE @ CF-SET
   req REQUEST-QUERY @ 0 SEC-ADD {: added:n :}
   added SEC-DUPLICATE <> if added SEC-STATUS exit then
   req REQUEST-QUERY @ data-key CF-REMOVE
   req REQUEST-QUERY @ req REQUEST-ATTRS @ SEC-UPDATE SEC-STATUS ;


: MAC-ERASE ( ptr request -- )
   {: req:ptr :}
   req MAC-OPEN
   req REQUEST-QUERY @ SEC-DELETE {: status:n :}
   status SEC-NOT-FOUND = if exit then
   status SEC-STATUS
   true req REQUEST-FOUND ! ;


: MAC-CLOSE ( ptr request -- )
   {: req:ptr :}
   req REQUEST-VALUE @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-ATTRS @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-QUERY @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-SERVICE-OBJ @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-ACCOUNT-OBJ @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-LIBRARY2 @ dup 0 <> if LIB-CLOSE 0 <> else drop false then
   req REQUEST-LIBRARY @ dup 0 <> if LIB-CLOSE 0 <> else drop false then
   or if E-ACCESS throw then ;

: INSTALL-MAC ( -- )
   ['] MAC-READ is NATIVE-READ
   ['] MAC-WRITE is NATIVE-WRITE
   ['] MAC-ERASE is NATIVE-ERASE
   ['] MAC-CLOSE is NATIVE-CLOSE ;
INSTALL-MAC
;package
