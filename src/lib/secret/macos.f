\ secret/macos.f - SECRET:READ through the Security framework's generic
\ passwords, loaded by src/lib/secret.f on macOS. Contract: src/lib/secret.f.
require src/lib/secret.f

package SECRET
private

\ https://developer.apple.com/documentation/security/secitemcopymatching(_:_:)
\ CF values and the returned data remain owned through each native call.
LIBRARY /System/Library/Frameworks/CoreFoundation.framework/CoreFoundation
FUNCTION: CF-STRING CFStringCreateWithBytes ( n ptr u8 n n n -- n ) ;FUNCTION
FUNCTION: CF-DICT CFDictionaryCreateMutable ( n n n n -- n ) ;FUNCTION
FUNCTION: CF-SET CFDictionarySetValue ( n n n -- ) ;FUNCTION
FUNCTION: CF-RELEASE CFRelease ( n -- ) ;FUNCTION
FUNCTION: CF-SIZE CFDataGetLength ( n -- n ) ;FUNCTION
FUNCTION: CF-BYTES CFDataGetBytePtr ( n -- ptr u8 ) ;FUNCTION

LIBRARY /System/Library/Frameworks/Security.framework/Security
FUNCTION: SEC-READ SecItemCopyMatching ( n ptr u8 -- i32 )
   1 8 WRITES-BYTES
;FUNCTION

PROCESS-SYMBOLS
FUNCTION: LIB-CLOSE dlclose ( n -- i32 ) ;FUNCTION

: OWNED ( n -- n )
   dup 0= if drop E-UNAVAILABLE throw then ;

: STRING-OWNED ( n -- n )
   dup 0= if drop E-CONFIG throw then ;

: SEC-SYMBOL ( ptr request ptr u8 -- n )
   {: req:ptr name:ptr :}
   req REQUEST-LIBRARY @ name FFI:DLSYM OWNED >BYTES CELL-VIEW @ ;

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
   req REQUEST-LIBRARY2 @ s\" kCFBooleanTrue\z" drop FFI:DLSYM OWNED >BYTES CELL-VIEW @ {: yes:n :}
   req REQUEST-QUERY @ req s\" kSecReturnData\z" drop SEC-SYMBOL yes CF-SET
   req REQUEST-QUERY @ req REQUEST-VALUE BYTE-VIEW SEC-READ {: status:n :}
   status -25300 = if exit then             \ errSecItemNotFound
   status -25291 = if E-UNAVAILABLE throw then \ errSecNotAvailable
   status 0 <> if E-ACCESS throw then
   req REQUEST-VALUE @ OWNED {: value:n :}
   value CF-BYTES value CF-SIZE req COPY-VALUE ;

: MAC-CLOSE ( ptr request -- )
   {: req:ptr :}
   req REQUEST-VALUE @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-QUERY @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-SERVICE-OBJ @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-ACCOUNT-OBJ @ dup 0 <> if CF-RELEASE else drop then
   req REQUEST-LIBRARY2 @ dup 0 <> if LIB-CLOSE 0 <> else drop false then
   req REQUEST-LIBRARY @ dup 0 <> if LIB-CLOSE 0 <> else drop false then
   or if E-ACCESS throw then ;

: INSTALL-MAC ( -- )
   ['] MAC-READ is NATIVE-READ
   ['] MAC-CLOSE is NATIVE-CLOSE ;
INSTALL-MAC
;package
