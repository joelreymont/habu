\ Private native writes exist only to exercise READ against dummy OS items.
require src/lib/secret.f
package SECRET
private

LIBRARY /System/Library/Frameworks/CoreFoundation.framework/CoreFoundation
FUNCTION: TEST-DATA CFDataCreate ( n ptr u8 n -- n ) ;FUNCTION
LIBRARY /System/Library/Frameworks/Security.framework/Security
FUNCTION: TEST-ADD SecItemAdd ( n n -- i32 ) ;FUNCTION
FUNCTION: TEST-REMOVE SecItemDelete ( n -- i32 ) ;FUNCTION

: TEST-MAC-SAVE ( ptr request -- )
   {: req:ptr :}
   req MAC-OPEN
   0 req REQUEST-DEST @ req REQUEST-CAPACITY @ TEST-DATA OWNED req REQUEST-VALUE !
   req REQUEST-QUERY @ req s\" kSecValueData\z" drop SEC-SYMBOL req REQUEST-VALUE @ CF-SET
   req REQUEST-QUERY @ 0 TEST-ADD 0 <> if E-ACCESS throw then ;

: TEST-MAC-DELETE ( ptr request -- )
   {: req:ptr :}
   req MAC-OPEN
   req REQUEST-QUERY @ TEST-REMOVE dup -25300 = if drop exit then
   0 <> if E-ACCESS throw then ;

: TEST-MAC-INSTALL ( -- )
   ['] TEST-MAC-SAVE is TEST-SAVE-NATIVE
   ['] TEST-MAC-DELETE is TEST-DELETE-NATIVE ;
TEST-MAC-INSTALL
;package
