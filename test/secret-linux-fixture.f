\ Private native writes exist only to exercise READ against dummy OS items.
require src/lib/secret.f
package SECRET
private

LIBRARY libsecret-1.so.0
FUNCTION: TEST-VALUE secret_value_new ( ptr u8 n ptr u8 -- n ) ;FUNCTION
FUNCTION: TEST-STORE secret_password_storev_binary_sync ( n n ptr u8 ptr u8 n n ptr u8 -- i32 )
   6 8 WRITES-BYTES
;FUNCTION
FUNCTION: TEST-CLEAR secret_password_clearv_sync ( n n n ptr u8 -- i32 )
   3 8 WRITES-BYTES
;FUNCTION

: TEST-LINUX-SAVE ( ptr request -- )
   {: req:ptr :}
   req LINUX-OPEN
   req REQUEST-DEST @ req REQUEST-CAPACITY @ s\" text/plain\z" drop TEST-VALUE
      OWNED req REQUEST-VALUE !
   0 req REQUEST-QUERY @ s\" default\z" drop s\" Habu credential E2E\z" drop
      req REQUEST-VALUE @ 0 req REQUEST-ERROR BYTE-VIEW TEST-STORE
      0= req REQUEST-ERROR @ 0 <> or if E-ACCESS throw then ;

: TEST-LINUX-DELETE ( ptr request -- )
   {: req:ptr :}
   req LINUX-OPEN
   0 req REQUEST-QUERY @ 0 req REQUEST-ERROR BYTE-VIEW TEST-CLEAR drop
   req REQUEST-ERROR @ 0 <> if E-ACCESS throw then ;

: TEST-LINUX-INSTALL ( -- )
   ['] TEST-LINUX-SAVE is TEST-SAVE-NATIVE
   ['] TEST-LINUX-DELETE is TEST-DELETE-NATIVE ;
TEST-LINUX-INSTALL
;package
