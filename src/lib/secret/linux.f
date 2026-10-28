\ secret/linux.f - SECRET:READ through libsecret's Secret Service search,
\ loaded by src/lib/secret.f on a Linux target. Contract: src/lib/secret.f.
require src/lib/secret.f

package SECRET
private

LIBRARY libglib-2.0.so.0
FUNCTION: HASH-NEW g_hash_table_new ( n n -- n ) ;FUNCTION
FUNCTION: HASH-INSERT g_hash_table_insert ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: HASH-FREE g_hash_table_unref ( n -- ) ;FUNCTION
FUNCTION: STR-DUP g_strndup ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: G-FREE g_free ( n -- ) ;FUNCTION
FUNCTION: ERROR-FREE g_error_free ( n -- ) ;FUNCTION
FUNCTION: LIST-DATA g_list_nth_data ( n n -- n ) ;FUNCTION
FUNCTION: LIST-FREE g_list_free ( n -- ) ;FUNCTION

LIBRARY libgobject-2.0.so.0
FUNCTION: OBJECT-FREE g_object_unref ( n -- ) ;FUNCTION

LIBRARY libsecret-1.so.0
FUNCTION: SECRET-SEARCH secret_service_search_sync ( n n n n n ptr u8 -- n )
   5 8 WRITES-BYTES
;FUNCTION
FUNCTION: SECRET-GET secret_item_get_secret ( n -- n ) ;FUNCTION
FUNCTION: VALUE-GET secret_value_get ( n ptr u8 -- ptr u8 )
   1 8 WRITES-BYTES
;FUNCTION
FUNCTION: VALUE-FREE secret_value_unref ( n -- ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: LIB-CLOSE dlclose ( n -- i32 ) ;FUNCTION

: OWNED ( n -- n )
   dup 0= if drop E-ACCESS throw then ;

: AVAILABLE ( n -- n )
   dup 0= if drop E-UNAVAILABLE throw then ;

: LINUX-OPEN ( ptr request -- )
   {: req:ptr :}
   s\" libglib-2.0.so.0\z" drop FFI:NOW FFI:DLOPEN AVAILABLE req REQUEST-LIBRARY !
   req REQUEST-LIBRARY @ s\" g_str_hash\z" drop FFI:DLSYM AVAILABLE
   req REQUEST-LIBRARY @ s\" g_str_equal\z" drop FFI:DLSYM AVAILABLE
   HASH-NEW OWNED req REQUEST-QUERY !
   req REQUEST-SERVICE @ req REQUEST-SERVICE-LEN @ STR-DUP OWNED req REQUEST-SERVICE-OBJ !
   req REQUEST-ACCOUNT @ req REQUEST-ACCOUNT-LEN @ STR-DUP OWNED req REQUEST-ACCOUNT-OBJ !
   \ No schema restricts this search: these attributes interoperate with desktop clients.
   req REQUEST-QUERY @ s\" service\z" drop req REQUEST-SERVICE-OBJ @ HASH-INSERT drop
   req REQUEST-QUERY @ s\" account\z" drop req REQUEST-ACCOUNT-OBJ @ HASH-INSERT drop ;

: LINUX-READ ( ptr request -- )
   {: req:ptr :}
   req LINUX-OPEN
   \ Search retains a matching locked item even when its unlock prompt is dismissed.
   \ https://gnome.pages.gitlab.gnome.org/libsecret/method.Service.search_sync.html
   0 0 req REQUEST-QUERY @ 12 0 req REQUEST-ERROR BYTE-VIEW SECRET-SEARCH
      req REQUEST-LIST !
   req REQUEST-ERROR @ 0 <> if E-ACCESS throw then
   req REQUEST-LIST @ 0= if exit then
   req REQUEST-LIST @ 0 LIST-DATA SECRET-GET req REQUEST-VALUE !
   req REQUEST-VALUE @ 0= if E-ACCESS throw then
   req REQUEST-VALUE @ req REQUEST-SIZE BYTE-VIEW VALUE-GET
   req REQUEST-SIZE @ req COPY-VALUE ;

: LINUX-CLOSE ( ptr request -- )
   {: req:ptr :}
   req REQUEST-VALUE @ dup 0 <> if VALUE-FREE else drop then
   req REQUEST-ERROR @ dup 0 <> if ERROR-FREE else drop then
   req REQUEST-LIST @ dup 0 <> if
      dup 0 LIST-DATA OBJECT-FREE LIST-FREE
   else drop then
   req REQUEST-QUERY @ dup 0 <> if HASH-FREE else drop then
   req REQUEST-SERVICE-OBJ @ dup 0 <> if G-FREE else drop then
   req REQUEST-ACCOUNT-OBJ @ dup 0 <> if G-FREE else drop then
   req REQUEST-LIBRARY @ dup 0 <> if LIB-CLOSE 0 <> if E-ACCESS throw then else drop then ;

: INSTALL-LINUX ( -- )
   ['] LINUX-READ is NATIVE-READ
   ['] LINUX-CLOSE is NATIVE-CLOSE ;
INSTALL-LINUX
;package
