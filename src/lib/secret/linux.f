\ secret/linux.f - SECRET:READ and SECRET:WRITE through libsecret's Secret
\ Service, loaded by src/lib/secret.f on a Linux target. An item is the one
\ whose two attributes hold the service and the account, with no schema, as
\ `secret-tool` stores and looks one up; LINUX-ITEM! names the attributes and
\ the label. Contract: src/lib/secret.f.
\ STORAGE CLASS. PROCESS-WIDE: the bound attribute names and label, written by
\ LINUX-ITEM! before use and only read afterwards.
HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or ?TARGET
require src/lib/secret.f
require src/lib/memory.f                      \ the identifiers' C copies
require src/lib/span.f
require src/lib/string.f
require src/lib/log.f

package SECRET
private

LIBRARY libglib-2.0.so.0
FUNCTION: HASH-NEW g_hash_table_new ( n n -- n ) ;FUNCTION
FUNCTION: HASH-INSERT g_hash_table_insert ( n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: HASH-FREE g_hash_table_unref ( n -- ) ;FUNCTION
FUNCTION: ERROR-FREE g_error_free ( n -- ) ;FUNCTION
FUNCTION: LIST-DATA g_list_nth_data ( n n -- n ) ;FUNCTION
FUNCTION: LIST-FREE g_list_free ( n -- ) ;FUNCTION

LIBRARY libgobject-2.0.so.0
FUNCTION: OBJECT-FREE g_object_unref ( n -- ) ;FUNCTION

LIBRARY libsecret-1.so.0
FUNCTION: SECRET-SEARCH secret_service_search_sync ( n n n n n ^u8 -- n ) ;FUNCTION
FUNCTION: SECRET-GET secret_item_get_secret ( n -- n ) ;FUNCTION
FUNCTION: VALUE-GET secret_value_get ( n ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: VALUE-FREE secret_value_unref ( n -- ) ;FUNCTION
FUNCTION: VALUE-NEW secret_value_new ( ^u8 n ^u8 -- n ) ;FUNCTION
FUNCTION: SECRET-STORE secret_service_store_sync ( n n n n ^u8 n n ^u8 -- i32 ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: LIB-CLOSE dlclose ( n -- i32 ) ;FUNCTION

\ The bound service key, account key and label, each NUL-terminated, back to
\ back; the hash tables READ and WRITE build point into it.
DYNAMIC-BUFFER ITEM-NAMES u8
variable ITEM-ACCOUNT-AT
variable ITEM-LABEL-AT
TYPED-VARIABLE ITEM-BOUND bool
false ITEM-BOUND !

: SERVICE-KEY ( -- ^u8 )
   ITEM-BOUND @ if 0 ITEM-NAMES exit then
   s\" service\z" drop ;

: ACCOUNT-KEY ( -- ^u8 )
   ITEM-BOUND @ if ITEM-ACCOUNT-AT @ ITEM-NAMES exit then
   s\" account\z" drop ;

: ITEM-LABEL ( ^request -- ^u8 )
   ITEM-BOUND @ if drop ITEM-LABEL-AT @ ITEM-NAMES exit then
   REQUEST-SERVICE-OBJ @ >BYTES ;

\ Copies the name to offset at with its NUL; answers the offset after it.
: ITEM-PUT ( ^u8 n n -- n )
   {: a:^ u:n at:n :}
   a at ITEM-NAMES u BYTE-COPY
   0 at u + ITEM-NAMES c!
   at u + 1+ ;

: OWNED ( n -- n )
   dup 0= if drop E-ACCESS throw then ;

: AVAILABLE ( n -- n )
   dup 0= if drop E-CONFIG throw then ;

\ The query table has no destroy functions, so each identifier's C copy is
\ Habu's: allocated and published before the copy, so LINUX-CLOSE frees it
\ after the table on every exit.
: ID-SPAN ( n n -- SPAN:span<u8> )
   {: cell:n u:n :}
   cell >BYTES u FFI:CSTR-ROOM SPAN:MAKE ;

: ID-ALLOC ( n -- n )
   FFI:CSTR-ROOM MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN SPAN:$ drop FFI:>CELL ;

: ID-COPY ( ^u8 n n -- )
   {: text u:n cell:n :}
   text u cell u ID-SPAN FFI:CSTR drop ;

: ID-FREE ( n n -- )
   {: cell:n u:n :}
   cell 0= if exit then
   cell u ID-SPAN MEM:FREE-SPAN ;

: LINUX-OPEN ( ^request -- )
   {: req:^ :}
   s\" libglib-2.0.so.0\z" drop FFI:NOW FFI:DLOPEN AVAILABLE req REQUEST-LIBRARY !
   req REQUEST-LIBRARY @ s\" g_str_hash\z" drop FFI:DLSYM AVAILABLE
   req REQUEST-LIBRARY @ s\" g_str_equal\z" drop FFI:DLSYM AVAILABLE
   HASH-NEW OWNED req REQUEST-QUERY !
   req REQUEST-SERVICE-LEN @ ID-ALLOC req REQUEST-SERVICE-OBJ !
   req REQUEST-SERVICE @ req REQUEST-SERVICE-LEN @ req REQUEST-SERVICE-OBJ @ ID-COPY
   req REQUEST-ACCOUNT-LEN @ ID-ALLOC req REQUEST-ACCOUNT-OBJ !
   req REQUEST-ACCOUNT @ req REQUEST-ACCOUNT-LEN @ req REQUEST-ACCOUNT-OBJ @ ID-COPY
   req REQUEST-QUERY @ SERVICE-KEY req REQUEST-SERVICE-OBJ @ HASH-INSERT drop
   req REQUEST-QUERY @ ACCOUNT-KEY req REQUEST-ACCOUNT-OBJ @ HASH-INSERT drop ;

\ A GError's message follows its two 32-bit fields, domain and code.
: ERROR-TEXT ( n -- ^u8 n )
   >BYTES 8 + CELL-VIEW @ >BYTES dup ZLEN ;

\ Logs the request's GError after the text, then throws E-ACCESS.
: GERROR-FAILED ( ^request ^u8 n -- )
   {: req:^ what:^ wu:n :}
   LOG:ERROR s" secret" LOG:LINE
   what wu LOG:TEXT+ s" : " LOG:TEXT+
   req REQUEST-ERROR @ ERROR-TEXT LOG:TEXT+ LOG:;LINE
   E-ACCESS throw ;

: LINUX-READ ( ^request -- )
   {: req:^ :}
   req LINUX-OPEN
   \ Search retains a matching locked item even when its unlock prompt is dismissed.
   0 0 req REQUEST-QUERY @ 12 0 req REQUEST-ERROR BYTE-VIEW SECRET-SEARCH
      req REQUEST-LIST !
   req REQUEST-ERROR @ 0 <> if
      req s" cannot search for the item" GERROR-FAILED
   then
   req REQUEST-LIST @ 0= if exit then
   req REQUEST-LIST @ 0 LIST-DATA SECRET-GET req REQUEST-VALUE !
   req REQUEST-VALUE @ 0= if E-ACCESS throw then
   req REQUEST-VALUE @ req REQUEST-SIZE BYTE-VIEW VALUE-GET
   req REQUEST-SIZE @ req COPY-VALUE ;

\ The NULL collection is the default one; an item with the same attributes
\ is replaced.
: LINUX-WRITE ( ^request -- )
   {: req:^ :}
   req LINUX-OPEN
   req REQUEST-DATA @ req REQUEST-CAPACITY @ s\" text/plain\z" drop
      VALUE-NEW OWNED req REQUEST-VALUE !
   0 0 req REQUEST-QUERY @ 0 req ITEM-LABEL req REQUEST-VALUE @ 0
      req REQUEST-ERROR BYTE-VIEW SECRET-STORE drop
   req REQUEST-ERROR @ 0 <> if
      req s" cannot store the item" GERROR-FAILED
   then ;

: LINUX-CLOSE ( ^request -- )
   {: req:^ :}
   req REQUEST-VALUE @ dup 0 <> if VALUE-FREE else drop then
   req REQUEST-ERROR @ dup 0 <> if ERROR-FREE else drop then
   req REQUEST-LIST @ dup 0 <> if
      dup 0 LIST-DATA OBJECT-FREE LIST-FREE
   else drop then
   req REQUEST-QUERY @ dup 0 <> if HASH-FREE else drop then
   req REQUEST-SERVICE-OBJ @ req REQUEST-SERVICE-LEN @ ID-FREE
   req REQUEST-ACCOUNT-OBJ @ req REQUEST-ACCOUNT-LEN @ ID-FREE
   req REQUEST-LIBRARY @ dup 0 <> if LIB-CLOSE 0 <> if E-ACCESS throw then else drop then ;

: INSTALL-LINUX ( -- )
   ['] LINUX-READ is NATIVE-READ
   ['] LINUX-WRITE is NATIVE-WRITE
   ['] LINUX-CLOSE is NATIVE-CLOSE ;
INSTALL-LINUX

public

\ Names the service attribute, the account attribute and the label of every
\ item, for the whole process, once, before any READ or WRITE: rebinding while
\ tasks call SECRET races. Unbound they are `service`, `account` and the
\ service. A name that is empty, holds a NUL or is malformed UTF-8, or two
\ equal keys, throw E-CONFIG and bind nothing.
: LINUX-ITEM! ( ^u8 n ^u8 n ^u8 n -- )
   {: sk:^ su:n ak:^ au:n label:^ lu:n :}
   sk su INPUT ak au INPUT label lu INPUT
   sk su ak au STR= if E-CONFIG throw then
   su au + lu + 3 + ITEM-NAMES-RESERVE
   sk su 0 ITEM-PUT dup ITEM-ACCOUNT-AT !
   ak au rot ITEM-PUT dup ITEM-LABEL-AT !
   label lu rot ITEM-PUT drop
   true ITEM-BOUND ! ;

;package
