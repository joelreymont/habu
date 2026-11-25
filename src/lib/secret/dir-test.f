\ E2E of SECRET:DIR-BIND through the public SECRET words over a fresh
\ directory under TMPDIR, removed whatever the cases do. Fails on: a wrong or
\ truncated item; a miss read as empty or the reverse; an item longer than
\ the vector's room cut short; an account naming no plain file of the
\ directory, or another
\ service, read; an item that is no regular file, a FIFO included, read or
\ waited on; a write or erase admitted; a path that is no directory, a
\ directory holding a NUL or an empty service bound, or a refused bind changing
\ the bound store.
require src/lib/secret/dir.f
require src/lib/secret.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/span.f
require src/lib/ffi-abi.f
require src/lib/vector.f
require src/lib/heap.f

package SECRET-DIR-TEST
private

TYPED-VARIABLE OUT-VEC ^VEC:vector<u8>
PATH-CAP SPAN-BUFFER: ROOT
variable ROOT-U
PATH-CAP SPAN-BUFFER: ENTRY-PATH
$180 constant FIFO-MODE                \ 0600

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ^u8 n -- i32 ) ;FUNCTION

: ROOT$ ( -- str ) ROOT SPAN:$ STR:DATA ROOT-U @ >str ;
: SERVICE$ ( -- str ) s" org.habu.secret-dir-test" ;
: OUT ( -- ^VEC:vector<u8> ) OUT-VEC @ ;
: OUT$ ( -- str ) OUT VEC:BYTES ;

\ Empties OUT, zeroing the bytes it held.
: CLEAR-OUT ( -- ) OUT 0 VEC:RESIZE ;

: HOLD ( -- ) CLEAR-OUT s" held" OUT VEC:APPEND ;

\ OUT holds what HOLD put there, and is emptied.
: HELD ( -- ) OUT$ s" held" T$= CLEAR-OUT ;


\ The path of the named entry in ROOT, borrowed until the next call.
: ENTRY ( str -- str )
   {: name:str :}
   ROOT$ name ENTRY-PATH FS:JOIN-PATH-INTO {: k:n :}
   ENTRY-PATH SPAN:$ STR:DATA k >str ;


: SETUP ( -- )
   s" habu-secret-dir" FS:MKTEMP-DIR {: dir:str :}
   dir ROOT SPAN:COPY dir STR:LEN ROOT-U !
   s" key" ENTRY s\" dir\x00value" FS:WRITE-ALL
   s" empty" ENTRY s" " FS:WRITE-ALL ;


: CLEAN ( -- )
   ROOT-U @ 0= if exit then
   ROOT$ FS:REMOVE-TREE ;


: FOUND ( str str -- )
   {: account:str want:str :}
   SERVICE$ account OUT SECRET:READ TTRUE
   OUT$ want T$=
   CLEAR-OUT ;


: TEST-READS ( -- )
   s" a present item reads its exact bytes after the vector's" T-LABEL
   HOLD s" key" s\" helddir\x00value" FOUND
   s" a missing item is a miss and keeps the vector's bytes" T-LABEL
   HOLD SERVICE$ s" absent" OUT SECRET:READ TFALSE HELD
   s" an empty item is found" T-LABEL
   SERVICE$ s" empty" OUT SECRET:READ TTRUE OUT VEC:COUNT 0 T= ;


: TEST-REFUSALS ( -- )
   s" an account naming no plain file is E-CONFIG" T-LABEL
   [: SERVICE$ s" a/b" OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE$ s" ." OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE$ s" .." OUT SECRET:READ drop ;] SECRET:E-CONFIG TTHROWSQ
   s" another service is E-CONFIG" T-LABEL
   [: s" org.habu.other" s" key" OUT SECRET:READ drop ;]
   SECRET:E-CONFIG TTHROWSQ
   s" write and erase are E-ACCESS" T-LABEL
   [: SERVICE$ s" key" s" new" SECRET:WRITE ;] SECRET:E-ACCESS TTHROWSQ
   [: SERVICE$ s" key" SECRET:ERASE drop ;] SECRET:E-ACCESS TTHROWSQ
   s" a FIFO item is E-ACCESS without waiting for a writer" T-LABEL
   s" fifo" ENTRY FS:>PATHZ FIFO-MODE MKFIFO-CALL 0 T=
   HOLD
   [: SERVICE$ s" fifo" OUT SECRET:READ drop ;] SECRET:E-ACCESS TTHROWSQ
   HELD
   s" key" s\" dir\x00value" FOUND ;


: TEST-BIND-REFUSALS ( -- )
   s" binding a regular file or a missing path is E-CONFIG" T-LABEL
   [: s" key" ENTRY SERVICE$ SECRET:DIR-BIND ;] SECRET:E-CONFIG TTHROWSQ
   [: s" absent" ENTRY SERVICE$ SECRET:DIR-BIND ;] SECRET:E-CONFIG TTHROWSQ
   s" binding a directory holding a NUL or an empty service is E-CONFIG" T-LABEL
   [: s\" \x00" ENTRY SERVICE$ SECRET:DIR-BIND ;] SECRET:E-CONFIG TTHROWSQ
   [: ROOT$ s" " SECRET:DIR-BIND ;] SECRET:E-CONFIG TTHROWSQ
   s" a refused bind leaves the bound store" T-LABEL
   s" key" s\" dir\x00value" FOUND ;


: TEST-BOUND ( -- )
   ROOT$ SERVICE$ SECRET:DIR-BIND
   TEST-READS TEST-REFUSALS TEST-BIND-REFUSALS ;


: TEST-MAIN ( -- )
   T-RESET 0 ROOT-U !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW OUT-VEC !
   [: SETUP TEST-BOUND ;] [: CLEAN ;] finally
   OUT VEC:DELETE
   T-REPORT ;
TEST-MAIN
;package
