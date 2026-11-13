\ E2E of SECRET:DIR-BIND through the public SECRET words over a fresh
\ directory under TMPDIR, removed whatever the cases do. Fails on: a wrong or
\ truncated item; a miss read as empty or the reverse; a copy past the
\ destination; an account naming no plain file of the directory, or another
\ service, read; an item that is no regular file, a FIFO included, read or
\ waited on; a write or erase admitted; a path that is no directory, a
\ directory holding a NUL or an empty service bound, or a refused bind changing
\ the bound store.
require src/lib/secret/dir.f
require src/lib/secret.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/span.f
require src/lib/ffi-abi.f

package SECRET-DIR-TEST
private

16 SPAN-BUFFER: OUT
PATH-CAP SPAN-BUFFER: ROOT
variable ROOT-U
PATH-CAP SPAN-BUFFER: ENTRY-PATH
$180 constant FIFO-MODE                \ 0600

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ptr u8 n -- i32 ) ;FUNCTION

: ROOT$ ( -- ptr u8 n ) ROOT SPAN:$ drop ROOT-U @ ;
: SERVICE$ ( -- ptr u8 n ) s" org.habu.secret-dir-test" ;


\ The path of the named entry in ROOT, borrowed until the next call.
: ENTRY ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u ENTRY-PATH FS:JOIN-PATH-INTO {: k:n :}
   ENTRY-PATH SPAN:$ drop k ;


: SETUP ( -- )
   s" habu-secret-dir" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u ROOT SPAN:COPY u ROOT-U !
   s" key" ENTRY s\" dir\x00value" FS:WRITE-ALL
   s" empty" ENTRY s" " FS:WRITE-ALL
   s" large" ENTRY s" nine byte" FS:WRITE-ALL ;


: CLEAN ( -- )
   ROOT-U @ 0= if exit then
   ROOT$ FS:REMOVE-TREE ;


: FOUND ( ptr u8 n ptr u8 n -- )
   {: account:ptr au:n want:ptr wu:n :}
   SERVICE$ account au OUT SECRET:READ TTRUE {: got:n :}
   got wu T=
   OUT SPAN:$ drop got want wu T$= ;


: TEST-READS ( -- )
   s" a present item reads its exact bytes" T-LABEL
   s" key" s\" dir\x00value" FOUND
   s" a missing item is a miss and an empty one is found" T-LABEL
   SERVICE$ s" absent" OUT SECRET:READ TFALSE 0 T=
   SERVICE$ s" empty" OUT 0 SPAN:TAKE SECRET:READ TTRUE 0 T=
   s" an item past the destination leaves it unchanged" T-LABEL
   $A5 OUT SPAN:FILL
   [: SERVICE$ s" large" OUT 8 SPAN:TAKE SECRET:READ 2drop ;]
   SECRET:E-CAPACITY TTHROWSQ
   16 0 ?do OUT i SPAN:U8@ $A5 T= loop ;


: TEST-REFUSALS ( -- )
   s" an account naming no plain file is E-CONFIG" T-LABEL
   [: SERVICE$ s" a/b" OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE$ s" ." OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE$ s" .." OUT SECRET:READ 2drop ;] SECRET:E-CONFIG TTHROWSQ
   s" another service is E-CONFIG" T-LABEL
   [: s" org.habu.other" s" key" OUT SECRET:READ 2drop ;]
   SECRET:E-CONFIG TTHROWSQ
   s" write and erase are E-ACCESS" T-LABEL
   [: SERVICE$ s" key" s" new" SECRET:WRITE ;] SECRET:E-ACCESS TTHROWSQ
   [: SERVICE$ s" key" SECRET:ERASE drop ;] SECRET:E-ACCESS TTHROWSQ
   s" a FIFO item is E-ACCESS without waiting for a writer" T-LABEL
   s" fifo" ENTRY FS:>PATHZ FIFO-MODE MKFIFO-CALL 0 T=
   [: SERVICE$ s" fifo" OUT SECRET:READ 2drop ;] SECRET:E-ACCESS TTHROWSQ
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
   [: SETUP TEST-BOUND ;] [: CLEAN ;] finally
   T-REPORT ;
TEST-MAIN
;package
