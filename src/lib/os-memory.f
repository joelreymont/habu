\ os-memory.f - the running host's memory facts that are not image ABI
\ constants. PAGE-SIZE reports the host's page size for OS mappings; Habu's
\ guard window uses the fixed STACK-ABI:PAGE-BYTES and PROT-PAGE-MAX instead.
\ AVAILABLE answers the bytes the host can give new allocations without
\ swapping, read at the call: Linux's /proc/meminfo line `MemAvailable: <n>
\ kB` (proc(5), Linux 3.14 and later), n times 1024. A host with no such file
\ or line, macOS among them, or a count that is not decimal kB fitting a cell
\ once scaled, is logged and throws E-AVAILABLE. A file it cannot open or
\ read, or one over 16 KiB, throws FS's code (E-FS-OPEN, E-FS-IO,
\ E-FS-CAPACITY), not logged here.
\ The FFI binding resolves at its first call in each process: a function
\ address is scratch (src/lib/ffi-abi.f FN-LOOKUP).
\ STORAGE CLASS. CALLER-OWNED: constants only; AVAILABLE reads the file into a
\ MEM:WITH-BYTES scope.
require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/fs.f
require src/lib/log.f
require src/lib/memory.f
require src/lib/string.f
require src/lib/string-option.f

package OS-MEMORY
private
-9310 constant E-FIRST
-9319 constant E-LAST
public
-9310 s" the host reports no available memory: it needs a /proc/meminfo with a `MemAvailable: <n> kB` line, as Linux 3.14 and later give" ERROR:FAULT E-AVAILABLE

private

PROCESS-SYMBOLS
FUNCTION: PAGE-SIZE-CALL getpagesize ( -- i32 ) ;FUNCTION

16384 constant MEMINFO-CAP    \ bytes; the file is under 2 KiB
1024 constant KB
$7FFFFFFFFFFFFFFF KB / constant KB-MAX    \ the most kB a cell holds as bytes

: MEMINFO$ ( -- str ) s" /proc/meminfo" ;
: KEY$ ( -- str ) s" MemAvailable:" ;

: FAIL ( str -- ) {: why:str :}
   LOG:ERROR s" os-memory" LOG:LINE why LOG:TEXT+
   s" : " LOG:TEXT+ E-AVAILABLE LOG:CODE+ LOG:;LINE
   E-AVAILABLE throw ;

\ True when text holds KEY$ at offset i, at the start of a line.
: KEY-AT? ( str n -- bool ) {: s:str i:n :}
   i 0 > if s STR:DATA i + 1- c@ STR:LF <> if false exit then then
   s STR:DATA i + s STR:LEN i - >str KEY$ STR:STARTS-WITH? ;

\ The offset just past the key of the text's MemAvailable line.
: KEY-END ( str -- n ) {: s:str :}
   s STR:LEN 0 ?do
      s i KEY-AT? if i KEY$ STR:LEN + unloop exit then
   loop
   s" /proc/meminfo has no MemAvailable line" FAIL ;

\ The text up to its first LF.
: LINE-OF ( str -- str ) {: s:str :}
   s STR:LF STR:INDEX-OF MATCH option
      none OF s ENDOF
      some OF IDX>N s STR:DATA swap >str ENDOF
   ;MATCH ;

\ The bytes a trimmed `<n> kB` field states.
: FIELD-BYTES ( str -- n ) {: s:str :}
   s s"  kB" STR:ENDS-WITH? 0= if s" MemAvailable is not in kB" FAIL then
   s STR:DATA s STR:LEN 3 - >str STR:RTRIM STR:PARSE-POS MATCH option
      none OF s" MemAvailable is not a decimal count" FAIL ENDOF
      some OF ENDOF
   ;MATCH {: kb:n :}
   kb KB-MAX > if s" MemAvailable overflows a cell" FAIL then
   kb KB * ;

: TEXT-BYTES ( str -- n ) {: s:str :}
   s KEY-END {: at:n :}
   s STR:DATA at + s STR:LEN at - >str LINE-OF STR:TRIM FIELD-BYTES ;

: READ-BYTES ( ^u8 NUM:alloc-byte-len -- n )
   drop {: buf:^ :}
   MEMINFO$ FS:FILE? 0= if s" the host has no /proc/meminfo" FAIL then
   MEMINFO$ buf MEMINFO-CAP FS:READ-ALL {: got:n :}
   buf got >str TEXT-BYTES ;

public

: PAGE-SIZE ( -- n )
   PAGE-SIZE-CALL ;

: AVAILABLE ( -- n )
   MEMINFO-CAP MEM:BYTES-ALLOC-LEN [: READ-BYTES ;] MEM:WITH-BYTES ;

;package
