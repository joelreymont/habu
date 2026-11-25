\ heap-test.f - src/lib/test/heap-churn.f through HEAP:ALLOCATOR in a module
\ `hb-build --target wasm` builds, validated by wasm-tools and run by bun
\ (test/wasm/harness.f): it must print no block taken or grown with a nonzero
\ byte, none freed or resized with a kept mark changed, E-MEM-MAP for the
\ request over 4 GiB and its highest live bytes, and end with status 0. Wasm
\ memory never shrinks, so its size at exit caps the heap's mapped bytes at
\ every step: it must stay within its size before the run plus 1.25 times the
\ highest live bytes and one region. test/wasm/device.f runs it. The source
\ and the module stay in the printed directory.

require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test.f
require test/wasm/harness.f

package WASM-HEAP-TEST
private

$100000 constant REGION            \ HEAP's private REGION-BYTES, 1 MiB

PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: MOD
variable MOD-U
PATH-CAP BUFFER: SRC
variable SRC-U

: SOURCE$ ( -- str )
   S\" require src/lib/heap.f\nrequire src/lib/test/heap-churn.f\n: MAIN ( -- ) HEAP:ALLOCATOR HEAP-CHURN:RUN {: u:n c:n r:n h:n :} u . c . r . h . ;\n" ;

: SETUP ( -- )
   s" wasm-heap" FS:MKTEMP-DIR {: s:str :}
   s STR:DATA DIR s STR:LEN BYTE-COPY  s STR:LEN DIR-U !
   s" wasm heap: source and module in " type  DIR s STR:LEN >str type cr
   DIR DIR-U @ >str s" churn.f" SRC FS:JOIN-PATH SRC-U !
   DIR DIR-U @ >str s" churn.wasm" MOD FS:JOIN-PATH MOD-U !
   SRC SRC-U @ >str SOURCE$ FS:WRITE-ALL ;


\ RUN's answers but the last, one a line, as `.` prints them in the module.
: WANT$ ( -- str )
   SB-RESET  S\" 0\n0\n" SB-APPEND
   E-MEM-MAP FMT:SB-INT  10 SB-APPEND-C  SB$ ;


\ OUT's lines after WANT$'s, the highest live bytes, or 0.
: HIGHEST ( -- n )
   WASM-HARNESS:OUT$ {: s:str :}
   WANT$ {: w:str :}
   s STR:DATA  w STR:LEN s STR:LEN min >str  w T$=
   s STR:DATA w STR:LEN s STR:LEN min +  s STR:LEN w STR:LEN - 0 max >str  STR:RTRIM STR:>NUMBER? MATCH option
      some OF ENDOF
      none OF 0 ENDOF
   ;MATCH ;


\ (memory grown - one region) / highest live, in thousandths.
: RATIO ( n -- n )
   {: high:n :}
   WASM-HARNESS:MEMORY-END WASM-HARNESS:MEMORY-START -  REGION -
   1000 *  high 1 max / ;


: SHOW ( n -- )
   {: high:n :}
   SB-RESET  s" wasm heap: memory " SB-APPEND
   WASM-HARNESS:MEMORY-START FMT:SB-INT  s"  to " SB-APPEND
   WASM-HARNESS:MEMORY-END FMT:SB-INT  s"  bytes, highest live " SB-APPEND
   high FMT:SB-INT  s"  bytes, (grown - 1 region) / highest live " SB-APPEND
   high RATIO 3 FMT:SB-SCALED  SB$ type cr ;


\ One region, the step the heap grows by: this churn's excess over 1.25 x live
\ measured -0.68 MB, -0.65 regions.
: BOUNDED ( n -- )
   {: high:n :}
   s" memory at exit is within its size before plus 1.25 x highest live plus one region" T-LABEL
   high 0 > TTRUE
   WASM-HARNESS:MEMORY-END
   WASM-HARNESS:MEMORY-START  high 5 * 4 /  +  REGION +  <= TTRUE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" 100000 blocks of 100 B to 1 MiB, freed or resized, come zeroed and keep their bytes in a Wasm module" T-LABEL
   SRC SRC-U @ >str MOD MOD-U @ >str WASM-HARNESS:BUILD 0 T=
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ >str WASM-HARNESS:RUN 0 T=
   HIGHEST {: high:n :}
   high SHOW
   high BOUNDED
   T-REPORT ;

;package

WASM-HEAP-TEST:RUN
