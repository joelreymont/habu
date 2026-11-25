\ storage-test.f - test/wasm/storage.f's entries, each the MAIN of a module
\ `hb-build --target wasm` builds, validated by wasm-tools and run by bun
\ (test/wasm/harness.f); each module's output must be its entry's lines and its
\ status the entry's: 0, or a trap for a page freed twice. test/wasm/device.f
\ runs it. The sources, the modules, the expected and the actual output stay in
\ the printed directory.

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test.f
require test/wasm/harness.f

package WASM-STORAGE-TEST
private

2 constant TRAPPED                     \ test/wasm/run.mjs's status for a trap
PATH-CAP BUFFER: DIR
variable DIR-U
PATH-CAP BUFFER: MOD
variable MOD-U
PATH-CAP BUFFER: SRC
variable SRC-U
PATH-CAP BUFFER: FILE
variable FILE-U

: SETUP ( -- )
   s" wasm-storage" FS:MKTEMP-DIR {: s:str :}
   s STR:DATA DIR s STR:LEN BYTE-COPY  s STR:LEN DIR-U !
   s" wasm storage: modules and output in " type  DIR s STR:LEN >str type cr ;

: FILE! ( str str -- )
   {: s:str name:str :}
   DIR DIR-U @ >str name FILE FS:JOIN-PATH FILE-U !
   FILE FILE-U @ >str s FS:WRITE-ALL ;

\ The source name.f in DIR, whose MAIN is storage.f's entry, built into
\ name.wasm there; answers the build's status.
: BUILD ( str str -- n )
   {: entry:str name:str :}
   SB-RESET name SB-APPEND s" .wasm" SB-APPEND
   DIR DIR-U @ >str SB$ MOD FS:JOIN-PATH MOD-U !
   SB-RESET name SB-APPEND s" .f" SB-APPEND
   DIR DIR-U @ >str SB$ SRC FS:JOIN-PATH SRC-U !
   SB-RESET S\" require test/wasm/storage.f\n: MAIN ( -- ) " SB-APPEND
   entry SB-APPEND  S\"  ;\n" SB-APPEND
   SRC SRC-U @ >str SB$ FS:WRITE-ALL
   SRC SRC-U @ >str MOD MOD-U @ >str WASM-HARNESS:BUILD ;

\ The entry's module, the file name with .wasm, builds, validates and runs to
\ status, printing want; want and the output stay as name-expected.txt and
\ name-actual.txt.
: ENTRY-CASE ( str str str n -- )
   {: entry:str name:str want:str status:n :}
   SB-RESET name SB-APPEND s" -expected.txt" SB-APPEND
   want SB$ FILE!
   entry name BUILD 0 T=
   MOD MOD-U @ >str WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ >str WASM-HARNESS:RUN status T=
   SB-RESET name SB-APPEND s" -actual.txt" SB-APPEND
   WASM-HARNESS:OUT$ SB$ FILE!
   WASM-HARNESS:OUT$ want T$= ;

\ GROW's lines, REFUSE's and REUSE's.
: BUFFER$ ( -- str )
   S\" 77\n88\n312\n1\n-3201\n-3201\n77\n88\n1\n0\n0\n" ;

: HOLES$ ( -- str )
   S\" 1\n0\n1\n1\n" ;

: FRACTIONAL$ ( -- str )
   S\" 7\n" ;

\ Each map's and free's ior, and INSIDE's pages' distance, up to the trap.
: TWICE$ ( -- str )
   S\" 0\n0\n" ;

: INSIDE$ ( -- str )
   S\" 0\n0\n65536\n0\n0\n" ;

\ The map's ior, the five refused frees', the free's, the map back's, that it
\ handed out the same page, and the last free's.
: REFUSED$ ( -- str )
   S\" 0\n-1\n-1\n-1\n-1\n-1\n0\n0\n1\n0\n" ;

public

: RUN ( -- )
   T-RESET
   SETUP
   s" generated structure accessors read and write distinct fields and rows" T-LABEL
   s" WSTORE:RECORD" s" record" S\" 23\n1.250000\n47\n-2.500000\n" 0 ENTRY-CASE
   s" one buffer reserves, grows across a page, refuses, releases and is reused cleared" T-LABEL
   s" WSTORE:BUFFER" s" buffer" BUFFER$ 0 ENTRY-CASE
   s" fractional capacity retains its tail" T-LABEL
   s" WSTORE:FRACTIONAL" s" fractional" FRACTIONAL$ 0 ENTRY-CASE
   s" freed neighbours join into one run, which a smaller request splits" T-LABEL
   s" WSTORE:HOLES" s" holes" HOLES$ 0 ENTRY-CASE
   s" a page freed twice traps" T-LABEL
   s" WSTORE:TWICE" s" twice" TWICE$ TRAPPED ENTRY-CASE
   s" a page freed again inside the run it joined traps" T-LABEL
   s" WSTORE:INSIDE" s" inside" INSIDE$ TRAPPED ENTRY-CASE
   s" a zero length, an unaligned address, a negative length, a length past 2^32 and an address past 2^32 are refused" T-LABEL
   s" WSTORE:REFUSED" s" refused" REFUSED$ 0 ENTRY-CASE
   T-REPORT ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ BOUNDS's lines: one per refused index or reserve.
: BOUNDS$ ( -- str )
   S\" 7122\n7121\n7122\n7121\n7122\n7122\n7122\n7122\n7122\n" ;

: RUN-BOUNDS ( -- )
   T-RESET
   SETUP
   s" generated accessors refuse out-of-range indices and reserves refuse bad counts" T-LABEL
   s" WSTORE:BOUNDS" s" bounds" BOUNDS$ 0 ENTRY-CASE
   T-REPORT ;
[then]

;package

WASM-STORAGE-TEST:RUN
false [if] \ off until dot habu-add-the-checks-a49d054d
WASM-STORAGE-TEST:RUN-BOUNDS
[then]
