\ storage-test.f - test/wasm/storage.f's entries, each the MAIN of a module
\ `hb-build --target wasm` builds, validated by wasm-tools and run by bun
\ (test/wasm/harness.f); each module's output must be its entry's lines and its
\ status the entry's: 0, or a trap for a page freed twice. Both tools must be on
\ PATH, so test/wasm/device.f runs it, not the ordinary gate. The sources, the
\ modules, the expected and the actual output stay in the printed directory.

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
   s" wasm-storage" FS:MKTEMP-DIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm storage: modules and output in " type  DIR u type cr ;

: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE FS:JOIN-PATH FILE-U !
   FILE FILE-U @ a u FS:WRITE-ALL ;

\ The source name.f in DIR, whose MAIN is storage.f's entry, built into
\ name.wasm there; answers the build's status.
: BUILD ( ptr u8 n ptr u8 n -- n )
   {: entry:ptr eu:n name:ptr nu:n :}
   SB-RESET name nu SB-APPEND s" .wasm" SB-APPEND
   DIR DIR-U @ SB$ MOD FS:JOIN-PATH MOD-U !
   SB-RESET name nu SB-APPEND s" .f" SB-APPEND
   DIR DIR-U @ SB$ SRC FS:JOIN-PATH SRC-U !
   SB-RESET S\" require test/wasm/storage.f\n: MAIN ( -- ) " SB-APPEND
   entry eu SB-APPEND  S\"  ;\n" SB-APPEND
   SRC SRC-U @ SB$ FS:WRITE-ALL
   SRC SRC-U @ MOD MOD-U @ WASM-HARNESS:BUILD ;

\ The entry's module, the file name with .wasm, builds, validates and runs to
\ status, printing want; want and the output stay as name-expected.txt and
\ name-actual.txt.
: ENTRY-CASE ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: entry:ptr eu:n name:ptr nu:n want:ptr wu:n status:n :}
   SB-RESET name nu SB-APPEND s" -expected.txt" SB-APPEND
   want wu SB$ FILE!
   entry eu name nu BUILD 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ WASM-HARNESS:RUN status T=
   SB-RESET name nu SB-APPEND s" -actual.txt" SB-APPEND
   WASM-HARNESS:OUT$ SB$ FILE!
   WASM-HARNESS:OUT$ want wu T$= ;

\ GROW's lines, REFUSE's and REUSE's.
: BUFFER$ ( -- ptr u8 n )
   S\" 77\n88\n312\n1\n7138\n7138\n77\n88\n1\n0\n0\n" ;

: HOLES$ ( -- ptr u8 n )
   S\" 1\n0\n1\n1\n" ;

: FRACTIONAL$ ( -- ptr u8 n )
   S\" 7\n" ;

\ Each map's and free's ior, and INSIDE's pages' distance, up to the trap.
: TWICE$ ( -- ptr u8 n )
   S\" 0\n0\n" ;

: INSIDE$ ( -- ptr u8 n )
   S\" 0\n0\n65536\n0\n0\n" ;

\ The map's ior, the five refused frees', the free's, the map back's, that it
\ handed out the same page, and the last free's.
: REFUSED$ ( -- ptr u8 n )
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
: BOUNDS$ ( -- ptr u8 n )
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
