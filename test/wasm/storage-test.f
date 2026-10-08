\ storage-test.f - test/wasm/storage.f's entries, each built into a module by
\ tools/wasm-build.f's command line, validated by wasm-tools and run by bun
\ (test/wasm/harness.f); each module's output must be its entry's lines and its
\ status the entry's: 0, or a trap for a page freed twice. Both tools must be on
\ PATH, so this is no row of the ordinary gate: test/wasm/device.f runs it, as
\ does `bin/hb --load test/wasm/storage-test.f` from the tree's root. The
\ modules, the expected and the actual output stay in the printed directory.

require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/test.f
require test/wasm/harness.f

package WASM-STORAGE-TEST
private

60000 constant DEADLINE-MS
2 constant TRAPPED                     \ test/wasm/run.mjs's status for a trap
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: MOD
variable MOD-U
FS-PATH-CAP BUFFER: FILE
variable FILE-U

: SETUP ( -- )
   s" wasm-storage" HB-TMP-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY  u DIR-U !
   s" wasm storage: modules and output in " type  DIR u type cr ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ The bytes written as the file name in DIR.
: FILE! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n name:ptr nu:n :}
   DIR DIR-U @ name nu FILE JOIN-PATH FILE-U !
   FILE FILE-U @ a u WRITE-ALL ;

\ storage.f built into the module file name in DIR, whose run calls entry;
\ answers the driver's status.
: BUILD ( ptr u8 n ptr u8 n -- n )
   {: entry:ptr eu:n name:ptr nu:n :}
   DIR DIR-U @ name nu MOD JOIN-PATH MOD-U !
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/wasm-build.f" ARG+  s" --" ARG+
   s" test/wasm/storage.f" ARG+  entry eu ARG+  MOD MOD-U @ ARG+
   s" bin/hb" >LEN  OUT CAP >LEN  ERR CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if  ERR erru LEN>N type cr  then
   rc ;

\ The entry's module, the file name with .wasm, builds, validates and runs to
\ status, printing want; want and the output stay as name-expected.txt and
\ name-actual.txt.
: ENTRY-CASE ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: entry:ptr eu:n name:ptr nu:n want:ptr wu:n status:n :}
   SB-RESET name nu SB-APPEND s" -expected.txt" SB-APPEND
   want wu SB$ FILE!
   SB-RESET name nu SB-APPEND s" .wasm" SB-APPEND
   entry eu SB$ BUILD 0 T=
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   MOD MOD-U @ WASM-HARNESS:RUN status T=
   SB-RESET name nu SB-APPEND s" -actual.txt" SB-APPEND
   WASM-HARNESS:OUT$ SB$ FILE!
   WASM-HARNESS:OUT$ want wu T$= ;

\ GROW's lines, REFUSE's and REUSE's.
: BUFFER$ ( -- ptr u8 n )
   S\" 7122\n77\n88\n312\n1\n7121\n7122\n7121\n7138\n7138\n77\n88\n7122\n7122\n1\n0\n0\n" ;

: HOLES$ ( -- ptr u8 n )
   S\" 1\n0\n1\n1\n" ;

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
   s" one buffer reserves, grows across a page, refuses, releases and is reused cleared" T-LABEL
   s" WSTORE:BUFFER" s" buffer" BUFFER$ 0 ENTRY-CASE
   s" freed neighbours join into one run, which a smaller request splits" T-LABEL
   s" WSTORE:HOLES" s" holes" HOLES$ 0 ENTRY-CASE
   s" a page freed twice traps" T-LABEL
   s" WSTORE:TWICE" s" twice" TWICE$ TRAPPED ENTRY-CASE
   s" a page freed again inside the run it joined traps" T-LABEL
   s" WSTORE:INSIDE" s" inside" INSIDE$ TRAPPED ENTRY-CASE
   s" a zero length, an unaligned address, a negative length, a length past 2^32 and an address past 2^32 are refused" T-LABEL
   s" WSTORE:REFUSED" s" refused" REFUSED$ 0 ENTRY-CASE
   T-REPORT ;

;package

WASM-STORAGE-TEST:RUN
