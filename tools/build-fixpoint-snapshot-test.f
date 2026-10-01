\ build-fixpoint-snapshot-test.f - checked fixture for tools/build-fixpoint.f:
\ the snapshot the refresh saves, with its trailer doctored until the loader
\ refuses it by name. The build and the cases that read its output are
\ tools/build-fixpoint-test.f; this is a gate row of its own, because one row
\ running every build-fixpoint case took 293-338 s in the gate's pool, and this
\ case alone is one native build and snapshot.
\ Run: bin/hb --load tools/build-fixpoint-snapshot-test.f

require tools/build-fixpoint-test-lib.f
require src/habu/snapshot-format.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

\ The snapshot trailer's size and field offsets are owned by src/habu/layout.f
\ (SNAP-TRL-BYTES, SNAP-TRL-NDICT, SNAP-TRL-REGLEN, SNAP-TRL-VERSION) and
\ src/habu/snapshot-format.f (HEAP-FIELD); the writer, the loader and this
\ fixture all read them from there, so a format change cannot leave one side
\ addressing the wrong cells.

TYPED-VARIABLE BFT-BYTES-A ptr u8
variable BFT-BYTES-N

\ Corrupt the current native snapshot's trailer and assert the loader's named
\ version and bounds refusals. Re-sign mutations so macOS reaches the loader.
: BFT-BYTES ( -- ptr u8 )
   BFT-BYTES-A @ ;

: BFT-SNAP0-BUILD ( -- )
   BF-BUILD-SNAP-FRESH
   s" hb-snap0" BF-REMOVE-TMP
   s" hb-new" s" hb-snap0" BF-RENAME-TMP ;

: BFT-EMPTY-STDIN! ( -- )
   s" empty-stdin" BF-A$ BFT-EMPTY$ WRITE-ALL ;

: BFT-SNAP-RUN ( ptr u8 n -- n )
   s" empty-stdin" BF-RUN-ENV-TMP-INFILE ;

: BFT-BYTES-READ ( -- )
   s" hb-snap0" BF-A$ FILE-SIZE {: sz:n :}
   sz MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BYTES-A !
   s" hb-snap0" BF-A$ BFT-BYTES sz READ-ALL BFT-BYTES-N ! ;

: BFT-BYTE@ ( n -- n ) {: off:n :}
   BFT-BYTES off BYTE+ c@ ;

: BFT-BYTE! ( n n -- ) {: val:n off:n :}
   val BFT-BYTES off BYTE+ c! ;

: U64@ ( n -- n ) {: off:n :}
   0
   8 0 ?do
      off i + BFT-BYTE@ i 8 * lshift or
   loop ;

: TRAILER-OFF ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - ;

: BFT-DOCTOR-WRITE ( -- )
   s" hb-doctored" BF-REMOVE-TMP
   s" hb-doctored" BF-A$ BFT-BYTES BFT-BYTES-N @ WRITE-ALL
   s" hb-doctored" BF-CODESIGN-FORCE-TMP
   s" hb-doctored" BF-CHMOD-X-TMP ;

variable BFT-DOC-ERR-U
variable BFT-DOC-OUT-U
variable BFT-DOC-EXITED
variable BFT-DOC-CODE

: BFT-DOC-ERR$ ( -- ptr u8 n )
   BFT-ERR BFT-DOC-ERR-U @ ;

: BFT-DOC-OUT$ ( -- ptr u8 n )
   BFT-OUT BFT-DOC-OUT-U @ ;

\ Doctor one trailer byte, run the patched snapshot engine with empty stdin and
\ its stderr CAPTURED (the labeled diagnostic goes to fd 2), record the exit
\ kind/code and stderr length, then restore the byte for the next case.
: BFT-DOCTORED-CAPTURE ( n n -- ) {: off:n val:n :}
   off BFT-BYTE@ {: orig:n :}
   val off BFT-BYTE!
   BFT-DOCTOR-WRITE
   PROC-ARGV-RESET
   s" hb-doctored" BF-A$ >LEN  BFT-EMPTY$ >LEN
   BFT-OUT BFT-CAPTURE-CAP >LEN  BFT-ERR BFT-CAPTURE-CAP >LEN  BFT-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   MATCH outcome
     exited OF BFT-DOC-CODE ! 0 0= BFT-DOC-EXITED ! ENDOF
     signaled OF BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
     timeout OF E-PROC-TIMEOUT throw ENDOF        \ a deadline, for BFT-STEP
   ;MATCH {: ou:len eu:len :}
   ou LEN>N BFT-DOC-OUT-U !
   eu LEN>N BFT-DOC-ERR-U !
   orig off BFT-BYTE! ;

\ A labeled fatal exit: process EXITed with the contract code and its stderr
\ carries the named diagnostic (proves the exit is no longer a bare rc-only).
: BFT-ASSERT-SNAP-EXIT ( n ptr u8 n -- ) {: code:n msg:ptr msgu:n :}
   BFT-DOC-EXITED @ TTRUE
   BFT-DOC-CODE @ code T=
   BFT-DOC-ERR$ msg msgu CONTAINS? TTRUE ;

: TEST-TRAILER ( -- )
   BFT-ROOT BF-TMP!
   BFT-SNAP0-BUILD
   BFT-EMPTY-STDIN!
   s" hb-snap0" BFT-SNAP-RUN 0 T=
   s" hb-snap0" BF-A$ s" lib/prelude.f" BF-RUN-LOAD-STAGE 0 T=
   BFT-BYTES-READ
   TRAILER-OFF {: tr:n :}
   tr SNAP-TRL-VERSION + BFT-BYTE@ SNAPSHOT-FORMAT:VERSION T=
   tr SNAP-TRL-VERSION + 2 BFT-DOCTORED-CAPTURE
   80 s" hb: snapshot format version unsupported" BFT-ASSERT-SNAP-EXIT
   tr SNAP-TRL-VERSION + 9 BFT-DOCTORED-CAPTURE
   80 s" hb: snapshot format version unsupported" BFT-ASSERT-SNAP-EXIT
   tr SNAP-TRL-VERSION + $FF BFT-DOCTORED-CAPTURE
   80 s" hb: snapshot format version unsupported" BFT-ASSERT-SNAP-EXIT
   \ +4/+3: a MIDDLE byte of the 8-byte field keeps the value positive but
   \ far above REGION/DICT-CAP (top bytes could go negative or SIGSEGV).
   tr SNAP-TRL-REGLEN + 4 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   tr SNAP-TRL-NDICT + 3 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   \ The heap's form is raw or grid; the next value names no form.
   tr SNAPSHOT-FORMAT:HEAP-FIELD + SNAPSHOT-FORMAT:HEAP-GRID 1+ BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   BF-TMP-RESET ;

\ Public so the driver below runs with the package CLOSED, as a gate row must.
public
: BFT-SNAPSHOT-RUN ( -- )
   T-RESET
   BFT-PREPARE
   s" snap trailer" [: TEST-TRAILER ;] BFT-STEP
   s" build-fixpoint-snapshot-test: ok" BFT-FINISH ;

;package

BUILD-FIXPOINT:BFT-SNAPSHOT-RUN
