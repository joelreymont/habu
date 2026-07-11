\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself.
\
\ Invariant (dot habu-silent-exit-70-215b2da7): every load leg that rejects a
\ definition exits 70 WITH a named diagnostic on stderr — a silent fail-closed
\ exit is the harness gap this campaign keeps paying for (cf. the seal-exit
\ labeling and the check.f E-FS-CAPACITY raw-67 fix). Pins the direct --load
\ leg, the require-chain leg, and the checked-body reject leg by spawning the
\ engine on generated rejecting fixtures and asserting stderr names the error.
\ The child engine is HABU_UNDER_TEST when the gate sets it, else bin/hb.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/load-reject-diag-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

$1000 constant LRD-CAP
60000 constant LRD-TIMEOUT-MS
70 constant LRD-REJECT-RC            \ the checked-reject exit class

create LRD-OUT LRD-CAP allot
create LRD-ERR LRD-CAP allot
create LRD-EMPTY 1 allot             \ zero-length stdin
create LRD-BASE FS-PATH-CAP allot
create LRD-UNDEF-PATH FS-PATH-CAP allot
create LRD-BODY-PATH FS-PATH-CAP allot
create LRD-OUTER-PATH FS-PATH-CAP allot

variable LRD-BASE-U
variable LRD-UNDEF-U
variable LRD-BODY-U
variable LRD-OUTER-U
variable LRD-OUT-U
variable LRD-ERR-U
variable LRD-KIND
variable LRD-RC

: LRD-BASE$ ( -- ptr u8 n )
   LRD-BASE LRD-BASE-U @ ;

: LRD-UNDEF$ ( -- ptr u8 n )
   LRD-UNDEF-PATH LRD-UNDEF-U @ ;

: LRD-BODY$ ( -- ptr u8 n )
   LRD-BODY-PATH LRD-BODY-U @ ;

: LRD-OUTER$ ( -- ptr u8 n )
   LRD-OUTER-PATH LRD-OUTER-U @ ;

\ Resolve the child engine (the gate-runner-entry-test pattern).
: LRD-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: LRD-SETUP ( -- )
   CLEANUP-RESET
   s" habu-load-reject-diag" TMPDIR-MKDIR {: a:ptr u:n :}
   a LRD-BASE u BYTE-COPY
   u LRD-BASE-U !
   LRD-BASE$ CLEANUP-TREE+
   LRD-BASE$ s" undef.f" LRD-UNDEF-PATH JOIN-PATH LRD-UNDEF-U !
   LRD-BASE$ s" body.f" LRD-BODY-PATH JOIN-PATH LRD-BODY-U !
   LRD-BASE$ s" outer.f" LRD-OUTER-PATH JOIN-PATH LRD-OUTER-U !
   LRD-UNDEF$ s" : LRD-X ( -- ) LRD-NO-SUCH-WORD-XYZ ;" WRITE-ALL
   LRD-BODY$ s" : LRD-Y ( n -- n ) drop ;" WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   LRD-UNDEF$ SB-APPEND
   LRD-OUTER$ SB$ WRITE-ALL ;

: LRD-STORE! ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
   kind LRD-KIND !  code LRD-RC !
   erru LEN>N LRD-ERR-U !  outu LEN>N LRD-OUT-U ! ;

\ Spawn `<hb> --load <fixture>` with empty stdin, capture the exit outcome.
: LRD-RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   LRD-HB$ >LEN  LRD-EMPTY 0 >LEN  LRD-OUT LRD-CAP >LEN
   LRD-ERR LRD-CAP >LEN  LRD-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   LRD-STORE! ;

: LRD-ERR$ ( -- ptr u8 n )
   LRD-ERR LRD-ERR-U @ ;

\ Every rejecting load: exit-kind EXIT, rc 70, EMPTY stdout, NON-EMPTY stderr
\ that names the failing token — the silent-exit-70 red/green discriminator.
: LRD-ASSERT-NAMED ( ptr u8 n -- ) {: name:ptr nameu:n :}
   LRD-KIND @ PROC-OUTCOME-EXIT T=
   LRD-RC @ LRD-REJECT-RC T=
   LRD-OUT-U @ 0 T=
   LRD-ERR-U @ 0 > TTRUE
   LRD-ERR$ name nameu CONTAINS? TTRUE ;

: LRD-TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word" T-LABEL
   LRD-UNDEF$ LRD-RUN
   s" E-UNDEFINED" LRD-ASSERT-NAMED
   LRD-ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: LRD-TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   LRD-BODY$ LRD-RUN
   s" in lrd-y" LRD-ASSERT-NAMED
   LRD-ERR$ s" at 'drop'" CONTAINS? TTRUE ;

: LRD-TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   LRD-OUTER$ LRD-RUN
   s" E-UNDEFINED" LRD-ASSERT-NAMED
   LRD-ERR$ s" LRD-NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: LRD-MAIN ( -- )
   T-RESET
   LRD-SETUP
   LRD-TEST-UNDEF
   LRD-TEST-BODY
   LRD-TEST-REQUIRE-CHAIN
   CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

LRD-MAIN
