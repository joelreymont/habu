\ ARENA records come from the real definer and runtime: NAME-NEW's records are
\ zeroed, cell-aligned and distinct, NAME-NEW-DISPOSE's actions run once,
\ newest first, at the arena's reset or release, a public declaration publishes
\ NAME-NEW and keeps its casts private, a HEAP or DYNAMIC-BUFFER element holds
\ an arena handle, and a refused declaration defines none of its four words.
\ Checked code can neither call nor tick the arena runtime words outside the
\ generated words, live and under tools/check.f --verify-only, and no package
\ defines its own `field-project`.
require src/lib/test.f
require src/lib/string.f
require src/lib/arena.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require src/lib/fs-mutate.f
require test/e2e-tree.f

\ The fixture package whose words ARENA-TEST calls from outside it.
package ARENA-FIX
public
STRUCTURE rec DERIVE addr FIELD id n FIELD note ptr u8 FIELD span SPAN:span<u8> ;STRUCTURE
ARENA RECS rec
: AFTER ( -- bool ) true ;
private
ARENA PRECS rec
public
: PRIV-NEW ( ptr ARENA:arena -- ptr rec ) PRECS-NEW ;
;package

package ARENA-TEST

STRUCTURE holder DERIVE addr FIELD arena ptr ARENA:arena FIELD id n ;STRUCTURE
HEAP HOLDERS holder
DYNAMIC-BUFFER HOLDBUF holder

100 constant TAKEN-N
TAKEN-N TYPED-BUFFER SEEN ptr ARENA-FIX:rec

\ What each disposal action saw: the record's id and address, in run order.
4 TYPED-BUFFER LOG-ID n
4 TYPED-BUFFER LOG-AT n
variable LOG-N

TYPED-VARIABLE EVAL-A ptr u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ INCLUDE-EVALUATE ;
: EVAL ( ptr u8 n -- n ) EVAL-U ! EVAL-A ! [: EVAL-RUN ;] catch ;

\ The refusals counted and the throw code when the text is evaluated under the
\ multi-error load state.
: MULTI ( ptr u8 n -- n n ) MULTI-ERR-BEGIN EVAL MULTI-ERR-END ;

\ Whether NAME+SFX is absent from the current wordlist.
: ABSENT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   SB-RESET  name nameu SB-APPEND  sfx sfxu SB-APPEND
   SB$ get-current search-wl 0= TTRUE ;


: NO-WORDS ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu s" -NEW" ABSENT
   name nameu s" -NEW-DISPOSE" ABSENT
   name nameu s" -NEW-PTR" ABSENT
   name nameu s" -NEW-ACT" ABSENT ;


\ `ARENA ovf ptr<spaces>n`: an element type padded until the declaration's
\ text passes the generator's buffer, refused with 7121 while that text is
\ built.
4000 constant OVF-PAD
OVF-PAD 32 + BUFFER: OVF-SRC
variable OVF-U
: OVF, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ OVF-SRC OVF-U @ + c! OVF-U @ 1 + OVF-U ! loop ;


: OVF-TEXT ( -- ptr u8 n )
   0 OVF-U !  s" ARENA ovf ptr" OVF,
   OVF-PAD 0 ?do s"  " OVF, loop
   s" n" OVF,  OVF-SRC OVF-U @ ;


\ TAKEN-N records span three chunks of one arena; a record that moved or
\ overlapped another would have lost its id.
: RECORDS ( -- )
   s" NAME-NEW answers zeroed, cell-aligned, distinct records that keep their fields" T-LABEL
   ARENA:NEW {: a:ptr :}
   TAKEN-N 0 do
      a ARENA-FIX:RECS-NEW
      dup ARENA--FIX-REC:ID @ 0 T=
      dup ARENA--FIX-REC:NOTE @ NULL-PTR = TTRUE
      dup ARENA--FIX-REC:SPAN @ SPAN:LEN 0 T=
      dup NULL-PTR - 1 cells mod 0 T=
      i over ARENA--FIX-REC:ID !
      i SEEN !
   loop
   TAKEN-N 0 do i SEEN @ ARENA--FIX-REC:ID @ i T= loop
   s" a record made in a private section works inside its package" T-LABEL
   a ARENA-FIX:PRIV-NEW ARENA--FIX-REC:ID @ 0 T=
   a ARENA:RELEASE ;


: NOTE ( ptr ARENA-FIX:rec -- )
   {: r:ptr :}
   LOG-N @ {: k:n :}
   r ARENA--FIX-REC:ID @ k LOG-ID !
   r NULL-PTR - k LOG-AT !
   k 1 + LOG-N ! ;


\ The first record is never written, so its action sees it zeroed.
: DISPOSALS ( -- )
   s" NAME-NEW-DISPOSE's actions run once at reset, newest first" T-LABEL
   0 LOG-N !
   ARENA:NEW {: a:ptr :}
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE NULL-PTR - {: at1:n :}
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE {: r2:ptr :}
   2 r2 ARENA--FIX-REC:ID !
   r2 NULL-PTR - {: at2:n :}
   LOG-N @ 0 T=
   a ARENA:RESET
   LOG-N @ 2 T=
   0 LOG-ID @ 2 T=  0 LOG-AT @ at2 T=
   1 LOG-ID @ 0 T=  1 LOG-AT @ at1 T=
   s" and not again at the next reset or at release" T-LABEL
   a ARENA:RESET
   LOG-N @ 2 T=
   s" an action registered after the reset runs at release" T-LABEL
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE {: r3:ptr :}
   3 r3 ARENA--FIX-REC:ID !
   r3 NULL-PTR - {: at3:n :}
   a ARENA:RELEASE
   LOG-N @ 3 T=
   2 LOG-ID @ 3 T=  2 LOG-AT @ at3 T= ;


\ RECORDS and DISPOSALS call ARENA-FIX's public RECS-NEW and RECS-NEW-DISPOSE.
: VISIBILITY ( -- )
   s" a public declaration leaves its package's section public" T-LABEL
   ARENA-FIX:AFTER TTRUE
   s" a private declaration's words are not found from another package" T-LABEL
   s" : P1 ( ptr ARENA:arena -- ptr ARENA-FIX:rec ) ARENA-FIX:PRECS-NEW ;" EVAL CHECKER-REJECT-RC T=
   s" : P2 ( ptr ARENA:arena [ ptr ARENA-FIX:rec -- ] -- ptr ARENA-FIX:rec ) ARENA-FIX:PRECS-NEW-DISPOSE ;" EVAL CHECKER-REJECT-RC T=
   s" nor are either declaration's casts" T-LABEL
   s" : P3 ( ptr u8 -- ptr ARENA-FIX:rec ) ARENA-FIX:RECS-NEW-PTR ;" EVAL CHECKER-REJECT-RC T=
   s" : P4 ( [ ptr ARENA-FIX:rec -- ] -- [ ptr u8 -- ] ) ARENA-FIX:RECS-NEW-ACT ;" EVAL CHECKER-REJECT-RC T=
   s" : P5 ( ptr u8 -- ptr ARENA-FIX:rec ) ARENA-FIX:PRECS-NEW-PTR ;" EVAL CHECKER-REJECT-RC T=
   s" : P6 ( [ ptr ARENA-FIX:rec -- ] -- [ ptr u8 -- ] ) ARENA-FIX:PRECS-NEW-ACT ;" EVAL CHECKER-REJECT-RC T=
   s" P1" get-current search-wl 0= TTRUE ;


\ The handle fetched back is the one stored, and still makes records.
: HANDLES ( -- )
   s" a HEAP element holds an arena handle" T-LABEL
   ARENA:NEW {: a:ptr :}
   HOLDERS-NEW {: h:ptr :}
   a h HOLDER-ARENA !
   h HOLDER-ARENA @ {: b:ptr :}
   b NULL-PTR - a NULL-PTR - T=
   b ARENA-FIX:RECS-NEW ARENA--FIX-REC:ID @ 0 T=
   h HOLDERS-RELEASE
   s" and so does a DYNAMIC-BUFFER element" T-LABEL
   2 HOLDBUF-RESERVE
   a 1 HOLDBUF HOLDER-ARENA !
   1 HOLDBUF HOLDER-ARENA @ {: c:ptr :}
   c NULL-PTR - a NULL-PTR - T=
   c ARENA-FIX:RECS-NEW ARENA--FIX-REC:ID @ 0 T=
   HOLDBUF-RELEASE
   a ARENA:RELEASE ;


\ Whether NAME+SFX (PUB-ABSENT) or each of NAME's four words (PUB-NO-WORDS) is
\ absent from ARENA-TEST's public section; the caller is in its private one.
: PUB-ABSENT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   s" public" EVAL 0 T=  name nameu sfx sfxu ABSENT  s" private" EVAL 0 T= ;
: PUB-NO-WORDS ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   s" public" EVAL 0 T=  name nameu NO-WORDS  s" private" EVAL 0 T= ;


: TAKEN-NEW-DISPOSE ( -- ) ;
: TAKEN2-NEW-PTR ( -- ) ;
\ Each taken in the section other than its ARENA's.
: PUBPTR-NEW-PTR ( -- ) ;
: PUBACT-NEW-ACT ( -- ) ;
: PUBDIS-NEW-DISPOSE ( -- ) ;
public
: PRIVNEW-NEW ( -- ) ;
private


: REFUSALS ( -- )
   s" a refused element type defines none of the four words" T-LABEL
   s" ARENA BAD u16" EVAL CHECKER-REJECT-RC T=
   s" BAD" NO-WORDS
   s" ARENA WORSE no-such-type" EVAL CHECKER-REJECT-RC T=
   s" WORSE" NO-WORDS
   s" a name whose derived word is taken defines none of the others" T-LABEL
   s" ARENA TAKEN n" EVAL E-DUP-DEFINITION T=
   s" TAKEN" s" -NEW" ABSENT  s" TAKEN" s" -NEW-PTR" ABSENT  s" TAKEN" s" -NEW-ACT" ABSENT
   s" ARENA TAKEN2 n" EVAL E-DUP-DEFINITION T=
   s" TAKEN2" s" -NEW" ABSENT  s" TAKEN2" s" -NEW-DISPOSE" ABSENT  s" TAKEN2" s" -NEW-ACT" ABSENT
   s" a declaration refused while it is built defines nothing" T-LABEL
   OVF-TEXT EVAL 7121 T=
   s" OVF" NO-WORDS
   s" ARENA OVF n" EVAL 0 T=
   s" ARENA:NEW dup OVF-NEW drop ARENA:RELEASE" EVAL 0 T=
   s" a refusal counted by a multi-error load defines nothing" T-LABEL
   s" ARENA MBAD u16" MULTI 1 T= 0 T=
   s" MBAD" NO-WORDS
   s" a name whose word is taken in the other section defines none of the others" T-LABEL
   s" public ARENA PUBPTR n" EVAL E-DUP-DEFINITION T=  s" private" EVAL 0 T=
   s" PUBPTR" PUB-NO-WORDS
   s" PUBPTR" s" -NEW" ABSENT  s" PUBPTR" s" -NEW-DISPOSE" ABSENT  s" PUBPTR" s" -NEW-ACT" ABSENT
   s" public ARENA PUBACT n" EVAL E-DUP-DEFINITION T=  s" private" EVAL 0 T=
   s" PUBACT" PUB-NO-WORDS
   s" PUBACT" s" -NEW" ABSENT  s" PUBACT" s" -NEW-DISPOSE" ABSENT  s" PUBACT" s" -NEW-PTR" ABSENT
   s" public ARENA PUBDIS n" EVAL E-DUP-DEFINITION T=  s" private" EVAL 0 T=
   s" PUBDIS" PUB-NO-WORDS
   s" PUBDIS" s" -NEW" ABSENT  s" PUBDIS" s" -NEW-PTR" ABSENT  s" PUBDIS" s" -NEW-ACT" ABSENT
   s" ARENA PRIVNEW n" EVAL E-DUP-DEFINITION T=
   s" PRIVNEW" NO-WORDS
   s" PRIVNEW" s" -NEW-DISPOSE" PUB-ABSENT  s" PRIVNEW" s" -NEW-PTR" PUB-ABSENT
   s" PRIVNEW" s" -NEW-ACT" PUB-ABSENT
   s" a qualified name defines none of the four words" T-LABEL
   s" ARENA ARENA-TEST:QUAL n" EVAL CHECKER-REJECT-RC T=
   s" QUAL" NO-WORDS  s" QUAL" PUB-NO-WORDS ;


\ The refusals' stderr lines are the boundary under test, and a top-level
\ declaration needs a file outside this package, so they run in a child.
: CHILD ( -- )
   s" test/arena-live.f" E2E-TREE:ARGS
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ E2E-TREE:RUN-ON ;
\ STRUCTURE's accessors bind the engine's projection: a package's own
\ `field-project` is refused at its definition.
: CAPTURE-CHILD ( -- )
   s" no package defines its own field-project" T-LABEL
   E2E-TREE:ERR$ s" reserved word cannot be a definition name: field-project" COUNT-SUB 1 T=
   E2E-TREE:OUT$ S\" struct-capture: 70\n" CONTAINS? TTRUE ;


\ tools/check.f --verify-only checks a file without running it, so it gets its
\ own child over a scratch tree's copy of HEAP's runtime words called in a body,
\ a quotation and ticked at top level, each refusal reported as its own record.
\ The pre-pass judges no top-level token after one it refuses, so the tick
\ comes last.
: VERIFY-SRC$ ( -- ptr u8 n )
   S\" : R1 ( -- ptr n ) 16 DYNAMIC-STORAGE:RECORD-NEW ;\n: R2 ( ptr n -- ) 16 DYNAMIC-STORAGE:RECORD-RELEASE ;\n: R3 ( -- ptr n ) 16 [: DYNAMIC-STORAGE:RECORD-NEW ;] execute ;\n' DYNAMIC-STORAGE:RECORD-RELEASE drop\n" ;

: VERIFY-CALLS ( -- )
   s" tools/check.f --verify-only refuses the record runtime calls, at top level too" T-LABEL
   s" arena-verify" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/arena-calls.f" E2E-TREE:AT-A VERIFY-SRC$ FS:WRITE-ALL
   s" tools/check.f" E2E-TREE:ARGS
   s" --" >LEN PROC-ARGV+
   s" --verify-only" >LEN PROC-ARGV+
   s" test/arena-calls.f" >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON
   E2E-TREE:ERR$ S\" \"code\":\"E-ARENA-CALL\"" COUNT-SUB 4 T=
   \ The top-level tick's token is at line 4, column 3.
   E2E-TREE:ERR$ S\" \"line\":4,\"column\":3,\"" COUNT-SUB 1 T=
   E2E-TREE:RC@ 0 <> TTRUE ;

: RUN ( -- )
   T-RESET
   CHILD RECORDS DISPOSALS VISIBILITY HANDLES REFUSALS CAPTURE-CHILD
   VERIFY-CALLS ;

RUN

public

\ Run at top level, where no package is open.
: TOP-LEVEL ( -- )
   s" a declaration outside any package defines none of the four words" T-LABEL
   s" ARENA TOP n" EVAL CHECKER-REJECT-RC T=
   s" TOP" NO-WORDS ;

;package

ARENA-TEST:TOP-LEVEL
T-REPORT
