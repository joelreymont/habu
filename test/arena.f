\ ARENA records come from the real definer and runtime: NAME-NEW's records are
\ zeroed, cell-aligned and distinct, NAME-NEW-DISPOSE's actions run once,
\ newest first, at the arena's reset or release, a public declaration publishes
\ NAME-NEW, a HEAP or DYNAMIC-BUFFER element holds an arena handle, and a
\ refused declaration defines neither of its two words.
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
STRUCTURE rec DERIVE addr FIELD id n FIELD note ^u8 FIELD span SPAN:span<u8> ;STRUCTURE
ARENA RECS rec
: AFTER ( -- bool ) true ;
private
ARENA PRECS rec
\ Taken in this private section for the qualified declaration TOP-LEVEL makes.
: TOP-TAKEN-NEW ( -- ) ;
public
: PRIV-NEW ( ^ARENA:arena -- ^rec ) PRECS-NEW ;
;package

\ The declaring package spells `drop` and `swap` before its ARENA; the words
\ ARENA makes still call the engine's.
package ARENA-CAP
: drop ( n -- n ) ;
: swap ( n n -- n n ) ;
ARENA CRECS n
undefine drop
undefine swap
variable HITS
public
: GO ( -- n n n )
   0 HITS !
   ARENA:NEW {: a:^ :}
   a CRECS-NEW {: r:^ :}
   7 r !
   a [: drop 1 HITS +! ;] CRECS-NEW-DISPOSE {: s:^ :}
   r @  s @  a ARENA:RELEASE  HITS @ ;
;package

package ARENA-TEST

STRUCTURE holder DERIVE addr FIELD arena ^ARENA:arena FIELD id n ;STRUCTURE
HEAP HOLDERS holder
DYNAMIC-BUFFER HOLDBUF holder

100 constant TAKEN-N
TAKEN-N TYPED-BUFFER SEEN ^ARENA-FIX:rec
TAKEN-N BUFFER: SPAN-BUF

\ What each disposal action saw: the record's id and address, in run order.
4 TYPED-BUFFER LOG-ID n
4 TYPED-BUFFER LOG-AT n
variable LOG-N

TYPED-VARIABLE EVAL-A ^u8
variable EVAL-U
: EVAL-RUN ( -- ) EVAL-A @ EVAL-U @ >str evaluate-closed ;
: EVAL ( str -- n ) {: s:str :} s STR:LEN EVAL-U ! s STR:DATA EVAL-A ! [: EVAL-RUN ;] catch ;

\ EVAL with its rejection record unprinted.
: QUIET-EVAL ( str -- n ) 1 REJECTION:QUIET +! EVAL -1 REJECTION:QUIET +! ;

\ The refusals counted and the throw code when the text is evaluated under the
\ multi-error load state.
: MULTI ( str -- n n ) CHECK:MULTI-ERR-BEGIN EVAL CHECK:MULTI-ERR-END ;

: ABSENT ( str str -- )
   {: name:str sfx:str :}
   SB-RESET  name SB-APPEND  sfx SB-APPEND
   SB$ get-current search-wl 0= TTRUE ;


: NO-WORDS ( str -- )
   {: name:str :}
   name s" -NEW" ABSENT
   name s" -NEW-DISPOSE" ABSENT ;


\ TAKEN-N records span three chunks of one arena; a record that moved, or one
\ narrower than `rec`, loses its id or the span of that length in its last field.
: RECORDS ( -- )
   s" NAME-NEW answers zeroed, cell-aligned, distinct records that keep their fields" T-LABEL
   ARENA:NEW {: a:^ :}
   TAKEN-N 0 do
      a ARENA-FIX:RECS-NEW
      dup ARENA--FIX-REC:ID @ 0 T=
      dup ARENA--FIX-REC:NOTE @ NULL-PTR = TTRUE
      dup ARENA--FIX-REC:SPAN @ SPAN:LEN 0 T=
      dup NULL-PTR - 1 cells mod 0 T=
      i over ARENA--FIX-REC:ID !
      i SEEN !
      SPAN-BUF i SPAN:MAKE  i SEEN @ ARENA--FIX-REC:SPAN !
   loop
   TAKEN-N 0 do
      i SEEN @ ARENA--FIX-REC:ID @ i T=
      i SEEN @ ARENA--FIX-REC:SPAN @ SPAN:LEN i T=
   loop
   s" a record made in a private section works inside its package" T-LABEL
   a ARENA-FIX:PRIV-NEW ARENA--FIX-REC:ID @ 0 T=
   a ARENA:RELEASE ;


: NOTE ( ^ARENA-FIX:rec -- )
   {: r:^ :}
   LOG-N @ {: k:n :}
   r ARENA--FIX-REC:ID @ k LOG-ID !
   r NULL-PTR - k LOG-AT !
   k 1 + LOG-N ! ;


\ The first record is never written, so its action sees it zeroed.
: DISPOSALS ( -- )
   s" NAME-NEW-DISPOSE's actions run once at reset, newest first" T-LABEL
   0 LOG-N !
   ARENA:NEW {: a:^ :}
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE NULL-PTR - {: at1:n :}
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE {: r2:^ :}
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
   a [: NOTE ;] ARENA-FIX:RECS-NEW-DISPOSE {: r3:^ :}
   3 r3 ARENA--FIX-REC:ID !
   r3 NULL-PTR - {: at3:n :}
   a ARENA:RELEASE
   LOG-N @ 3 T=
   2 LOG-ID @ 3 T=  2 LOG-AT @ at3 T= ;


: CAPTURE ( -- )
   s" a package's own drop and swap before its ARENA capture neither word" T-LABEL
   ARENA-CAP:GO 1 T= 0 T= 7 T= ;


\ RECORDS and DISPOSALS call ARENA-FIX's public RECS-NEW and RECS-NEW-DISPOSE.
: VISIBILITY ( -- )
   s" a public declaration leaves its package's section public" T-LABEL
   ARENA-FIX:AFTER TTRUE
   s" a private declaration's words are not found from another package" T-LABEL
   s" : P1 ( ^ARENA:arena -- ^ARENA-FIX:rec ) ARENA-FIX:PRECS-NEW ;" EVAL 70 T=
   s" : P2 ( ^ARENA:arena [ ^ARENA-FIX:rec -- ] -- ^ARENA-FIX:rec ) ARENA-FIX:PRECS-NEW-DISPOSE ;" EVAL 70 T=
   s" P1" get-current search-wl 0= TTRUE ;


: HANDLES ( -- )
   s" a HEAP element holds an arena handle" T-LABEL
   ARENA:NEW {: a:^ :}
   HOLDERS-NEW {: h:^ :}
   a h HOLDER-ARENA !
   h HOLDER-ARENA @ {: b:^ :}
   b a = TTRUE
   b ARENA-FIX:RECS-NEW ARENA--FIX-REC:ID @ 0 T=
   h HOLDERS-RELEASE
   s" and so does a DYNAMIC-BUFFER element" T-LABEL
   2 HOLDBUF-RESERVE
   a 1 HOLDBUF HOLDER-ARENA !
   1 HOLDBUF HOLDER-ARENA @ {: c:^ :}
   c a = TTRUE
   c ARENA-FIX:RECS-NEW ARENA--FIX-REC:ID @ 0 T=
   HOLDBUF-RELEASE
   a ARENA:RELEASE ;


\ Whether NAME+SFX (PUB-ABSENT) or each of NAME's two words (PUB-NO-WORDS) is
\ absent from ARENA-TEST's public section; the caller is in its private one.
: PUB-ABSENT ( str str -- )
   {: name:str sfx:str :}
   s" public" EVAL 0 T=  name sfx ABSENT  s" private" EVAL 0 T= ;
: PUB-NO-WORDS ( str -- )
   {: name:str :}
   s" public" EVAL 0 T=  name NO-WORDS  s" private" EVAL 0 T= ;


: TAKEN-NEW-DISPOSE ( -- ) ;
\ Taken in the section other than its ARENA's.
: PUBDIS-NEW-DISPOSE ( -- ) ;
public
: PRIVNEW-NEW ( -- ) ;
private


: REFUSALS ( -- )
   s" a refused element type defines neither of the two words" T-LABEL
   s" ARENA BAD u16" EVAL 70 T=
   s" BAD" NO-WORDS
   s" ARENA WORSE no-such-type" EVAL 70 T=
   s" WORSE" NO-WORDS
   s" an element type ARENA cannot store defines neither word" T-LABEL
   s" ARENA XQ [ -- xt ]" EVAL 70 T=
   s" XQ" NO-WORDS
   s" ARENA XC [ xt -- ]" EVAL 70 T=
   s" XC" NO-WORDS
   s" ARENA XW [ width<n> -- ]" EVAL 70 T=
   s" XW" NO-WORDS
   s" ARENA XV [ -- width<n> ]" EVAL 70 T=
   s" XV" NO-WORDS
   s" ARENA OPENQ [ a -- a ]" EVAL 70 T=
   s" OPENQ" NO-WORDS
   s" a name whose derived word is taken defines neither word" T-LABEL
   s" ARENA TAKEN n" QUIET-EVAL 70 T=
   s" TAKEN" s" -NEW" ABSENT
   s" a refusal counted by a multi-error load defines nothing" T-LABEL
   s" ARENA MBAD u16" MULTI 1 T= 0 T=
   s" MBAD" NO-WORDS
   s" a name whose word is taken in the other section defines neither word" T-LABEL
   s" public ARENA PUBDIS n" QUIET-EVAL 70 T=  s" private" EVAL 0 T=
   s" PUBDIS" PUB-NO-WORDS
   s" PUBDIS" s" -NEW" ABSENT
   s" ARENA PRIVNEW n" QUIET-EVAL 70 T=
   s" PRIVNEW" NO-WORDS
   s" PRIVNEW" s" -NEW-DISPOSE" PUB-ABSENT
   s" a qualified name declares both words in that package's public section" T-LABEL
   s" ARENA ARENA-TEST:QUAL n" EVAL 0 T=
   s" ARENA:NEW dup ARENA-TEST:QUAL-NEW drop ARENA:RELEASE" EVAL 0 T=
   s" : QUAL-USE ( ^ARENA:arena -- ^n ) [: drop ;] ARENA-TEST:QUAL-NEW-DISPOSE ;"
   EVAL 0 T=
   s" and refuses one whose word is taken in that package's private section" T-LABEL
   s" public ARENA ARENA-TEST:TAKEN n" QUIET-EVAL 70 T=  s" private" EVAL 0 T=
   s" ARENA ARENA-TEST:PUBDIS n" QUIET-EVAL 70 T=
   s" ARENA ARENA-FIX:PRECS n" QUIET-EVAL 70 T= ;


: VERIFY-USES-SRC$ ( -- str )
   S\" require src/lib/arena.f\npackage Q public\nSTRUCTURE rec DERIVE addr FIELD id n ;STRUCTURE\nARENA X rec\n: INSIDE ( ^ARENA:arena -- ^rec ) [: drop ;] X-NEW-DISPOSE ;\n;package\npackage R public\n: U ( ^ARENA:arena -- ^Q:rec ) dup Q:X-NEW drop [: drop ;] Q:X-NEW-DISPOSE ;\n;package\npackage P\nprivate\nARENA X n\npublic\n: MK ( ^ARENA:arena -- ^n ) X-NEW ;\n;package\n" ;

: VERIFY-WRONG-SRC$ ( -- str )
   S\" require src/lib/arena.f\npackage Q public\nARENA X n\n;package\npackage R public\n: U ( ^ARENA:arena -- ^u8 ) Q:X-NEW ;\n: V ( ^ARENA:arena -- ^n ) [: c@ drop ;] Q:X-NEW-DISPOSE ;\n;package\n" ;

: VERIFY-TAKEN-SRC$ ( -- str )
   S\" require src/lib/arena.f\npackage Q\nprivate\n: X-NEW ( -- ) ;\n;package\nARENA Q:X n\n" ;

: VERIFY-QUAL-SRC$ ( -- str )
   S\" require src/lib/arena.f\npackage Q public\n;package\nARENA Q:Y n\npackage R public\n: U ( ^ARENA:arena -- ^n ) Q:Y-NEW ;\n;package\n" ;

: VERIFY-FILE ( str -- )
   {: rel:str :}
   s" tools/check.f" E2E-TREE:ARGS
   s" --" PROC-ARGV+
   s" --verify-only" PROC-ARGV+
   s" --json-errors" PROC-ARGV+
   rel PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ E2E-TREE:ROOT$ E2E-TREE:RUN-ON ;


: VERIFY-USES ( -- )
   s" tools/check.f --verify-only admits an ARENA's words called as declared" T-LABEL
   s" arena-uses" E2E-TREE:TREE
   E2E-TREE:ROOT$ FS:CLEANUP-TREE+
   s" test/arena-uses.f" E2E-TREE:AT-A VERIFY-USES-SRC$ FS:WRITE-ALL
   s" test/arena-wrong.f" E2E-TREE:AT-A VERIFY-WRONG-SRC$ FS:WRITE-ALL
   s" test/arena-taken.f" E2E-TREE:AT-A VERIFY-TAKEN-SRC$ FS:WRITE-ALL
   s" test/arena-qual.f" E2E-TREE:AT-A VERIFY-QUAL-SRC$ FS:WRITE-ALL
   s" test/arena-uses.f" VERIFY-FILE
   E2E-TREE:OUT$ s" deferred to the run" STR:CONTAINS? 0= TTRUE
   E2E-TREE:ERR$ STR:LEN 0 T=
   E2E-TREE:RC@ 0 T=
   s" and refuses each called with the wrong effect" T-LABEL
   s" test/arena-wrong.f" VERIFY-FILE
   E2E-TREE:ERR$ S\" \"code\":\"" STR:COUNT-SUB 2 T=
   E2E-TREE:ERR$ S\" \"code\":\"E-MISMATCH\"" STR:COUNT-SUB 2 T=
   E2E-TREE:ERR$ S\" \"token\":\"Q:X-NEW\"," STR:COUNT-SUB 1 T=
   E2E-TREE:ERR$ S\" \"token\":\"Q:X-NEW-DISPOSE\"," STR:COUNT-SUB 1 T=
   E2E-TREE:RC@ 0 <> TTRUE
   s" and rejects a qualified one whose word is taken in that package's private section" T-LABEL
   s" test/arena-taken.f" VERIFY-FILE
   E2E-TREE:ERR$ S\" \"code\":\"" STR:COUNT-SUB 1 T=
   E2E-TREE:ERR$ S\" \"code\":\"E-DUPLICATE-DEFINITION\"" STR:COUNT-SUB 1 T=
   E2E-TREE:ERR$ S\" \"token\":\"Q:X\"," STR:COUNT-SUB 1 T=
   E2E-TREE:RC@ 0 <> TTRUE
   s" and admits a qualified one whose words are free, called as declared" T-LABEL
   s" test/arena-qual.f" VERIFY-FILE
   E2E-TREE:OUT$ s" deferred to the run" STR:CONTAINS? 0= TTRUE
   E2E-TREE:ERR$ STR:LEN 0 T=
   E2E-TREE:RC@ 0 T= ;

: RUN ( -- )
   T-RESET
   RECORDS DISPOSALS CAPTURE VISIBILITY HANDLES REFUSALS VERIFY-USES ;

RUN

public

\ Run at top level, where no package is open.
: TOP-LEVEL ( -- )
   s" a declaration outside any package defines both words" T-LABEL
   s" ARENA TOP n" EVAL 0 T=
   s" ARENA:NEW dup TOP-NEW drop ARENA:RELEASE" EVAL 0 T=
   s" : TOP-USE ( ^ARENA:arena -- ^n ) [: drop ;] TOP-NEW-DISPOSE ;" EVAL 0 T=
   s" and a qualified one whose word is taken in that package's private section is refused" T-LABEL
   s" ARENA ARENA-FIX:TOP-TAKEN n" QUIET-EVAL 70 T= ;

;package

ARENA-TEST:TOP-LEVEL
T-REPORT
