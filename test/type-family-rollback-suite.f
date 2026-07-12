\ type-family-rollback-suite.f — transactional rollback-frame behavior suite for
\ the checker's depth-safe candidate/scope rollback (PLAN.md item 3). Run BY THE
\ ENGINE over stdin, exactly like test/type-family-suite.f (rollback + registry
\ words resolve only at top-level interpret, never inside a checked ':' body):
\     bin/hb < test/type-family-rollback-suite.f
\ Every rollback op (CHECKER-SCOPE-START/DONE, CHECK-CANDIDATE-START/DONE) is a
\ top-level interpret line so the checker-internal state stays in scope. A failure
\ prints F<index> + detail; REPORT exits 1 on any fail.

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;
: T$= ( ptr u8 n ptr u8 n -- ) {: ga:ptr gu:n wa:ptr wu:n :}
   #CASE @ 1 + #CASE !
   gu wu <> if
      T-FAIL s" assert string len: expected " type wu . s" got " type gu . cr exit
   then
   0 begin dup gu < while
      dup ga + c@  over wa + c@ <> if
         drop T-FAIL s" assert string byte mismatch" type cr exit
      then
      1+
   repeat drop ;

\ scratch cells (all rollback query values are stashed before compare).
variable FOUNDF
\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): checker-internal colon
\ words probed at top level go through named trusted shims.
TRUSTED: TWX-CAND-START ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: TWX-CAND-DONE ( n -- n ) CHECK-CANDIDATE-DONE ;
TRUSTED: TWX-FIND-DEFER ( ptr u8 n -- bool ) CHECKER-FIND-ACTIVE-DEFER ;
TRUSTED: TWX-FIND-USIG ( ptr u8 n -- bool ) CHECKER-FIND-USIG ;
TRUSTED: TWX-USIG-ADD ( ptr u8 n ptr u8 n -- ) CHECKER-USIG-ADD ;
TRUSTED: TWX-CTL-FLAGS ( ptr u8 n -- n ) CTL-FLAGS ;
TRUSTED: TWX-TFAM-RESET ( -- ) TFAM-RESET ;
TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-SUMV-ADD ( n ptr u8 n n n n n -- n ) SUMV-ADD ;
TRUSTED: TWX-TFAM-FIND-IN ( ptr u8 n ptr u8 n -- n bool ) TFAM-FIND-IN ;
TRUSTED: TWX-SCHEMA-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: TWX-SCHEMA-CON ( n -- n ) SCHEMA-CON ;
TRUSTED: TWX-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: TWX-LAY-N@ ( -- n ) LAY-N@ ;
TRUSTED: TWX-LAY-ADD ( n n n n n -- n ) LAY-ADD ;
TRUSTED: TWX-PF-ADD ( n ptr u8 n n n -- n ) PF-ADD ;

   variable TC
variable P-TFAM   variable P-SUMV   variable P-PF   variable P-LAY
variable P-SCHN   variable P-SCHR   variable P-STRU  variable P-PKN
variable P-SYMN   variable P-SYMU   variable P-DEPTH
variable A-TFAM   variable A-SYMN   variable B-TFAM  variable B-SYMN

\ clean TFAM/SCHEMA slate (SYM/USIG/DFER/package remain live checker state).
TWX-TFAM-RESET
TWX-SCHEMA-RESET

\ ---------------------------------------------------------------------------
\ 1. GENERIC candidate rollback: mutate EVERY registry class inside a candidate,
\    reject it, and prove every high-water mark is restored, finds miss, and a
\    same-name re-add succeeds cleanly (string pool + counters rewound).
\ ---------------------------------------------------------------------------
\ seed one pre-existing family so restore is to a nonzero baseline (not just 0).
s" rbpre" CHECKER-PACKAGE-PUBLIC s" base" 1 TK-SUM TWX-TFAM-DECL drop
s" rbpre" s" base" 0 0 0 0 TWX-SUMV-ADD drop
0 TWX-SCHEMA-PARAM drop

TFAM-N@       P-TFAM !
SUMV-N@       P-SUMV !
PF-N@         P-PF !
TWX-LAY-N@        P-LAY !
SCHEMA-N@     P-SCHN !
SCHEMA-ROOT-N@ P-SCHR !
TF-STR-U @    P-STRU !
TF-PK-N @     P-PKN !

TWX-CAND-START
   s" rbc" CHECKER-PACKAGE-PRIVATE s" cand" 2 TK-PRODUCT TWX-TFAM-DECL drop
   s" rbc" s" cand" 0 0 0 0 TWX-SUMV-ADD drop
   \ a product field + layout keyed on the just-added candidate family.
   s" rbc" s" cand" TWX-TFAM-FIND-IN FOUNDF ! drop
   FOUNDF @ s" fld" 0 0 TWX-PF-ADD drop
   FOUNDF @ TL-BOXED 8 8 8 TWX-LAY-ADD drop
   1 TWX-SCHEMA-CON drop   2 TWX-SCHEMA-PARAM drop
   0 TWX-SCHEMA-PARAM TWX-SCHEMA-ROOT+ drop
   \ found INSIDE the candidate.
   s" rbc" s" cand" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
0 TWX-CAND-DONE drop

\ every counter restored to the pre-candidate baseline.
TFAM-N@        P-TFAM @ T=
SUMV-N@        P-SUMV @ T=
PF-N@          P-PF @ T=
TWX-LAY-N@         P-LAY @ T=
SCHEMA-N@      P-SCHN @ T=
SCHEMA-ROOT-N@ P-SCHR @ T=
TF-STR-U @     P-STRU @ T=
TF-PK-N @      P-PKN @ T=
\ the candidate family cannot be found post-rollback.
s" rbc" s" cand" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=
\ re-adding the exact same (package,tail) after rollback succeeds (no leaked dup).
s" rbc" CHECKER-PACKAGE-PRIVATE s" cand" 2 TK-PRODUCT ' TWX-TFAM-DECL catch  TC !  drop
TC @ 0 T=
\ and the seeded baseline family is untouched.
s" rbpre" s" base" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
\ reset the extra re-added family so later baselines stay clean.
TWX-TFAM-RESET TWX-SCHEMA-RESET
s" rbpre" CHECKER-PACKAGE-PUBLIC s" base" 1 TK-SUM TWX-TFAM-DECL drop

\ ---------------------------------------------------------------------------
\ 2. SCOPE rollback (rejecting scoped load path): CHECKER-SCOPE-START/DONE roll
\    back the same registry classes as a candidate.
\ ---------------------------------------------------------------------------
TFAM-N@ P-TFAM !   SUMV-N@ P-SUMV !
CHECKER-SCOPE-START
   s" rbs" CHECKER-PACKAGE-PUBLIC s" scoped" 0 TK-ENUM TWX-TFAM-DECL drop
   s" rbs" s" scoped" 0 0 0 0 TWX-SUMV-ADD drop
   s" rbs" s" scoped" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
CHECKER-SCOPE-DONE
TFAM-N@ P-TFAM @ T=
SUMV-N@ P-SUMV @ T=
s" rbs" s" scoped" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 3. DEPTH >=3 nesting + parent-frame survival. Mix scope / candidate / scope;
\    prove each pop restores to its own parent (single-slot state would clobber a
\    grandparent's saved marks). Track TFAM-N (feature) + SYM-N (core depth-safe).
\ ---------------------------------------------------------------------------
TFAM-N@ P-TFAM !   SYM-N @ P-SYMN !
CHECKER-SCOPE-START                                    \ frame A (scope)
   s" rbn" CHECKER-PACKAGE-PUBLIC s" a" 0 TK-ENUM TWX-TFAM-DECL drop
   s" -- n" s" RBN-A-SYM" TWX-USIG-ADD
   TFAM-N@ A-TFAM !   SYM-N @ A-SYMN !                 \ state after A's mutations
   TWX-CAND-START                               \ frame B (candidate)
      s" rbn" CHECKER-PACKAGE-PUBLIC s" b" 0 TK-ENUM TWX-TFAM-DECL drop
      s" -- n" s" RBN-B-SYM" TWX-USIG-ADD
      TFAM-N@ B-TFAM !   SYM-N @ B-SYMN !              \ state after B's mutations
      TWX-CAND-START                            \ frame C (candidate, depth 3)
         s" rbn" CHECKER-PACKAGE-PUBLIC s" c" 0 TK-ENUM TWX-TFAM-DECL drop
         s" -- n" s" RBN-C-SYM" TWX-USIG-ADD
         s" rbn" s" c" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=
      0 TWX-CAND-DONE drop                      \ pop C -> parent B survives
      TFAM-N@ B-TFAM @ T=                              \ B's family count intact
      SYM-N @ B-SYMN @ T=                              \ B's symbols intact
      s" rbn" s" c" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=     \ C's family gone
      s" rbn" s" b" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=    \ B's family present
   0 TWX-CAND-DONE drop                         \ pop B -> parent A survives
   TFAM-N@ A-TFAM @ T=                                 \ A's family count intact (not B's)
   SYM-N @ A-SYMN @ T=                                 \ A's symbols intact (not B's)
   s" rbn" s" b" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=       \ B's family gone
   s" rbn" s" a" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=      \ A's family present
CHECKER-SCOPE-DONE                                     \ pop A -> back to P0
TFAM-N@ P-TFAM @ T=                                    \ all nested families gone
SYM-N @ P-SYMN @ T=                                    \ all nested symbols gone
s" rbn" s" a" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 4. PACKAGE mode/name restore across a candidate that opens a different package
\    and flips visibility.
\ ---------------------------------------------------------------------------
s" outer" CHECKER-PACKAGE                              \ MODE=PRIVATE name="outer"
TWX-CAND-START
   s" inner" CHECKER-PACKAGE                           \ MODE=PRIVATE name="inner"
   CHECKER-PUBLIC                                      \ MODE=PUBLIC
   CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-PUBLIC T=
   CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ s" inner" T$=
0 TWX-CAND-DONE drop
CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-PRIVATE T=
CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ s" outer" T$=
\ a scope layered on top restores likewise.
CHECKER-SCOPE-START
   s" deeper" CHECKER-PACKAGE   CHECKER-PUBLIC
CHECKER-SCOPE-DONE
CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-PRIVATE T=
CHECKER-PACKAGE-NAME CHECKER-PACKAGE-U @ s" outer" T$=
CHECKER-END-PACKAGE
CHECKER-PACKAGE-MODE @ CHECKER-PACKAGE-NONE T=

\ ---------------------------------------------------------------------------
\ 5. DEFER (DFER) restore + deferred-target cache. A candidate that defers a NEW
\    name and a SURVIVING (pre-existing) name must roll both back, and the cached
\    "deferred" answer for the surviving symbol must not remain stale-true.
\ ---------------------------------------------------------------------------
s" RBD-KEEP" CHECKER-DEFER                             \ top-level defer that survives
s" RBD-KEEP" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=
\ pre-existing symbol, NOT deferred yet -> caches a "false" answer.
s" -- n" s" RBD-SURV" TWX-USIG-ADD
s" RBD-SURV" TWX-FIND-DEFER FOUNDF !  FOUNDF @ 0 T=
TWX-CAND-START
   s" RBD-CAND" CHECKER-DEFER                          \ defer a NEW name
   s" RBD-SURV" CHECKER-DEFER                          \ defer an EXISTING symbol
   s" RBD-CAND" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=
   s" RBD-SURV" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=
0 TWX-CAND-DONE drop
\ new-name defer fully retired.
s" RBD-CAND" TWX-FIND-DEFER FOUNDF !  FOUNDF @ 0 T=
\ surviving symbol's defer rolled back AND its cache is not stale-true.
s" RBD-SURV" TWX-FIND-DEFER FOUNDF !  FOUNDF @ 0 T=
\ the top-level defer still holds.
s" RBD-KEEP" TWX-FIND-DEFER FOUNDF !  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ 6. SYM hash-index ENTRY retirement (not just the counter): add a signature in a
\    candidate, prove it is found via the live index inside, then prove the index
\    entry is retired post-rollback and a same-name re-add resolves cleanly.
\ ---------------------------------------------------------------------------
s" -- n" s" RBX-SEED" TWX-USIG-ADD                 \ build/validate the SYM index
s" RBX-SEED" TWX-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
SYM-N @ P-SYMN !   SYM-STR-U @ P-SYMU !
TWX-CAND-START
   s" -- n" s" RBX-CAND" TWX-USIG-ADD
   s" RBX-CAND" TWX-FIND-USIG FOUNDF !  FOUNDF @ -1 T=   \ found via live index
0 TWX-CAND-DONE drop
SYM-N @ P-SYMN @ T=                                    \ counter rewound
SYM-STR-U @ P-SYMU @ T=                                \ sym string pool rewound
s" RBX-CAND" TWX-FIND-USIG FOUNDF !  FOUNDF @ 0 T=  \ index entry retired
\ re-add the same name after rollback: resolves to the fresh entry, no ghost row.
s" -- n" s" RBX-CAND" TWX-USIG-ADD
s" RBX-CAND" TWX-FIND-USIG FOUNDF !  FOUNDF @ -1 T=

\ ---------------------------------------------------------------------------
\ 7. THROW-SAFE candidate: a REAL throw (E-TFAM-DUP) between TWX-CAND-START
\    and DONE must still pop the frame and roll back the candidate's earlier
\    successful mutations. This is exactly the discipline CHECK-CANDIDATE! now
\    follows — body under catch, DONE unconditional, re-throw the caught code.
\    Without it the frame leaks (depth stuck) and rejected rows survive, so the
\    next probe runs on corrupted state (cases 1-6 never threw between START/DONE).
\ ---------------------------------------------------------------------------
s" rbt" CHECKER-PACKAGE-PUBLIC s" dupbase" 1 TK-SUM TWX-TFAM-DECL drop   \ pre-seed the dup target
RBF-DEPTH @ P-DEPTH !
TFAM-N@ P-TFAM !
TWX-CAND-START
   s" rbt" CHECKER-PACKAGE-PUBLIC s" tcand" 0 TK-ENUM TWX-TFAM-DECL drop \ succeeds inside the candidate
   s" rbt" s" tcand" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=      \ present before the throw
   s" rbt" CHECKER-PACKAGE-PUBLIC s" dupbase" 1 TK-SUM ' TWX-TFAM-DECL catch
      TC ! 2drop 2drop 2drop drop                                    \ real throw, caught
0 TWX-CAND-DONE drop                                          \ pop the frame unconditionally
TC @ E-TFAM-DUP T=                                                   \ the throw really fired
RBF-DEPTH @ P-DEPTH @ T=                                             \ frame popped: depth balanced
TFAM-N@ P-TFAM @ T=                                                  \ candidate mutation rolled back
s" rbt" s" tcand" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ 0 T=          \ tcand gone
s" rbt" s" dupbase" TWX-TFAM-FIND-IN FOUNDF ! drop  FOUNDF @ -1 T=       \ pre-seed intact
\ direct CHECK-CANDIDATE! regression: the no-throw path returns its verdict and
\ balances the frame depth (top-level stdin interpret yields 1 = uncheckable).
RBF-DEPTH @ P-DEPTH !
s" : RBT-OK ( -- n ) 0 ;" CHECK-CANDIDATE! FOUNDF !  FOUNDF @ 1 T=
RBF-DEPTH @ P-DEPTH @ T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-family-rollback-suite: failures" 1 die ;
REPORT
