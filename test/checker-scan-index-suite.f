\ checker-scan-index-suite.f — the checker's symbol-keyed store indexes.
\
\ A WHITEBOX-SUITE row, like test/type-family-rollback-suite.f: every case is a
\ top-level interpret line, because the stores and their indexes are checker
\ internals that resolve only there, reached through named TRUSTED: shims. The
\ gate runs it in its unsealed whitebox engine; standalone under bin/hb it
\ exits 70 (docs/gate.md "How a suite runs").
\
\ WHAT IS UNDER TEST. Four lookups stopped walking their store and started
\ asking an index (dot habu-the-checker-s-8c4e7273):
\
\   SCAN-USIGS-SYM       newest effect record for a symbol      HT-USX
\   NORET-SCAN-SYM       newest control-flag entry for a symbol HT-NRX
\   SUMV-FROM-CTOR-SYM   lowest variant for a constructor sym   HT-SVX
\   TFAM-FIND-IN         family row for a (package, tail)       TFX buckets
\
\ Each store still carries the walk that defines the answer — USIG-NEWEST-LINEAR,
\ NORET-NEWEST-LINEAR, SUMV-CTOR-FIRST-LINEAR, TFAM-FIND-IN-LINEAR — and section
\ 2 differentials the index against it for EVERY symbol and EVERY family in the
\ live image. Section 1 comes first and is the one that would notice a
\ specification and an index that are wrong together: it pins the ORDER the
\ answer depends on (redefinition, deletion, shadowing) through the ordinary
\ load path, before any index word is named. Section 3 pins the same order
\ across the checker's rollback frames.
\
\ TWO ROWS, ONE FIXTURE. The assertions, the shims and the differential are
\ test/checker-scan-index-lib.f. This row holds the cases that share state and
\ run in order in one image: section 1 defines SCXA, SCXT and the scxfam rows;
\ section 2's differential builds all four indexes before a rollback case reads
\ their marks; 3d and 3e roll back records of SCXA and SCXT; section 5's
\ survival checks read section 1's answers. The other row,
\ test/checker-scan-index-rollback-suite.f, holds the cases that touch none of
\ that state — 3a to 3c, which create their symbol or family inside the frame,
\ and section 4's capacity refusals — so the two run side by side in the gate
\ pool.
\
\ Section 5 proves the indexes survive their store outgrowing its initial
\ capacity: the family tail buckets grow with the record arena they index
\ (TFX-RESIZE), and forcing the symbol table past its capacity re-lays-out the
\ symbol mapping, after which the differential runs again.

require lib/string.f
require test/checker-scan-index-lib.f

\ Reopened, not imported: the cases run inside the fixture's package, where
\ the definitions they make land (test/checker-scan-index-lib.f).
using TFAM

package SCANIDX-TEST

\ ---------------------------------------------------------------------------
\ 1. ORDER PINNING. Every one of these answers depends on WHICH record of a
\    symbol wins. They run through the ordinary load path — a definition, an
\    `undefine`, a redefinition — and read the answer back through the entry
\    points the checker itself uses, naming no index.
\ ---------------------------------------------------------------------------

\ 1a. effect records: newest wins, and a deletion is an absence rather than a
\     fall-back to the record it shadows.
TRUSTED: SCX-DEF1 ( -- ) s" : SCXA ( n -- n ) ;" evaluate ;
TRUSTED: SCX-UNDEF ( -- ) s" undefine SCXA" evaluate ;
TRUSTED: SCX-DEF2 ( -- ) s" : SCXA ( n n -- n ) drop ;" evaluate ;
TRUSTED: SCX-DEF3 ( -- ) s" undefine SCXA : SCXA ( n n n -- n ) drop drop ;" evaluate ;
TRUSTED: SCX-DEF4 ( -- ) s" : SCXA ( n n n -- n ) drop drop ;" evaluate ;

s" SCXA" SCX-SIG-MIN-IN -1 T=                  \ nothing recorded yet
' SCX-DEF1 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 1 T=                   \ the first record answers
' SCX-UNDEF catch TC !  TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN -1 T=                  \ the deletion shadows it, not the reverse
' SCX-DEF2 catch TC !   TC @ 0 T=
s" SCXA" SCX-SIG-MIN-IN 2 T=                   \ the newest record answers again
' SCX-DEF3 catch TC !   TC @ 0 T=
SCX-UEND REC-END !
s" SCXA" SCX-SIG-MIN-IN 3 T=                   \ four records deep, still the newest

\ the same symbol, read straight off the index, off the walk that specifies it,
\ and off the scan that consumes it — the entry point above caches, so a wrong
\ scan can hide behind a hit.
s" SCXA" SCX-ACTIVE-SYM IX !
IX @ 0 <> TTRUE
IX @ SCX-USIG-NEWEST 1- 0 > TTRUE              \ the record is at a nonzero offset
IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=                              \ the scan reports the newest record's arity
SCX-FMEND REC-END @ T=                        \ absolute completed end, not a span
IX @ SCX-WIRE-NEXT REC-END @ T=               \ owner wire preserves that same end

\ ... and with the newest record a DELETION, the scan reports no record at all
\ rather than the live one it shadows.
' SCX-UNDEF catch TC !  TC @ 0 T=
SCX-UEND REC-END !
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TFALSE
SCX-FMEND REC-END @ T=                         \ deletion still depends on its completed end
IX @ SCX-WIRE-NEXT REC-END @ T=
s" SCXA" SCX-SIG-MIN-IN -1 T=
' SCX-DEF4 catch TC !   TC @ 0 T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=

\ 1b. control flags: later wins, and a redefinition clears the stale metadata.
TRUSTED: SCX-CTLDEF ( -- ) s" : SCXT ( n -- n ) 7101 throw ;" evaluate ;
TRUSTED: SCX-CTLREDEF ( -- ) s" undefine SCXT : SCXT ( n -- n ) ;" evaluate ;

s" SCXT" SCX-CTL-FLAGS 0 T=
' SCX-CTLDEF catch TC !   TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and CTL-THROW T=    \ the throw edge is recorded
' SCX-CTLREDEF catch TC !  TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and 0 T=            \ ... and the redefinition clears it

s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-NORET-NEWEST 0 <> TTRUE               \ the differential below is not vacuous
IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR T=
IX @ SCX-SCAN-NORET
SCX-NORET-FLAGS CTL-THROW and 0 T=              \ the scan reports the LATEST entry's flags

\ 1e. an entry the store cannot key. CHECKER-RECORD-SYM answers 0 for a token it
\     cannot resolve, and a NORETS entry keyed 0 is indistinguishable from the
\     store's own terminator — it would hide every entry appended after it from
\     every reader. Nothing is recorded for it instead.
TRUSTED: SCX-RECSYM ( ptr u8 n -- n ) CHECKER-RECORD-SYM ;
TRUSTED: SCX-CTLADD-BAD ( -- ) s" a:b:c" CTL-DEAD NORET-ADD ;
TRUSTED: SCX-CTLADD-DEAD ( -- ) s" SCXT" CTL-DEAD NORET-ADD ;
TRUSTED: SCX-CTLADD-CLEAR ( -- ) s" SCXT" 0 NORET-ADD ;

s" a:b:c" SCX-RECSYM 0 T=                            \ the token really is unkeyable
' SCX-CTLADD-BAD catch TC !  TC @ 0 T=
' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=              \ an entry appended AFTER it
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and CTL-DEAD T=      \ ... is still visible
s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-NORET-NEWEST  IX @ SCX-NORET-NEWEST-LINEAR T=
' SCX-CTLADD-CLEAR catch TC !  TC @ 0 T=
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=

\ 1c. family rows: a package row and a global row may share a tail, and each
\     exact (package, tail) resolves to its own row.
s" " CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL IX !
s" scxpk" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL
IX @ <> TTRUE                                        \ two distinct rows, one tail
s" " s" scxfam" SCX-TFAM-FIND-IN TTRUE IX @ T=
s" scxpk" s" scxfam" SCX-TFAM-FIND-IN TTRUE IX @ <> TTRUE
s" scxpk" s" nosuchtail" SCX-TFAM-FIND-IN TFALSE drop
\ the global row is lexical and never enters the public fallback set, so the
\ package row is the sole public answer for this tail.
s" scxfam" SCX-TFAM-FIND-PUBLIC TTRUE IX @ <> TTRUE

\ a second package exporting the same tail makes the unqualified answer
\ genuinely ambiguous, and the index must reproduce the refusal, not pick one.
s" scxpk2" CHECKER-PACKAGE-PUBLIC s" scxfam" 0 TK-CELL SCX-TFAM-DECL drop
s" scxfam" ' SCX-TFAM-FIND-PUBLIC catch TC ! 2drop
TC @ E-TFAM-AMBIG T=

\ 1d. constructor symbols: a generated constructor resolves to its variant, and
\     an ordinary word symbol resolves to nothing.
TRUSTED: SCX-SUMDECL ( -- )
   s" SUMTYPE scxsum 0 VARIANT scxva n ;VARIANT VARIANT scxvb ;VARIANT ;SUMTYPE" evaluate ;
' SCX-SUMDECL catch TC !  TC @ 0 T=
s" SCXA" SCX-ACTIVE-SYM SCX-SUMV-FROM-CTOR TFALSE drop

\ ---------------------------------------------------------------------------
\ 2. DIFFERENTIAL. For every symbol the image has interned and every family it
\    has declared, the index and the walk that specifies it agree
\    (SCX-DIFF-ALL, test/checker-scan-index-lib.f). It also builds all four
\    indexes, so the rollback cases below read marks that mean something.
\ ---------------------------------------------------------------------------
SCX-TFAM-N 0 > TTRUE                           \ the family differential is not vacuous
SCX-SYM-N 1 > TTRUE                            \ nor the symbol ones
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 3. ROLLBACK. The checker's own rollback frames retire store records; the
\    index has to retire with them. Here the frame adds records for a symbol
\    that EXISTED before it, from section 1; 3a to 3c, in the rollback row,
\    create theirs inside the frame.
\ ---------------------------------------------------------------------------

\ 3d. a record added inside a rejected frame for a symbol that EXISTED before it.
\     The symbol survives the rollback, so its head cannot simply be dropped —
\     it has to revert to the record the frame did not touch.
s" SCXA" SCX-SIG-MIN-IN 3 T=
SCX-CAND-START
   s" n n n n -- n" s" SCXA" SCX-USIG-ADD            \ TWO records for the one symbol, so
   s" n n n n n -- n" s" SCXA" SCX-USIG-ADD          \ the repair has to reach past both
   s" SCXA" SCX-SIG-MIN-IN 5 T=
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXA" SCX-ACTIVE-SYM IX !
IX @ SCX-USIG-NEWEST  IX @ SCX-USIG-NEWEST-LINEAR T=
IX @ SCX-SCAN-USIG
SCX-FEP-HIT? TTRUE
SCX-FEP-MINI 3 T=                                    \ the record below the frame answers
s" SCXA" SCX-SIG-MIN-IN 3 T=
SCX-DIFF-ALL

\ 3e. the same for the control store: a flag entry added for an existing symbol
\     inside a rejected frame reverts to the entry below it, in place.
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=
SCX-CAND-START
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=
   ' SCX-CTLADD-DEAD catch TC !  TC @ 0 T=           \ two entries, one symbol
   s" SCXT" SCX-CTL-FLAGS CTL-DEAD and CTL-DEAD T=
0 SCX-CAND-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXT" SCX-ACTIVE-SYM IX !
IX @ SCX-SCAN-NORET
SCX-NORET-FLAGS CTL-DEAD and 0 T=                     \ the entry below the frame answers
s" SCXT" SCX-CTL-FLAGS CTL-DEAD and 0 T=
SCX-DIFF-ALL

\ ---------------------------------------------------------------------------
\ 5. GROWTH. Both index families are sized from the store they key, so both
\    have to survive that store outgrowing its initial capacity: the mapping is
\    re-laid-out at the new SYM-CAP and every table in it rebuilds, and the
\    family tail buckets are resized and rehashed. (Section 4 is in the
\    rollback row.)
\ ---------------------------------------------------------------------------

\ the boot image already declares far more than TFX-SLOTS-INIT families, so the
\ bucket array has already been resized and rehashed at least once
SCX-TFX-SLOTS SCX-TFX-SLOTS-INIT > TTRUE
SCX-DIFF-TFAM 0 T=

\ force the symbol table past its current capacity, which drops the mapping and
\ rebuilds every table in it at the new cap.
10 constant SCX-RADIX
48 constant SCX-ZERO

: SCX-NAME$ ( n -- ptr u8 n ) {: n:n :}
   SB-RESET
   s" scxsym" SB-APPEND
   1000000
   BEGIN dup 0 > WHILE
      n over / SCX-RADIX mod SCX-ZERO + SB-APPEND-C
      SCX-RADIX /
   REPEAT drop
   SB$ ;

: SCX-FILL-SYMS ( n -- ) {: target:n :}
   0 IX !
   BEGIN SCX-SYM-N target < WHILE
      IX @ SCX-NAME$ SCX-NAME! SCX-SYM-INTERN drop
      IX @ 1 + IX !
   REPEAT ;

PTR-VARIABLE RETIRED-SYMS
variable RETIRED-SYMS-U
TRUSTED: SCX-SYM-STORAGE ( -- ptr u8 n ) SYMS-P @ SYM-CAP SYM-REC * ;
: SCX-ZERO? ( ptr u8 n -- bool ) {: base:ptr size:n :}
   size 0 ?do base i + c@ 0<> if false unloop exit then loop true ;

SCX-SYM-STORAGE RETIRED-SYMS-U ! RETIRED-SYMS !
RETIRED-SYMS @ RETIRED-SYMS-U @ SCX-ZERO? TFALSE
SCX-SYM-CAP IX !
IX @ 1 + SCX-FILL-SYMS
SCX-SYM-CAP IX @ > TTRUE                       \ the symbol table really did grow
RETIRED-SYMS @ RETIRED-SYMS-U @ SCX-ZERO? TTRUE  \ no dead pointers enter a capture
SCX-DIFF-ALL                                   \ ... and every index answers at the new cap

\ the answers pinned in section 1 survive the rebuild
s" SCXA" SCX-SIG-MIN-IN 3 T=
s" SCXT" SCX-CTL-FLAGS CTL-THROW and 0 T=
s" scxpk" s" scxfam" SCX-TFAM-FIND-IN TTRUE drop

s" checker-scan-index-suite: failures" REPORT

;package

;using
