\ prim-owner-scope-child.f - what a package-OWNED primitive row admits, and where.
\
\ Runs inside test/native-window-owner-child.f's reset window, which re-includes
\ src/core/checker.f from source: primitive rows are declarable at that bootstrap
\ boundary and nowhere else (in a sealed engine PRIM: / PPRIM: are DNAME-INT).
\ No require: the window's replacement prefix has no include words.
\
\ Two rows are declared here, both through the GENERAL private closer, which is
\ the subject: before it was hoisted beside PPRIM; only package CHECKER-DECL-FRAME
\ could spell CLOSE-PRIVATE, so this file could not have been written.
\
\ 1. PRIM-OWNER-AXIOM, a fresh name with no other row and no engine word. It
\    isolates the closer's own effect - the axiom exists for the owner package
\    and for nobody else - with no engine-side resolution mixed in. It is asked
\    of the CHECKER only (CHECK-CANDIDATE!), because an engine compile of a name
\    the dictionary does not carry says nothing about which package owns the row.
\ 2. A second row for addrmap-set, beside the global PRIM-TRUSTED-ONLY! row
\    checker.f already carries for it. That is the shape an owned capability prim
\    takes today: the global row keeps the outside boundary (E-CAP-TRUSTED) and
\    keeps the dictionary record callable, because the seal classifies a record by
\    its BARE name; the private row admits a CHECKED caller inside the owner.
\    addrmap-set is never EXECUTED here - every case compiles a definition and
\    throws the body away.

PPRIM: PRIM-OWNER-SCOPE PRIM-OWNER-AXIOM PE-N PE-IN CLOSE-PRIVATE
PPRIM: PRIM-OWNER-SCOPE addrmap-set PE-N PE-IN CLOSE-PRIVATE

\ Compiling a candidate is the subject, so the compile boundary is unchecked on
\ purpose: EVC reports the reject code instead of letting it exit the window, EV
\ moves the live package the next case is measured in, and SELECT picks the
\ compiler it is measured under.
TRUSTED: EVC ( ptr u8 n -- n ) [: evaluate ;] catch ;
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: SELECT ( n -- ) set-tier ;

package PRIM-OWNER-CHILD

$0A constant LF-C

: VERDICT$ ( n -- ptr u8 n ) {: v:n :}
   v -1 = if s" admitted" exit then
   v 0 = if s" refused" exit then
   v 1 = if s" unresolvable" exit then
   s" unexpected" ;

: OUTCOME$ ( n -- ptr u8 n ) {: rc:n :}
   rc 0 = if s" compiled" exit then
   rc 70 = if s" rejected" exit then
   s" unexpected" ;

\ The label is written BEFORE the subject runs. A rejected case raises through
\ the diagnostic renderer, and reading the caller's label string back out on the
\ far side of that printed an empty name.
: LABEL ( ptr u8 n -- ) {: la:ptr lu:n :}
   s" prim-owner: " type la lu type s" : " type ;

\ The checker's verdict on a candidate definition, with no engine compilation:
\ -1 admitted, 0 refused, 1 unresolvable.
: CAND ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n sa:ptr su:n :}
   la lu LABEL
   sa su CHECK-CANDIDATE! VERDICT$ type LF-C emit ;

\ The whole path: the checker certifies and the engine compiles, or one of them
\ refuses with the compile-reject rc.
: EVAL ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n sa:ptr su:n :}
   la lu LABEL
   sa su EVC OUTCOME$ type LF-C emit ;

: OWNER-OPEN ( -- ) s" package PRIM-OWNER-SCOPE" EV ;
: OTHER-OPEN ( -- ) s" package PRIM-OWNER-OTHER" EV ;
: PKG-CLOSE ( -- ) s" ;package" EV ;

\ ---- the fresh axiom: the closer's effect, with no engine word involved ------
: AXIOM-CASES ( -- )
   s" axiom top level"
   s" POS-AX-TOP ( n -- ) PRIM-OWNER-AXIOM" CAND
   OWNER-OPEN
   s" axiom inside owner"
   s" POS-AX-IN ( n -- ) PRIM-OWNER-AXIOM" CAND
   PKG-CLOSE
   OTHER-OPEN
   s" axiom other package"
   s" POS-AX-OTH ( n -- ) PRIM-OWNER-AXIOM" CAND
   PKG-CLOSE
   OWNER-OPEN
   s" axiom reopened owner"
   s" POS-AX-RE ( n -- ) PRIM-OWNER-AXIOM" CAND
   PKG-CLOSE ;

\ ---- the real capability prim, compiled at both tiers ------------------------
\ Every definition name is used once: a candidate records a signature under its
\ name, and a second definition of that name is a duplicate, not a second
\ measurement of the same question.
: TIER0-CASES ( -- )
   0 SELECT
   OWNER-OPEN
   s" t0 checked inside owner"
   s" : POS-T0-IN ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   s" t0 checked top level"
   s" : POS-T0-TOP ( n -- ) addrmap-set ;" EVAL
   OTHER-OPEN
   s" t0 checked other package"
   s" : POS-T0-OTH ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   OWNER-OPEN
   s" t0 checked reopened owner"
   s" : POS-T0-RE ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   s" t0 trusted top level"
   s" TRUSTED: POS-T0-TR ( n -- ) addrmap-set ;" EVAL ;

\ Tier 1 is the tier the global row exists for: the optimizing compiler reads a
\ callee's cell widths out of the prim table for every name a body writes, so the
\ trusted caller outside the owner proves the outside path still has a row.
: TIER1-CASES ( -- )
   1 SELECT
   OWNER-OPEN
   s" t1 checked inside owner"
   s" : POS-T1-IN ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   s" t1 checked top level"
   s" : POS-T1-TOP ( n -- ) addrmap-set ;" EVAL
   s" t1 trusted top level"
   s" TRUSTED: POS-T1-TR ( n -- ) addrmap-set ;" EVAL
   0 SELECT ;

public

: RUN ( -- )
   AXIOM-CASES
   TIER0-CASES
   TIER1-CASES
   s" prim-owner: ok" type LF-C emit ;

;package

PRIM-OWNER-CHILD:RUN
