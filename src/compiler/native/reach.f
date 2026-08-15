\ reach.f - carrying a migration back to the callers that were compiled before
\ it. One concern: the call instructions already in the image.
\
\ Only a direct branch-with-link can be moved. A caller that carries a verbatim
\ COPY of a small body holds no call site, and its callers have to be recompiled.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/compiler/native/branch.f
require src/compiler/native/codewalk.f
require src/compiler/native/clobber.f
require src/compiler/native/publish.f

package NREACH

private

\ ---- the one primitive that writes ------------------------------------------
\ `patch32` flips the code region writable, stores, flips it back and syncs the
\ i-cache; it is PRIM-TRUSTED-ONLY! by design, so checked code wraps it here.
TRUSTED: POKE ( n ptr a -- )
   patch32 ;

\ ---- the one reader of a declared effect ------------------------------------
: EFFECT ( ptr u8 n -- n n )
   EFFECT-QUERY if EFFECT-DIN-N EFFECT-DOUT-N else -1 -1 then ;

-1 constant EFFECT-NONE

\ ---- the two records ---------------------------------------------------------
: REC-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-FIND-INDEX dup 0 < if E-NREACH-NAME throw then ;

: CODED-CK ( n -- n ) {: k:n :}
   k NWALK:CODED? 0= if E-NREACH-NAME throw then
   k ;

: TAIL$ ( n -- ptr u8 n ) {: k:n :}
   k XREF-REC XREF-NAME$ ;

: WID-OF ( n -- n ) {: k:n :}
   k XREF-REC XREF-WORDLIST ;

\ Matched the way the engine matches a name, which is without regard to case.
: SAME-WORD-CK ( n n -- ) {: o:n w:n :}
   o TAIL$ w TAIL$ XREF-STR=CI 0= if E-NREACH-WORD throw then ;

\ Both halves are asked - that the seam republished this name in this wordlist,
\ and that the record still points at the routine it wrote.
: ROUTINE-CK ( n -- ) {: k:n :}
   k TAIL$ k WID-OF {: a:ptr u:n wid:n :}
   a u wid NPUB:REPUBLISHED? 0= if E-NREACH-ROUTINE throw then
   a u wid NPUB:NEW-START  k NWALK:REC-START <> if E-NREACH-ROUTINE throw then ;

\ ---- the declared effects ----------------------------------------------------
\ The new side must have an effect; it was defined in this process. The old side
\ often has none, its store stripped at seal (habu-hold-a-redirect-82ae7668).
: EFFECT-CK ( ptr u8 n ptr u8 n -- ) {: oa:ptr ou:n na:ptr nu:n :}
   na nu EFFECT {: nin:n nout:n :}
   nin EFFECT-NONE = if E-NREACH-EFFECT throw then
   oa ou EFFECT {: oin:n oout:n :}
   oin EFFECT-NONE = if exit then
   oin nin <> if E-NREACH-EFFECT throw then
   oout nout <> if E-NREACH-EFFECT throw then ;

\ ---- what the new routine destroys, against what a caller assumed ------------
\ Nothing in a Habu word's convention is callee-saved, so one test covers every
\ site: the new routine must destroy no register outside the old address's set.
: CLOBBER-CK ( n n -- ) {: o:n w:n :}
   w A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   o A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   invert and 0<> if E-NREACH-CLOBBER throw then
   w A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   o A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   invert and 0<> if E-NREACH-CLOBBER throw then ;

\ ---- the sites ---------------------------------------------------------------
\ Parked in variables: a quotation cannot read the enclosing word's locals.
variable OLD-ENTRY
variable NEW-ENTRY
variable NEW-LO                       \ the new routine's own code, so a site
variable NEW-HI                       \ inside it can be told from a caller's
variable SITES
variable WRITING

: SITE-CK ( n -- ) {: at:n :}
   at NEW-LO @ >=  at NEW-HI @ <  and if E-NREACH-SELF throw then
   at NEW-ENTRY @ NBR:REACHES? 0= if E-NREACH-RANGE throw then
   SITES @ 1+ SITES ! ;

: SITE-WRITE ( n -- ) {: at:n :}
   at NEW-ENTRY @ NBR:BL-WORD  at XREF-N>REC POKE ;

: SITE ( n n -- ) {: at:n w:n :}
   w NBR:BL? 0= if exit then
   at w NBR:BL-TARGET OLD-ENTRY @ <> if exit then
   WRITING @ 0= if at SITE-CK exit then
   at SITE-WRITE ;

: SWEEP ( -- )
   [: SITE ;] NWALK:LIVE-EACH ;

: COUNT-SITES ( -- )
   0 SITES !  0 WRITING !
   SWEEP
   SITES @ 0= if E-NREACH-NONE throw then ;

: MOVE-SITES ( -- )
   1 WRITING !
   SWEEP
   0 WRITING ! ;

public

\ Answers how many call sites moved. Each name is resolved TWICE, against the
\ dictionary and against the checker's effect store, so a package word must be
\ written qualified or the two answers are about two different words.
\ Everything that can refuse refuses before the first instruction is written.
: REDIRECT ( ptr u8 n ptr u8 n -- n ) {: oa:ptr ou:n na:ptr nu:n :}
   oa ou REC-OF CODED-CK {: o:n :}
   na nu REC-OF CODED-CK {: w:n :}
   o w = if E-NREACH-NAME throw then
   o w SAME-WORD-CK
   w ROUTINE-CK
   oa ou na nu EFFECT-CK
   o NWALK:REC-START OLD-ENTRY !
   w NWALK:REC-START NEW-ENTRY !
   w NWALK:REC-START NEW-LO !
   w NWALK:REC-START w NWALK:REC-LEN + NEW-HI !
   OLD-ENTRY @ NEW-ENTRY @ CLOBBER-CK
   COUNT-SITES
   MOVE-SITES
   SITES @ ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
