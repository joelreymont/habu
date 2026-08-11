\ reach.f - carrying a migration back to the callers that were compiled before
\ it. One concern: the call instructions already in the image.
\
\ WHAT THIS IS FOR. A republication points a dictionary record at new code, and
\ from that moment every definition the engine compiles reaches it. The
\ definitions compiled EARLIER do not, and there are two separate reasons, both
\ decided by src/habu/habu2.f's C-CALL at the moment each caller was compiled.
\ A caller of a small straight-line word carries a verbatim COPY of the word's
\ body and holds no call instruction at all. Every other caller holds one direct
\ branch-with-link whose displacement names the address the callee occupied
\ then. So the whole engine - the interpreter, the checker, the emitter, all of
\ it compiled into bin/hb and never recompiled - keeps running the old code for
\ a word that has just been migrated, and the chain's wins stop at that
\ boundary. This file moves the branches. It cannot move a copy, and says so
\ below.
\
\ THE SUBJECT IS TWO RECORDS AND NOT ONE, BECAUSE THE ENGINE REFUSES A DUPLICATE
\ DEFINITION. The migration entry compiles a definition through the engine's own
\ interpret path, and a tail may be defined once in a wordlist
\ (src/habu/habu2.f, C-REJECT-DUP-DEF), so a word that already exists cannot be
\ migrated in place: its new code is published as a record of its own, in a
\ package of its own, and what is left is to move the call sites. Hence a
\ redirection names the word whose callers are to move and the word whose
\ routine they are to move onto.
\
\ WHAT MAKES MOVING A SITE SOUND, AND WHAT DOES NOT.
\
\   THE MACHINE'S PART is exact and is checked here. A branch-with-link carries
\   a signed twenty-six-bit count of instructions from its own address, so a
\   site can only be moved onto a target the field can name; that is
\   src/compiler/native/branch.f's answer and a site it cannot reach is refused
\   before anything is written.
\
\   THE CALLER-SAVE PART is exact too, and the argument is one line. Nothing in
\   a Habu word's convention is callee-saved, so a call site puts every value it
\   is holding somewhere the callee cannot reach - EXCEPT the registers
\   src/compiler/native/clobber.f said the routine at that address does not
\   destroy. A site compiled before that record knew anything about the old
\   address saved everything; a site compiled after it saved everything outside
\   the old address's row. Both are covered by the row if there is one and by
\   the whole file if there is not, so ONE test decides every site at once: the
\   new routine must destroy no register outside what the old address's answer
\   names. That is E-NREACH-CLOBBER, and it is asked before any site is
\   rewritten rather than site by site, because the answer cannot differ between
\   sites.
\
\   AND IT KEEPS THE PUBLICATION SEAM'S OWN PROOF TRUE. A routine the chain
\   published carries a recorded clobber set that covers what everything it
\   calls destroys (src/compiler/native/publish.f, BRANCH-CK). If such a routine
\   is one of the callers being moved, its recorded set covers the OLD address's
\   set, and the test above says the new routine's set is inside that one - so
\   the covering still holds afterwards. Moving a call site therefore cannot
\   falsify a publication that has already happened, and no caller needs to be
\   left out.
\
\   THE MEANING IS THE CALLER'S STATEMENT. Nothing here can prove that the two
\   words compute the same function; the old word's source is not kept and its
\   code is only bytes. Two of the three things that would follow from sameness
\   ARE held: the two records must carry the same word (the same tail), and
\   their declared stack effects must agree wherever the checker still holds
\   them. THE SECOND IS THE PART WITH A HOLE IN IT, and it is worth stating
\   plainly: the checker's effect store is stripped at seal time, so a word that
\   came out of the image - which is exactly the interesting subject, the one
\   with callers nothing can recompile - has no declared effect to hold, and a
\   redirection of one rests on the same-tail rule and on the caller. The new
\   side always has an effect and is always checked, because it was defined in
\   this process. Dot habu-hold-a-redirect-82ae7668 carries closing the gap by
\   keeping a migrated definition's accepted effect beside its published
\   routine, which is what the old side is missing.
\
\ AND WHAT A REDIRECTION DOES NOT DO: IT DOES NOT REWRITE THE OLD RECORD. The
\ old word keeps its own code and keeps meaning it, so a definition compiled
\ AFTER a redirection and naming the old word still gets the old word's code -
\ correct code, just not the new code. That is deliberate rather than
\ overlooked: the interesting subjects are engine-internal words
\ (src/core/internal-mark.f), and the publication seam refuses to rewrite the
\ record of one because it is a word the interpreter will not enter. Dot
\ habu-make-the-name-97a9839c carries making the name follow its callers.
\
\ NOTHING IS WRITTEN UNTIL EVERYTHING HAS BEEN ASKED. The sites are walked
\ TWICE: once to count them and to hold every one of them against the branch
\ field, and once to write. A refusal therefore leaves every caller entering the
\ code it was entering, which is the contract every other seam of this chain
\ keeps. And a subject with no call site at all is a refusal and not a quiet
\ zero - E-NREACH-NONE - because "the republication reached nobody" is precisely
\ the outcome this file exists to make visible: a body the engine copied has no
\ site to move, and the honest answer for its callers is that they have to be
\ recompiled (dot habu-recompile-the-callers-5de6012c).

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
\ `patch32` is the engine's 32-bit poke: it flips the code region writable,
\ stores, flips it back and syncs the instruction cache for the line it wrote,
\ all inside engine text, and its own span guard refuses an address outside the
\ bands the engine will write to. It is refused from checked code, so it is
\ wrapped once here and this word chooses nothing: the address is a call site
\ this file decoded out of a live record, and the value is the instruction
\ src/compiler/native/branch.f builds from that address and the target.
\ src/compiler/native/publish.f and src/habu/xref.f wrap the same primitive the
\ same way for the same reason.
\
\ IT IS REFUSED BY DESIGN AND SO THIS WRAPPER STAYS. `patch32` already carries a
\ `PRIM:` row, marked PRIM-TRUSTED-ONLY! so a checked caller earns E-CAP-TRUSTED;
\ dot habu-turn-the-registry-4c064064 converted the chain's other TRUSTED: sites
\ and left this one, because converting it means deleting that flag rather than
\ crossing the boundary it draws.
TRUSTED: POKE ( n ptr a -- )
   patch32 ;

\ ---- the one reader of a declared effect ------------------------------------
\ Both halves of the answer in one word, so a name the checker holds no effect
\ for answers a pair nothing can be mistaken for. This was an unchecked boundary
\ because the store's readers are name-stripped past the seal; src/core/checker.f
\ now carries a `PRIM:` row for each of the three, so the body is ordinary
\ checked Habu (dot habu-turn-the-registry-4c064064). src/core/top-row.f's hook
\ and test/effect-read-api-test.f still reach the same readers their own way.
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

\ The same word, spelled the same way. A name is matched the way the engine
\ matches one, which is without regard to case.
: SAME-WORD-CK ( n n -- ) {: o:n w:n :}
   o TAIL$ w TAIL$ XREF-STR=CI 0= if E-NREACH-WORD throw then ;

\ The code being pointed at has to be a routine the publication seam wrote for
\ THIS record: the seam publishes only emissions the allocation validator
\ accepted, so this is what stands between a caller and being moved onto bytes
\ nobody vouched for. Both halves are asked - that the seam republished this
\ name in this wordlist, and that the record still points at the routine it
\ wrote - because a record retargeted since by anything else is no longer that
\ routine.
: ROUTINE-CK ( n -- ) {: k:n :}
   k TAIL$ k WID-OF {: a:ptr u:n wid:n :}
   a u wid NPUB:REPUBLISHED? 0= if E-NREACH-ROUTINE throw then
   a u wid NPUB:NEW-START  k NWALK:REC-START <> if E-NREACH-ROUTINE throw then ;

\ ---- the declared effects ----------------------------------------------------
\ The new side must have one: it was defined in this process, so an absent
\ effect means the name being pointed at is not the definition the migration
\ published. The old side is held against it when the checker still has it, and
\ the file header says what it means when it does not.
: EFFECT-CK ( ptr u8 n ptr u8 n -- ) {: oa:ptr ou:n na:ptr nu:n :}
   na nu EFFECT {: nin:n nout:n :}
   nin EFFECT-NONE = if E-NREACH-EFFECT throw then
   oa ou EFFECT {: oin:n oout:n :}
   oin EFFECT-NONE = if exit then
   oin nin <> if E-NREACH-EFFECT throw then
   oout nout <> if E-NREACH-EFFECT throw then ;

\ ---- what the new routine destroys, against what a caller assumed ------------
: CLOBBER-CK ( n n -- ) {: o:n w:n :}
   w A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   o A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N
   invert and 0<> if E-NREACH-CLOBBER throw then
   w A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   o A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N
   invert and 0<> if E-NREACH-CLOBBER throw then ;

\ ---- the sites ---------------------------------------------------------------
\ Everything one pass of the walk needs, parked: a quotation cannot read the
\ enclosing word's locals.
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

\ Move every call instruction in the live dictionary that enters the first
\ word's code onto the routine the publication seam wrote for the second, and
\ answer how many moved.
\
\ EACH NAME IS RESOLVED TWICE AND BOTH RESOLUTIONS ARE THE ENGINE'S: once
\ against the dictionary, for the record and its code, and once against the
\ checker's effect store, for the declared effect. A BARE name is resolved by
\ the first in the global wordlist and by the second against whatever package
\ scope is open, so a package word must be written the way its package writes
\ it - qualified - or the two answers are about two different words. Everything
\ below states its subjects that way, and so does every caller in the tree.
\
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
