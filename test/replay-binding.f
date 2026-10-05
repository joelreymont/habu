\ replay-binding.f - a checker replay binds each name to the word the compiler
\ would call, and hands the engine back unchanged.
\
\ VERIFY:SOURCE-BUF checks a source text without running it. While it replays,
\ the engine's dictionary carries an overlay: the replay's package and using
\ declarations drive the engine's scope cells, its definitions are published as
\ records, and every body binds through the engine's own lookup. When the
\ replay ends, clean or thrown, the overlay is gone: the dictionary count, the
\ data pointer, the current wordlist, the wordlist counter, the open package
\ and the using depth read as before, and no namespace row the replay made
\ survives. Each row runs twice: in the candidate scope VERIFY:SOURCE-BUF opens
\ in the caller's package, and in a package-neutral scope, as tools/check.f
\ preverifies a source.
\
\ Before the overlay a replay bound against the checker's mirror of the scope.
\ A package word the checker had no record of was invisible to the mirror, so a
\ reopened body spelling it was typed as the engine word of that spelling (the
\ fetch below certified where the compiler calls RB-RAW's `@`), and a qualified
\ name the engine resolves was undefined.
\
\ While a neutral pair holds the overlay open the engine defines nothing live.
\ The close puts the dictionary count, the code pointer and the wordlist
\ counter back where the pair found them, so a word, a package row or a
\ wordlist made inside the pair would vanish with it or be handed out twice.
\ The engine refuses each where it is made, ENGINE-ERROR:OVERLAY-OPEN with its
\ name, and inside `evaluate` the refusal is a throw. The close gives back only
\ what the overlay retired, so a live `undefine` there is refused the same way
\ before it retires the word.
\
\ Run: bin/hb --load test/replay-binding.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/eval.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require src/habu/verify-source.f
require test/reopen-binding-lib.f
require test/reopen-binding-late.f
require test/replay-binding-raw.f

T-RESET

\ A does> definer the engine holds. Its clause is a record of its own,
\ RB-MK;does, in the definer's wordlist (src/habu/habu2.f DOES-REC).
package RB-DOES

public

: RB-MK ( n -- ) create , does> ( -- n ) @ ;

;package

\ A word, a live alias of it (EXPORT publishes a record of its own) and an
\ alias only the checker's store holds (a direct CHECKER-EXPORT, as
\ test/type-export-suite.f records its aliases), all made before any pair.
package RB-XS
public
: RB-XI ( n -- n ) 1 + ;
;package

package RB-XD
public
EXPORT RB-XS:RB-XI
;package

package RB-XQ
public
s" RB-XS:RB-XI" CHECKER-EXPORT
;package

\ A public dup that is not the primitive, with an effect the primitive's is
\ not: a bare dup under `using RB-DUP` binds it only once the global dup is
\ retired.
package RB-DUP
public
: dup ( -- n ) 7 ;
;package

\ A namespace a qualified definition made: a public wordlist and no private
\ one.
: RB-PQ:X ( -- n ) 5 ;

\ A word only the checker's store holds: a CHECK! row, no engine record.
s" RB-GHOST ( -- n ) 7" CHECK! drop

\ A word holding the name the clause of a definer RB-CK would take.
package RB-CC
public
: RB-CK;does ( -- n ) 1 ;
;package

\ A does> definer the engine holds and a live export of it, each with its
\ clause, as a warm engine holds the loaded twins of the source it replays
\ (WARM-CASE).
package RB-WD
public
: RB-WMK ( n -- ) create , does> ( -- n ) @ ;
;package

package RB-WX
public
EXPORT RB-WD:RB-WMK
;package

\ A word of the definer's name that is no definer, and a live export of it
\ beside a word holding the clause's name: neither has a clause (WARM-CASE).
package RB-WN
public
: RB-WMK ( n -- ) drop ;
;package

package RB-WY
public
EXPORT RB-WN:RB-WMK
: RB-WMK;does ( -- n ) 1 ;
;package

package REPLAY-BINDING-TEST

private

create DIAG-BUF $1000 allot
variable DIAG-U
TYPED-VARIABLE SRC-A ptr u8
variable SRC-U
variable NEUTRAL       \ nonzero: replay in a package-neutral scope
variable COMPOSE       \ nonzero: there, compose the text as a file the load reads

: NEUTRAL-ACT ( -- )
   COMPOSE @ 0 <> IF
      SRC-A @ SRC-U @ s" replay-binding-subject.f" VERIFY:SOURCE-COMPOSE-IN-SCOPE EXIT
   THEN
   SRC-A @ SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: ACT ( -- )
   NEUTRAL @ 0= IF SRC-A @ SRC-U @ VERIFY:SOURCE-BUF EXIT THEN
   CHECKER-SCOPE-START-NEUTRAL
   [: NEUTRAL-ACT ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 <> IF rc throw THEN ;

\ The verdict of replaying a text: 0 certified, else the throw code.
: VERDICT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a SRC-A !
   u SRC-U !
   DIAG-BUF $1000 DIAG-BUFFER!
   [: ACT ;] catch {: rc:n :}
   DIAG-BUFFER$ nip DIAG-U !
   DIAG-BUFFER-OFF
   rc ;

: DIAG$ ( -- ptr u8 n )
   DIAG-BUF DIAG-U @ ;

\ Engine cells a replay must hand back, by their layout.f offsets: the open
\ package's public wid, the wordlist counter and the using depth.
: PKG-PUB ( -- n ) data-base $78 + @ ;
: WIDN ( -- n ) data-base $30 + @ ;
: USE-DEPTH ( -- n ) data-base $9C08 + @ ;

\ Replay TEXT, expect verdict WANT, and require the engine state read before
\ the replay to read the same after it.
: REPLAY ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   ndict@ cp@ get-current WIDN PKG-PUB USE-DEPTH
   {: nd:n cp:n cur:n widn:n pub:n use:n :}
   a u VERDICT want T=
   ndict@ nd T=
   cp@ cp T=
   get-current cur T=
   WIDN widn T=
   PKG-PUB pub T=
   USE-DEPTH use T= ;

: NAMESPACE? ( ptr u8 n -- bool )
   XREF-NAMESPACE-WL XREF-FIND-WL XREF-FOUND? ;

: RAW-CASE ( -- )
   s" a reopened body binds the package word the checker has no record of" T-LABEL
   S\" package RB-RAW\npublic\n: RB-G ( ptr n -- n ) @ ;\n;package\n" 70 REPLAY
   DIAG$ s" rb-g" CONTAINS? TTRUE
   DIAG$ s" '@' is a trust-boundary primitive" CONTAINS? TTRUE ;

: QUALIFIED-CASE ( -- )
   s" a qualified name binds as the engine resolves it" T-LABEL
   s" RB-Q" NAMESPACE? TFALSE
   S\" package RB-Q\npublic\n: RB-QD ( n -- n n ) RB-Q:dup ;\n;package\n" 0 REPLAY
   s" and the package the replay made is gone after it" T-LABEL
   s" RB-Q" NAMESPACE? TFALSE ;

\ Replay twins of test/reopen-binding.f's candidate rows, whose live verdicts
\ are RE-CORE-FETCH refused and the other three certified.
: REOPEN-CASE ( -- )
   s" a replayed reopen binds the tails the package owns, as the live check does" T-LABEL
   S\" package REOPEN-BIND\npublic\n: RE-CORE-FETCH ( -- n ) REC @ ;\n;package\n" 70 REPLAY
   S\" package REOPEN-BIND\npublic\n: RE-PKG-FETCH ( -- n ) REC 2 @ ;\n;package\n" 0 REPLAY
   S\" package REOPEN-BIND\npublic\n: RE-PKG-DUP ( -- n ) 5 dup ;\n;package\n" 0 REPLAY
   S\" package REOPEN-BIND\npublic\n: RE-CONTROL ( -- n ) REC 2 NTH ;\n;package\n" 0 REPLAY
   s" the tail in a comment and a string binds nothing" T-LABEL
   S\" package REOPEN-BIND\npublic\n: RE-GET ( -- n )\n   REC \\ @ in a comment is no token\n   s\" @ !\" 2drop\n   2 @ ;\n;package\n" 0 REPLAY
   S\" package REOPEN-BIND\npublic\n: RE-GET1 ( -- n )\n   REC \\ 2 @ in a comment is no token\n   s\" 2 @\" 2drop\n   @ ;\n;package\n" 70 REPLAY
   s" bare and qualified in one body are one word" T-LABEL
   S\" package REOPEN-BIND\npublic\n: RE-BOTH ( -- n n ) REC 2 @  REC 2 REOPEN-BIND:@ ;\n;package\n" 0 REPLAY
   s" a reopen in a later file binds the tail it added" T-LABEL
   S\" package REOPEN-ORDER\n: RO-CORE-FETCH ( -- n ) SLOT @ ;\n;package\n" 70 REPLAY
   S\" package REOPEN-ORDER\n: RO-PKG-FETCH ( -- n ) SLOT 0 @ ;\n;package\n" 0 REPLAY
   s" a tail the package does not own is the engine word" T-LABEL
   S\" package REOPEN-BIND\npublic\n: RE-SWAP ( -- n n ) 1 2 swap ;\n;package\n" 0 REPLAY ;

: OWN-CASE ( -- )
   s" a replay binds its own earlier definition, a later one does not reach back" T-LABEL
   S\" package RB-OWN\npublic\n: RB-ONE ( n -- n ) 1 + ;\n: RB-TWO ( n -- n ) RB-ONE RB-ONE ;\n;package\n" 0 REPLAY
   S\" package RB-OWN\npublic\n: RB-TWO ( n -- n ) RB-ONE ;\n: RB-ONE ( n -- n ) 1 + ;\n;package\n" 70 REPLAY
   s" RB-OWN" NAMESPACE? TFALSE
   s" a second definition of one name in one replay is refused, as the engine refuses it" T-LABEL
   S\" package RB-OWN\npublic\n: RB-TWICE ( n -- n ) 1 + ;\n: RB-TWICE ( n -- n ) 2 + ;\n;package\n" 78 REPLAY
   S\" : RB-TWICE ( n -- n ) 1 + ;\n: RB-TWICE ( n -- n ) 2 + ;\n" 78 REPLAY ;

: USING-CASE ( -- )
   s" a replayed using imports the package's public words and is undone" T-LABEL
   S\" using REOPEN-BIND\n: RB-U ( -- n ) GET ;\n;using\n" 0 REPLAY
   S\" using REOPEN-BIND\n: RB-U2 ( -- n ) REC 2 NTH ;\n;using\n" 70 REPLAY
   s" a used word that shadows a global is ambiguous bare (E-USING-SHADOW-GLOBAL)" T-LABEL
   S\" using REOPEN-BIND\n: RB-U3 ( n -- n n ) dup ;\n;using\n" 7141 REPLAY ;

\ The engine's `undefine R` retires R's clause with R (src/habu/xref.f
\ XREF-RETIRE-INDEX), so a trust row naming the clause then names no word. The
\ first row is the control: while R lives, the clause's name resolves. A definer
\ the replay makes has its clause as well, as the engine's `does>` makes one;
\ without it the trust of the clause was E-TRUST-UNRESOLVED, where the live
\ load certifies it. The clause holds its name though no checker record does,
\ so a later colon definition of that name is refused, as the live load refuses
\ it ("duplicate definition: MK;does", rc 78). An export of a definer publishes
\ the clause in the export's wordlist too (src/habu/habu2.f C-EXPORT): without
\ it the trust of RB-XQ2:MK;does was E-TRUST-UNRESOLVED and the definition of
\ MK;does after the export certified, where the live load certifies the one and
\ refuses the other, 78. The engine's `does>` makes the clause for a TRUSTED:
\ definer as for a checked one, and so does the replay: without it the trust of
\ RB-TP2:TD;does was E-TRUST-UNRESOLVED and the definition of TD;does after the
\ definer, or after its export, certified, where the live load certifies the
\ first and refuses the other two, 78.
: CLAUSE-CASE ( -- )
   s" a replayed undefine retires the definer's does> clause with it" T-LABEL
   S\" s\" RB-DOES:RB-MK;does\" s\" -- n\" trust\n" 0 REPLAY
   S\" undefine RB-DOES:RB-MK\ns\" RB-DOES:RB-MK;does\" s\" -- n\" trust\n"
      E-TRUST-UNRESOLVED REPLAY
   s" a replayed definer makes its clause, and a replayed undefine retires both" T-LABEL
   S\" package RB-NC\npublic\n: MK ( n -- ) create , does> ( -- n ) @ ;\ns\" MK;does\" s\" -- n\" trust\n;package\n"
      0 REPLAY
   S\" package RB-NC\npublic\n: MK ( n -- ) create , does> ( -- n ) @ ;\nundefine MK\ns\" MK;does\" s\" -- n\" trust\n;package\n"
      E-TRUST-UNRESOLVED REPLAY
   s" a word holding the clause's name refuses the definer, as the engine's does> does" T-LABEL
   S\" package RB-CC\npublic\n: RB-CK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" 78 REPLAY
   s" a definition of the clause's name after its definer is refused" T-LABEL
   S\" package RB-NC\npublic\n: MK ( n -- ) create , does> ( -- n ) @ ;\n: MK;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY
   S\" : RB-GK ( n -- ) create , does> ( -- n ) @ ;\n: RB-GK;does ( -- n ) 1 ;\n" 78 REPLAY
   s" an export of a definer publishes its clause, and a definition of that name after it is refused" T-LABEL
   S\" package RB-XP2\npublic\n: MK ( n -- ) create , does> ( -- n ) @ ;\n;package\npackage RB-XQ2\npublic\nEXPORT RB-XP2:MK\n;package\ns\" RB-XQ2:MK;does\" s\" -- n\" trust\n"
      0 REPLAY
   S\" package RB-XP2\npublic\n: MK ( n -- ) create , does> ( -- n ) @ ;\n;package\npackage RB-XQ2\npublic\nEXPORT RB-XP2:MK\n: MK;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY
   s" a TRUSTED: definer makes its clause too, and so does an export of it" T-LABEL
   S\" package RB-TP2\npublic\nTRUSTED: TD ( n -- ) create , does> ( -- n ) @ ;\n;package\ns\" RB-TP2:TD;does\" s\" -- n\" trust\n"
      0 REPLAY
   S\" package RB-TP\npublic\nTRUSTED: TD ( n -- ) create , does> ( -- n ) @ ;\n: TD;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY
   S\" package RB-TP3\npublic\nTRUSTED: TD ( n -- ) create , does> ( -- n ) @ ;\n;package\npackage RB-TQ3\npublic\nEXPORT RB-TP3:TD\n: TD;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY ;

\ The engine's `package` gives a namespace with no private wordlist one
\ (src/habu/packages.f PKG-REOPEN), so a replayed `package RB-PQ` opens it as
\ the live load does, and the close takes the wordlist back with the rest.
: PRIVATE-CASE ( -- )
   s" a replayed package opens a namespace a qualified definition made" T-LABEL
   S\" package RB-PQ\npublic\n: RB-PY ( -- n ) X ;\n;package\n" 0 REPLAY
   s" and the namespace has no private wordlist after it" T-LABEL
   s" RB-PQ" XREF-NAMESPACE-WL XREF-FIND-WL XREF-PKG-PRIVATE 0 T= ;

: CASES ( -- )
   RAW-CASE
   QUALIFIED-CASE
   REOPEN-CASE
   OWN-CASE
   USING-CASE
   CLAUSE-CASE
   PRIVATE-CASE ;

\ A word the engine holds, as a warm engine holds the loaded twin of the
\ source it replays: the replay's own definition of it is the first of its
\ pass and certifies, and a second is refused, as the live load refuses a
\ second compile. The candidate scope alone runs it: the package-neutral
\ scope, where tools/check.f preverifies a source no engine has loaded,
\ refuses even one definition of a name the checker's store holds, 78.
\ A record the engine holds is a replayed definition's twin only if the
\ definition makes such a record. No definition makes a does> clause, so a
\ definition of the name of RB-WD:RB-WMK's clause, or of its alias's in RB-WX,
\ is refused whatever the replay made before it: the definer, nothing, or a
\ word of the definer's name that is no definer. A definer or an export of one
\ has a clause, so one is refused where the engine's record of its name has
\ none, as RB-WN:RB-WMK and its alias in RB-WY have none. The warm live engine
\ refuses each text, 78; each certified while the replay took any record of
\ the name for the twin. A TRUSTED: definer makes its clause as a checked one
\ does (CLAUSE-CASE), and the twin test reads the clause of the engine's
\ record, not its trust: RB-WD's checked definer is the twin of a replayed
\ TRUSTED: one, and RB-WN's word, which has no clause, is none, 78.
: RB-WARM ( n -- n ) 1 + ;

: WARM-CASE ( -- )
   s" a replay's second definition of a word the engine holds is refused" T-LABEL
   S\" : RB-WARM ( n -- n ) 1 + ;\n: RB-WARM ( n -- n ) 2 + ;\n" 78 REPLAY
   s" and its one definition of that word certifies" T-LABEL
   S\" : RB-WARM ( n -- n ) 1 + ;\n" 0 REPLAY
   s" a replayed definer the engine holds has its clause" T-LABEL
   S\" package RB-WD\npublic\n: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n: RB-WMK;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY
   S\" package RB-WD\npublic\n: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" 0 REPLAY
   s" and so has a replayed export of it the engine holds" T-LABEL
   S\" package RB-WX\npublic\nEXPORT RB-WD:RB-WMK\n: RB-WMK;does ( -- n ) 1 ;\n;package\n" 78 REPLAY
   S\" package RB-WX\npublic\nEXPORT RB-WD:RB-WMK\n;package\n" 0 REPLAY
   s" the clause is no twin, before its definer or after a word that is none" T-LABEL
   S\" package RB-WD\npublic\n: RB-WMK;does ( -- n ) 1 ;\n: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n;package\n"
      78 REPLAY
   S\" package RB-WD\npublic\n: RB-WMK ( n -- ) drop ;\n: RB-WMK;does ( -- n ) 1 ;\n;package\n" 78 REPLAY
   s" a definer or an export of one whose engine record has no clause is refused" T-LABEL
   S\" package RB-WN\npublic\n: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n: RB-WMK;does ( -- n ) 1 ;\n;package\n"
      78 REPLAY
   S\" package RB-WN\npublic\n: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n;package\npackage RB-WY\npublic\nEXPORT RB-WN:RB-WMK\n;package\n"
      78 REPLAY
   S\" package RB-WN\npublic\nEXPORT RB-WD:RB-WMK\n;package\n" 78 REPLAY
   s" a replayed TRUSTED: definer takes the engine's definer for its twin, not a word that is none" T-LABEL
   S\" package RB-WD\npublic\nTRUSTED: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" 0 REPLAY
   S\" package RB-WN\npublic\nTRUSTED: RB-WMK ( n -- ) create , does> ( -- n ) @ ;\n;package\n" 78 REPLAY ;

\ The live `undefine dup` retires the seeded primitive's record (src/habu/xref.f
\ XREF-RETIRE-WL), so a used public's dup binds bare after it
\ (tools/check-verify-test.f top-retired-import). A replayed one retires it
\ under the overlay, and the close gives the record back its wid: the same
\ record answers dup after the replay, by the hash index and by a scan. The
\ row runs in the neutral scope: in a package's scope `undefine dup` names no
\ word of the current wordlist, and the live load refuses it.
: SEEDED-CASE ( -- )
   s" dup" 0 XREF-FIND-WL-INDEX {: ix:n :}
   s" dup" 0 search-wl {: xt:n :}
   s" a replayed undefine retires a seeded primitive, so a used public binds its name" T-LABEL
   S\" undefine dup\nusing RB-DUP\n: RB-UD ( -- n ) dup ;\n;using\n" 0 REPLAY
   s" and the close gives the primitive's record back its wordlist" T-LABEL
   ix XREF-REC XREF-WORDLIST 0 T=
   s" dup" 0 XREF-FIND-WL-INDEX ix T=
   s" dup" 0 search-wl xt T=
   s" 3 dup +" TEST-EVAL:N 6 T= ;

\ A replay in a neutral scope inside another: the outer scope opened the
\ overlay, so the inner scope closes through its rollback frame
\ (CHECKER-OVERLAY ROLLBACK), not replay-close, and REPLAY reads the engine
\ while the outer scope is still open. The package row takes two wordlists and
\ the definition's name, longer than the 16 bytes a record holds inline
\ (layout.f DNAME-INL), takes bytes at CP.
: INNER-CASE ( -- )
   s" a replay in a scope inside an open overlay hands back NDICT, CP and WIDN" T-LABEL
   CHECKER-SCOPE-START-NEUTRAL
   -1 NEUTRAL !
   S\" package RB-INNER\npublic\n: RB-INNER-SPILLED-NAME ( -- n ) 1 ;\n;package\n" 0 REPLAY
   0 NEUTRAL !
   CHECKER-SCOPE-DONE ;

\ The certify path's answers for the aliases made before any pair: the live
\ one certifies and refuses as its word does; the store-only one has no record
\ for the engine's lookup to bind.
: PRIOR ( -- )
   s" RBX1 ( n -- n ) RB-XD:RB-XI" VERIFY:CANDIDATE-IN-SCOPE -1 T=
   s" RBX2 ( -- n ) RB-XD:RB-XI" VERIFY:CANDIDATE-IN-SCOPE 0 T=
   s" RBX3 ( n -- n ) RB-XQ:RB-XI" VERIFY:CANDIDATE-IN-SCOPE 1 T= ;

\ The overlay hides and changes nothing the engine held before it opened.
: PRIOR-CASE ( -- )
   s" an alias made before a pair answers the same inside it and after it" T-LABEL
   PRIOR
   CHECKER-SCOPE-START-NEUTRAL
   [: PRIOR ;] catch {: rc:n :}
   CHECKER-SCOPE-DONE
   rc 0 T=
   PRIOR ;

$4000 constant CAP
120000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot
variable RC
variable OUT-U
variable ERR-U

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! ENDOF
     signaled OF drop -1 RC ! ENDOF
     timeout OF -1 RC ! ENDOF
   ;MATCH
   LEN>N ERR-U !
   LEN>N OUT-U ! ;

: CHECK-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

: CHECK-CASE ( -- )
   s" tools/check.f refuses a reopened body by name in its preverify" T-LABEL
   s" test/replay-binding-use.f" CHECK-FILE
   RC @ 70 T=
   ERR$ s\" \"word\":\"get-reopened\"" CONTAINS? TTRUE
   ERR$ s" source preverify failed" CONTAINS? TTRUE ;

\ A fresh engine reads the text on its standard input.
: SESSION ( ptr u8 n -- ) {: a:ptr u:n :}
   PROC-ARGV-RESET
   HB$ >LEN  a u >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

\ The product engine seals every package it bakes (src/core/internal-mark.f
\ SEAL-PACKAGES), so no source reopens the overlay's package to reach the
\ engine writers only it may call.
: SEALED-CASE ( -- )
   s" the overlay's package is closed to source" T-LABEL
   s" package CHECKER-OVERLAY ;package" SESSION
   RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   ERR$ s" CHECKER-OVERLAY" CONTAINS? TTRUE ;

\ The session ends with ENGINE-ERROR:OVERLAY-OPEN and names what it refused.
: REFUSED ( ptr u8 n ptr u8 n -- ) {: a:ptr u:n na:ptr nu:n :}
   a u SESSION
   RC @ ENGINE-ERROR:OVERLAY-OPEN T=
   ERR$ na nu CONTAINS? TTRUE ;

\ Each refusal ran on an engine without it: the live word ended the session
\ with the close's silent exit 83, or, after a replay row raised the overlay's
\ high-water mark past it, survived the close as a name that no longer
\ resolved; the wordlist was handed out again after the close; the undefined
\ word stayed retired after it, E-UNDEFINED at its next use.
: LIVE-CASE ( -- )
   s" a live definition inside a neutral pair is refused by name" T-LABEL
   S\" CHECKER-SCOPE-START-NEUTRAL\n: RB-LIVE ( -- n ) 5 ;\nCHECKER-SCOPE-DONE\n"
      s" RB-LIVE" REFUSED
   s" create, variable, constant, defer and EXPORT are refused alike" T-LABEL
   S\" CHECKER-SCOPE-START-NEUTRAL\ncreate RB-LC\nCHECKER-SCOPE-DONE\n" s" RB-LC" REFUSED
   S\" CHECKER-SCOPE-START-NEUTRAL\nvariable RB-LV\nCHECKER-SCOPE-DONE\n" s" RB-LV" REFUSED
   S\" CHECKER-SCOPE-START-NEUTRAL\n5 constant RB-LK\nCHECKER-SCOPE-DONE\n" s" RB-LK" REFUSED
   S\" CHECKER-SCOPE-START-NEUTRAL\ndefer RB-LD ( -- )\nCHECKER-SCOPE-DONE\n" s" RB-LD" REFUSED
   S\" package RB-XP\n;package\n: RB-XW ( -- n ) 1 ;\nCHECKER-SCOPE-START-NEUTRAL\npackage RB-XP\npublic\nEXPORT RB-XW\n;package\nCHECKER-SCOPE-DONE\n"
      s" RB-XW" REFUSED
   s" a new package and a wordlist are refused" T-LABEL
   S\" CHECKER-SCOPE-START-NEUTRAL\npackage RB-LIVE-PKG\n;package\nCHECKER-SCOPE-DONE\n"
      s" RB-LIVE-PKG" REFUSED
   S\" CHECKER-SCOPE-START-NEUTRAL\nwordlist drop\nCHECKER-SCOPE-DONE\n" s" wordlist" REFUSED
   s" reopening a package the engine holds is no definition" T-LABEL
   S\" package RB-OLD-PKG\n;package\nCHECKER-SCOPE-START-NEUTRAL\npackage RB-OLD-PKG\npublic\n;package\nCHECKER-SCOPE-DONE\n"
      SESSION
   RC @ 0 T=
   s" inside evaluate the refusal is a throw, and the pair still closes" T-LABEL
   S\" require lib/test/eval.f\nCHECKER-SCOPE-START-NEUTRAL\ns\" : RB-E ( -- n ) 5 ;\" TEST-EVAL:RC\nCHECKER-SCOPE-DONE\ns\" closed\" type\ns\" \" rot die\n"
      SESSION
   RC @ ENGINE-ERROR:OVERLAY-OPEN T=
   OUT$ s" closed" CONTAINS? TTRUE
   ERR$ s" RB-E" CONTAINS? TTRUE
   s" a live undefine is refused by name before it retires the word" T-LABEL
   S\" : RB-UK ( -- n ) 7007 ;\nCHECKER-SCOPE-START-NEUTRAL\nundefine RB-UK\nCHECKER-SCOPE-DONE\n"
      s" RB-UK" REFUSED
   S\" require lib/test/eval.f\n: RB-UK ( -- n ) 7007 ;\nCHECKER-SCOPE-START-NEUTRAL\ns\" undefine RB-UK\" TEST-EVAL:RC\nCHECKER-SCOPE-DONE\nRB-UK .\ns\" \" rot die\n"
      SESSION
   RC @ ENGINE-ERROR:OVERLAY-OPEN T=
   OUT$ s" 7007" CONTAINS? TTRUE
   ERR$ s" RB-UK" CONTAINS? TTRUE ;

\ A word only the checker's store holds has no engine record, so the engine's
\ find never answers it. At top level the load retries an open package's own
\ qualified name that its public wordlist lacks in the global wordlist
\ (src/habu/habu1.f EMIT-FIND), finds no RB-GHOST there and refuses the token,
\ E-UNDEFINED; a replay composing the same text refuses it too.
: GHOST-CASE ( -- )
   s" a top-level name only the checker's store holds binds nothing" T-LABEL
   -1 NEUTRAL !  -1 COMPOSE !
   S\" package RB-TOP\nRB-TOP:RB-GHOST drop\n;package\n" CHECKER-REJECT-RC REPLAY
   0 COMPOSE !  0 NEUTRAL !
   s" live, the load refuses the same token" T-LABEL
   S\" s\" RB-GHOST ( -- n ) 7\" CHECK! drop\npackage RB-TOP\nRB-TOP:RB-GHOST drop\n;package\n" SESSION
   RC @ CHECKER-REJECT-RC T=
   ERR$ s" RB-TOP:RB-GHOST" CONTAINS? TTRUE ;

: RUN ( -- )
   0 NEUTRAL !
   CASES
   WARM-CASE
   -1 NEUTRAL !
   CASES
   SEEDED-CASE
   INNER-CASE
   0 NEUTRAL !
   PRIOR-CASE
   CHECK-CASE
   SEALED-CASE
   LIVE-CASE
   GHOST-CASE ;

RUN

;package

package RB-RAW

public

s" live, the same body is refused" T-LABEL
s" RB-G ( ptr n -- n ) @" CHECK-QUIET-CANDIDATE! 0 T=

;package

package RB-Q

public

s" live, the same qualified name certifies" T-LABEL
s" RB-QD ( n -- n n ) RB-Q:dup" CHECK-QUIET-CANDIDATE! -1 T=

;package

s" live, undefine retires the clause too" T-LABEL
S\" undefine RB-DOES:RB-MK s\" RB-DOES:RB-MK;does\" s\" -- n\" trust" TEST-EVAL:RC
E-TRUST-UNRESOLVED T=

T-REPORT
