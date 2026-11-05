\ decl-replay-verify-source.f - src/habu/verify-source.f's RECORD-* arms rebuild
\ the registry a live declaration builds; tools on verify-source see types only
\ through those arms, so a drift is silent. Each case replays a declaration and
\ asserts each candidate's verdict, or the code and diagnostic a refused replay gives.

require src/lib/errors.f
require src/lib/test.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture
require src/habu/verify-source.f

package VSPARITY

T-RESET

public

\ Registers a source's declarations without executing it, as the in-process
\ tools do. VERIFY:SOURCE-BUF rolls every registration back in a candidate
\ scope; SOURCE-BUF-IN-SCOPE keeps them, so its result is the one this file can
\ read.
: VS-LOAD ( ptr u8 n -- ) {: src:ptr u:n :}
   ndict@ {: before:n :}
   src u VERIFY:SOURCE-BUF-IN-SCOPE
   ndict@ before T= ;

\ VS-LOAD's load in a neutral pair: CHECKER-OVERLAY binds the source's names until
\ VS-DONE drops them, keeps the registrations and checks the dictionary count.
variable PAIR-BEFORE
: VS-OPEN ( ptr u8 n -- ) {: src:ptr u:n :}
   ndict@ PAIR-BEFORE !
   CHECKER-SCOPE-START-NEUTRAL
   src u VERIFY:SOURCE-BUF-IN-SCOPE ;
: VS-DONE ( -- )
   CHECKER-SCOPE-FINALIZE
   ndict@ PAIR-BEFORE @ T= ;

\ Source builder for bodies too long to write out.
$4000 constant SRC-CAP        \ 16384: comfortably past BODYBUF-CAP (8000)
create SRC-BUF SRC-CAP allot
variable SRC-U

\ Same `create`-region boundary the product code documents: a checked body
\ cannot address a `create` region as a typed `ptr u8` span.
: SRC-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF E-TEST-CAPACITY throw THEN
   c SRC-BUF SRC-U @ + c!
   SRC-U @ 1 + SRC-U ! ;
: SRC$ ( -- ptr u8 n ) SRC-BUF SRC-U @ ;

variable SB-I   variable SB-J
: SRC-RESET ( -- ) 0 SRC-U ! ;
: SRC-PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SB-I !
   BEGIN SB-I @ u < WHILE
      a SB-I @ + c@ SRC-C
      SB-I @ 1 + SB-I !
   REPEAT ;
: SRC-DIGIT ( n -- ) 48 + SRC-C ;
: SRC-VARIANT ( n -- ) {: i:n :}      \ one `vNNNN ` variant name: 6 bytes
   118 SRC-C
   i 1000 / 10 mod SRC-DIGIT
   i 100 / 10 mod SRC-DIGIT
   i 10 / 10 mod SRC-DIGIT
   i 10 mod SRC-DIGIT
   32 SRC-C ;

\ Each ENUM is declared in its own package so successive cases cannot collide on
\ the name.
: ENUM-SOURCE ( ptr u8 n n -- ptr u8 n ) {: sa:ptr su:n n:n :}
   SRC-RESET
   s" package " SRC-PUT  sa su SRC-PUT  s"  public ENUM big " SRC-PUT
   0 SB-J !
   BEGIN SB-J @ n < WHILE
      SB-J @ SRC-VARIANT
      SB-J @ 1 + SB-J !
   REPEAT
   s" ;ENUM ;package" SRC-PUT
   SRC$ ;

\ VSTRY registers under catch, so a refusal is a value.
: VS-RUN ( ptr u8 n -- ) VS-LOAD ;
: VSTRY ( ptr u8 n -- n ) [: 2dup VS-RUN ;] catch {: rc:n :} 2drop rc ;

\ A `\` comment ends at a newline, so this source needs a real one embedded.
: BACKSLASH-ENUM-SOURCE ( -- ptr u8 n )
   SRC-RESET
   s" package vr public ENUM cm3 red \ trailing note" SRC-PUT
   10 SRC-C
   s" green ;ENUM ;package" SRC-PUT
   SRC$ ;

;package

\ 1. STRUCTURE replayed into package `sdrep`: its constructor and destructor
\    check, and a candidate with the wrong effect is refused.
s" package sdrep public STRUCTURE pair FIELD lo n FIELD hi n ;STRUCTURE ;package"
VSPARITY:VS-OPEN
s" PAIR ( n n -- sdrep:pair ) SDREP-PAIR:MAKE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" UNPAIR ( sdrep:pair -- n n ) SDREP-PAIR:UNMAKE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" WRONG-PAIR ( n -- sdrep:pair ) SDREP-PAIR:MAKE" VERIFY:CANDIDATE-IN-SCOPE 0 T=
VSPARITY:VS-DONE

\ 2. Compact ENUM: the replay drives ENUM-DECL:ED-REPLAY-OUTCOME; its
\    constructors check, and a candidate with the wrong effect is refused.
s" package edrep public ENUM colour red green blue ;ENUM ;package"
VSPARITY:VS-OPEN
s" COLOUR ( -- edrep:colour ) EDREP-COLOUR:RED" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" COLOUR-GREEN ( -- edrep:colour ) EDREP-COLOUR:GREEN" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" COLOUR-BLUE ( -- edrep:colour ) EDREP-COLOUR:BLUE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" WRONG-COLOUR ( -- n ) EDREP-COLOUR:RED" VERIFY:CANDIDATE-IN-SCOPE 0 T=
VSPARITY:VS-DONE

\ Checked uses work immediately after replay, in the replay itself and, for
\ the sdrep and edrep candidates above, in the pair it ran in, including
\ parametric payloads and product make/unmake. No runtime word is evaluated
\ here. The replay compiles nothing, so the names it registers are facts of
\ the certify path, and every candidate asks that path
\ (VERIFY:CANDIDATE-IN-SCOPE): compiled code holds no record of them.
s" package vruse public ENUM box<a> VARIANT ok FIELD value a ;VARIANT VARIANT no ;VARIANT ;ENUM : WRAP ( a -- box<a> ) VRUSE-BOX:OK ; ;package"
VSPARITY:VS-LOAD

\ 3. A STRUCTURE named as a later declaration's payload type: without the
\    STRUCTURE arm on this path the payload does not resolve.
s" package edpay public STRUCTURE slotrec FIELD v n ;STRUCTURE SUMTYPE box VARIANT full slotrec ;VARIANT VARIANT empty ;VARIANT ;SUMTYPE ;package"
VSPARITY:VS-OPEN
s" PAY-FULL ( edpay:slotrec -- edpay:box ) EDPAY-BOX:FULL" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" PAY-EMPTY ( -- edpay:box ) EDPAY-BOX:EMPTY" VERIFY:CANDIDATE-IN-SCOPE -1 T=
VSPARITY:VS-DONE

\ 4. A body the capture buffer cannot hold raises; it is never truncated. The
\    replay entries have no length gate: they parse whatever arrives. A
\    BODY-APPEND that skips the token that does not fit and keeps appending
\    builds a well-formed, wrong family (measured: a 1302-variant compact ENUM
\    replayed with rc 0 and registered 1142 variants, every later tag shifted).
\    The engine's TDECL-CAP ($1000) and this buffer's BODYBUF-CAP (8000) differ
\    on purpose: the live keyword and this replay are the unified front end,
\    which has no 4096-byte cap, so each path is pinned to its own bound.

\ Under the buffer's bound: replays completely, every variant present. A body of
\ 1000 variants is 6000 bytes, past 4096, and accepted.
s" capa" 1000 VSPARITY:ENUM-SOURCE VSPARITY:VS-OPEN
s" CAPA-LAST ( -- capa:big ) CAPA-BIG:V0999" VERIFY:CANDIDATE-IN-SCOPE -1 T=
VSPARITY:VS-DONE

\ Over the buffer's bound: raises the declaration layer's own "declaration too
\ long" code, and registers nothing at all.
s" capb" 1400 VSPARITY:ENUM-SOURCE VSPARITY:VSTRY 7118 T=
s" CAPB-ID ( capb:big -- capb:big )" VERIFY:CANDIDATE-IN-SCOPE 0 T=

\ 5. Comments are not laundered out of a declaration body. The engine reads a
\    declaration body with `parse-name`, which has no comment rule, so `\` and
\    `(` inside one are ordinary tokens that hit the name gate. verify-source's
\    NEXT-SCAN strips them, right between definitions and wrong inside a
\    declaration: the replay would accept source the engine refuses. The two
\    replay windows read with NEXT-RAW, and these cases pin that they reject
\    with the same code the live front end gives.
DECL-DIAG:PROSE
s" package vp public ENUM cm1 red ( note ) green ;ENUM ;package"
VSPARITY:VSTRY 7101 T=
s" habu: bad enum declaration 'cm1': name must be a lowercase tail at '('"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" package vq public STRUCTURE cm2 FIELD a n ( note ) FIELD b n ;STRUCTURE ;package"
VSPARITY:VSTRY 7107 T=
s" habu: bad structure declaration 'cm2': unexpected token in structure declaration at '('"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
VSPARITY:BACKSLASH-ENUM-SOURCE VSPARITY:VSTRY 7101 T=
s" habu: bad enum declaration 'cm3': name must be a lowercase tail at '\'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ 6. `DERIVE addr`. The address participant (src/core/structure-make.f) mints
\    the accessors in the commit step, after the body generated the make/unmake
\    set; a replay that stopped there had constructors and no accessors, so a
\    source reading its own fields failed the pre-scan while loading live. The
\    replay renders the same plan the live path evaluates and stops after its
\    first reading: each row is armed and checked, so the accessor's effect is
\    registered under its own name, and no row reaches the evaluator, so no
\    dictionary word appears (VS-DONE's ndict@ assertion). The effects are real:
\    the wrong-typed uses below are refused as after a live declaration.
s" package adrep public STRUCTURE rec DERIVE addr FIELD a n FIELD b n ;STRUCTURE : ADR-USE ( ptr rec -- ptr n ) ADREP-REC:A ; : ADR-SPAN ( -- n ) ADREP-REC:BYTES ADREP-REC:CELLS + ; ;package"
VSPARITY:VS-OPEN

s" ADR-A ( ptr adrep:rec -- ptr n ) ADREP-REC:A" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" ADR-B ( ptr adrep:rec -- ptr n ) ADREP-REC:B" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" ADR-AT ( ptr adrep:rec n -- ptr adrep:rec ) ADREP-REC:AT" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" ADR-BYTES ( -- n ) ADREP-REC:BYTES" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" ADR-CELLS ( -- n ) ADREP-REC:CELLS" VERIFY:CANDIDATE-IN-SCOPE -1 T=
\ the projection is the field's, not the record's: a replayed accessor that
\ answered its own input would be a registered lie
s" ADR-SELF ( ptr adrep:rec -- ptr adrep:rec ) ADREP-REC:A" VERIFY:CANDIDATE-IN-SCOPE 0 T=
s" ADR-NOARG ( -- ptr n ) ADREP-REC:A" VERIFY:CANDIDATE-IN-SCOPE 0 T=
s" ADR-AT-WRONG ( ptr adrep:rec -- ptr adrep:rec ) ADREP-REC:AT" VERIFY:CANDIDATE-IN-SCOPE 0 T=
VSPARITY:VS-DONE

\ The private spelling. A private product publishes FAMILY-MEMBER into its
\ declaring package's private wordlist, so the only place the names resolve is
\ inside that package — which is where the source below uses them.
s" package adprv STRUCTURE prec DERIVE addr FIELD lo n FIELD hi n ;STRUCTURE : PRV-HI ( ptr prec -- ptr n ) PREC-HI ; : PRV-STEP ( ptr prec -- ptr prec ) 1 PREC-AT ; : PRV-BYTES ( -- n ) PREC-BYTES ; ;package"
VSPARITY:VS-LOAD

\ and stays private: the same name outside the package does not resolve
s" ADR-LEAK ( ptr adprv:prec -- ptr n ) PREC-HI" VERIFY:CANDIDATE-IN-SCOPE 0 T=

T-REPORT
