\ prim-owner-scope.f - a package-owned primitive row admits the owner and nobody else.
\
\ Run: bin/hb --load test/prim-owner-scope.f
\
\ The subject is the general private row closer CLOSE-PRIVATE (src/core/checker.f,
\ beside PPRIM;) and what the seal makes of it. CLOSE-PRIVATE interns a PPRIM:
\ row into the OWNER package's private wordlist, so the row types the engine's
\ record only inside the owner (CK-REC-BIND's owner branch).
\ src/core/internal-mark.f then classifies the global record such a row reaches
\ by that row, so a primitive may exist as its private row alone: callable from
\ checked code inside its owner at both tiers, refused by name everywhere else.
\ A checked `[']` of it is admitted and refused exactly where such a call is. A
\ primitive that TRUSTED: bodies outside its owner call keeps a global
\ trusted-only row beside the private one, and the matrix measures that shape
\ too. set-tier is the private-only seed primitive: package TIER (lib/tier.f)
\ owns it, and TIER:SELECT is its public word.
\
\ TWO ENGINES ANSWER. The owner cases need the owner open, and the product seals
\ every package it bakes - NPUB and FFI included - so they run in a child under
\ test/native-window-owner-child.f, the native-build handoff window, which
\ re-includes src/core/checker.f FROM SOURCE and, through
\ test/prim-owner-scope-prepare.f, runs the production seal over the window. The
\ outside refusal is also asked of the running engine itself, in SUBJECT forks:
\ that is the shipped answer a user's definition gets.
\
\ The transcript is compared whole rather than line by line: a case that stops
\ emitting is as much a failure as a case that answers wrongly, and only an exact
\ comparison catches the first.

require lib/test.f
require lib/string.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/subject.f
require test/whitebox-child.f

package PRIM-OWNER-SCOPE-SUITE

$4000 constant IO-CAP
180000 constant TIMEOUT-MS
20000 constant SUBJECT-MS
70 constant REJECT-RC

create OUT IO-CAP allot
create ERR IO-CAP allot

\ The expected transcript outgrows lib/string.f's builder, so it has its own.
create EXP IO-CAP allot
variable EXP-U

: EXP+ ( ptr u8 n -- ) {: a:ptr u:n :}
   EXP-U @ u + IO-CAP > if E-STR-CAPACITY throw then
   a EXP EXP-U @ + u BYTE-COPY
   EXP-U @ u + EXP-U ! ;

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" prim-owner-scope" WHITEBOX-CHILD:PROVIDE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The window stops at src/core/cell-effects.f. These are the prefix files the
\ include words and the seal stand on, in the build's order, before the
\ fixture's rows and the pass itself (test/prim-owner-scope-prepare.f).
: TARGET-ARGS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/target.f" ARG s" src/os/linux-x86-64/layout.f" ARG exit
   then
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG s" src/os/linux/layout.f" ARG exit
   then
   s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/prim-owner-scope-child.f" ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   TARGET-ARGS
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/habu/code-span.f" ARG
   s" test/prim-owner-scope-prepare.f" ARG
   WHITEBOX-CHILD:ENV! ;

: CASE+ ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n va:ptr vu:n :}
   s" prim-owner: " EXP+
   la lu EXP+
   s" : " EXP+
   va vu EXP+
   S\" \n" EXP+ ;

\ The fresh axiom has no engine word: a row for a word the dictionary does not
\ carry binds nowhere, its owner included. The addrmap-set and FFI rows below
\ prove owner-only admission on real records.
: AXIOM-LINES ( -- )
   s" axiom inside owner"   s" unresolvable" CASE+ ;

\ A private-only row: the owner compiles a checked caller, a reopened owner too,
\ and `rejected` is the compile-reject rc 70 everywhere else.
: PRIVATE-T0-LINES ( -- )
   s" t0 ptr>cell inside owner"   s" compiled" CASE+
   s" t0 cell>ptr inside owner"   s" compiled" CASE+
   s" t0 ptr>cell top level"      s" rejected" CASE+
   s" t0 cell>ptr top level"      s" rejected" CASE+
   s" t0 cell>ptr other package"  s" rejected" CASE+
   s" t0 ptr>cell reopened owner" s" compiled" CASE+ ;

: PRIVATE-T1-LINES ( -- )
   s" t1 ptr>cell inside owner" s" compiled" CASE+
   s" t1 cell>ptr inside owner" s" compiled" CASE+
   s" t1 ptr>cell top level"    s" rejected" CASE+ ;

\ A tick of a private-only row is admitted where the call is and nowhere else.
: TICK-T0-LINES ( -- )
   s" t0 tick ptr>cell inside owner"  s" compiled" CASE+
   s" t0 tick cell>ptr inside owner"  s" compiled" CASE+
   s" t0 tick ptr>cell top level"     s" rejected" CASE+
   s" t0 tick cell>ptr other package" s" rejected" CASE+ ;

: TICK-T1-LINES ( -- )
   s" t1 tick cell>ptr inside owner"  s" compiled" CASE+
   s" t1 tick ptr>cell top level"     s" rejected" CASE+
   s" t1 tick cell>ptr other package" s" rejected" CASE+ ;

\ A package's row for its OWN word leaves the same-named global DNAME-INT.
: SHADOW-LINES ( -- )
   s" shadowed global top level" s" rejected" CASE+ ;

\ The dual row: the owner's row admits its checked callers and ticks, the global
\ trusted-only row refuses them elsewhere, and a TRUSTED: caller outside the
\ owner compiles at both tiers. On a tree without addrmap-set's global row the
\ tier-1 trusted line reads `unexpected` (E-HIR-UNMODELED, rc 67).
: DUAL-T0-LINES ( -- )
   s" ffi call inside owner"              s" compiled" CASE+
   s" ffi call top level"                 s" rejected" CASE+
   s" ffi call other package"             s" rejected" CASE+
   s" t0 ffi call trusted top level"      s" compiled" CASE+
   s" t0 addrmap-set inside owner"        s" compiled" CASE+
   s" t0 tick addrmap-set inside owner"   s" compiled" CASE+
   s" t0 addrmap-set top level"           s" rejected" CASE+
   s" t0 tick addrmap-set top level"      s" rejected" CASE+
   s" t0 addrmap-set other package"       s" rejected" CASE+
   s" t0 addrmap-set reopened owner"      s" compiled" CASE+
   s" t0 addrmap-set trusted top level"   s" compiled" CASE+ ;

: DUAL-T1-LINES ( -- )
   s" t1 ffi call trusted top level"      s" compiled" CASE+
   s" t1 addrmap-set inside owner"        s" compiled" CASE+
   s" t1 tick addrmap-set inside owner"   s" compiled" CASE+
   s" t1 addrmap-set top level"           s" rejected" CASE+
   s" t1 tick addrmap-set other package"  s" rejected" CASE+
   s" t1 addrmap-set trusted top level"   s" compiled" CASE+ ;

\ A seed primitive with its owner's row alone: set-tier, owned by TIER. The
\ owner's public TIER:SELECT compiles in every scope.
: OWNER-T0-LINES ( -- )
   s" t0 set-tier inside owner"           s" compiled" CASE+
   s" t0 set-tier top level"              s" rejected" CASE+
   s" t0 tier select top level"           s" compiled" CASE+
   s" t0 set-tier other package"          s" rejected" CASE+
   s" t0 tier select other package"       s" compiled" CASE+ ;

: OWNER-T1-LINES ( -- )
   s" t1 set-tier inside owner"           s" compiled" CASE+
   s" t1 set-tier top level"              s" rejected" CASE+
   s" t1 tier select top level"           s" compiled" CASE+
   s" t1 set-tier other package"          s" rejected" CASE+
   s" t1 tier select other package"       s" compiled" CASE+ ;

\ The engine's own owners. Each owner compiles its checked callers and ticks at
\ both tiers, a reopened owner too; everywhere else they are refused by name.
\ code-publish, xref-retarget and does-record have no global row; every other
\ primitive here keeps its trusted-only one, so its TRUSTED: callers outside the
\ owner compile at both tiers. FIELD-PROJ! is sealed (REG-PROTECT), so TYPE-DECL's
\ admission is measured before the seal, in test/prim-owner-scope-prepare.f,
\ where a refusal stops the window before the first line here.
: NPUB-SITE-LINES ( -- )
   s" t0 code-publish inside owner"          s" compiled" CASE+
   s" t0 xref-retarget inside owner"         s" compiled" CASE+
   s" t0 does-record inside owner"           s" compiled" CASE+
   s" t0 callmap-set inside owner"           s" compiled" CASE+
   s" t0 tick code-publish inside owner"     s" compiled" CASE+
   s" t0 code-publish top level"             s" rejected" CASE+
   s" t0 tick code-publish top level"        s" rejected" CASE+
   s" t0 does-record top level"              s" rejected" CASE+
   s" t0 xref-retarget other package"        s" rejected" CASE+
   s" t0 callmap-set other package"          s" rejected" CASE+
   s" t0 callmap-set trusted top level"      s" compiled" CASE+
   s" t1 code-publish inside owner"          s" compiled" CASE+
   s" t1 xref-retarget inside owner"         s" compiled" CASE+
   s" t1 does-record inside owner"           s" compiled" CASE+
   s" t1 callmap-set inside owner"           s" compiled" CASE+
   s" t1 xref-retarget top level"            s" rejected" CASE+
   s" t1 tick does-record other package"     s" rejected" CASE+
   s" t1 callmap-set trusted top level"      s" compiled" CASE+ ;

: RECLAIM-LINES ( -- )
   s" t0 reloc-maps-clear inside owner"        s" compiled" CASE+
   s" t0 tick reloc-maps-clear inside owner"   s" compiled" CASE+
   s" t0 reloc-maps-clear top level"           s" rejected" CASE+
   s" t0 tick reloc-maps-clear other package"  s" rejected" CASE+
   s" t0 reloc-maps-clear reopened owner"      s" compiled" CASE+
   s" t0 reloc-maps-clear trusted top level"   s" compiled" CASE+
   s" t1 reloc-maps-clear inside owner"        s" compiled" CASE+
   s" t1 reloc-maps-clear other package"       s" rejected" CASE+
   s" t1 reloc-maps-clear trusted top level"   s" compiled" CASE+ ;

: TOP-ROW-LINES ( -- )
   s" t0 set-top-check inside owner"            s" compiled" CASE+
   s" t0 set-top-check wrong hook inside owner" s" rejected" CASE+
   s" t0 tick set-top-check inside owner"       s" compiled" CASE+
   s" t0 set-top-check top level"               s" rejected" CASE+
   s" t0 tick set-top-check other package"      s" rejected" CASE+
   s" t0 set-top-check reopened owner"          s" compiled" CASE+
   s" t0 set-top-check trusted top level"       s" compiled" CASE+
   s" t1 set-top-check inside owner"            s" compiled" CASE+
   s" t1 set-top-check other package"           s" rejected" CASE+
   s" t1 set-top-check trusted top level"       s" compiled" CASE+ ;

: CK-OWNER-LINES ( -- )
   s" t0 check-does! inside owner"          s" compiled" CASE+
   s" t0 tick check-does! inside owner"     s" compiled" CASE+
   s" t0 check-does! top level"             s" rejected" CASE+
   s" t0 tick check-does! other package"    s" rejected" CASE+
   s" t0 check-does! reopened owner"        s" compiled" CASE+
   s" t0 check-does! trusted top level"     s" compiled" CASE+
   s" t1 check-does! inside owner"          s" compiled" CASE+
   s" t1 check-does! other package"         s" rejected" CASE+
   s" t1 check-does! trusted top level"     s" compiled" CASE+ ;

: TYPE-DECL-LINES ( -- )
   s" t0 field-proj! reopened owner after the seal" s" rejected" CASE+
   s" t0 field-proj! top level"                     s" rejected" CASE+
   s" t0 tick field-proj! other package"            s" rejected" CASE+
   s" t0 field-proj! trusted top level"             s" compiled" CASE+
   s" t1 field-proj! trusted top level"             s" compiled" CASE+ ;

: OWNER-SITE-LINES ( -- )
   NPUB-SITE-LINES
   RECLAIM-LINES
   TOP-ROW-LINES
   CK-OWNER-LINES
   TYPE-DECL-LINES ;

\ An internal primitive its owner's rows type: the owner's checked callers
\ compile at both tiers, nobody's tick does, and the global trusted-only row
\ refuses a checked caller outside the owner.
: OWNED-LINES ( -- )
   s" t0 package-scope! inside owner"      s" compiled" CASE+
   s" t0 tick package-scope! inside owner" s" rejected" CASE+
   s" t0 package-scope! top level"         s" rejected" CASE+
   s" t1 package-scope! inside owner"      s" compiled" CASE+
   s" t1 namespace-record inside owner"    s" compiled" CASE+
   s" t1 tick package-scope! inside owner" s" rejected" CASE+ ;

: EXPECT$ ( -- ptr u8 n )
   0 EXP-U !
   AXIOM-LINES
   PRIVATE-T0-LINES
   PRIVATE-T1-LINES
   TICK-T0-LINES
   TICK-T1-LINES
   SHADOW-LINES
   DUAL-T0-LINES
   DUAL-T1-LINES
   OWNER-T0-LINES
   OWNER-T1-LINES
   OWNER-SITE-LINES
   OWNED-LINES
   S\" prim-owner: ok\nwindow: 0\n" EXP+
   EXP EXP-U @ ;

\ Each refusal is asked for by its whole text, which also labels the case.
: ERR-HAS ( ptr u8 n ptr u8 n -- ) {: ea:ptr eu:n la:ptr lu:n :}
   la lu T-LABEL
   ea eu la lu CONTAINS? TTRUE ;

\ The engine owners' refusals, by name: a primitive with no global row is
\ undefined outside its owner, to a call and a tick alike, and a global
\ trusted-only row answers E-CAP-TRUSTED. Inside TOP-ROW a hook of the wrong
\ shape fails on its type, not its trust. After the seal FIELD-PROJ! is an
\ internal engine word: undefined to a call, refused to a tick.
: OWNER-SITE-ERRS ( ptr u8 n -- ) {: ea:ptr eu:n :}
   ea eu s" E-UNDEFINED habu: in pos-cp0-top: undefined word 'code-publish'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tcp0-top: undefined word 'code-publish'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-dr0-top: undefined word 'does-record'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-xr0-oth: undefined word 'xref-retarget'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-xr1-top: undefined word 'xref-retarget'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tdr1-oth: undefined word 'does-record'" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-cm0-oth: 'callmap-set' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-rm0-top: 'reloc-maps-clear' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-rm1-oth: 'reloc-maps-clear' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-stc0-top: 'set-top-check' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-stc1-oth: 'set-top-check' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-cd0-top: 'CHECK-DOES!' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-cd1-oth: 'CHECK-DOES!' is a trust-boundary primitive" ERR-HAS
   ea eu s" habu: in pos-stc0-bad: at 'set-top-check' expected:" ERR-HAS
   ea eu s" E-UNDEFINED: FIELD-PROJ!" ERR-HAS
   ea eu s" hb: internal engine word: FIELD-PROJ!" ERR-HAS ;

: WINDOW ( -- )
   ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N {: oa:ptr ou:n :}
   ERR erru LEN>N {: ea:ptr eu:n :}
   rc 0 <> if oa ou type ea eu type cr then
   rc 0 T=
   s" owner-scope transcript" T-LABEL
   oa ou EXPECT$ T$=
   \ A private-only row refuses outside callers by name, and a tick outside the
   \ owner gets the call's refusal. The window's prefix words carry no ABI-only
   \ row (RUNNING-ENGINE below says where the product's come from), so outside
   \ the owner each name is undefined.
   ea eu s" E-UNDEFINED habu: in pos-p2c0-top: undefined word 'FFI-PTR>CELL'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-c2p0-oth: undefined word 'FFI-CELL>PTR'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-p2c1-top: undefined word 'FFI-PTR>CELL'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tp2c0-top: undefined word 'FFI-PTR>CELL'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tc2p0-oth: undefined word 'FFI-CELL>PTR'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tp2c1-top: undefined word 'FFI-PTR>CELL'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-tc2p1-oth: undefined word 'FFI-CELL>PTR'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-st0-top: undefined word 'set-tier'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-st0-oth: undefined word 'set-tier'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-st1-top: undefined word 'set-tier'" ERR-HAS
   ea eu s" E-UNDEFINED habu: in pos-st1-oth: undefined word 'set-tier'" ERR-HAS
   \ The shadowed global is still an internal engine word.
   ea eu s" hb: internal engine word: CORE-FOLD-C" ERR-HAS
   \ A dual row's global row keeps the named capability reject outside the owner.
   ea eu s" E-CAP-TRUSTED habu: in pos-ffi-top: 'ffi-call-bounded' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-am0-top: 'addrmap-set' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-am0-oth: 'addrmap-set' is a trust-boundary primitive" ERR-HAS
   ea eu s" E-CAP-TRUSTED habu: in pos-am1-top: 'addrmap-set' is a trust-boundary primitive" ERR-HAS
   ea eu OWNER-SITE-ERRS
   \ An owned primitive's global row refuses a checked caller outside its owner.
   ea eu s" E-CAP-TRUSTED habu: in pos-ps0-top: 'package-scope!' is a trust-boundary primitive" ERR-HAS ;

\ ---- the running engine -------------------------------------------------------
\ A user's checked definition outside the owner, compiled by the engine that runs
\ this suite. Each runs in its own fork, so a refusal ends only that fork. RUN is
\ called at top level, so a case is top-level source and may open a package.
\
\ THE SHIPPED ANSWER IS A DIFFERENT CODE, still naming the word. The FFI pair
\ are words of the engine prefix, and a build's tier-1 window records every
\ prefix word's declaration as an ABI-only global row (the tier-0 window above
\ does not; test/field-proj-boundary-prepare.f replays one for that reason). A
\ checked caller that binds such a row, with no primitive row behind it, gets
\ the trust-boundary reject. A checked tick gets the same answer as the call, so
\ neither record yields an xt outside its owner. Nor does an EXPORT alias: it
\ carries its source's row under its source's tail, and its call and its tick
\ answer as the source's do.
: SEALED ( ptr u8 n ptr u8 n -- ) {: sa:ptr su:n la:ptr lu:n :}
   sa su OUT IO-CAP >LEN ERR IO-CAP >LEN SUBJECT-MS >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   la lu T-LABEL
   rc REJECT-RC T=
   ERR erru LEN>N la lu ERR-HAS ;

: RUNNING-ENGINE ( -- )
   s" : POS-SEALED-P2C ( ptr a -- n ) FFI-PTR>CELL ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-p2c: 'FFI-PTR>CELL' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-C2P ( n -- ptr u8 ) FFI-CELL>PTR ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-c2p: 'FFI-CELL>PTR' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-TP2C ( -- ) ['] FFI-PTR>CELL drop ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-tp2c: 'FFI-PTR>CELL' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-TC2P ( -- ) ['] FFI-CELL>PTR drop ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-tc2p: 'FFI-CELL>PTR' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" package POS-ALIAS public EXPORT FFI-PTR>CELL ;package : POS-SEALED-ALIAS ( ptr a -- n ) POS-ALIAS:FFI-PTR>CELL ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-alias: 'POS-ALIAS:FFI-PTR>CELL' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" package POS-ALIAS public EXPORT FFI-PTR>CELL ;package : POS-SEALED-TALIAS ( -- ) ['] POS-ALIAS:FFI-PTR>CELL drop ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-talias: 'POS-ALIAS:FFI-PTR>CELL' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-ST ( n -- ) set-tier ;"
   s" E-UNDEFINED habu: in pos-sealed-st: undefined word 'set-tier'" SEALED
   s" : POS-SEALED-XSW ( ptr u8 n n -- ptr n ) xref-search-wl ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-xsw: 'xref-search-wl' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-MIM ( n n -- ) min-in-mark ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-mim: 'min-in-mark' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED
   s" : POS-SEALED-NDA ( n -- ) ndict-append ;"
   s" E-CAP-TRUSTED habu: in pos-sealed-nda: 'ndict-append' is a trust-boundary primitive; call it only from a TRUSTED: definition" SEALED ;

\ set-tier is a seed primitive, not a prefix word, so no ABI-only row stands in
\ for its owner's: outside TIER the shipped answer is E-UNDEFINED, and the
\ owner's public word is how a user's checked definition selects a tier.
: TIER-SELECT-RUNS ( -- )
   s" require lib/tier.f : POS-OPEN-TS ( n -- ) TIER:SELECT ; 1 POS-OPEN-TS tier@ ."
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUBJECT-MS >MS SUBJECT:RUN
   PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   s" a checked caller of TIER:SELECT selects tier 1" T-LABEL
   rc 0 T=
   OUT outu LEN>N S\" 1\n" T$= ;

\ NPUB's private-only primitives are engine primitives, not prefix words, so the
\ build records no ABI-only row for them: the shipped engine refuses a checked
\ call or tick outside NPUB as the window does, by the undefined name.
: SITE-RUNNING-ENGINE ( -- )
   s" : POS-SEALED-CP ( ptr u8 n n -- ) code-publish ;"
   s" E-UNDEFINED habu: in pos-sealed-cp: undefined word 'code-publish'" SEALED
   s" : POS-SEALED-TCP ( -- ) ['] code-publish drop ;"
   s" E-UNDEFINED habu: in pos-sealed-tcp: undefined word 'code-publish'" SEALED
   s" : POS-SEALED-XR ( n n n -- ) xref-retarget ;"
   s" E-UNDEFINED habu: in pos-sealed-xr: undefined word 'xref-retarget'" SEALED
   s" : POS-SEALED-DR ( n n -- ) does-record ;"
   s" E-UNDEFINED habu: in pos-sealed-dr: undefined word 'does-record'" SEALED ;

public
: RUN ( -- )
   T-RESET
   RUNNING-ENGINE
   TIER-SELECT-RUNS
   SITE-RUNNING-ENGINE
   [: PREPARE WINDOW ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;
;package

PRIM-OWNER-SCOPE-SUITE:RUN
