\ prim-owner-scope-child.f - what a package-OWNED primitive row admits, and
\ where, once the production seal has run. Runs inside
\ test/native-window-owner-child.f's reset window, which re-includes
\ src/core/checker.f; test/prim-owner-scope-prepare.f declared this fixture's
\ rows and ran src/core/internal-mark.f over the window, so every record below
\ is classified exactly as the product build classifies it.

require src/lib/errors.f

\ Nothing here is EXECUTED: every compile case defines a word and throws the
\ body away, and the shadowed global's token is refused before it runs.
\ Compiling a candidate through evaluate-closed: a text that opens or closes a
\ package moves the live package the next case is measured in, and set-tier
\ picks the compiler. EVC reports the reject code instead of letting it exit the
\ window. A refusal's throw puts the text's two cells back, so EVC evaluates a
\ copy and drops them on either path: this closed program may leave nothing.
: EVC ( ptr u8 n -- n ) [: 2dup evaluate-closed ;] catch {: rc:n :} 2drop rc ;

package PRIM-OWNER-CHILD

$0A constant LF-C

: T0? ( -- bool ) HB-TARGET-LINUX-X86-64? 0= ;
: RESTORE-T0 ( -- ) T0? if 0 set-tier then ;

: VERDICT$ ( n -- ptr u8 n ) {: v:n :}
   v -1 = if s" admitted" exit then
   v 0 = if s" refused" exit then
   v 1 = if s" unresolvable" exit then
   s" unexpected" ;

: OUTCOME$ ( n -- ptr u8 n ) {: rc:n :}
   rc 0 = if s" compiled" exit then
   rc 70 = if s" rejected" exit then
   s" unexpected" ;

\ The checker admits a tick of an owned internal word inside its owner, through
\ the owner's row; the compiler then refuses the token, tier 0 with the
\ compile-reject rc and tier 1's elaborator with E-NELAB-QUOT.
: TICK-OUTCOME$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-NELAB-QUOT = if s" rejected" exit then
   rc OUTCOME$ ;

\ The label is written BEFORE the subject runs: a rejected case raises through
\ the diagnostic renderer, after which the caller's label string read back
\ empty.
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

: EVAL-TICK ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n sa:ptr su:n :}
   la lu LABEL
   sa su EVC TICK-OUTCOME$ type LF-C emit ;

: OWNER-OPEN ( -- ) s" package PRIM-OWNER-SCOPE" evaluate-closed ;
: OTHER-OPEN ( -- ) s" package PRIM-OWNER-OTHER" evaluate-closed ;
: FFI-OPEN ( -- ) s" package FFI" evaluate-closed ;
: NPUB-OPEN ( -- ) s" package NPUB" evaluate-closed ;
: OVERLAY-OPEN ( -- ) s" package CHECKER-OVERLAY" evaluate-closed ;
: PKG-CLOSE ( -- ) s" ;package" evaluate-closed ;

\ The fresh axiom PRIM-OWNER-AXIOM, asked of the checker only
\ (CHECK-CANDIDATE!): an owner-private row binds only as its owner's view of the
\ record the engine's lookup binds (src/core/checker.f CK-REC-BIND), so a row
\ for a word the dictionary does not carry binds nowhere, inside its owner too.
: AXIOM-CASES ( -- )
   OWNER-OPEN
   s" axiom inside owner"
   s" POS-AX-IN ( n -- ) PRIM-OWNER-AXIOM" CAND
   PKG-CLOSE ;

\ Owner-private typed rows (FFI-PTR>CELL, FFI-CELL>PTR): the owner, reopened
\ too, compiles a checked caller at both tiers. The window's scan of their empty
\ bodies leaves an ABI-only global row, so every other scope gets E-CAP-TRUSTED.
\ That row grants nothing: a seal that read only the top-level row would mark
\ each record DNAME-INT and refuse the owner's call too. Every definition name
\ is used once: a second definition of a name is a duplicate, not a second
\ measurement. The first `package` of FFI and NPUB in this window creates it;
\ each later one reopens it.
: PRIVATE-T0-CASES ( -- )
   0 set-tier
   FFI-OPEN
   s" t0 ptr>cell inside owner"
   s" : POS-P2C0-IN ( ptr a -- n ) FFI-PTR>CELL ;" EVAL
   s" t0 cell>ptr inside owner"
   s" : POS-C2P0-IN ( n -- ptr u8 ) FFI-CELL>PTR ;" EVAL
   PKG-CLOSE
   s" t0 ptr>cell top level"
   s" : POS-P2C0-TOP ( ptr a -- n ) FFI-PTR>CELL ;" EVAL
   s" t0 cell>ptr top level"
   s" : POS-C2P0-TOP ( n -- ptr u8 ) FFI-CELL>PTR ;" EVAL
   OTHER-OPEN
   s" t0 cell>ptr other package"
   s" : POS-C2P0-OTH ( n -- ptr u8 ) FFI-CELL>PTR ;" EVAL
   PKG-CLOSE
   FFI-OPEN
   s" t0 ptr>cell reopened owner"
   s" : POS-P2C0-RE ( ptr a -- n ) FFI-PTR>CELL ;" EVAL
   PKG-CLOSE ;

\ Tier 1 reads a callee's cell widths through the checker's scope, so the owner's
\ row is the only one that can answer, and the call binds the sealed record.
: PRIVATE-T1-CASES ( -- )
   1 set-tier
   FFI-OPEN
   s" t1 ptr>cell inside owner"
   s" : POS-P2C1-IN ( ptr a -- n ) FFI-PTR>CELL ;" EVAL
   s" t1 cell>ptr inside owner"
   s" : POS-C2P1-IN ( n -- ptr u8 ) FFI-CELL>PTR ;" EVAL
   PKG-CLOSE
   s" t1 ptr>cell top level"
   s" : POS-P2C1-TOP ( ptr a -- n ) FFI-PTR>CELL ;" EVAL
   RESTORE-T0 ;

\ A checked `[']` of the name compiles where a checked call of it compiles, and
\ elsewhere gets the call's refusal of the name. Every owner block here reopens
\ its package, which the cases above created.
: TICK-T0-CASES ( -- )
   0 set-tier
   FFI-OPEN
   s" t0 tick ptr>cell inside owner"
   s" : POS-TP2C0-IN ( -- ) ['] FFI-PTR>CELL drop ;" EVAL
   s" t0 tick cell>ptr inside owner"
   s" : POS-TC2P0-IN ( -- ) ['] FFI-CELL>PTR drop ;" EVAL
   PKG-CLOSE
   s" t0 tick ptr>cell top level"
   s" : POS-TP2C0-TOP ( -- ) ['] FFI-PTR>CELL drop ;" EVAL
   OTHER-OPEN
   s" t0 tick cell>ptr other package"
   s" : POS-TC2P0-OTH ( -- ) ['] FFI-CELL>PTR drop ;" EVAL
   PKG-CLOSE ;

: TICK-T1-CASES ( -- )
   1 set-tier
   FFI-OPEN
   s" t1 tick cell>ptr inside owner"
   s" : POS-TC2P1-IN ( -- ) ['] FFI-CELL>PTR drop ;" EVAL
   PKG-CLOSE
   s" t1 tick ptr>cell top level"
   s" : POS-TP2C1-TOP ( -- ) ['] FFI-PTR>CELL drop ;" EVAL
   OTHER-OPEN
   s" t1 tick cell>ptr other package"
   s" : POS-TC2P1-OTH ( -- ) ['] FFI-CELL>PTR drop ;" EVAL
   PKG-CLOSE
   RESTORE-T0 ;

\ A row a package declares for its own word: util.f's global CORE-FOLD-C is
\ still an internal engine word, since no row types that record, so its token
\ refuses.
: SHADOW-CASES ( -- )
   s" shadowed global top level"
   s" 65 CORE-FOLD-C drop" EVAL ;

\ The dual row on ffi-call-bounded (FFI) and addrmap-set (NPUB): inside the
\ owner the private row binds, so checked callers and ticks compile at both
\ tiers. Elsewhere the global PRIM-TRUSTED-ONLY! row binds, refusing both with
\ E-CAP-TRUSTED and admitting a TRUSTED: caller, whose tier-1 call window is
\ built from it: without it a TRUSTED: caller of addrmap-set outside NPUB is
\ E-HIR-UNMODELED.
: DUAL-T0-CASES ( -- )
   0 set-tier
   FFI-OPEN
   s" ffi call inside owner"
   s" : POS-FFI-IN ( ptr a ptr n n n -- n ) ffi-call-bounded ;" EVAL
   PKG-CLOSE
   s" ffi call top level"
   s" : POS-FFI-TOP ( ptr a ptr n n n -- n ) ffi-call-bounded ;" EVAL
   OTHER-OPEN
   s" ffi call other package"
   s" : POS-FFI-OTH ( ptr a ptr n n n -- n ) ffi-call-bounded ;" EVAL
   PKG-CLOSE
   s" t0 ffi call trusted top level"
   s" TRUSTED: POS-FFI0-TR ( ptr a ptr n n n -- n ) ffi-call-bounded ;" EVAL
   NPUB-OPEN
   s" t0 addrmap-set inside owner"
   s" : POS-AM0-IN ( n -- ) addrmap-set ;" EVAL
   s" t0 tick addrmap-set inside owner"
   s" : POS-TAM0-IN ( -- ) ['] addrmap-set drop ;" EVAL
   PKG-CLOSE
   s" t0 addrmap-set top level"
   s" : POS-AM0-TOP ( n -- ) addrmap-set ;" EVAL
   s" t0 tick addrmap-set top level"
   s" : POS-TAM0-TOP ( -- ) ['] addrmap-set drop ;" EVAL
   OTHER-OPEN
   s" t0 addrmap-set other package"
   s" : POS-AM0-OTH ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   NPUB-OPEN
   s" t0 addrmap-set reopened owner"
   s" : POS-AM0-RE ( n -- ) addrmap-set ;" EVAL
   PKG-CLOSE
   s" t0 addrmap-set trusted top level"
   s" TRUSTED: POS-AM0-TR ( n -- ) addrmap-set ;" EVAL ;

: DUAL-T1-CASES ( -- )
   1 set-tier
   s" t1 ffi call trusted top level"
   s" TRUSTED: POS-FFI1-TR ( ptr a ptr n n n -- n ) ffi-call-bounded ;" EVAL
   NPUB-OPEN
   s" t1 addrmap-set inside owner"
   s" : POS-AM1-IN ( n -- ) addrmap-set ;" EVAL
   s" t1 tick addrmap-set inside owner"
   s" : POS-TAM1-IN ( -- ) ['] addrmap-set drop ;" EVAL
   PKG-CLOSE
   s" t1 addrmap-set top level"
   s" : POS-AM1-TOP ( n -- ) addrmap-set ;" EVAL
   OTHER-OPEN
   s" t1 tick addrmap-set other package"
   s" : POS-TAM1-OTH ( -- ) ['] addrmap-set drop ;" EVAL
   PKG-CLOSE
   s" t1 addrmap-set trusted top level"
   s" TRUSTED: POS-AM1-TR ( n -- ) addrmap-set ;" EVAL
   RESTORE-T0 ;

\ The engine's own owners.
\ Each primitive here has one checked caller in the engine, a word of the
\ package that owns its row: NPUB's publication window and record writers
\ (src/compiler/native/publish.f), CODE-RECLAIM's relocation-map clear
\ (src/habu/xref.f), TOP-ROW's hook install (src/core/top-row.f), CHECKER-OWNER's
\ does> check (src/compiler/native/checker-owner.f) and TYPE-DECL's field
\ projection (src/core/sumtype.f). code-publish, xref-retarget and does-record
\ are their NPUB row alone, as no TRUSTED: body outside NPUB calls one: outside
\ it a checked call or tick misses the name. Every other primitive here keeps
\ its global trusted-only row beside its owner's for its TRUSTED: callers
\ outside the owner, so a checked caller there gets the named E-CAP-TRUSTED.
: RECLAIM-OPEN ( -- ) s" package CODE-RECLAIM" evaluate-closed ;
: TOP-ROW-OPEN ( -- ) s" package TOP-ROW" evaluate-closed ;
: CK-OWNER-OPEN ( -- ) s" package CHECKER-OWNER" evaluate-closed ;
: TYPE-DECL-OPEN ( -- ) s" package TYPE-DECL" evaluate-closed ;

\ NPUB exists from the dual-row cases above, so each block here reopens it.
: NPUB-SITE-T0-CASES ( -- )
   0 set-tier
   NPUB-OPEN
   s" t0 code-publish inside owner"
   s" : POS-CP0-IN ( ptr u8 n n -- ) code-publish ;" EVAL
   s" t0 xref-retarget inside owner"
   s" : POS-XR0-IN ( n n n -- ) xref-retarget ;" EVAL
   s" t0 does-record inside owner"
   s" : POS-DR0-IN ( n n -- ) does-record ;" EVAL
   s" t0 callmap-set inside owner"
   s" : POS-CM0-IN ( n -- ) callmap-set ;" EVAL
   s" t0 tick code-publish inside owner"
   s" : POS-TCP0-IN ( -- ) ['] code-publish drop ;" EVAL
   PKG-CLOSE
   s" t0 code-publish top level"
   s" : POS-CP0-TOP ( ptr u8 n n -- ) code-publish ;" EVAL
   s" t0 tick code-publish top level"
   s" : POS-TCP0-TOP ( -- ) ['] code-publish drop ;" EVAL
   s" t0 does-record top level"
   s" : POS-DR0-TOP ( n n -- ) does-record ;" EVAL
   s" t0 native-unit-publish top level"
   s" : POS-NUP0-TOP ( ptr u8 n ptr u8 n -- ) native-unit-publish ;" EVAL
   s" t0 unit-compile-run top level"
   s" : POS-UCR0-TOP ( [ ptr u8 n n n -- n ] [ -- ] -- n ) unit-compile-run ;" EVAL
   OTHER-OPEN
   s" t0 xref-retarget other package"
   s" : POS-XR0-OTH ( n n n -- ) xref-retarget ;" EVAL
   s" t0 callmap-set other package"
   s" : POS-CM0-OTH ( n -- ) callmap-set ;" EVAL
   PKG-CLOSE
   s" t0 callmap-set trusted top level"
   s" TRUSTED: POS-CM0-TR ( n -- ) callmap-set ;" EVAL ;

: NPUB-SITE-T1-CASES ( -- )
   1 set-tier
   NPUB-OPEN
   s" t1 code-publish inside owner"
   s" : POS-CP1-IN ( ptr u8 n n -- ) code-publish ;" EVAL
   s" t1 xref-retarget inside owner"
   s" : POS-XR1-IN ( n n n -- ) xref-retarget ;" EVAL
   s" t1 does-record inside owner"
   s" : POS-DR1-IN ( n n -- ) does-record ;" EVAL
   s" t1 callmap-set inside owner"
   s" : POS-CM1-IN ( n -- ) callmap-set ;" EVAL
   PKG-CLOSE
   s" t1 xref-retarget top level"
   s" : POS-XR1-TOP ( n n n -- ) xref-retarget ;" EVAL
   OTHER-OPEN
   s" t1 tick does-record other package"
   s" : POS-TDR1-OTH ( -- ) ['] does-record drop ;" EVAL
   PKG-CLOSE
   s" t1 callmap-set trusted top level"
   s" TRUSTED: POS-CM1-TR ( n -- ) callmap-set ;" EVAL
   RESTORE-T0 ;

: RECLAIM-CASES ( -- )
   T0? if
   0 set-tier
   RECLAIM-OPEN
   s" t0 reloc-maps-clear inside owner"
   s" : POS-RM0-IN ( n n -- ) reloc-maps-clear ;" EVAL
   s" t0 tick reloc-maps-clear inside owner"
   s" : POS-TRM0-IN ( -- ) ['] reloc-maps-clear drop ;" EVAL
   PKG-CLOSE
   s" t0 reloc-maps-clear top level"
   s" : POS-RM0-TOP ( n n -- ) reloc-maps-clear ;" EVAL
   OTHER-OPEN
   s" t0 tick reloc-maps-clear other package"
   s" : POS-TRM0-OTH ( -- ) ['] reloc-maps-clear drop ;" EVAL
   PKG-CLOSE
   RECLAIM-OPEN
   s" t0 reloc-maps-clear reopened owner"
   s" : POS-RM0-RE ( n n -- ) reloc-maps-clear ;" EVAL
   PKG-CLOSE
   s" t0 reloc-maps-clear trusted top level"
   s" TRUSTED: POS-RM0-TR ( n n -- ) reloc-maps-clear ;" EVAL
   then
   1 set-tier
   RECLAIM-OPEN
   s" t1 reloc-maps-clear inside owner"
   s" : POS-RM1-IN ( n n -- ) reloc-maps-clear ;" EVAL
   PKG-CLOSE
   OTHER-OPEN
   s" t1 reloc-maps-clear other package"
   s" : POS-RM1-OTH ( n n -- ) reloc-maps-clear ;" EVAL
   PKG-CLOSE
   s" t1 reloc-maps-clear trusted top level"
   s" TRUSTED: POS-RM1-TR ( n n -- ) reloc-maps-clear ;" EVAL
   RESTORE-T0 ;

\ The owner's row types the hook: an xt of the shape the top-level tracker calls,
\ where the global row takes any cell. A hook of another shape is refused.
: TOP-ROW-CASES ( -- )
   T0? if
   0 set-tier
   TOP-ROW-OPEN
   s" t0 set-top-check inside owner"
   s" : POS-STC0-IN ( [ ptr u8 n n n -- ] -- ) set-top-check ;" EVAL
   s" t0 set-top-check wrong hook inside owner"
   s" : POS-STC0-BAD ( [ ptr u8 n n -- ] -- ) set-top-check ;" EVAL
   s" t0 tick set-top-check inside owner"
   s" : POS-TSTC0-IN ( -- ) ['] set-top-check drop ;" EVAL
   PKG-CLOSE
   s" t0 set-top-check top level"
   s" : POS-STC0-TOP ( n -- ) set-top-check ;" EVAL
   OTHER-OPEN
   s" t0 tick set-top-check other package"
   s" : POS-TSTC0-OTH ( -- ) ['] set-top-check drop ;" EVAL
   PKG-CLOSE
   TOP-ROW-OPEN
   s" t0 set-top-check reopened owner"
   s" : POS-STC0-RE ( [ ptr u8 n n n -- ] -- ) set-top-check ;" EVAL
   PKG-CLOSE
   s" t0 set-top-check trusted top level"
   s" TRUSTED: POS-STC0-TR ( n -- ) set-top-check ;" EVAL
   then
   1 set-tier
   TOP-ROW-OPEN
   s" t1 set-top-check inside owner"
   s" : POS-STC1-IN ( [ ptr u8 n n n -- ] -- ) set-top-check ;" EVAL
   s" t1 set-top-check wrong hook inside owner"
   s" : POS-STC1-BAD ( [ ptr u8 n n -- ] -- ) set-top-check ;" EVAL
   PKG-CLOSE
   OTHER-OPEN
   s" t1 set-top-check other package"
   s" : POS-STC1-OTH ( n -- ) set-top-check ;" EVAL
   PKG-CLOSE
   s" t1 set-top-check trusted top level"
   s" TRUSTED: POS-STC1-TR ( n -- ) set-top-check ;" EVAL
   RESTORE-T0 ;

\ CHECK-DOES! is a word of the re-included src/core/checker.f, and its global
\ row keeps the seal from marking it DNAME-INT, so its owner calls it here.
: CK-OWNER-CASES ( -- )
   T0? if
   0 set-tier
   CK-OWNER-OPEN
   s" t0 check-does! inside owner"
   s" : POS-CD0-IN ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   s" t0 tick check-does! inside owner"
   s" : POS-TCD0-IN ( -- ) ['] CHECK-DOES! drop ;" EVAL
   PKG-CLOSE
   s" t0 check-does! top level"
   s" : POS-CD0-TOP ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   OTHER-OPEN
   s" t0 tick check-does! other package"
   s" : POS-TCD0-OTH ( -- ) ['] CHECK-DOES! drop ;" EVAL
   PKG-CLOSE
   CK-OWNER-OPEN
   s" t0 check-does! reopened owner"
   s" : POS-CD0-RE ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   PKG-CLOSE
   s" t0 check-does! trusted top level"
   s" TRUSTED: POS-CD0-TR ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   then
   1 set-tier
   CK-OWNER-OPEN
   s" t1 check-does! inside owner"
   s" : POS-CD1-IN ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   PKG-CLOSE
   OTHER-OPEN
   s" t1 check-does! other package"
   s" : POS-CD1-OTH ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   PKG-CLOSE
   s" t1 check-does! trusted top level"
   s" TRUSTED: POS-CD1-TR ( ptr u8 n ptr u8 n -- n ) CHECK-DOES! ;" EVAL
   RESTORE-T0 ;

\ FIELD-PROJ! is REG-PROTECT (src/core/checker.f): the owner's checked call
\ compiles before the seal in test/prim-owner-scope-prepare.f. After the seal,
\ both tiers refuse the reopened owner's call to the internal word as undefined
\ (rc 70). Outside the owner, the global trusted-only row
\ refuses checked calls and ticks, while a TRUSTED: body still compiles
\ (src/core/checker.f FIELD-PROJ!).
: TYPE-DECL-CASES ( -- )
   T0? if
   0 set-tier
   TYPE-DECL-OPEN
   s" t0 field-proj! reopened owner after the seal"
   s" : POS-FP0-RE ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   PKG-CLOSE
   s" t0 field-proj! top level"
   s" : POS-FP0-TOP ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   OTHER-OPEN
   s" t0 tick field-proj! other package"
   s" : POS-TFP0-OTH ( -- ) ['] FIELD-PROJ! drop ;" EVAL
   PKG-CLOSE
   s" t0 field-proj! trusted top level"
   s" TRUSTED: POS-FP0-TR ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   then
   1 set-tier
   TYPE-DECL-OPEN
   s" t1 field-proj! reopened owner after the seal"
   s" : POS-FP1-RE ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   PKG-CLOSE
   s" t1 field-proj! top level"
   s" : POS-FP1-TOP ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   OTHER-OPEN
   s" t1 tick field-proj! other package"
   s" : POS-TFP1-OTH ( -- ) ['] FIELD-PROJ! drop ;" EVAL
   PKG-CLOSE
   s" t1 field-proj! trusted top level"
   s" TRUSTED: POS-FP1-TR ( ptr u8 n n n -- ) FIELD-PROJ! ;" EVAL
   RESTORE-T0 ;

: OWNER-SITE-CASES ( -- )
   T0? if NPUB-SITE-T0-CASES then
   NPUB-SITE-T1-CASES
   RECLAIM-CASES
   TOP-ROW-CASES
   CK-OWNER-CASES
   TYPE-DECL-CASES ;

\ Internal engine primitives a package row types: package-scope! and
\ namespace-record carry DNAME-INT|DNAME-OWNED and are CHECKER-OVERLAY's
\ (src/habu/prims.f). The owner's checked callers compile at both tiers; a
\ checked caller elsewhere gets the global trusted-only row's reject. A tick
\ stays refused at both tiers, the owner's included, as every internal word's
\ is: an xt would carry the call out of the owner.
: OWNED-CASES ( -- )
   0 set-tier
   OVERLAY-OPEN
   s" t0 package-scope! inside owner"
   s" : POS-PS0-IN ( n n -- ) package-scope! ;" EVAL
   s" t0 tick package-scope! inside owner"
   s" : POS-TPS0-IN ( -- ) ['] package-scope! drop ;" EVAL-TICK
   PKG-CLOSE
   s" t0 package-scope! top level"
   s" : POS-PS0-TOP ( n n -- ) package-scope! ;" EVAL
   1 set-tier
   OVERLAY-OPEN
   s" t1 package-scope! inside owner"
   s" : POS-PS1-IN ( n n -- ) package-scope! ;" EVAL
   s" t1 namespace-record inside owner"
   s" : POS-NS1-IN ( ptr u8 n bool -- n ) namespace-record ;" EVAL
   s" t1 tick package-scope! inside owner"
   s" : POS-TPS1-IN ( -- ) ['] package-scope! drop ;" EVAL-TICK
   PKG-CLOSE
   0 set-tier ;

public

: RUN ( -- )
   AXIOM-CASES
   T0? if PRIVATE-T0-CASES then
   PRIVATE-T1-CASES
   T0? if TICK-T0-CASES then
   TICK-T1-CASES
   SHADOW-CASES
   T0? if DUAL-T0-CASES then
   DUAL-T1-CASES
   OWNER-SITE-CASES
   OWNED-CASES
   s" prim-owner: ok" type LF-C emit ;

;package

PRIM-OWNER-CHILD:RUN
