\ decl-replay-verify-source.f — does src/habu/verify-source.f rebuild the same
\ registry a real declaration builds?
\
\ CONCERN. verify-source reads a file's declarations without executing it, so
\ every tool built on it (the all-errors reporter, the fixpoint builder, the
\ tree shaker, the public-signature dumper, the AOT closure walker) sees types
\ only through what its RECORD-* arms register. If that view drifts from what
\ the declaration itself would have registered, those tools disagree with the
\ engine about what a family IS, and the disagreement is silent.
\
\ Two arms are under test. RECORD-ENUM used to drive sumtype.f's CHECKER-DEFENUM
\ — the legacy metadata-only entry, which the type-DSL cutover deletes and which
\ only ever understood a compact list of bare variant names. There was no arm for
\ the unified STRUCTURE at all, so a STRUCTURE family was simply invisible on
\ this path. Both now drive the front ends' registration-only replay entries.
\
\ HOW THE COMPARISON IS MADE. The same declaration text is registered twice: once
\ by executing it (the live front end), once by handing it to
\ VERIFY:SOURCE-BUF-IN-SCOPE. Two different packages, so both families exist at
\ once and can be read side by side. Then every field the registry exposes for a
\ family is compared between the two ids:
\   family:   kind (product/sum/enum), arity, declared width, layout policy,
\             derive-eq, derive-hash, variant count, field count
\   variants: name and tag of every row, in declaration order, plus the
\             constructor package stamped on that row
\   fields:   the TYPE-FIELD row count keyed (family, variant), and each row's
\             name, schema root, slot, and cell width
\ The ONE intended difference is the constructor SYMBOL: executing a declaration
\ renders its constructor words, replaying it must not, so the live rows carry a
\ symbol and the replayed rows carry zero. That asymmetry is asserted, not
\ tolerated — it is the property that makes the replay safe to run inside a tool
\ that is reading source rather than building a program.
\
\ IN-SCOPE, deliberately: VERIFY:SOURCE-BUF wraps its run in a candidate scope
\ that rolls every registration back, which is right for validating a candidate
\ and useless for inspecting what was registered. SOURCE-BUF-IN-SCOPE is the
\ entry the in-process tools use when the registrations must persist, and it is
\ the one whose result this file can actually read.

require test/checker-assert.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture
require src/habu/verify-source.f

package VSPARITY

variable #FAIL
variable #CASE

\ --- the registry reflection surface. Every one is a read; none mutates.
\ TRUSTED: because each forwards to a sealed pre-hook registry word the checker
\ cannot type from a post-hook checked body — the same idiom the declaration
\ suites use for their own reflection helpers.
TRUSTED: FAMID ( ptr u8 n -- n ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop ;
TRUSTED: F-KIND ( n -- n ) TFAM-KIND@ ;
TRUSTED: F-ARITY ( n -- n ) TFAM-ARITY@ ;
TRUSTED: F-WIDTH ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: F-POLICY ( n -- n ) TFAM-LAYOUT-POLICY@ ;
\ compared as raw registry flag values, not used as conditions, so these
\ answer the stored cell rather than a bool.
TRUSTED: F-EQ ( n -- n ) TFAM-DERIVE-EQ? ;
TRUSTED: F-HASH ( n -- n ) TFAM-DERIVE-HASH? ;
TRUSTED: F-VSTART ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: F-VCOUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: F-FCOUNT ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: V-NAME$ ( n -- ptr u8 n ) SUMV-NAME$ ;
TRUSTED: V-TAG ( n -- n ) SUMV-TAG@ ;
TRUSTED: V-PKG$ ( n -- ptr u8 n ) SUMV-CTOR-PKG$ ;
TRUSTED: V-SYM ( n -- n ) SUMV-CTOR-SYM@ ;
TRUSTED: R-FAM ( n -- n ) TYPE-FIELD:FAMILY@ ;
TRUSTED: R-VAR ( n -- n ) TYPE-FIELD:VARIANT@ ;
TRUSTED: R-NAME$ ( n -- ptr u8 n ) PF-NAME$ ;
TRUSTED: R-SCH ( n -- n ) PF-SCH@ ;
TRUSTED: R-SLOT ( n -- n ) PF-SLOT@ ;
TRUSTED: R-CELLS ( n -- n ) PF-CELLS@ ;
TRUSTED: R-TOTAL ( -- n ) TYPE-FIELD:COUNT ;

variable AV   variable BV
variable SI   variable SJ
variable ACC

public

: FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want = IF EXIT THEN
   FAIL
   s" assert: expected " type want . cr
   s" got " type got . cr ;

: T-TRUE ( bool -- ) {: b:bool :}
   #CASE @ 1 + #CASE !
   b IF EXIT THEN
   FAIL
   s" assert: expected true" type cr ;

private

\ --- field-row walking. TYPE-FIELD rows are a flat table keyed (family,
\ variant), so a family's rows are found by scanning for that key; the Nth match
\ in table order is the Nth field in declaration order.
: ROWS ( n n -- n ) {: fam:n vid:n :}
   0 ACC !   0 SI !
   BEGIN SI @ R-TOTAL < WHILE
      SI @ R-FAM fam = SI @ R-VAR vid = and IF ACC @ 1 + ACC ! THEN
      SI @ 1 + SI !
   REPEAT
   ACC @ ;

: ROW-AT ( n n n -- n ) {: fam:n vid:n want:n :}   \ table index of the want'th row, or -1
   0 ACC !   0 SI !
   BEGIN SI @ R-TOTAL < WHILE
      SI @ R-FAM fam = SI @ R-VAR vid = and IF
         ACC @ want = IF SI @ EXIT THEN
         ACC @ 1 + ACC !
      THEN
      SI @ 1 + SI !
   REPEAT
   -1 ;

: SAME-FIELDS ( n n n n -- ) {: af:n av:n bf:n bv:n :}
   af av ROWS bf bv ROWS T=
   0 SJ !
   BEGIN SJ @ af av ROWS < WHILE
      af av SJ @ ROW-AT {: ai:n :}
      bf bv SJ @ ROW-AT {: bi:n :}
      ai 0 >= T-TRUE
      bi 0 >= T-TRUE
      ai R-NAME$ bi R-NAME$ CORE-STR= T-TRUE
      ai R-SCH   bi R-SCH   T=
      ai R-SLOT  bi R-SLOT  T=
      ai R-CELLS bi R-CELLS T=
      SJ @ 1 + SJ !
   REPEAT ;

: SAME-VARIANTS ( n n -- ) {: af:n bf:n :}
   af F-VCOUNT bf F-VCOUNT T=
   af F-VSTART AV !
   bf F-VSTART BV !
   0 SI !
   BEGIN SI @ af F-VCOUNT < WHILE
      AV @ SI @ + {: a:n :}
      BV @ SI @ + {: b:n :}
      a V-NAME$ b V-NAME$ CORE-STR= T-TRUE
      a V-TAG b V-TAG T=
      \ The constructor package is DERIVED from the declaring package and the
      \ family name, and the two copies must live in different packages to
      \ coexist, so the two stamps differ by construction. What matters is that
      \ the replayed row carries one at all: that stamp is the registration a
      \ later FAMILY:VARIANT resolves through, and it is exactly what would be
      \ lost by skipping the arming instead of the rendering.
      b V-PKG$ nip 0 > T-TRUE
      a V-PKG$ nip 0 > T-TRUE
      \ the one intended asymmetry: words rendered live, never on replay
      a V-SYM 0 <> T-TRUE
      b V-SYM 0 T=
      af a bf b SAME-FIELDS
      SI @ 1 + SI !
   REPEAT ;

public

\ COMPARE ( live-name replayed-name -- ) : assert two families are registered
\ identically, field by field.
: COMPARE ( ptr u8 n ptr u8 n -- ) {: aa:ptr au:n ba:ptr bu:n :}
   aa au FAMID {: af:n :}
   ba bu FAMID {: bf:n :}
   af F-KIND   bf F-KIND   T=
   af F-ARITY  bf F-ARITY  T=
   af F-WIDTH  bf F-WIDTH  T=
   af F-POLICY bf F-POLICY T=
   af F-EQ     bf F-EQ     T=
   af F-HASH   bf F-HASH   T=
   af F-FCOUNT bf F-FCOUNT T=
   af bf SAME-VARIANTS ;

\ VS-LOAD ( source -- ) : register a source's declarations the way the
\ in-process tools do, without executing it.
TRUSTED: VS-LOAD ( ptr u8 n -- ) VERIFY:SOURCE-BUF-IN-SCOPE ;

: VCOUNT ( ptr u8 n -- n ) FAMID F-VCOUNT ;
: FCOUNT ( ptr u8 n -- n ) FAMID F-FCOUNT ;

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" decl-replay-verify-source: failures" 1 die ;

\ --- source builder, for the bodies that have to be too long to write out.
$4000 constant SRC-CAP        \ 16384: comfortably past BODYBUF-CAP (8000)
9001 constant E-SRCB-CAP      \ fixture builder overflow (harness bound, not a product code)
create SRC-BUF SRC-CAP allot
variable SRC-U

\ Same `create`-region boundary the product code documents: a checked body
\ cannot address a `create` region as a typed `ptr u8` span.
TRUSTED: SRC-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF E-SRCB-CAP throw THEN
   c SRC-BUF SRC-U @ + c!
   SRC-U @ 1 + SRC-U ! ;
TRUSTED: SRC$ ( -- ptr u8 n ) SRC-BUF SRC-U @ ;

variable SB-I   variable SB-J
: SRC-RESET ( -- ) 0 SRC-U ! ;
: SRC-PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SB-I !
   BEGIN SB-I @ u < WHILE
      a SB-I @ + c@ SRC-C
      SB-I @ 1 + SB-I !
   REPEAT ;

\ The explicit-list result distinguishes a bare zero-arity head from an explicit
\ empty list. Parsing must also own the binder bytes: reusing the caller's source
\ buffer before lookup cannot change the installed map.
: BINDER-SOURCE-REUSE ( -- )
   s" plain" DECL-HEAD:PARSE 0= T-TRUE 0 T= s" plain" CORE-STR= T-TRUE
   s" empty<>" DECL-HEAD:PARSE T-TRUE 0 T= s" empty" CORE-STR= T-TRUE
   SRC-RESET
   s" pair<e,a>" SRC-PUT
   SRC$ DECL-HEAD:PARSE T-TRUE 2 T= s" pair" CORE-STR= T-TRUE
   SRC-RESET
   s" xxxxxxxxx" SRC-PUT
   s" e" DECL-HEAD:PARAM? T-TRUE 0 T=
   s" a" DECL-HEAD:PARAM? T-TRUE 1 T= ;

BINDER-SOURCE-REUSE

: SRC-DIGIT ( n -- ) 48 + SRC-C ;
: SRC-VARIANT ( n -- ) {: i:n :}      \ one `vNNNN ` variant name: 6 bytes
   118 SRC-C
   i 1000 / 10 mod SRC-DIGIT
   i 100 / 10 mod SRC-DIGIT
   i 10 / 10 mod SRC-DIGIT
   i 10 mod SRC-DIGIT
   32 SRC-C ;

\ ENUM-SOURCE ( stem n -- source ) : a compact ENUM of n distinct variants,
\ declared in its own package so successive cases cannot collide on the name.
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

\ VSTRY ( source -- code ) : register under catch, so a refusal is a value.
: VS-RUN ( ptr u8 n -- ) VS-LOAD ;
TRUSTED: VSTRY ( ptr u8 n -- n ) ['] VS-RUN catch ;

\ The over-cap declaration must leave NO family behind, not a short one.
TRUSTED: FAM-FIND ( ptr u8 n -- n bool ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE ;
: CAP-FAMILY-ABSENT ( -- )
   s" capb:big" FAM-FIND 0= T-TRUE drop ;

\ A `\` comment ends at a newline, so this source needs a real one embedded.
: BACKSLASH-ENUM-SOURCE ( -- ptr u8 n )
   SRC-RESET
   s" package vr public ENUM cm3 red \ trailing note" SRC-PUT
   10 SRC-C
   s" green ;ENUM ;package" SRC-PUT
   SRC$ ;

;package

\ ---------------------------------------------------------------------------
\ 1. STRUCTURE. Executed live in package `sdlive`, replayed through
\    verify-source into package `sdrep`. Before this arm existed the replayed
\    family did not exist at all, so every assertion below is new coverage.
\ ---------------------------------------------------------------------------
package sdlive
public
STRUCTURE pair<e,a>
   FIELD lo e
   FIELD hi a
;STRUCTURE
;package

s" package sdrep public STRUCTURE pair<e,a> FIELD lo e FIELD hi a ;STRUCTURE ;package"
VSPARITY:VS-LOAD

s" sdlive:pair" s" sdrep:pair" VSPARITY:COMPARE

\ ---------------------------------------------------------------------------
\ 2. Compact ENUM. This is the arm that changed owner: it drove the legacy
\    CHECKER-DEFENUM and now drives ENUM-DECL:ED-REPLAY, so the registry it
\    produces must still match an executed declaration exactly.
\ ---------------------------------------------------------------------------
package edlive
public
ENUM colour red green blue ;ENUM
;package

s" package edrep public ENUM colour red green blue ;ENUM ;package"
VSPARITY:VS-LOAD

s" edlive:colour" s" edrep:colour" VSPARITY:COMPARE

\ ---------------------------------------------------------------------------
\ 3. A STRUCTURE named as a later declaration's payload type — the shape that
\    was broken end to end. maki/db/obligation.f declares `STRUCTURE evidence`
\    and then names `evidence` inside `SUMTYPE discharge-result`; with no
\    STRUCTURE arm on this path the payload was unresolvable.
\ ---------------------------------------------------------------------------
s" package edpay public STRUCTURE slotrec FIELD v n ;STRUCTURE SUMTYPE box 0 VARIANT full slotrec ;VARIANT VARIANT empty ;VARIANT ;SUMTYPE ;package"
VSPARITY:VS-LOAD
s" edpay:box" VSPARITY:VCOUNT 2 VSPARITY:T=
s" edpay:slotrec" VSPARITY:FCOUNT 1 VSPARITY:T=

\ ---------------------------------------------------------------------------
\ 4. A body the capture buffer cannot hold RAISES; it is never truncated.
\
\    This is the failure the replay entries made dangerous. verify-source's
\    BODY-APPEND used to skip the single token that would not fit and keep
\    appending the shorter ones after it, including the terminator — safe while
\    the only consumers were the legacy definers, which re-checked the length
\    against the engine's own TDECL-CAP and rejected. The replay entries have no
\    length gate: they parse whatever arrives. Measured on the previous commit, a
\    1302-variant compact ENUM replayed with rc 0 and registered 1142 variants —
\    160 silently gone and every later tag shifted. A well-formed, wrong family.
\
\    ON THE TWO DIFFERENT BOUNDS, deliberately not reconciled. The engine's
\    legacy TDECL-CAP is $1000 (4096 bytes); this capture buffer's BODYBUF-CAP is
\    8000. A body between them replays fine here and would have been refused by
\    the legacy definer — and that is correct, not a gap to emulate. Before the
\    cutover such a declaration could never have loaded at all (the engine
\    rejects it at 4096), so no existing source reaches this window; after the
\    cutover both the live keyword and this replay ARE the unified front end,
\    which has no 4096-byte cap of its own. Emulating a cap that is being deleted
\    would only pin a bound nothing will enforce. What must hold is that each
\    path is honest about ITS OWN bound, which is what these two cases pin.
\ ---------------------------------------------------------------------------

\ Under the buffer's bound: replays completely, every variant present. A body of
\ 1000 variants is 6000 bytes — well past the legacy 4096 cap, and accepted,
\ which is the "no legacy-cap emulation" ruling made executable.
s" capa" 1000 VSPARITY:ENUM-SOURCE VSPARITY:VS-LOAD
s" capa:big" VSPARITY:VCOUNT 1000 VSPARITY:T=

\ Over the buffer's bound: raises the declaration layer's own "declaration too
\ long" code, and registers nothing at all.
s" capb" 1400 VSPARITY:ENUM-SOURCE VSPARITY:VSTRY 7118 VSPARITY:T=
VSPARITY:CAP-FAMILY-ABSENT

\ ---------------------------------------------------------------------------
\ 5. Comments are NOT laundered out of a declaration body.
\
\    The engine reads a declaration body with `parse-name`, which has no comment
\    rule, so `\` and `(` inside one are ordinary tokens that hit the name gate.
\    verify-source's ordinary NEXT-SCAN does strip them, which is right between
\    definitions and wrong inside a declaration: stripping would let the replay
\    accept source the engine refuses and register a family that cannot exist.
\    The two replay windows therefore read with NEXT-RAW, and these cases pin
\    that they reject with the SAME code the live front end gives.
\ ---------------------------------------------------------------------------
DECL-DIAG:PROSE
s" package vp public ENUM cm1 red ( note ) green ;ENUM ;package"
VSPARITY:VSTRY 7101 VSPARITY:T=
s" habu: bad enum declaration 'cm1': name must be a lowercase tail at '('"
DECL-DIAG:HAS? -1 VSPARITY:T=

DECL-DIAG:PROSE
s" package vq public STRUCTURE cm2 FIELD a n ( note ) FIELD b n ;STRUCTURE ;package"
VSPARITY:VSTRY 7107 VSPARITY:T=
s" habu: bad structure declaration 'cm2': unexpected token in structure declaration at '('"
DECL-DIAG:HAS? -1 VSPARITY:T=

\ a `\` comment runs to end of line, so this one needs a real newline in it
DECL-DIAG:PROSE
VSPARITY:BACKSLASH-ENUM-SOURCE VSPARITY:VSTRY 7101 VSPARITY:T=
s" habu: bad enum declaration 'cm3': name must be a lowercase tail at '\'"
DECL-DIAG:HAS? -1 VSPARITY:T=
DECL-DIAG:OFF

VSPARITY:REPORT
