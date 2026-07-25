\ enum-decl-suite.f — behavior + rollback suite for the ENUM typed declaration
\ front end (src/core/enum-decl.f, package ENUM-DECL, reached as ENUM-DECL:ED-RUN;
\ dot habu-enum-parse-full-39c0dc1b). Run BY THE ENGINE over stdin, exactly like
\ test/structure-decl-suite.f (the front end parses the live input stream and
\ mutates the type registry, so it resolves only at top-level interpret):
\     bin/hb < test/enum-decl-suite.f
\ Proves: a FULL declaration publishes a TK-SUM family with named variants and
\ named FIELD payloads reaching TYPE-FIELD reflection keyed (family, variant-id)
\ with SV.SCH-COUNT=0 (the settled seam); a COMPACT declaration publishes
\ payloadless TK-ENUM variants matching legacy-compact registry semantics; POLICY
\ and DERIVE headers are accepted before compact variants and reach both the
\ family record and the event stream; the shared
\ variant open/close and field events sequence correctly and each field event
\ carries the open variant selector; every reject anchor (mixed mode,
\ arity-then-compact, late compact header, compact positional payload, missing
\ ;VARIANT / ;ENUM, malformed arity, empty enum, reserved / case family name,
\ duplicate variant, every reserved variant-name category, package-scoped family
\ collisions, and the field record's own dup / reserved / case / schema gate)
\ fires at the offending token; a mid-declaration reject leaves every
\ registry cursor byte-identical to the pre-declaration baseline; and the
\ deterministic snapshot identity is reproducible for an identical declaration
\ against a fresh registry.
\ A failure prints F<index> + detail; REPORT exits 1.
\
\ ENUM-DECL:ED-RUN is the front-end entry: the global ENUM token still belongs to
\ the legacy sumtype.f definer until the hard cutover, so this suite drives the
\ new front end through its package-qualified entry rather than a global keyword.

require test/checker-assert.f      \ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected, 1 uncheckable

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
: T-TRUE ( bool -- ) {: b:bool :}
   #CASE @ 1 + #CASE !
   b 0= if T-FAIL s" assert: expected true" type cr then ;

\ --- boundary shims: the ENUM-DECL:ED-RUN entry, evaluate, and the sealed
\ pre-hook registry / schema reflection words are reached at top level through
\ named trusted forwarders (the same idiom test/structure-decl-suite.f uses).
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;            \ evaluate under catch -> throw code
TRUSTED: FAMID ( ptr u8 n -- n ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop ;
TRUSTED: F-ENUM? ( n -- bool ) TFAM-ENUM? ;
TRUSTED: F-SUM? ( n -- bool ) TFAM-SUM? ;
TRUSTED: F-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: F-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: F-FLD-COUNT ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: F-WIDTH ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: F-POLICY@ ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: F-EQ? ( n -- bool ) TFAM-DERIVE-EQ? ;
TRUSTED: F-HASH? ( n -- bool ) TFAM-DERIVE-HASH? ;
TRUSTED: SV-NAME$ ( n -- ptr u8 n ) SUMV-NAME$ ;
TRUSTED: SV-TAG@ ( n -- n ) SUMV-TAG@ ;
TRUSTED: SV-SCH-COUNT@ ( n -- n ) SUMV-SCH-COUNT@ ;
TRUSTED: SV-FAM@ ( n -- n ) SUMV-FAM@ ;
TRUSTED: SCH-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: SCH-TAG@ ( n -- n ) SCHEMA-TAG@ ;
TRUSTED: SCH-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: PACKED# ( -- n ) TL-PACKED-TAG ;
TRUSTED: SCHCON# ( -- n ) SCH-CON ;
TRUSTED: CCN# ( -- n ) CC-N ;

\ --- registry snapshot, so a reject can be proven byte-identical and the identity
\ test can re-run an identical declaration against a fresh registry (family id
\ restored, event log reset).
variable RB-TFAM  variable RB-STR  variable RB-PK  variable RB-SUMV
variable RB-LAY   variable RB-SCH  variable RB-ROOT  variable RB-PFN  variable RB-PFC
TRUSTED: REG-MARK ( -- )
   TFAM-N @ RB-TFAM !  TF-STR-U @ RB-STR !  TF-PK-N @ RB-PK !
   SUMV-N @ RB-SUMV !  LAY-N @ RB-LAY !  SCH-N @ RB-SCH !  SCH-ROOT-N @ RB-ROOT !
   PF-N @ RB-PFN !  PF-COMMIT-N @ RB-PFC ! ;
TRUSTED: REG-RESTORE ( -- )
   RB-TFAM @ TFAM-N !  RB-STR @ TF-STR-U !  RB-PK @ TF-PK-N !
   RB-SUMV @ SUMV-N !  RB-LAY @ LAY-N !  RB-SCH @ SCH-N !  RB-ROOT @ SCH-ROOT-N !
   RB-PFN @ PF-N !  RB-PFC @ PF-COMMIT-N ! ;
TRUSTED: TFAMN@ ( -- n ) TFAM-N @ ;
TRUSTED: SCHN@ ( -- n ) SCH-N @ ;
TRUSTED: SUMVN@ ( -- n ) SUMV-N @ ;

variable RC   variable FID   variable B   variable NODE   variable PFB   variable DEVB
variable VS0  variable VID

\ The production parser and GENERATED-DECL coordinator stay unchanged.  These
\ private drivers pause only the test body between DRIVE and coordinator
\ PREPARE, while declaration events and field rows are still provisional.
package ENUM-DECL

: TEST-PAYLOAD-VIEW ( -- )
   FAM @ FID !
   VBASE @ VS0 !
   VBASE @ 2 + VID !
   TOK @ RC !

   s" ENUM-DECL:ED-RUN epnested 0 VARIANT foreign FIELD nested n ;VARIANT ;ENUM" EV
   VBASE @ NODE !

   s" DECL-EVENT:CURRENT FID @ VS0 @ DECL-EVENT:PAYLOAD-N 0 T=" EV
   s" DECL-EVENT:CURRENT FID @ VS0 @ DECL-EVENT:PAYLOAD-CELLS 0 T=" EV

   s" DECL-EVENT:CURRENT FID @ VS0 @ 1 + DECL-EVENT:PAYLOAD-N 1 T=" EV
   s" DECL-EVENT:CURRENT FID @ VS0 @ 1 + 0 DECL-EVENT:PAYLOAD-SCHEMA@ SCH-ROOT@ SCH-A@ CCN# T=" EV
   s" DECL-EVENT:CURRENT FID @ VS0 @ 1 + 0 DECL-EVENT:PAYLOAD-WIDTH@ 1 T=" EV
   s" DECL-EVENT:CURRENT FID @ VS0 @ 1 + DECL-EVENT:PAYLOAD-CELLS 1 T=" EV

   s" DECL-EVENT:CURRENT FID @ VID @ DECL-EVENT:PAYLOAD-N 2 T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ 0 DECL-EVENT:PAYLOAD-SCHEMA@ SCH-ROOT@ SCH-A@ CCN# T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ 1 DECL-EVENT:PAYLOAD-SCHEMA@ SCH-ROOT@ SCH-A@ PFB @ T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ 0 DECL-EVENT:PAYLOAD-WIDTH@ 1 T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ 1 DECL-EVENT:PAYLOAD-WIDTH@ 2 T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ DECL-EVENT:PAYLOAD-CELLS 3 T=" EV

   s" DECL-EVENT:CURRENT PFB @ VID @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop B @ 7173 T=" EV
   s" DECL-EVENT:CURRENT FID @ -1 ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop B @ 7172 T=" EV
   s" DECL-EVENT:CURRENT FID @ VS0 @ 0 ' DECL-EVENT:PAYLOAD-SCHEMA@ catch B ! drop drop drop drop B @ 7172 T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ -1 ' DECL-EVENT:PAYLOAD-WIDTH@ catch B ! drop drop drop drop B @ 7172 T=" EV
   s" DECL-EVENT:CURRENT FID @ VID @ 2 ' DECL-EVENT:PAYLOAD-SCHEMA@ catch B ! drop drop drop drop B @ 7172 T=" EV
   s" DECL-EVENT:CURRENT FID @ NODE @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop B @ 7172 T=" EV

   DECL-EVENT:OPEN NODE !
   NODE @ FID @ DECL-EVENT:DECL NODE !
   s" RC @ FID @ VID @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop B @ 7161 T=" EV
   s" NODE @ FID @ VID @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop B @ 7172 T=" EV
   NODE @ DECL-EVENT:ROLLBACK ;

: TEST-PAYLOAD-BODY ( -- )
   ED-RESET DRIVE
   TEST-PAYLOAD-VIEW ;

: TEST-PAYLOAD-RUN ( -- )
   [: TEST-PAYLOAD-BODY ;] GENERATED-DECL:RUN ;

: TEST-PAYLOAD-XT ( -- [ -- ] )
   [: TEST-PAYLOAD-RUN ;] ;

: TEST-PAYLOAD-ROLL-BODY ( -- )
   ED-RESET DRIVE
   FAM @ FID !
   VBASE @ VS0 !
   TOK @ RC !
   E-SYNTAX throw ;

: TEST-PAYLOAD-ROLL ( -- )
   [: TEST-PAYLOAD-ROLL-BODY ;] GENERATED-DECL:RUN ;

: TEST-PAYLOAD-ROLL-XT ( -- [ -- ] )
   [: TEST-PAYLOAD-ROLL ;] ;

;package

\ ---------------------------------------------------------------------------
\ 1. A compact declaration persists a TK-ENUM family and one payloadless SUMV row
\    per bare variant, in declaration-order tags, owned by the family.
\ ---------------------------------------------------------------------------
TFAMN@ FID !
s" ENUM-DECL:ED-RUN color red green blue ;ENUM" EV
TFAMN@ FID @ 1 + T=                                  \ exactly one new family
s" color" FAMID F-ENUM? T-TRUE                       \ registered as the enum kind
s" color" FAMID F-VAR-COUNT 3 T=                     \ three variants
s" color" FAMID F-FLD-COUNT 0 T=                     \ payloadless: no field rows
s" color" FAMID F-WIDTH 1 T=                         \ enum width = one tag cell, no payload
s" color" FAMID F-VAR-START VS0 !
VS0 @ SV-NAME$ s" red" CORE-STR= T-TRUE              \ variant 0 = red, tag 0
VS0 @ SV-TAG@ 0 T=
VS0 @ SV-SCH-COUNT@ 0 T=                             \ payloadless SUMV row
VS0 @ SV-FAM@ s" color" FAMID T=                     \ variant owned by the family
VS0 @ 1 + SV-NAME$ s" green" CORE-STR= T-TRUE
VS0 @ 1 + SV-TAG@ 1 T=
VS0 @ 2 + SV-NAME$ s" blue" CORE-STR= T-TRUE
VS0 @ 2 + SV-TAG@ 2 T=

\ ---------------------------------------------------------------------------
\ 2. A full declaration publishes a TK-SUM family; its named FIELD payloads reach
\    TYPE-FIELD reflection keyed (family, variant-id) with the declared names,
\    slots, and byte offsets; the SUMV rows carry no positional schema (the
\    settled named-field seam, SV.SCH-COUNT=0); and the family width is the widest
\    variant payload plus one tag cell.
\ ---------------------------------------------------------------------------
TYPE-FIELD:COUNT B !
s" ENUM-DECL:ED-RUN msg 0 VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" EV
s" msg" FAMID F-SUM? T-TRUE                           \ a full enum is a tagged sum
s" msg" FAMID F-VAR-COUNT 2 T=                        \ quit, move
s" msg" FAMID F-FLD-COUNT 2 T=                        \ x, y (only move has fields)
s" msg" FAMID F-WIDTH 3 T=                            \ widest payload (move = 2 cells) + one tag
s" msg" FAMID F-VAR-START VS0 !
VS0 @ SV-NAME$ s" quit" CORE-STR= T-TRUE
VS0 @ SV-SCH-COUNT@ 0 T=                              \ named-field seam: no positional payload schema
VS0 @ 1 + SV-NAME$ s" move" CORE-STR= T-TRUE
VS0 @ 1 + SV-SCH-COUNT@ 0 T=
VS0 @ 1 + VID !                                       \ the move variant id
B @ TYPE-FIELD:NAME$ s" x" CORE-STR= T-TRUE           \ first field is x
B @ TYPE-FIELD:FAMILY@ s" msg" FAMID T=               \ owned by the msg family
B @ TYPE-FIELD:VARIANT@ VID @ T=                      \ keyed to the move variant
B @ TYPE-FIELD:SLOT@ 0 T=                             \ x at variant-payload slot 0
B @ TYPE-FIELD:BYTE-OFF@ 0 T=
B @ 1 + TYPE-FIELD:NAME$ s" y" CORE-STR= T-TRUE       \ second field is y
B @ 1 + TYPE-FIELD:VARIANT@ VID @ T=                  \ also keyed to the move variant
B @ 1 + TYPE-FIELD:SLOT@ 1 T=                         \ y at variant-payload slot 1
B @ 1 + TYPE-FIELD:BYTE-OFF@ CELL T=                  \ y at byte offset = one cell

\ ---------------------------------------------------------------------------
\ 3. The field schema reaches reflection: field type `n` is a concrete con node.
\ ---------------------------------------------------------------------------
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=                            \ a concrete-con schema node
NODE @ SCH-A@ CCN# T=                                 \ con code = n

\ ---------------------------------------------------------------------------
\ 4. A full declaration with arity resolves a positional-parameter field: FIELD v
\    a is parameter 0 within arity 1, so the field commits (no unresolved reject).
\ ---------------------------------------------------------------------------
TFAMN@ FID !
s" ENUM-DECL:ED-RUN boxe 1 VARIANT hold FIELD v a ;VARIANT ;ENUM" EV
TFAMN@ FID @ 1 + T=                                   \ family registered (param resolved, no rollback)
s" boxe" FAMID F-FLD-COUNT 1 T=                       \ the parameter field committed

\ ---------------------------------------------------------------------------
\ 4b. The full ENUM parser consumes the shared declaration alphabet.  A
\     maximum-arity declaration accepts g and z while f/n/r remain concrete;
\     the exact inverse table is tested once in type-family-suite.f.
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN emap 23 VARIANT values FIELD pa a FIELD pb b FIELD pc c FIELD pd d FIELD pe e FIELD pg g FIELD flag f FIELD integer n FIELD real r FIELD last z ;VARIANT ;ENUM" EV
s" emap" FAMID F-FLD-COUNT 10 T=                      \ every mapped/scalar field committed

\ ---------------------------------------------------------------------------
\ 4c. The production ENUM parser exposes only the current transaction's ordered
\     payload view before publication.  An empty variant, a scalar field, and
\     two ordered fields of widths one and two cover every public query.
\ ---------------------------------------------------------------------------
s" STRUCTURE epwide 0 FIELD left n FIELD right n ;STRUCTURE" EV
s" epwide" FAMID PFB !

package ENUM-DECL
TEST-PAYLOAD-XT
execute epview 0
   VARIANT empty ;VARIANT
   VARIANT scalar FIELD value n ;VARIANT
   VARIANT mixed FIELD first n FIELD pair epwide ;VARIANT
;ENUM
;package

RC @ FID @ VID @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop
B @ 7161 T=                                            \ published token is stale

package ENUM-DECL
TEST-PAYLOAD-ROLL-XT
catch eproll 0 VARIANT gone FIELD value n ;VARIANT ;ENUM
;package
7107 T=                                                \ forced body failure rolled back
RC @ FID @ VS0 @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop
B @ 7161 T=                                            \ rolled-back token is stale

\ ---------------------------------------------------------------------------
\ 5. Compact event stream: DECL then one VARIANT + VARIANT-END pair per variant,
\    with no arity header (compact is implicitly arity zero).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN evtc ea eb ;ENUM" EV
DECL-EVENT:COUNT 5 T=                                 \ DECL + (VARIANT + VARIANT-END) x 2
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:VARIANT? T-TRUE
2 DECL-EVENT:VARIANT-END? T-TRUE
3 DECL-EVENT:VARIANT? T-TRUE
4 DECL-EVENT:VARIANT-END? T-TRUE

\ ---------------------------------------------------------------------------
\ 6. Full event stream: DECL, ARITY header, then variant open/close bracketing
\    with the shared field event carrying the open variant as its selector.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN evtf 0 VARIANT quit ;VARIANT VARIANT move FIELD mx n ;VARIANT ;ENUM" EV
DECL-EVENT:COUNT 7 T=                                 \ DECL, ARITY, VARIANT, VARIANT-END, VARIANT, FIELD, VARIANT-END
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:ARITY? T-TRUE
2 DECL-EVENT:VARIANT? T-TRUE
3 DECL-EVENT:VARIANT-END? T-TRUE
4 DECL-EVENT:VARIANT? T-TRUE
5 DECL-EVENT:FIELD? T-TRUE
6 DECL-EVENT:VARIANT-END? T-TRUE
5 DECL-EVENT:VAR@ 4 DECL-EVENT:VAR@ T=               \ the field carries the open (move) variant selector
5 DECL-EVENT:VAR@ DECL-EVENT:NO-VARIANT <> T-TRUE    \ which is a real variant, not NO-VARIANT

\ ---------------------------------------------------------------------------
\ 7. Compact POLICY and DERIVE clauses use the same family and event owners as
\    full mode, remain TK-ENUM, and may precede variants in either header order.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-policy POLICY packed-tag alpha ;ENUM" EV
s" compact-policy" FAMID F-ENUM? T-TRUE
s" compact-policy" FAMID F-POLICY@ PACKED# T=
s" compact-policy" FAMID F-VAR-COUNT 1 T=
DECL-EVENT:COUNT 4 T=                                 \ DECL, POLICY, VARIANT, VARIANT-END
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:POLICY? T-TRUE
2 DECL-EVENT:VARIANT? T-TRUE
3 DECL-EVENT:VARIANT-END? T-TRUE

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-derive-a DERIVE eq hash alpha ;ENUM" EV
s" compact-derive-a" FAMID F-ENUM? T-TRUE
s" compact-derive-a" FAMID F-EQ? T-TRUE
s" compact-derive-a" FAMID F-HASH? T-TRUE
DECL-EVENT:COUNT 5 T=                                 \ DECL, DERIVE x2, VARIANT, VARIANT-END
1 DECL-EVENT:DERIVE? T-TRUE
2 DECL-EVENT:DERIVE? T-TRUE
3 DECL-EVENT:VARIANT? T-TRUE

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-derive-b DERIVE hash eq alpha ;ENUM" EV
s" compact-derive-b" FAMID F-EQ? T-TRUE
s" compact-derive-b" FAMID F-HASH? T-TRUE
1 DECL-EVENT:DERIVE? T-TRUE
2 DECL-EVENT:DERIVE? T-TRUE

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-derive-split DERIVE eq DERIVE hash alpha ;ENUM" EV
s" compact-derive-split" FAMID F-EQ? T-TRUE
s" compact-derive-split" FAMID F-HASH? T-TRUE
DECL-EVENT:COUNT 5 T=                                 \ distinct clauses retain one event per feature
1 DECL-EVENT:DERIVE? T-TRUE
2 DECL-EVENT:DERIVE? T-TRUE

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-both-a POLICY packed-tag DERIVE eq hash alpha beta ;ENUM" EV
s" compact-both-a" FAMID F-ENUM? T-TRUE
s" compact-both-a" FAMID F-POLICY@ PACKED# T=
s" compact-both-a" FAMID F-EQ? T-TRUE
s" compact-both-a" FAMID F-HASH? T-TRUE
s" compact-both-a" FAMID F-VAR-COUNT 2 T=
DECL-EVENT:COUNT 8 T=                                 \ both headers, then two variant pairs
1 DECL-EVENT:POLICY? T-TRUE
2 DECL-EVENT:DERIVE? T-TRUE
3 DECL-EVENT:DERIVE? T-TRUE
4 DECL-EVENT:VARIANT? T-TRUE

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN compact-both-b DERIVE hash eq POLICY packed-tag alpha ;ENUM" EV
s" compact-both-b" FAMID F-ENUM? T-TRUE
s" compact-both-b" FAMID F-POLICY@ PACKED# T=
s" compact-both-b" FAMID F-EQ? T-TRUE
s" compact-both-b" FAMID F-HASH? T-TRUE
DECL-EVENT:COUNT 6 T=                                 \ DERIVE x2, POLICY, one variant pair
1 DECL-EVENT:DERIVE? T-TRUE
2 DECL-EVENT:DERIVE? T-TRUE
3 DECL-EVENT:POLICY? T-TRUE
4 DECL-EVENT:VARIANT? T-TRUE

\ ---------------------------------------------------------------------------
\ 8. POLICY reaches both the family record and the event stream (full mode).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN opt 0 POLICY packed-tag VARIANT alpha ;VARIANT ;ENUM" EV
s" opt" FAMID F-POLICY@ PACKED# T=                    \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 9. DERIVE reaches both the family record and the event stream; two features on
\    one clause are accepted, each recorded once (full mode).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN der 0 DERIVE eq hash VARIANT alpha ;VARIANT ;ENUM" EV
s" der" FAMID F-EQ? T-TRUE                            \ eq derived
s" der" FAMID F-HASH? T-TRUE                          \ hash derived
2 DECL-EVENT:DERIVE? T-TRUE                           \ two DERIVE events after DECL + ARITY
3 DECL-EVENT:DERIVE? T-TRUE

\ ---------------------------------------------------------------------------
\ 10. A duplicate FIELD name inside a variant rejects through the field record
\    (E-TFAM-DUP 7102) and the whole provisional declaration rolls back
\    byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
TYPE-FIELD:COUNT PFB !
DECL-EVENT:COUNT DEVB !
s" ENUM-DECL:ED-RUN dupf 0 VARIANT alpha FIELD z n FIELD z n ;VARIANT ;ENUM" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SCHN@ RB-SCH @ T=                                     \ schema nodes retired
SUMVN@ RB-SUMV @ T=                                   \ variant rows retired
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
DECL-EVENT:COUNT DEVB @ T=                            \ nothing new published

\ ---------------------------------------------------------------------------
\ 11. A duplicate variant name rejects (E-TFAM-DUP 7102 from SUMV-ADD) and the
\     whole provisional declaration rolls back byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
s" ENUM-DECL:ED-RUN dupv red red ;ENUM" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SUMVN@ RB-SUMV @ T=                                   \ variant rows retired

\ ---------------------------------------------------------------------------
\ 12. Grammar / mode / arity / name / field rejects, each at the offending token.
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN emix red VARIANT alpha ;VARIANT ;ENUM" TRY 7107 T=       \ mixed modes (block token in compact)
s" ENUM-DECL:ED-RUN eac 2 red ;ENUM" TRY 7107 T=                         \ arity then a bare compact variant
s" ENUM-DECL:ED-RUN ech red POLICY packed-tag ;ENUM" TRY 7107 T=         \ header clause after a compact variant
s" ENUM-DECL:ED-RUN ecp red FIELD y n ;ENUM" TRY 7107 T=                 \ positional/named payload in compact
s" ENUM-DECL:ED-RUN emv 0 VARIANT alpha FIELD x n ;ENUM" TRY 7107 T=         \ missing ;VARIANT
s" ENUM-DECL:ED-RUN eme red green" TRY 7107 T=                           \ missing ;ENUM
s" ENUM-DECL:ED-RUN ear 24 VARIANT alpha ;VARIANT ;ENUM" TRY 7108 T=         \ arity above the shared 23 cap
s" ENUM-DECL:ED-RUN eempty ;ENUM" TRY 7107 T=                            \ an enum needs a variant
s" ENUM-DECL:ED-RUN enum red ;ENUM" TRY 7110 T=                          \ reserved opener keyword as a name
s" ENUM-DECL:ED-RUN Bad red ;ENUM" TRY 7101 T=                           \ upper-case family name (case)
s" ENUM-DECL:ED-RUN n red ;ENUM" TRY 7110 T=                             \ single-letter family name
s" ENUM-DECL:ED-RUN erf 0 VARIANT alpha FIELD make n ;VARIANT ;ENUM" TRY 7125 T=   \ reserved field name
s" ENUM-DECL:ED-RUN ecf 0 VARIANT alpha FIELD Zed n ;VARIANT ;ENUM" TRY 7101 T=    \ upper-case field name (case)
s" ENUM-DECL:ED-RUN ebs 0 VARIANT alpha FIELD x nope ;VARIANT ;ENUM" TRY 7109 T=   \ unresolved field type
s" ENUM-DECL:ED-RUN euc 0 VARIANT alpha FIELD x Q ;VARIANT ;ENUM" TRY 7109 T=      \ upper-case single-letter type
s" ENUM-DECL:ED-RUN epa 0 VARIANT alpha FIELD x a ;VARIANT ;ENUM" TRY 7109 T=      \ parameter outside declared arity
s" ENUM-DECL:ED-RUN epg 6 VARIANT alpha FIELD x h ;VARIANT ;ENUM" TRY 7109 T=      \ parameter 6 is outside arity 6

\ ---------------------------------------------------------------------------
\ 13. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN twice red ;ENUM" EV
s" ENUM-DECL:ED-RUN twice red ;ENUM" TRY 7102 T=

\ ---------------------------------------------------------------------------
\ 14. Deterministic snapshot identity: an identical declaration against a fresh
\     registry (family id restored, event log reset) folds to the same identity;
\     a different declaration folds to a different one.
\
\     These declarations are package-scoped, which makes them private, which
\     keeps constructor generation out of the way. REG-RESTORE is a whitebox
\     reset of the REGISTRY cursors only: it rewinds the family, variant, schema,
\     and field counters so the same family id can be re-declared, but it cannot
\     rewind the native dictionary. A public re-declaration would therefore ask
\     the generator to define constructor names that the first pass already
\     published, and sumtype.f's TDPLAN-NAME+ correctly refuses to render a plan
\     row for a live word. Visibility is not part of the event stream the
\     identity folds over — the events are DECL / VARIANT / FIELD keyed by family
\     id — so scoping these costs the assertion nothing.
\ ---------------------------------------------------------------------------
package enum-identity-test
REG-MARK
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN idc ia ib ic ;ENUM" EV
DECL-EVENT:IDENTITY RC !                              \ RC holds identity A
REG-RESTORE                                           \ retire family + variants; fresh registry
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN idc ia ib ic ;ENUM" EV
DECL-EVENT:IDENTITY RC @ T=                           \ identical declaration -> same identity
REG-RESTORE
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN idc ia ib ic id ;ENUM" EV
DECL-EVENT:IDENTITY RC @ <> T-TRUE                    \ different declaration -> different identity
REG-RESTORE

\ Named payload declarations have the same deterministic snapshot contract for
\ their ordered schema-bearing FIELD event sequence.
REG-MARK
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN ids 0 VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f ;VARIANT ;ENUM" EV
DECL-EVENT:IDENTITY RC !
REG-RESTORE
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN ids 0 VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f ;VARIANT ;ENUM" EV
DECL-EVENT:IDENTITY RC @ T=
REG-RESTORE
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN ids 0 VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f FIELD third n ;VARIANT ;ENUM" EV
DECL-EVENT:IDENTITY RC @ <> T-TRUE
REG-RESTORE
;package

\ ---------------------------------------------------------------------------
\ Compact and full declarations reject reserved variant names before publication;
\ the compact `variant` keyword is a syntax token, while the full name position
\ reaches the shared 7110 policy. Every failure restores all registry and
\ published-event cursors. Family collisions are lexical: global and
\ active-package families reserve their tails, while a family owned only by
\ another package does not.
\ ---------------------------------------------------------------------------
package other-enum-name-test
s" ENUM-DECL:ED-RUN foreign-variant member ;ENUM" EV
;package

package enum-name-test

VALUE-RECORD enum-record payload n END-VALUE-RECORD
s" ENUM-DECL:ED-RUN local-variant member ;ENUM" EV

: REG-SAME ( -- )
   TFAMN@ RB-TFAM @ T=
   TF-STR-U@ RB-STR @ T=
   TF-PK-N@ RB-PK @ T=
   SUMVN@ RB-SUMV @ T=
   LAY-N@ RB-LAY @ T=
   SCHN@ RB-SCH @ T=
   SCHEMA-ROOT-N@ RB-ROOT @ T=
   TYPE-FIELD:COUNT RB-PFN @ T=
   TYPE-FIELD:COUNT RB-PFC @ T=
   DECL-EVENT:COUNT DEVB @ T= ;

: REJECT-SAME ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   REG-MARK
   DECL-EVENT:COUNT DEVB !
   a u TRY want T=
   REG-SAME ;

public

s" ENUM-DECL:ED-RUN compact-dup-policy POLICY packed-tag POLICY stack-cell-tag alpha ;ENUM" 7163 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-dup-feature DERIVE eq eq alpha ;ENUM" 7164 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-dup-split DERIVE eq DERIVE eq alpha ;ENUM" 7164 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-missing-policy POLICY ;ENUM" 7116 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-missing-derive DERIVE ;ENUM" 7119 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-bad-policy POLICY unknown alpha ;ENUM" 7116 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-bad-feature DERIVE unknown alpha ;ENUM" 7119 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-order DERIVE order alpha ;ENUM" 7119 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-no-variant POLICY packed-tag ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-late-policy alpha POLICY packed-tag ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-late-derive alpha DERIVE eq ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-policy-name alpha policy ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN compact-derive-name alpha derive ;ENUM" 7107 REJECT-SAME

private

s" ENUM-DECL:ED-RUN reject-compact ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact n ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact q ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact if ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact variant ;ENUM" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact bool ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact enum-record ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact space-x ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact color ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-compact local-variant ;ENUM" 7110 REJECT-SAME

s" ENUM-DECL:ED-RUN reject-full 0 VARIANT" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT n ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT q ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT if ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT variant ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT bool ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT enum-record ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT space-x ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT color ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full 0 VARIANT local-variant ;VARIANT ;ENUM" 7110 REJECT-SAME

s" ENUM-DECL:ED-RUN allowed-compact foreign-variant ;ENUM" EV
s" allowed-compact" FAMID F-VAR-COUNT 1 T=
s" ENUM-DECL:ED-RUN allowed-full 0 VARIANT foreign-variant ;VARIANT ;ENUM" EV
s" allowed-full" FAMID F-VAR-COUNT 1 T=
s" ENUM-DECL:ED-RUN duplicate-order ready ready ;ENUM" 7102 REJECT-SAME

;package

\ ---------------------------------------------------------------------------
\ 20. Constructor generation. A public ENUM family with variants owns one sealed
\     checked FAMILY:VARIANT constructor per variant, rendered, evaluated,
\     certified and published by the ORDER 820 participant inside this
\     declaration's own transaction (src/core/generated-declaration.f, package
\     GENERATED-DECL-CTOR). The participant commits after DECL-EVENT has promoted
\     this declaration's TYPE-FIELD rows past PF-COMMIT-N, so it generates from
\     the ordinary committed provider and needs no provisional reader.
\
\     Both ENUM modes generate. The legacy sumtype.f definer already publishes
\     constructors for a compact payloadless enum, so gating on the full TK-SUM
\     mode alone would leave compact enums a parity gap the global-token cutover
\     could never close. TK-PRODUCT is excluded: the STRUCTURE front end owns its
\     own make/unmake generation.
\ ---------------------------------------------------------------------------
package enum-ctor-test
public

variable CT-I   variable CT-N
variable CT-DICT   variable CT-CP   variable CT-FAM

TRUSTED: CTOR-PKG$ ( n -- ptr u8 n ) SUMV-CTOR-PKG$ ;
TRUSTED: CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;
TRUSTED: DICT-RECS ( -- n ) ndict@ ;
TRUSTED: DICT-CODE ( -- n ) cp@ ;
TRUSTED: ARM-RC ( n -- n ) ['] GENERATED-DECL-CTOR:ARM catch ;

\ Arm the participant with CT-FAM from inside a real coordinator transaction, so
\ the depth precondition is satisfied and whatever ARM rejects is rejected on its
\ own merits. The throw escapes RUN through the ordinary body-failure path, which
\ also rolls the transaction back.
: ARM-IN-TX-BODY ( -- ) CT-FAM @ GENERATED-DECL-CTOR:ARM ;
: ARM-IN-TX ( -- ) [: ARM-IN-TX-BODY ;] GENERATED-DECL:RUN ;
TRUSTED: ARM-IN-TX-RC ( -- n ) ['] ARM-IN-TX catch ;

\ The committed payload arity, counted straight off the TYPE-FIELD registry
\ rather than through SUMV-PAY-N — the reader the generator itself uses. This is
\ the corrected observable: SV.SCH-COUNT is 0 for every ENUM-front-end variant by
\ design, so the row count keyed (family, variant) is what a rendered
\ constructor's input arity has to equal.
: PAY-ROWS ( n n -- n ) {: fam:n vid:n :}
   0 CT-N !   0 CT-I !
   BEGIN CT-I @ TYPE-FIELD:COUNT < WHILE
      CT-I @ TYPE-FIELD:FAMILY@ fam =
      CT-I @ TYPE-FIELD:VARIANT@ vid = and IF CT-N @ 1 + CT-N ! THEN
      CT-I @ 1 + CT-I !
   REPEAT
   CT-N @ ;

: DICT-MARK ( -- ) DICT-RECS CT-DICT !  DICT-CODE CT-CP ! ;
: DICT-SAME ( -- ) DICT-RECS CT-DICT @ T=  DICT-CODE CT-CP @ T= ;
: DICT-MOVED ( -- ) DICT-RECS CT-DICT @ > T-TRUE ;

\ A generation failure must leave every registry cursor, the published event log,
\ AND the native dictionary byte-identical: whatever the generator had already
\ defined is truncated with the rest of the declaration.
: CTOR-REJECT ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   REG-MARK
   DICT-MARK
   DECL-EVENT:COUNT DEVB !
   a u TRY want T=
   TFAMN@ RB-TFAM @ T=
   SUMVN@ RB-SUMV @ T=
   SCHN@ RB-SCH @ T=
   SCHEMA-ROOT-N@ RB-ROOT @ T=
   LAY-N@ RB-LAY @ T=
   TF-STR-U@ RB-STR @ T=
   TYPE-FIELD:COUNT RB-PFN @ T=
   DECL-EVENT:COUNT DEVB @ T=
   DICT-SAME ;

private
;package

\ 20a. A full payload ENUM publishes a mixed set: a nullary constructor for the
\      payloadless variant and a two-input one for the variant with two FIELDs.
\      Generation moves the native dictionary, which is what the dictionary
\      participant's savepoint has to be able to undo.
enum-ctor-test:DICT-MARK
s" ENUM-DECL:ED-RUN msgctor 0 VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" EV
enum-ctor-test:DICT-MOVED

s" msgctor" FAMID FID !
FID @ F-VAR-START VS0 !
VS0 @ enum-ctor-test:CTOR-PKG$ s" MSGCTOR" CORE-STR= T-TRUE       \ package derived and stamped on the rows
VS0 @ 1 + enum-ctor-test:CTOR-PKG$ s" MSGCTOR" CORE-STR= T-TRUE

\ rendered arity == committed TYPE-FIELD row count for (family, variant)
FID @ VS0 @ enum-ctor-test:PAY-ROWS 0 T=
FID @ VS0 @ 1 + enum-ctor-test:PAY-ROWS 2 T=
s" C1 ( -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! -1 T=
s" C2 ( n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! -1 T=

\ the effect is pinned in both directions: one input too few, one too many, and a
\ wrong result type all reject
s" C3 ( n -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! 0 T=
s" C4 ( n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C5 ( n n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=
s" C6 ( n n -- n ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! 0 T=

\ 20b. DERIVE rides the same commit. TDECL-DERIVE-REQUIRE reads committed SUMV /
\      TYPE-FIELD rows, which is exactly what ORDER 820 gives it, so a payload
\      family that derives publishes its constructors AND its derived tag and
\      equality words from one pass.
s" ENUM-DECL:ED-RUN dctor 0 DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two ;VARIANT ;ENUM" EV
s" D1 ( n -- dctor ) DCTOR:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" D2 ( -- dctor ) DCTOR:TWO" CHECK-QUIET-CANDIDATE! -1 T=
s" D3 ( dctor -- n ) DCTOR:TAG" CHECK-QUIET-CANDIDATE! -1 T=
s" D4 ( dctor dctor -- f ) DCTOR:EQ" CHECK-QUIET-CANDIDATE! -1 T=

\ 20c. Compact-mode parity with the legacy definer: the same three variant names
\      declared through `ENUM` and through ED-RUN produce the same derived
\      constructor package spelling, the same declaration-order tags, and
\      constructors that certify and reject identically.
s" ENUM lgpar red green blue ;ENUM" EV
s" ENUM-DECL:ED-RUN fepar red green blue ;ENUM" EV
s" lgpar" FAMID F-VAR-START B !
s" fepar" FAMID F-VAR-START VID !
B @ enum-ctor-test:CTOR-PKG$ s" LGPAR" CORE-STR= T-TRUE
VID @ enum-ctor-test:CTOR-PKG$ s" FEPAR" CORE-STR= T-TRUE
B @ SV-TAG@ VID @ SV-TAG@ T=
B @ 1 + SV-TAG@ VID @ 1 + SV-TAG@ T=
B @ 2 + SV-TAG@ VID @ 2 + SV-TAG@ T=
s" P1 ( -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P2 ( -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! -1 T=
s" P3 ( -- lgpar ) LGPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P4 ( -- fepar ) FEPAR:BLUE" CHECK-QUIET-CANDIDATE! -1 T=
s" P5 ( n -- lgpar ) LGPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=
s" P6 ( n -- fepar ) FEPAR:RED" CHECK-QUIET-CANDIDATE! 0 T=

\ 20d. The gate. A PRODUCT family is not owned here (STRUCTURE generates its own
\      make/unmake), and the existing PRODUCT generation is untouched. Arming the
\      participant with an unowned family is refused at the boundary rather than
\      three phases later, and arming outside a declaration transaction is
\      refused too.
s" PRODUCT prodctor 0 FIELD a n FIELD b n ;PRODUCT" EV
s" prodctor" FAMID GENERATED-DECL-CTOR:OWNS? 0= T-TRUE
s" G1 ( n n -- prodctor ) PRODCTOR:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" prodctor" FAMID enum-ctor-test:ARM-RC 7176 T=      \ wrong kind, and no open transaction
s" msgctor" FAMID GENERATED-DECL-CTOR:OWNS? T-TRUE
s" msgctor" FAMID enum-ctor-test:ARM-RC 7176 T=       \ right kind, but depth 0

\ 20e. A private ENUM stays inert: the family and its variants register, but the
\      gate refuses it, so no constructor package is derived onto the variant
\      rows and no constructor symbol is recorded for them.
package enum-ctor-private
s" ENUM-DECL:ED-RUN privctor 0 VARIANT alpha FIELD a n ;VARIANT ;ENUM" EV
s" privctor" FAMID GENERATED-DECL-CTOR:OWNS? 0= T-TRUE
s" privctor" FAMID F-VAR-COUNT 1 T=                   \ the variant really is there
s" privctor" FAMID F-VAR-START enum-ctor-test:CTOR-PKG$ nip 0 T=  \ but carries no constructor package
s" privctor" FAMID F-VAR-START enum-ctor-test:CTOR-SYM 0 T=       \ and no constructor symbol
;package

\ 20f. A generation failure rolls the WHOLE declaration back. Both anchors fail
\      inside the participant's commit, after the family, its variants, its field
\      rows and its events are all in place: a payload role with no derived
\      equality, and a variant spelled like the derived word the same DERIVE
\      clause generates. Every registry cursor, the published event log, and the
\      native dictionary come back byte-identical.
s" ENUM-DECL:ED-RUN rollctor 0 DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two FIELD b r ;VARIANT ;ENUM"
   7119 enum-ctor-test:CTOR-REJECT
s" ENUM-DECL:ED-RUN rollctor2 0 DERIVE eq VARIANT tag FIELD a n ;VARIANT ;ENUM"
   7110 enum-ctor-test:CTOR-REJECT
s" rollctor" FAMID 0 T=                               \ the family itself never landed
s" rollctor2" FAMID 0 T=

\ 20g. Arming a family whose constructors are already live is refused by name.
\      This is the boundary that keeps a caller away from sumtype.f's
\      TDPLAN-NAME+ duplicate guard, which answers a second plan row for a live
\      word with `76 die` — a process exit that no transaction can roll back and
\      no `catch` can see (test/enum-ctor-collide-bad.f pins that behaviour where
\      it can still be observed). The check is an existence test on the variant
\      row's recorded constructor symbol, so it is independent of the kind and
\      visibility gate: msgctor still OWNS? its constructors, and its published
\      words survive the refused transaction untouched.
s" msgctor" FAMID GENERATED-DECL-CTOR:OWNS? T-TRUE     \ still an owning kind
s" msgctor" FAMID F-VAR-START enum-ctor-test:CTOR-SYM 0 <> T-TRUE   \ and already generated
s" msgctor" FAMID enum-ctor-test:CT-FAM !
enum-ctor-test:ARM-IN-TX-RC 7176 T=                   \ named reject, inside a real transaction
GENERATED-DECL:DEPTH 0 T=                             \ which rolled back and left no frame
s" R1 ( -- msgctor ) MSGCTOR:QUIT" CHECK-QUIET-CANDIDATE! -1 T=   \ live words untouched
s" R2 ( n n -- msgctor ) MSGCTOR:MOVE" CHECK-QUIET-CANDIDATE! -1 T=
\ The two other ARM preconditions are held satisfied above rather than assumed:
\ the depth clause cannot be what fired, because ARM-IN-TX runs inside a live
\ GENERATED-DECL:RUN, and the kind clause cannot be what fired, because OWNS? is
\ true for this same family. Deleting the already-generated clause reds this
\ block and puts sumtype.f's die back within reach of ARM.

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" enum-decl-suite: failures" 1 die ;
REPORT
