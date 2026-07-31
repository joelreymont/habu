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
\ positional payload, late compact header, compact payload, missing
\ ;VARIANT / ;ENUM, malformed head, retired numeric arity, empty enum,
\ reserved / case family name,
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
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture

\ Every reject below now renders a declaration diagnostic. Capture it from the
\ start so the suite's own output stays clean; section 22 turns the capture into
\ assertions about the exact rendered line.
DECL-DIAG:PROSE

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

   s" ENUM-DECL:ED-RUN epnested<> VARIANT foreign FIELD nested n ;VARIANT ;ENUM" EV
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
s" ENUM-DECL:ED-RUN msg<> VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" EV
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
\ 4. Binder order is schema order. pair<e,a> records two parameters in the
\    declared order, and phantom<e> retains arity one without using its binder.
\ ---------------------------------------------------------------------------
TYPE-FIELD:COUNT B !
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN pair<e,a> VARIANT pair FIELD left e FIELD right a ;VARIANT ;ENUM" EV
1 DECL-EVENT:ARITY? T-TRUE
1 DECL-EVENT:VAR@ 2 T=
s" pair" FAMID F-FLD-COUNT 2 T=
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 0 T=
B @ 1 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 1 T=

DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN phantom<e> VARIANT none ;VARIANT ;ENUM" EV
1 DECL-EVENT:ARITY? T-TRUE
1 DECL-EVENT:VAR@ 1 T=
s" phantom" FAMID F-FLD-COUNT 0 T=

\ ---------------------------------------------------------------------------
\ 4b. The full ENUM parser consumes the shared declaration alphabet. A head
\     declaring the maximum binder set accepts g and z while f/n/r remain
\     concrete; the exact alphabet is tested once in type-family-suite.f.
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN emap<a,b,c,d,e,g,h,i,j,k,l,m,o,p,q,s,t,u,v,w,x,y,z> VARIANT values FIELD pa a FIELD pb b FIELD pc c FIELD pd d FIELD pe e FIELD pg g FIELD flag f FIELD integer n FIELD real r FIELD last z ;VARIANT ;ENUM" EV
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
execute epview<>
   VARIANT empty ;VARIANT
   VARIANT scalar FIELD value n ;VARIANT
   VARIANT mixed FIELD first n FIELD pair epwide ;VARIANT
;ENUM
;package

RC @ FID @ VID @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop
B @ 7161 T=                                            \ published token is stale

package ENUM-DECL
TEST-PAYLOAD-ROLL-XT
catch eproll<> VARIANT gone FIELD value n ;VARIANT ;ENUM
;package
7107 T=                                                \ forced body failure rolled back
RC @ FID @ VS0 @ ' DECL-EVENT:PAYLOAD-N catch B ! drop drop drop
B @ 7161 T=                                            \ rolled-back token is stale

\ ---------------------------------------------------------------------------
\ 5. Compact event stream: DECL then one VARIANT + VARIANT-END pair per variant,
\    with no binder-derived arity event (compact is implicitly arity zero).
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
\ 6. Full event stream: DECL, binder-derived ARITY event, then variant open/close bracketing
\    with the shared field event carrying the open variant as its selector.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN evtf<> VARIANT quit ;VARIANT VARIANT move FIELD mx n ;VARIANT ;ENUM" EV
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
s" ENUM-DECL:ED-RUN opt<> POLICY packed-tag VARIANT alpha ;VARIANT ;ENUM" EV
s" opt" FAMID F-POLICY@ PACKED# T=                    \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 9. DERIVE reaches both the family record and the event stream; two features on
\    one clause are accepted, each recorded once (full mode).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN der<> DERIVE eq hash VARIANT alpha ;VARIANT ;ENUM" EV
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
s" ENUM-DECL:ED-RUN dupf<> VARIANT alpha FIELD z n FIELD z n ;VARIANT ;ENUM" TRY 7102 T=
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
\ 12. Grammar / mode / head / name / field rejects, each at the offending token.
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN emix red VARIANT alpha ;VARIANT ;ENUM" TRY 7107 T=       \ mixed modes (block token in compact)
s" ENUM-DECL:ED-RUN eac<a> VARIANT red a ;VARIANT ;ENUM" TRY 7107 T=       \ positional payload is not a FIELD clause
s" ENUM-DECL:ED-RUN efield<a> VARIANT red FIELD value a ;VARIANT ;ENUM" TRY 0 T= \ named FIELD payload succeeds
s" ENUM-DECL:ED-RUN ech red POLICY packed-tag ;ENUM" TRY 7107 T=         \ header clause after a compact variant
s" ENUM-DECL:ED-RUN ecp red FIELD y n ;ENUM" TRY 7107 T=                 \ positional/named payload in compact
s" ENUM-DECL:ED-RUN emv<> VARIANT alpha FIELD x n ;ENUM" TRY 7107 T=         \ missing ;VARIANT
s" ENUM-DECL:ED-RUN eme red green" TRY 7107 T=                           \ missing ;ENUM
s" ENUM-DECL:ED-RUN ear<e,e> VARIANT alpha ;VARIANT ;ENUM" TRY 7108 T=       \ duplicate binder
s" ENUM-DECL:ED-RUN eempty ;ENUM" TRY 7107 T=                            \ an enum needs a variant
s" ENUM-DECL:ED-RUN enum red ;ENUM" TRY 7110 T=                          \ reserved opener keyword as a name
s" ENUM-DECL:ED-RUN Bad red ;ENUM" TRY 7101 T=                           \ upper-case family name (case)
s" ENUM-DECL:ED-RUN n red ;ENUM" TRY 7110 T=                             \ single-letter family name
s" ENUM-DECL:ED-RUN erf<> VARIANT alpha FIELD make n ;VARIANT ;ENUM" TRY 7125 T=   \ reserved field name
s" ENUM-DECL:ED-RUN ecf<> VARIANT alpha FIELD Zed n ;VARIANT ;ENUM" TRY 7101 T=    \ upper-case field name (case)
s" ENUM-DECL:ED-RUN ebs<> VARIANT alpha FIELD x nope ;VARIANT ;ENUM" TRY 7109 T=   \ unresolved field type
s" ENUM-DECL:ED-RUN euc<> VARIANT alpha FIELD x Q ;VARIANT ;ENUM" TRY 7109 T=      \ upper-case single-letter type
s" ENUM-DECL:ED-RUN epa<> VARIANT alpha FIELD x a ;VARIANT ;ENUM" TRY 7109 T=      \ valid but undeclared binder
s" ENUM-DECL:ED-RUN epg<a,b,c,d,e,g> VARIANT alpha FIELD x h ;VARIANT ;ENUM" TRY 7109 T= \ valid but undeclared binder

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
s" ENUM-DECL:ED-RUN ids<> VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f ;VARIANT ;ENUM" EV
DECL-EVENT:IDENTITY RC !
REG-RESTORE
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN ids<> VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f ;VARIANT ;ENUM" EV
DECL-EVENT:IDENTITY RC @ T=
REG-RESTORE
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN ids<> VARIANT empty ;VARIANT VARIANT pair FIELD first n FIELD second f FIELD third n ;VARIANT ;ENUM" EV
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

public

\ REG-SAME / REJECT-SAME are the suite's shared "this reject changed nothing"
\ proof. They are public so section 24 (control-word names) can reuse them
\ instead of keeping a second copy of the cursor list.
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
s" ENUM-DECL:ED-RUN headdup<e,e> VARIANT alpha ;VARIANT ;ENUM" 7108 REJECT-SAME
s" ENUM-DECL:ED-RUN headbad<e,> VARIANT alpha ;VARIANT ;ENUM" 7108 REJECT-SAME
s" ENUM-DECL:ED-RUN retired-arity 1 VARIANT alpha ;VARIANT ;ENUM" 7101 REJECT-SAME
s" ENUM-DECL:ED-RUN undeclared<e> VARIANT alpha FIELD value a ;VARIANT ;ENUM" 7109 REJECT-SAME

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

s" ENUM-DECL:ED-RUN reject-full<> VARIANT" 7107 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT n ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT q ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT if ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT variant ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT bool ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT enum-record ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT space-x ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT color ;VARIANT ;ENUM" 7110 REJECT-SAME
s" ENUM-DECL:ED-RUN reject-full<> VARIANT local-variant ;VARIANT ;ENUM" 7110 REJECT-SAME

s" ENUM-DECL:ED-RUN allowed-compact foreign-variant ;ENUM" EV
s" allowed-compact" FAMID F-VAR-COUNT 1 T=
s" ENUM-DECL:ED-RUN allowed-full<> VARIANT foreign-variant ;VARIANT ;ENUM" EV
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

TRUSTED: CTOR-NS$ ( n -- ptr u8 n ) SUMV-CTOR-NS$ ;
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
s" ENUM-DECL:ED-RUN msgctor<> VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM" EV
enum-ctor-test:DICT-MOVED

s" msgctor" FAMID FID !
FID @ F-VAR-START VS0 !
VS0 @ enum-ctor-test:CTOR-NS$ s" MSGCTOR" CORE-STR= T-TRUE       \ namespace derived and stamped on the rows
VS0 @ 1 + enum-ctor-test:CTOR-NS$ s" MSGCTOR" CORE-STR= T-TRUE

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
s" ENUM-DECL:ED-RUN dctor<> DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two ;VARIANT ;ENUM" EV
s" D1 ( n -- dctor ) DCTOR:ONE" CHECK-QUIET-CANDIDATE! -1 T=
s" D2 ( -- dctor ) DCTOR:TWO" CHECK-QUIET-CANDIDATE! -1 T=
s" D3 ( dctor -- n ) DCTOR:TAG" CHECK-QUIET-CANDIDATE! -1 T=
s" D4 ( dctor dctor -- f ) DCTOR:EQ" CHECK-QUIET-CANDIDATE! -1 T=

\ 20c. Compact-mode parity with the legacy definer: the same three variant names
\      declared through `ENUM` and through ED-RUN produce the same derived
\      constructor namespace spelling, the same declaration-order tags, and
\      constructors that certify and reject identically.
s" ENUM lgpar red green blue ;ENUM" EV
s" ENUM-DECL:ED-RUN fepar red green blue ;ENUM" EV
s" lgpar" FAMID F-VAR-START B !
s" fepar" FAMID F-VAR-START VID !
B @ enum-ctor-test:CTOR-NS$ s" LGPAR" CORE-STR= T-TRUE
VID @ enum-ctor-test:CTOR-NS$ s" FEPAR" CORE-STR= T-TRUE
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
\      gate refuses it, so no constructor namespace is recorded on the variant
\      rows and no constructor symbol is recorded for them.
package enum-ctor-private
s" ENUM-DECL:ED-RUN privctor<> VARIANT alpha FIELD a n ;VARIANT ;ENUM" EV
s" privctor" FAMID GENERATED-DECL-CTOR:OWNS? 0= T-TRUE
s" privctor" FAMID F-VAR-COUNT 1 T=                   \ the variant really is there
s" privctor" FAMID F-VAR-START enum-ctor-test:CTOR-NS$ nip 0 T=  \ but carries no constructor namespace
s" privctor" FAMID F-VAR-START enum-ctor-test:CTOR-SYM 0 T=       \ and no constructor symbol
;package

\ 20f. A generation failure rolls the WHOLE declaration back. Both anchors fail
\      inside the participant's commit, after the family, its variants, its field
\      rows and its events are all in place: a payload role with no derived
\      equality, and a variant spelled like the derived word the same DERIVE
\      clause generates. Every registry cursor, the published event log, and the
\      native dictionary come back byte-identical.
s" ENUM-DECL:ED-RUN rollctor<> DERIVE eq VARIANT one FIELD a n ;VARIANT VARIANT two FIELD b r ;VARIANT ;ENUM"
   7119 enum-ctor-test:CTOR-REJECT
s" ENUM-DECL:ED-RUN rollctor2<> DERIVE eq VARIANT tag FIELD a n ;VARIANT ;ENUM"
   7110 enum-ctor-test:CTOR-REJECT
s" rollctor" FAMID 0 T=                               \ the family itself never landed
s" rollctor2" FAMID 0 T=

\ 20g. Arming a family whose constructors are already live is refused by name.
\      This is the boundary that keeps a caller away from planning a second set
\      for a family whose words are already live. The ARM check is an existence
\      test on the variant row's recorded constructor symbol, so it is independent
\      of the kind and
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
\ 21. POLICY packed-tag bakes the memory ABI descriptor (docs §22.2). The
\     descriptor is a LAY registry row sized from the variant count and the
\     payload slot width, so it can only be baked once those are bound: the
\     legacy definer does it in sumtype.f CHECKER-DEFENUM-BODY (`fam
\     TDECL-LAYOUT-DESC`, between TFAM-SLOTS! and the constructor publish), and
\     the front end does it at the same point in ED-CLOSE. Before that call
\     existed, a POLICY packed-tag ENUM through ENUM-DECL:ED-RUN recorded the
\     policy on the family record but baked NO row, so the front end and the
\     global token disagreed about the family's memory ABI.
\ ---------------------------------------------------------------------------
package enum-layout-test
public

TRUSTED: L-ROW? ( n -- n bool ) LAY-FIND ;        \ family -> layout row id, found?
TRUSTED: L-POLICY ( n -- n ) LAY-POLICY@ ;
TRUSTED: L-SIZE ( n -- n ) LAY-SIZE@ ;
TRUSTED: L-ALIGN ( n -- n ) LAY-ALIGN@ ;
TRUSTED: L-TAGW ( n -- n ) LAY-TAGW@ ;

: PARITY ( n n -- )                  \ two families bake field-identical descriptors
   {: a:n b:n :}
   a L-ROW? {: la:n fa:bool :}
   b L-ROW? {: lb:n fb:bool :}
   fa T-TRUE   fb T-TRUE
   fa 0= fb 0= or IF EXIT THEN       \ a missing row already failed; do not read a bogus id
   la L-POLICY lb L-POLICY T=
   la L-SIZE   lb L-SIZE   T=
   la L-ALIGN  lb L-ALIGN  T=
   la L-TAGW   lb L-TAGW   T= ;

: SHAPE ( n n n n -- )               \ family + expected record size, alignment, tag width
   {: fam:n size:n align:n tagw:n :}
   fam L-ROW? {: lid:n found:bool :}
   found T-TRUE
   found 0= IF EXIT THEN
   lid L-POLICY PACKED# T=
   lid L-SIZE  size  T=
   lid L-ALIGN align T=
   lid L-TAGW  tagw  T= ;

: NO-ROW ( n -- )                    \ the default policy bakes nothing
   {: fam:n :}
   fam L-ROW? drop 0= T-TRUE ;

private

\ 21a. Compact mode, both paths: three payloadless variants under packed-tag.
\      Tag-only, so the record is one byte, byte-aligned, with a one-byte tag.
ENUM lay-legacy POLICY packed-tag lgred lggrn lgblu ;ENUM
s" ENUM-DECL:ED-RUN lay-unified POLICY packed-tag unred ungrn unblu ;ENUM" EV
s" lay-legacy" FAMID s" lay-unified" FAMID enum-layout-test:PARITY
s" lay-unified" FAMID 1 1 1 enum-layout-test:SHAPE

\ 21b. The default policy (stack-cell-tag) bakes no row on either path.
ENUM lay-legacy-def dfred ;ENUM
s" ENUM-DECL:ED-RUN lay-unified-def dfblu ;ENUM" EV
s" lay-legacy-def" FAMID enum-layout-test:NO-ROW
s" lay-unified-def" FAMID enum-layout-test:NO-ROW

\ 21c. Full mode: the descriptor is sized from the WIDEST variant, so the bake
\      has to follow FAM-SLOTS!. Legacy ENUM has no payload grammar, so the
\      legacy comparison partner is SUMTYPE — the same TK-SUM kind, the same
\      one-cell payload, the same packed policy. This pairing keeps working
\      after the global ENUM token moves to the front end.
SUMTYPE lay-legacy-pay 0 POLICY packed-tag VARIANT lpnone ;VARIANT VARIANT lpone n ;VARIANT ;SUMTYPE
s" ENUM-DECL:ED-RUN lay-unified-pay<> POLICY packed-tag VARIANT upnone ;VARIANT VARIANT upone FIELD x n ;VARIANT ;ENUM" EV
s" lay-legacy-pay" FAMID s" lay-unified-pay" FAMID enum-layout-test:PARITY
s" lay-unified-pay" FAMID 16 8 1 enum-layout-test:SHAPE   \ 8 payload bytes + 1 tag byte, cell-aligned

;package

\ 21d. A reject AFTER the bake retires the descriptor with the family. The two
\      declarations below differ in one token — the payload type — and both reach
\      ED-CLOSE, so both bake a row; the second is then refused at ORDER 820 by
\      the derive payload-role gate (a pointer payload has no derived equality,
\      7119), which runs in the commit phase, well after ED-CLOSE. LAY-N is one
\      of the marks the checker participant's savepoint carries, so the refused
\      declaration's row goes back with its family.
REG-MARK
s" ENUM-DECL:ED-RUN lay-keep<> POLICY packed-tag DERIVE eq VARIANT one FIELD p n ;VARIANT ;ENUM" TRY 0 T=
LAY-N@ RB-LAY @ 1 + T=                                \ the accepted twin baked exactly one row
REG-MARK
s" ENUM-DECL:ED-RUN lay-drop<> POLICY packed-tag DERIVE eq VARIANT one FIELD p ptr n ;VARIANT ;ENUM" TRY 7119 T=
LAY-N@ RB-LAY @ T=                                    \ the refused twin left none
TFAMN@ RB-TFAM @ T=

\ ---------------------------------------------------------------------------
\ 22. Reject diagnostics. Before this section existed, ED-RUN threw every code
\     below with no message at all, while the legacy ENUM definer printed
\     "habu: bad enum declaration 'x': <reason> at 'tok'". Both spellings now
\     render through render.f's TDECL-DIAG, so the assertions here read the exact
\     bytes that reach the diagnostic channel, captured through the same
\     DIAG-BUFFER! / DIAG-BUFFER$ pair tools/check-core.f's CHK-DECL-CAPTURE and
\     CHK-DECL-FLUSH use. That proves the channel. It is not an end-to-end run of
\     the check tool over a unified declaration: check-core drives the legacy
\     definers today and does not scan STRUCTURE at all, so that leg waits on the
\     buffer-driven registration entry.
\
\     Each case asserts the WHOLE rendered line, not a substring: a message that
\     named the wrong family, dropped the token, or picked a stale reason fails,
\     and text supplied by the declaration itself cannot satisfy the assertion by
\     appearing somewhere else in the buffer.
\ ---------------------------------------------------------------------------

\ 22a. One case per reject code this suite pins, each rendering family + reason +
\      token and rethrowing the exact code.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgempty ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dgempty': empty enum at 'dgempty'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN Dgcase red ;ENUM" TRY 7101 T=
s" habu: bad enum declaration 'Dgcase': name must be a lowercase family tail at 'Dgcase'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN n red ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'n': reserved name at 'n'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdup red red ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgdup': duplicate variant at 'red'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgar<e,e> VARIANT vv ;VARIANT ;ENUM" TRY 7108 T=
s" habu: bad enum declaration 'dgar<e,e>': binder list must contain unique declaration parameters at 'dgar<e,e>'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgpay<> VARIANT vv FIELD payfld nosuchtype ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'dgpay': unknown declaration term at 'nosuchtype'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgpol POLICY nosuch red ;ENUM" TRY 7116 T=
s" habu: bad enum declaration 'dgpol': unknown layout policy at 'nosuch'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdrv DERIVE nosuch red ;ENUM" TRY 7119 T=
s" habu: bad enum declaration 'dgdrv': unknown derive feature at 'nosuch'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgord DERIVE order red ;ENUM" TRY 7119 T=
s" habu: bad enum declaration 'dgord': derive feature not yet supported at 'order'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgfn<> VARIANT vv FIELD make n ;VARIANT ;ENUM" TRY 7125 T=
s" habu: bad enum declaration 'dgfn': reserved field name at 'make'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgkw variant ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dgkw': block keyword in a compact enum at 'variant'"
DECL-DIAG:HAS? -1 T=

\ a terminator that never arrives anchors on the family, exactly as the legacy
\ unterminated-declaration packet does (sumtype.f TDECL-ENUM-NOEND-BODY).
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgnoend red" TRY 7107 T=
s" habu: bad enum declaration 'dgnoend': missing ;ENUM at 'dgnoend'" DECL-DIAG:HAS? -1 T=

\ 22b. Legacy / unified parity. The same malformed shape through the legacy ENUM
\      definer and through ED-RUN produces the same code and the same message,
\      differing only in the family name the two declarations had to be given.
DECL-DIAG:PROSE
s" ENUM dglegacy red red ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dglegacy': duplicate variant at 'red'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgunified red red ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgunified': duplicate variant at 'red'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM dglegacy2 ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dglegacy2': empty enum at 'dglegacy2'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgunified2 ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dgunified2': empty enum at 'dgunified2'" DECL-DIAG:HAS? -1 T=

\ 22c. Clause nesting inside one declaration. A FIELD inside a VARIANT inside an
\      ENUM reports the ENUM's family, not the variant or the field it is nested
\      in, while the token moves to the offending payload token.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgnest<> VARIANT nestvar FIELD nestfld nosuchtype ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'dgnest': unknown declaration term at 'nosuchtype'" DECL-DIAG:HAS? -1 T=
s" declaration 'nestvar'" DECL-DIAG:HAS? 0 T=
s" declaration 'nestfld'" DECL-DIAG:HAS? 0 T=

\ 22d. A declaration that ends before its own name reports an empty family, not
\      the family dgnest declared just above. What this pins is DRIVE's ordering:
\      the front end names the family from the token it just read, unconditionally
\      and before validating it, so the empty token replaces the previous family.
\      It does NOT exercise OPEN's clear — DRIVE reaches FAMILY! on this path — so
\      OPEN's own contract is pinned directly in 22g below.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN" TRY 7107 T=
s" habu: bad enum declaration '': missing name" DECL-DIAG:HAS? -1 T=
s" dgnest" DECL-DIAG:HAS? 0 T=

\ 22e. Hostile declarations. Text supplied BY the declaration cannot forge or
\      displace the packet: a variant spelled like a fragment of the message is
\      reported as the token it is, the reason still comes from the reject, and
\      exactly one line is emitted (length = the line plus its newline).
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgforge duplicate duplicate ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgforge': duplicate variant at 'duplicate'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:LEN 71 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgat at at ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgat': duplicate variant at 'at'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:LEN 61 T=

\      A family name spelled like one of the JSON packet's own keys keeps key and
\      value distinguishable, and a token carrying a double quote is escaped
\      rather than closing the JSON string early.
DECL-DIAG:JSON
s" ENUM-DECL:ED-RUN reason red red ;ENUM" TRY 7102 T=
s\" \"family\":\"reason\"" DECL-DIAG:HAS? -1 T=
s\" \"reason\":\"duplicate variant\"" DECL-DIAG:HAS? -1 T=
s\" \"token\":\"red\"" DECL-DIAG:HAS? -1 T=

DECL-DIAG:JSON
s\" ENUM-DECL:ED-RUN dgquote aq\"b red ;ENUM" TRY 7101 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? -1 T=
s\" \"family\":\"dgquote\"" DECL-DIAG:HAS? -1 T=
s\" \"code\":\"E-BAD-DECLARATION\"" DECL-DIAG:HAS? -1 T=

\ 22f. Accepted declarations stay silent, and a rendered reject still leaves the
\      registry byte-identical: rendering happens after the coordinator has
\      rolled back and touches no registry cursor.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgok1 red green ;ENUM" TRY 0 T=
s" ENUM-DECL:ED-RUN dgok2<> VARIANT okvar FIELD okfld n ;VARIANT ;ENUM" TRY 0 T=
s" ENUM-DECL:ED-RUN dgok3 POLICY packed-tag redd greend ;ENUM" TRY 0 T=
s" ENUM-DECL:ED-RUN dgok4 DERIVE eq rede greene ;ENUM" TRY 0 T=
DECL-DIAG:SILENT? -1 T=

REG-MARK
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgroll red red ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgroll': duplicate variant at 'red'" DECL-DIAG:HAS? -1 T=
TFAMN@ RB-TFAM @ T=
SUMVN@ RB-SUMV @ T=
SCHN@ RB-SCH @ T=

DECL-DIAG:OFF

\ 22g. The packet's own contract, asserted on DECL-REJECT's public surface rather
\      than inferred from a front end that happens to exercise it.
\
\      An armed reason describes ONE code. It answers for that code, the table
\      answers for any other, and reading a new token retires the arming outright
\      — an arming names a fault expected AT a token, so once the front end has
\      moved on it can no longer explain anything.
s" enum" DECL-REJECT:OPEN
s" armed-for-7102" 7102 DECL-REJECT:EXPECT
7102 DECL-REJECT:REASON$ s" armed-for-7102" DECL-DIAG:SAME? -1 T=
7107 DECL-REJECT:REASON$ s" malformed declaration" DECL-DIAG:SAME? -1 T=
s" moved-on" DECL-REJECT:TOKEN!
7102 DECL-REJECT:REASON$ s" duplicate name in this package" DECL-DIAG:SAME? -1 T=

\      OPEN clears every field. The packet below is fully populated first, so a
\      clear that stopped happening would leave one of these three assertions
\      reading the previous declaration's value.
s" enum" DECL-REJECT:OPEN
s" leaked-family" DECL-REJECT:FAMILY!
s" leaked-token" DECL-REJECT:TOKEN!
s" leaked-reason" 7102 DECL-REJECT:EXPECT
s" enum" DECL-REJECT:OPEN
DECL-REJECT:FAMILY$ nip 0 T=
DECL-REJECT:TOKEN$ nip 0 T=
7102 DECL-REJECT:REASON$ s" duplicate name in this package" DECL-DIAG:SAME? -1 T=

\ 22h. SLOT! bounds. A span longer than the 96-byte slot is capped and MARKED
\      with a trailing "...", so the packet never presents a prefix as if it were
\      the whole name, and the copy never runs past its own slot into the next
\      one. The token is written FIRST here and the over-long family SECOND,
\      because S-FAMILY sits immediately below S-TOKEN: an unclamped copy would
\      overwrite the token that was already there.
s" enum" DECL-REJECT:OPEN
s" survivor" DECL-REJECT:TOKEN!
s" qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop"
DECL-REJECT:FAMILY!
DECL-REJECT:TOKEN$ s" survivor" DECL-DIAG:SAME? -1 T=
DECL-REJECT:FAMILY$ nip 96 T=
DECL-REJECT:FAMILY$
s" qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwe..."
DECL-DIAG:SAME? -1 T=

\      and the marked cap reaches the rendered line through the real front end.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiop ;ENUM"
TRY 7107 T=
s" habu: bad enum declaration 'qwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwertyuiopqwe...': empty enum"
DECL-DIAG:HAS? -1 T=

\ 22i. The reasons no case above reaches: the armed duplicate-family text, the
\      declaration-event clause duplicates, the variant-block and header-order
\      grammar faults, and the three codes whose reason can only come from the
\      table because the owner that raises them is past the front end's last
\      token.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgtwice red ;ENUM" TRY 0 T=
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgtwice blue ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgtwice': duplicate family at 'dgtwice'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdupfld<> VARIANT vv FIELD zz n FIELD zz n ;VARIANT ;ENUM" TRY 7102 T=
s" habu: bad enum declaration 'dgdupfld': duplicate field name at 'zz'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dg2pol POLICY packed-tag POLICY packed-tag red ;ENUM" TRY 7163 T=
s" habu: bad enum declaration 'dg2pol': a second POLICY clause in one declaration at 'packed-tag'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dg2drv DERIVE eq DERIVE eq red ;ENUM" TRY 7164 T=
s" habu: bad enum declaration 'dg2drv': the same DERIVE feature twice in one declaration at 'eq'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dghdr red POLICY packed-tag ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dghdr': header clause after the first variant at 'POLICY'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvb<> VARIANT vv stray ;VARIANT ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'dgvb': unexpected token in variant block at 'stray'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvn<> VARIANT" TRY 7107 T=
s" habu: bad enum declaration 'dgvn': missing variant name" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgdgd<e> DERIVE eq VARIANT vv ;VARIANT ;ENUM" TRY 7119 T=
s" habu: bad enum declaration 'dgdgd': derive requires a concrete (arity 0) family at 'eq'"
DECL-DIAG:HAS? -1 T=

\      7119 raised by the constructor participant's payload-role check, two
\      phases after the body: no reason is armed for it, so the table answers.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgroleq<> DERIVE eq VARIANT vv FIELD p ptr n ;VARIANT ;ENUM" TRY 7119 T=
s" habu: bad enum declaration 'dgroleq': a payload type or role has no derived equality at 'dgroleq'"
DECL-DIAG:HAS? -1 T=

\      7110 raised by the variant-name gate, and 7101 raised by the field record:
\      both are deeper owners, both answered from the table.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgvrsv<> VARIANT n ;VARIANT ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'dgvrsv': name is reserved or already taken at 'n'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN dgfcase<> VARIANT vv FIELD Zed n ;VARIANT ;ENUM" TRY 7101 T=
s" habu: bad enum declaration 'dgfcase': name must be a lowercase tail at 'Zed'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ 23. ED-REPLAY — registering an ENUM from tokens a tool already lexed.
\
\     tools/check-core.f's nominal pass and src/habu/verify-source.f scan a file
\     token by token and must register every family they meet, so a later
\     signature in the same file resolves. They cannot let the front end call
\     `parse-name` (they are not interpreting the file) and must define no word
\     (they are reading source, not building a program). ED-REPLAY is that entry:
\     the SAME grammar loop, validation, registry writes and reject packet as
\     ED-RUN, reading its tokens from the caller's buffer.
\
\     What these cases have to separate is registration from generation, because
\     the two used to be one step. Registration includes the constructor namespace
\     stamped on each variant row — that is metadata the checker resolves types
\     through, and the legacy metadata-only entry CHECKER-DEFENUM published it
\     too. Generation is the rendering of the constructor WORDS, and that is the
\     only thing a replay skips. So each case below pins both halves: the rows
\     are there, the words are not.
\ ---------------------------------------------------------------------------
package enum-replay-test
public

\ The replay entry under `catch`, so a reject answers its code the way TRY does
\ for the live entry. TRUSTED: for the same reason TRY is — `catch` over a word
\ the checker cannot type through a quotation boundary here.
TRUSTED: RP-EV ( ptr u8 n ptr u8 n -- ) ENUM-DECL:ED-REPLAY ;
TRUSTED: RP-TRY ( ptr u8 n ptr u8 n -- n ) ['] RP-EV catch ;

\ Force the replay stream open so the re-entry guard can be reached at all; the
\ production callers never nest, which is exactly why the guard needs a test.
TRUSTED: RP-FORCE-OPEN ( -- ) s" x" s" y" DECL-REPLAY:RP-CLAIM ;

\ second variant cursor, for the live-vs-replayed side-by-side comparison
variable VS1
TRUSTED: RP-FORCE-CLOSE ( -- ) DECL-REPLAY:RP-RELEASE ;

private
;package

\ 23a. A compact ENUM replays to the same family shape the live front end
\      registers, and moves the native dictionary NOT AT ALL.
enum-ctor-test:DICT-MARK
s" rpcompact" s" red green blue ;ENUM" enum-replay-test:RP-TRY 0 T=
enum-ctor-test:DICT-SAME

s" rpcompact" FAMID FID !
FID @ F-ENUM? -1 T=                       \ compact mode still picks TK-ENUM
FID @ F-VAR-COUNT 3 T=
FID @ F-WIDTH 1 T=
FID @ F-VAR-START VS0 !
VS0 @ SV-NAME$ s" red" CORE-STR= T-TRUE
VS0 @ 1 + SV-NAME$ s" green" CORE-STR= T-TRUE
VS0 @ 2 + SV-NAME$ s" blue" CORE-STR= T-TRUE
VS0 @ SV-TAG@ 0 T=
VS0 @ 2 + SV-TAG@ 2 T=

\ REGISTRATION happened: the constructor namespace is stamped on every row, which
\ is what a later `RPCOMPACT:RED` in the same source resolves through.
VS0 @ enum-ctor-test:CTOR-NS$ s" RPCOMPACT" CORE-STR= T-TRUE
VS0 @ 2 + enum-ctor-test:CTOR-NS$ s" RPCOMPACT" CORE-STR= T-TRUE

\ GENERATION did not: no constructor symbol was recorded on any row, and the
\ constructor word genuinely does not exist. This is the observable that fails
\ the moment the replay driver is allowed to generate.
VS0 @ enum-ctor-test:CTOR-SYM 0 T=
VS0 @ 2 + enum-ctor-test:CTOR-SYM 0 T=
\ 1 = uncheckable: the family type resolves (it IS registered) but the
\ constructor word does not exist, so the body cannot be checked at all. The
\ identical live declaration in section 20a answers -1, accepted.
s" R1 ( -- rpcompact ) RPCOMPACT:RED" CHECK-QUIET-CANDIDATE! 1 T=

\ 23b. FULL mode replays too. The legacy CHECKER-DEFENUM this entry replaces read
\      a compact list of bare variant names and nothing else, so an explicit
\      binder head or a VARIANT block was unregisterable through the old consumers.
s" rpfull<>" s" VARIANT quit ;VARIANT VARIANT move FIELD x n FIELD y n ;VARIANT ;ENUM"
enum-replay-test:RP-TRY 0 T=
s" rpfull" FAMID FID !
FID @ F-SUM? -1 T=                        \ full mode still picks TK-SUM
FID @ F-VAR-COUNT 2 T=
FID @ F-FLD-COUNT 2 T=
FID @ F-VAR-START VS0 !
VS0 @ SV-NAME$ s" quit" CORE-STR= T-TRUE
VS0 @ 1 + SV-NAME$ s" move" CORE-STR= T-TRUE
FID @ VS0 @ enum-ctor-test:PAY-ROWS 0 T=
FID @ VS0 @ 1 + enum-ctor-test:PAY-ROWS 2 T=
VS0 @ 1 + enum-ctor-test:CTOR-SYM 0 T=    \ still no words

\ 23c. Header clauses replay: POLICY and DERIVE reach the same family record.
s" rppol" s" POLICY packed-tag DERIVE eq hash red green ;ENUM"
enum-replay-test:RP-TRY 0 T=
s" rppol" FAMID FID !
FID @ F-POLICY@ PACKED# T=
FID @ F-EQ? -1 T=
FID @ F-HASH? -1 T=

\ 23d. A malformed replayed declaration reports through the SAME renderer as a
\      live one — the end-to-end half of the channel claim the diagnostics leaf
\      could only make for the live path.
DECL-DIAG:PROSE
s" rpbadv<>" s" VARIANT vv stray ;VARIANT ;ENUM" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration 'rpbadv': unexpected token in variant block at 'stray'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" rpbadname" s" red red ;ENUM" enum-replay-test:RP-TRY 7102 T=
s" habu: bad enum declaration 'rpbadname': duplicate variant at 'red'"
DECL-DIAG:HAS? -1 T=

\ 23e. A buffer whose terminator is missing rejects through the front end's own
\      end-of-input gate, exactly as a truncated live declaration does. The
\      consumers therefore cannot register a half-read declaration by accident.
DECL-DIAG:PROSE
s" rpnoend" s" red green" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration 'rpnoend': missing ;ENUM" DECL-DIAG:HAS? -1 T=

\ 23f. A zero-length name reaches the missing-name gate instead of silently
\      promoting the first body token to the family name.
DECL-DIAG:PROSE
s" " s" red green ;ENUM" enum-replay-test:RP-TRY 7107 T=
s" habu: bad enum declaration '': missing name" DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ 23g. A REJECTED replay leaves the stream closed, so the next LIVE declaration
\      reads the input source again rather than a spent buffer. This is the
\      failure the two-exit close in ED-REPLAY exists to prevent.
DECL-DIAG:PROSE
s" rpdangle<>" s" VARIANT" enum-replay-test:RP-TRY 7107 T=
DECL-DIAG:OFF
s" ENUM-DECL:ED-RUN rpafterbad<> VARIANT vv ;VARIANT ;ENUM" TRY 0 T=
s" rpafterbad" FAMID F-VAR-COUNT 1 T=

\ 23h. Re-entry is refused with its own named code rather than retargeting a
\      stream that is already installed.
enum-replay-test:RP-FORCE-OPEN
s" rpbusy" s" red ;ENUM" enum-replay-test:RP-TRY 7177 T=
enum-replay-test:RP-FORCE-CLOSE
s" ENUM-DECL:ED-RUN rpafterbusy<> VARIANT vv ;VARIANT ;ENUM" TRY 0 T=

\ 23i. Live and replayed declarations of the SAME shape register the SAME
\      registry state. Declared in two packages so both families coexist; every
\      reflected field is compared, and the only intended difference is that the
\      replayed one carries no constructor symbol.
package rp-live-test
public
s" ENUM-DECL:ED-RUN shape<> POLICY packed-tag VARIANT alpha ;VARIANT VARIANT beta FIELD px n ;VARIANT ;ENUM" EV
;package
package rp-copy-test
public
s" shape<>" s" POLICY packed-tag VARIANT alpha ;VARIANT VARIANT beta FIELD px n ;VARIANT ;ENUM"
enum-replay-test:RP-TRY 0 T=
;package

s" rp-live-test:shape" FAMID FID !
s" rp-copy-test:shape" FAMID VID !
FID @ F-SUM?      VID @ F-SUM?      T=
FID @ F-ENUM?     VID @ F-ENUM?     T=
FID @ F-VAR-COUNT VID @ F-VAR-COUNT T=
FID @ F-FLD-COUNT VID @ F-FLD-COUNT T=
FID @ F-WIDTH     VID @ F-WIDTH     T=
FID @ F-POLICY@   VID @ F-POLICY@   T=
FID @ F-EQ?       VID @ F-EQ?       T=
FID @ F-HASH?     VID @ F-HASH?     T=
FID @ F-VAR-START VS0 !
VID @ F-VAR-START enum-replay-test:VS1 !
VS0 @ SV-NAME$ enum-replay-test:VS1 @ SV-NAME$ CORE-STR= T-TRUE
VS0 @ 1 + SV-NAME$ enum-replay-test:VS1 @ 1 + SV-NAME$ CORE-STR= T-TRUE
VS0 @ SV-TAG@ enum-replay-test:VS1 @ SV-TAG@ T=
VS0 @ 1 + SV-TAG@ enum-replay-test:VS1 @ 1 + SV-TAG@ T=
FID @ VS0 @ enum-ctor-test:PAY-ROWS  VID @ enum-replay-test:VS1 @ enum-ctor-test:PAY-ROWS  T=
FID @ VS0 @ 1 + enum-ctor-test:PAY-ROWS VID @ enum-replay-test:VS1 @ 1 + enum-ctor-test:PAY-ROWS T=
\ the one intended divergence: words for the live family, none for the replayed
VS0 @ enum-ctor-test:CTOR-SYM 0 <> T-TRUE
enum-replay-test:VS1 @ enum-ctor-test:CTOR-SYM 0 T=

\ ---------------------------------------------------------------------------
\ 24. Control words are reserved in every declaration name position, and the list
\     of them has exactly one owner.
\
\     A family, variant, or field named `if` would be compiled as the control
\     word `if` wherever the generated code names it, so no declaration position
\     may take one. The legacy definers have always refused them (sumtype.f
\     TDECL-RESERVED?); this front end only consulted the grammar-keyword list,
\     so `ENUM-DECL:ED-RUN if red green ;ENUM` was accepted here while
\     `ENUM if red green ;ENUM` was refused 7110 — measured on the parent commit,
\     and the reason the global ENUM token could not move to this front end
\     without losing the reject. The list now lives once, in TYPE-NAME:CONTROL?
\     (src/core/type-family.f); this front end reads it through CONTROL-KW?, the
\     legacy definer reads it from TDECL-RESERVED?, and field rows read it from
\     PF-RESERVED?. A second copy is what let the two drift apart.
\
\     24a walks the whole list so a word silently dropped from the owner is a
\     failure here; 24b proves the two spellings of the same declaration answer
\     identically, code and rendered line; 24c-24d cover the other two name
\     positions; 24e proves the match is on the whole token, not a prefix or a
\     substring, so ordinary names that merely contain a control word still
\     declare.
\ ---------------------------------------------------------------------------
package enum-control-test
public

\ 24a. Every word on the shared list, in the family-name position. Three of them
\      (`?do`, `+loop`, `;match`) never reach the reserved-name gate: their
\      leading byte is not a lowercase letter, so the canonical-tail gate refuses
\      them first with 7101. Both codes are the same on the legacy definer, which
\      runs the same two gates in the same order.
DECL-DIAG:PROSE                          \ these 23 rejects are expected; keep stderr clean
s" ENUM-DECL:ED-RUN if red ;ENUM"        7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN then red ;ENUM"      7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN else red ;ENUM"      7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN begin red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN until red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN again red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN while red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN repeat red ;ENUM"    7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN case red ;ENUM"      7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN of red ;ENUM"        7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN endof red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN endcase red ;ENUM"   7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN do red ;ENUM"        7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN loop red ;ENUM"      7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN leave red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN unloop red ;ENUM"    7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN exit red ;ENUM"      7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN recurse red ;ENUM"   7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN construct red ;ENUM" 7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN match red ;ENUM"     7110 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN ?do red ;ENUM"       7101 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN +loop red ;ENUM"     7101 enum-name-test:REJECT-SAME
s" ENUM-DECL:ED-RUN ;match red ;ENUM"    7101 enum-name-test:REJECT-SAME
DECL-DIAG:OFF

\ 24b. The two spellings agree to the byte: same code, same rendered line. This
\      is the parity the global-token cutover needs, so it is asserted on the
\      legacy definer as well as on the front end.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN if red green ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM if red green ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ 24c. Variant-name position, both modes. These already refused control words
\      before this section existed (the variant gate is TYPE-NAME:VARIANT-REQUIRE,
\      which reads the same owner); the fixtures pin that the shared owner keeps
\      serving them.
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwvc do loop ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'cwvc': name is reserved or already taken at 'do'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwvf<> VARIANT loop ;VARIANT ;ENUM" TRY 7110 T=
s" habu: bad enum declaration 'cwvf': name is reserved or already taken at 'loop'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ 24d. Field-name position. The field row's own gate (PF-RESERVED?) answers, so
\      the code is 7125 — the same code and the same wording a reserved
\      generated-operation name such as `make` already produced, and the same
\      answer the legacy PRODUCT definer now gives for the identical field name
\      (test/type-decl-suite.f pins that half).
DECL-DIAG:PROSE
s" ENUM-DECL:ED-RUN cwfld<> VARIANT vv FIELD then n ;VARIANT ;ENUM" TRY 7125 T=
s" habu: bad enum declaration 'cwfld': reserved field name at 'then'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ 24e. The match is on the WHOLE token. A name that starts with, ends with, or
\      contains a control word is an ordinary name and still declares — in every
\      one of the three positions.
s" ENUM-DECL:ED-RUN iffy dolly matcher constructor elsewhere ;ENUM" EV
s" iffy" FAMID F-VAR-COUNT 4 T=
s" ENUM-DECL:ED-RUN doing<> VARIANT looping FIELD ifs n FIELD thence n ;VARIANT ;ENUM" EV
s" doing" FAMID F-FLD-COUNT 2 T=

;package

\ ---------------------------------------------------------------------------
\ 25. A declaration longer than the legacy body buffer registers WHOLE.
\
\     sumtype.f's compact definer copied the body into one fixed 4096-byte
\     buffer before parsing it, and TDECL-REQUIRE-FIT refused anything longer.
\     That bound belonged to the collection strategy, not to the language: this
\     front end reads tokens straight from the input source with a one-token
\     pushback and never buffers a body, so the same declaration is simply
\     accepted. The risk worth pinning is not the length itself but silent loss
\     — a body that overflows a buffer and registers a prefix, with every tag
\     after the gap shifted (that really happened once on the verify-source path,
\     which is why its buffer raises instead of truncating). So this declares 700
\     variants, ~4.2KB of body, and proves all 700 arrive with the last tag equal
\     to 699. It is package-private, which keeps constructor generation out of it
\     and the case fast.
\ ---------------------------------------------------------------------------
package enum-long-test

$2000 constant SRC-CAP
create SRC-BUF SRC-CAP allot
variable SRC-U
variable LI

700 constant LONG-N

TRUSTED: SRC$ ( -- ptr u8 n ) SRC-BUF SRC-U @ ;
TRUSTED: PUT-C ( n -- ) {: c:n :}
   SRC-U @ SRC-CAP >= IF s" enum-decl-suite: long-enum buffer overflow" 1 die THEN
   c SRC-BUF SRC-U @ + c!  SRC-U @ 1 + SRC-U ! ;
: PUT$ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LI !
   BEGIN LI @ u < WHILE  a LI @ + c@ PUT-C  LI @ 1 + LI !  REPEAT ;
: PUT-DIGIT ( n -- ) 48 + PUT-C ;
: PUT-VARIANT ( n -- ) {: i:n :}       \ one `vNNN ` name, 5 bytes, tag = i
   118 PUT-C
   i 100 / 10 mod PUT-DIGIT
   i 10 / 10 mod PUT-DIGIT
   i 10 mod PUT-DIGIT
   32 PUT-C ;
: BUILD ( -- )                         \ the whole declaration, as one line of source
   0 SRC-U !
   s" ENUM-DECL:ED-RUN longenum " PUT$
   0 LI !
   LONG-N 0 ?DO I PUT-VARIANT LOOP
   s" ;ENUM" PUT$ ;

private
BUILD
SRC$ TRY 0 T=
s" longenum" FAMID FID !
FID @ F-VAR-COUNT LONG-N T=            \ every variant arrived
FID @ F-ENUM? -1 T=
FID @ F-VAR-START VS0 !
VS0 @ SV-NAME$ s" v000" CORE-STR= T-TRUE
VS0 @ SV-TAG@ 0 T=
VS0 @ LONG-N 1 - + SV-NAME$ s" v699" CORE-STR= T-TRUE
VS0 @ LONG-N 1 - + SV-TAG@ LONG-N 1 - T=   \ no gap: the last tag is N-1

;package

\ ---------------------------------------------------------------------------
\ A variant payload may name a family that owns a linear value, and the enum then
\ owns that obligation by containment (dot habu-checker-enum-payload-9e1ae6cc).
\
\ The resolver used to refuse any family whose schemas reach a linear value. That
\ made `FIELD res WSTORE:resident` legal but `FIELD model gpt2-model` reject 7109
\ as an "unknown declaration term", even though gpt2-model owns that resident through
\ a structure field. Both forms carry the resident's obligation, so refusing the
\ nested form bought no soundness; it only blocked the name. What actually
\ enforces the discipline is TFAM-CONCRETE-LINEAR?, which
\ walks each variant's payload schemas, follows an application node into the
\ family it names, and reports the containing enum linear. That walk already
\ recursed, so it needed no change: only the refusal had to go.
\
\ These cases use the live ENUM keyword, which is what real source writes. They
\ pin the registry side; test/type-linear-suite.f pins what the checker does with
\ such a value on a row, including what a MATCH arm may do with the payload.
\
\ The linear owner and the registry reader are owned by package EDLIN. Production
\ writes the owner that way too (maki/infer/weight-store.f owns
\ `WSTORE:resident`), so naming it as a payload type also exercises the qualified
\ spelling the resolver meets in real source.
\ ---------------------------------------------------------------------------
package EDLIN
public
DEFLINEAR EDLIN:tok                                   \ the linear owner these fixtures nest
TRUSTED: LINEAR? ( n -- bool ) TFAM-CONCRETE-LINEAR? ;   \ owns one, directly or through a payload
;package

\ depth 1, legal before this change: a payload naming the linear con itself.
s" ENUM edlone<> VARIANT hold FIELD t EDLIN:tok ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" edlone" FAMID EDLIN:LINEAR? T-TRUE

\ depth 2, the shape this dot unblocks: a payload naming a linear family. This is
\ the frozen load-result shape, with a stand-in for gpt2-model.
s" STRUCTURE edlmodel 0 FIELD res EDLIN:tok FIELD nl n ;STRUCTURE" TRY 0 T=
s" edlmodel" FAMID EDLIN:LINEAR? T-TRUE
s" ENUM edl-load-result<> VARIANT loaded FIELD model edlmodel ;VARIANT VARIANT rejected FIELD code n ;VARIANT ;ENUM" TRY 0 T=
s" edl-load-result" FAMID EDLIN:LINEAR? T-TRUE                    \ linear by containment
s" edl-load-result" FAMID F-VAR-COUNT 2 T=
s" edl-load-result" FAMID F-FLD-COUNT 2 T=                    \ one named payload per variant
s" edl-load-result" FAMID F-WIDTH 3 T=                        \ tag cell + the widest payload (two cells)

\ depth 3: the walk recurses again rather than stopping one level down.
s" STRUCTURE edl-via-load 0 FIELD result edl-load-result ;STRUCTURE" TRY 0 T=
s" edl-via-load" FAMID EDLIN:LINEAR? T-TRUE
s" ENUM edlnest<> VARIANT loaded FIELD result edl-via-load ;VARIANT VARIANT rejected FIELD code n ;VARIANT ;ENUM" TRY 0 T=
s" edlnest" FAMID EDLIN:LINEAR? T-TRUE

\ the control: a chain with no linear value anywhere stays non-linear, so the
\ walk is answering about the chain and not about nesting as such.
s" STRUCTURE edlplain 0 FIELD v n ;STRUCTURE" TRY 0 T=
s" ENUM edlctl<> VARIANT hold FIELD m edlplain ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" edlctl" FAMID EDLIN:LINEAR? 0= T-TRUE

\ wrong role: the same word in the FIELD NAME position is a name, never a type,
\ so it neither resolves nor makes the enum linear.
s" ENUM edlrole<> VARIANT hold FIELD edlmodel n ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" edlrole" FAMID EDLIN:LINEAR? 0= T-TRUE

\ reordering: naming a family before it is declared is still an unknown type,
\ so acceptance comes from resolution and not from the spelling alone.
DECL-DIAG:PROSE
s" ENUM edlfwd<> VARIANT hold FIELD m edllater ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlfwd': unknown declaration term at 'edllater'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ A name that DOES resolve says why it cannot be a payload type. A parametric
\ family named bare is the one such case source can reach, and reporting it as
\ "unknown declaration term" sent readers looking for a declaration that was right
\ there. A name that resolves to nothing still reports unknown, which is true.
\ ---------------------------------------------------------------------------
s" ENUM edlgen<a> VARIANT hold FIELD m a ;VARIANT ;ENUM" TRY 0 T=
DECL-DIAG:PROSE
s" ENUM edlgenuse<> VARIANT hold FIELD m edlgen ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlgenuse': declaration term family needs type arguments at 'edlgen'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ A variant payload may NOT name a POINTER to a linear value.
\
\ Accepting the family spelling above also let `FIELD p ptr edlmodel` through, and
\ that spelling is the opposite of the one above: a pointer is a non-owning
\ boundary, so the linearity walk stops at the pointer node and the containing
\ enum reads NON-linear. It would then copy and drop freely while a linear
\ resource sat behind the address — the resource laundered, which is exactly what
\ the containment rule exists to prevent. Nothing in the tree writes that
\ spelling, and it was legal only because no producer route happened to reach it.
\ It is refused at the declaration door instead.
\
\ This is the same rule structure-decl.f applies to a field, decided by the same
\ node walk (TFCL-NODE?), because a variant payload carries exactly the same
\ obligation as a field. So the con spelling and the family spelling are refused
\ alike, and the refusal holds at every depth: `ptr ptr edlmodel` rejects on the
\ inner recursion, which anchors the diagnostic on the family that actually owns
\ the resource. A pointer to something that owns nothing stays legal.
\ ---------------------------------------------------------------------------

\ the two spellings side by side. Naming the value owns it; pointing at it cannot.
s" ENUM edlowns<> VARIANT hold FIELD m edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" edlowns" FAMID EDLIN:LINEAR? T-TRUE

DECL-DIAG:PROSE
s" ENUM edlptr<> VARIANT hold FIELD p ptr edlmodel ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptr': pointer to a linear declaration term is not allowed at 'edlmodel'"
DECL-DIAG:HAS? -1 T=

\ the con spelling launders the same way, so the same rule refuses it, and the
\ diagnostic names the con it found rather than some enclosing family.
DECL-DIAG:PROSE
s" ENUM edlptrcon<> VARIANT hold FIELD p ptr EDLIN:tok ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptrcon': pointer to a linear declaration term is not allowed at 'EDLIN:tok'"
DECL-DIAG:HAS? -1 T=

\ depth: a second pointer does not launder past the rule. The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" ENUM edlptr2<> VARIANT hold FIELD p ptr ptr edlmodel ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptr2': pointer to a linear declaration term is not allowed at 'edlmodel'"
DECL-DIAG:HAS? -1 T=

\ reaching the resource through a nested family or through a sum is still reaching
\ it, so a pointer to either is refused for the same reason.
DECL-DIAG:PROSE
s" ENUM edlptrsum<> VARIANT hold FIELD p ptr edl-load-result ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptrsum': pointer to a linear declaration term is not allowed at 'edl-load-result'"
DECL-DIAG:HAS? -1 T=

\ every variant is checked, not just the first, and the token names the offending
\ payload rather than the declaration's first payload.
DECL-DIAG:PROSE
s" ENUM edlptrlate<> VARIANT good FIELD c n ;VARIANT VARIANT bad FIELD p ptr edlmodel ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptrlate': pointer to a linear declaration term is not allowed at 'edlmodel'"
DECL-DIAG:HAS? -1 T=

\ the controls. A pointer to a family that owns nothing declares and leaves the
\ enum non-linear — so the rejects above answer the POINTEE's linearity and not
\ the word `ptr`.
s" ENUM edlptrok<> VARIANT hold FIELD p ptr edlplain ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" edlptrok" FAMID EDLIN:LINEAR? 0= T-TRUE
s" edlptrok" FAMID F-WIDTH 2 T=                       \ tag cell + the one-cell pointer payload
s" ENUM edlptrok2<> VARIANT hold FIELD p ptr ptr edlplain ;VARIANT ;ENUM" TRY 0 T=
s" edlptrok2" FAMID EDLIN:LINEAR? 0= T-TRUE
s" ENUM edlptrn<> VARIANT hold FIELD p ptr n ;VARIANT ;ENUM" TRY 0 T=
s" edlptrn" FAMID EDLIN:LINEAR? 0= T-TRUE

\ wrong role: the same words in the FIELD NAME position are names, never types.
\ Neither resolves, neither is refused, and neither makes the enum linear.
s" ENUM edlptrrole<> VARIANT hold FIELD ptr n ;VARIANT ;ENUM" TRY 0 T=
s" edlptrrole" FAMID EDLIN:LINEAR? 0= T-TRUE
s" ENUM edlptrrole2<> VARIANT hold FIELD edlmodel n ;VARIANT ;ENUM" TRY 0 T=
s" edlptrrole2" FAMID EDLIN:LINEAR? 0= T-TRUE

\ hostile comments. A variant block has NO comment syntax: its reader takes plain
\ tokens, so `(` and `\` are ordinary tokens in a type or clause position and the
\ whole declaration is malformed. Text that merely reads like a payload can
\ therefore neither smuggle a type in nor be quietly skipped — and it rejects
\ 7107, a DIFFERENT code from this rule's 7109, so no verdict here is ever
\ produced by scanning prose.
DECL-DIAG:PROSE
s" ENUM edlptrpar<> VARIANT hold FIELD v n ( FIELD p ptr edlmodel ) ;VARIANT ;ENUM" TRY 7107 T=
s" habu: bad enum declaration 'edlptrpar': unexpected token in variant block at '('"
DECL-DIAG:HAS? -1 T=                                  \ the paren itself was the token
DECL-DIAG:PROSE
s" ENUM edlptrbsl<> VARIANT hold FIELD v n \ FIELD p ptr edlmodel" TRY 7107 T=
s" habu: bad enum declaration 'edlptrbsl': unexpected token in variant block at '\'"
DECL-DIAG:HAS? -1 T=                                  \ and so was the backslash
\ and trailing text after the offending payload cannot suppress the reject,
\ because the payload is resolved before anything following it is read.
DECL-DIAG:PROSE
s" ENUM edlptrtail<> VARIANT hold FIELD p ptr edlmodel ( note ) ;VARIANT ;ENUM" TRY 7109 T=
s" habu: bad enum declaration 'edlptrtail': pointer to a linear declaration term is not allowed at 'edlmodel'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" enum-decl-suite: failures" 1 die ;
REPORT
