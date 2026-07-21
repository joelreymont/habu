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
\ and DERIVE headers reach both the family record and the event stream; the shared
\ variant open/close and field events sequence correctly and each field event
\ carries the open variant selector; every reject anchor (mixed mode,
\ arity-then-compact, compact header, compact positional payload, missing
\ ;VARIANT / ;ENUM, malformed arity, empty enum, reserved / case family name,
\ duplicate variant, and the field record's own dup / reserved / case / schema
\ gate) fires at the offending token; a mid-declaration reject leaves every
\ registry cursor byte-identical to the pre-declaration baseline; and the
\ deterministic snapshot identity is reproducible for an identical declaration
\ against a fresh registry.
\ A failure prints F<index> + detail; REPORT exits 1.
\
\ ENUM-DECL:ED-RUN is the front-end entry: the global ENUM token still belongs to
\ the legacy sumtype.f definer until the hard cutover, so this suite drives the
\ new front end through its package-qualified entry rather than a global keyword.

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
\ 7. POLICY reaches both the family record and the event stream (full mode).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN opt 0 POLICY packed-tag VARIANT a ;VARIANT ;ENUM" EV
s" opt" FAMID F-POLICY@ PACKED# T=                    \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 8. DERIVE reaches both the family record and the event stream; two features on
\    one clause are accepted, each recorded once (full mode).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" ENUM-DECL:ED-RUN der 0 DERIVE eq hash VARIANT a ;VARIANT ;ENUM" EV
s" der" FAMID F-EQ? T-TRUE                            \ eq derived
s" der" FAMID F-HASH? T-TRUE                          \ hash derived
2 DECL-EVENT:DERIVE? T-TRUE                           \ two DERIVE events after DECL + ARITY
3 DECL-EVENT:DERIVE? T-TRUE

\ ---------------------------------------------------------------------------
\ 9. A duplicate FIELD name inside a variant rejects through the field record
\    (E-TFAM-DUP 7102) and the whole provisional declaration rolls back
\    byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
TYPE-FIELD:COUNT PFB !
DECL-EVENT:COUNT DEVB !
s" ENUM-DECL:ED-RUN dupf 0 VARIANT a FIELD z n FIELD z n ;VARIANT ;ENUM" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SCHN@ RB-SCH @ T=                                     \ schema nodes retired
SUMVN@ RB-SUMV @ T=                                   \ variant rows retired
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
DECL-EVENT:COUNT DEVB @ T=                            \ nothing new published

\ ---------------------------------------------------------------------------
\ 10. A duplicate variant name rejects (E-TFAM-DUP 7102 from SUMV-ADD) and the
\     whole provisional declaration rolls back byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
s" ENUM-DECL:ED-RUN dupv red red ;ENUM" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SUMVN@ RB-SUMV @ T=                                   \ variant rows retired

\ ---------------------------------------------------------------------------
\ 11. Grammar / mode / arity / name / field rejects, each at the offending token.
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN emix red VARIANT a ;VARIANT ;ENUM" TRY 7107 T=       \ mixed modes (block token in compact)
s" ENUM-DECL:ED-RUN eac 2 red ;ENUM" TRY 7107 T=                         \ arity then a bare compact variant
s" ENUM-DECL:ED-RUN ech POLICY packed-tag red ;ENUM" TRY 7107 T=         \ header clause in a compact body
s" ENUM-DECL:ED-RUN ecp red FIELD y n ;ENUM" TRY 7107 T=                 \ positional/named payload in compact
s" ENUM-DECL:ED-RUN emv 0 VARIANT a FIELD x n ;ENUM" TRY 7107 T=         \ missing ;VARIANT
s" ENUM-DECL:ED-RUN eme red green" TRY 7107 T=                           \ missing ;ENUM
s" ENUM-DECL:ED-RUN ear 24 VARIANT a ;VARIANT ;ENUM" TRY 7108 T=         \ arity above the shared 23 cap
s" ENUM-DECL:ED-RUN eempty ;ENUM" TRY 7107 T=                            \ an enum needs a variant
s" ENUM-DECL:ED-RUN enum red ;ENUM" TRY 7110 T=                          \ reserved opener keyword as a name
s" ENUM-DECL:ED-RUN Bad red ;ENUM" TRY 7101 T=                           \ upper-case family name (case)
s" ENUM-DECL:ED-RUN n red ;ENUM" TRY 7110 T=                             \ single-letter family name
s" ENUM-DECL:ED-RUN erf 0 VARIANT a FIELD make n ;VARIANT ;ENUM" TRY 7125 T=   \ reserved field name
s" ENUM-DECL:ED-RUN ecf 0 VARIANT a FIELD Zed n ;VARIANT ;ENUM" TRY 7101 T=    \ upper-case field name (case)
s" ENUM-DECL:ED-RUN ebs 0 VARIANT a FIELD x nope ;VARIANT ;ENUM" TRY 7109 T=   \ unresolved field type
s" ENUM-DECL:ED-RUN euc 0 VARIANT a FIELD x Q ;VARIANT ;ENUM" TRY 7109 T=      \ upper-case single-letter type
s" ENUM-DECL:ED-RUN epa 0 VARIANT a FIELD x a ;VARIANT ;ENUM" TRY 7109 T=      \ parameter outside declared arity
s" ENUM-DECL:ED-RUN epg 6 VARIANT a FIELD x h ;VARIANT ;ENUM" TRY 7109 T=      \ parameter 6 is outside arity 6

\ ---------------------------------------------------------------------------
\ 12. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
\ ---------------------------------------------------------------------------
s" ENUM-DECL:ED-RUN twice red ;ENUM" EV
s" ENUM-DECL:ED-RUN twice red ;ENUM" TRY 7102 T=

\ ---------------------------------------------------------------------------
\ 13. Deterministic snapshot identity: an identical declaration against a fresh
\     registry (family id restored, event log reset) folds to the same identity;
\     a different declaration folds to a different one.
\ ---------------------------------------------------------------------------
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

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" enum-decl-suite: failures" 1 die ;
REPORT
