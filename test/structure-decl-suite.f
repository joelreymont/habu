\ structure-decl-suite.f — behavior + rollback suite for the STRUCTURE typed
\ declaration front end (src/core/structure-decl.f, package STRUCTURE-DECL; dot
\ habu-structure-parse-typed-c5a01e1f). Run BY THE ENGINE over stdin, exactly
\ like test/decl-event-suite.f (the definer parses the live input stream and
\ mutates the type registry, so it resolves only at top-level interpret):
\     bin/hb < test/structure-decl-suite.f
\ Proves: a successful declaration persists its family + fields; field events
\ reach TYPE-FIELD reflection with the declared names, schemas, order, slots, and
\ byte offsets; POLICY and DERIVE reach both the event stream and the family
\ record; every reject anchor (E-TDECL-* family + the field record's own name
\ gate) fires at the offending token; a mid-declaration reject leaves every
\ registry cursor byte-identical to the pre-declaration baseline; and the
\ deterministic snapshot identity is reproducible for an identical declaration
\ against a fresh registry. A failure prints F<index> + detail; REPORT exits 1.

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

\ --- boundary shims: the STRUCTURE opener, evaluate, and the sealed pre-hook
\ registry / schema reflection words are reached at top level through named
\ trusted forwarders (the same idiom test/decl-event-suite.f uses).
TRUSTED: EV ( ptr u8 n -- ) evaluate ;
TRUSTED: TRY ( ptr u8 n -- n ) ['] EV catch ;            \ evaluate under catch -> throw code
TRUSTED: FAMID ( ptr u8 n -- n ) TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop ;
TRUSTED: FAM-POLICY@ ( n -- n ) TFAM-LAYOUT-POLICY@ ;
TRUSTED: FAM-EQ? ( n -- bool ) TFAM-DERIVE-EQ? ;
TRUSTED: FAM-HASH? ( n -- bool ) TFAM-DERIVE-HASH? ;
TRUSTED: FAM-SLOTS@ ( n -- n ) TFAM-WIDTH@ ;
TRUSTED: SCH-ROOT@ ( n -- n ) SCHEMA-ROOT@ ;
TRUSTED: SCH-TAG@ ( n -- n ) SCHEMA-TAG@ ;
TRUSTED: SCH-A@ ( n -- n ) SCHEMA-A@ ;
TRUSTED: PACKED# ( -- n ) TL-PACKED-TAG ;
TRUSTED: STACK# ( -- n ) TL-STACK-CELL-TAG ;
TRUSTED: SCHCON# ( -- n ) SCH-CON ;
TRUSTED: CCN# ( -- n ) CC-N ;

\ --- registry snapshot, so a reject can be proven byte-identical and the
\ identity test can re-run an identical declaration against a fresh registry
\ (the in-process proxy for a fresh process: same family id both times).
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

\ ---------------------------------------------------------------------------
\ 1. A product declaration persists its family and both field rows, reachable
\    through TYPE-FIELD reflection in declaration order with the right layout.
\ ---------------------------------------------------------------------------
TFAM-N@ FID !
TYPE-FIELD:COUNT B !
s" STRUCTURE point 0 FIELD x n FIELD y n ;STRUCTURE" EV
TFAM-N@ FID @ 1 + T=                                  \ exactly one new family
TYPE-FIELD:COUNT B @ 2 + T=                           \ two committed field rows
B @ TYPE-FIELD:NAME$ s" x" CORE-STR= T-TRUE           \ first field is x
B @ TYPE-FIELD:SLOT@ 0 T=                             \ x at cell slot 0
B @ TYPE-FIELD:BYTE-OFF@ 0 T=
B @ 1 + TYPE-FIELD:NAME$ s" y" CORE-STR= T-TRUE       \ second field is y
B @ 1 + TYPE-FIELD:SLOT@ 1 T=                         \ y at cell slot 1
B @ 1 + TYPE-FIELD:BYTE-OFF@ CELL T=                  \ y at byte offset = one cell
s" point" FAMID FAM-SLOTS@ 2 T=                       \ product width = two field cells

\ ---------------------------------------------------------------------------
\ 2. The field schema reaches reflection: field type `n` is a concrete con node.
\ ---------------------------------------------------------------------------
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCH-TAG@ SCHCON# T=                            \ a concrete-con schema node
NODE @ SCH-A@ CCN# T=                                 \ con code = n

\ ---------------------------------------------------------------------------
\ 3. Field events reach the shared event stream in order (DECL, FIELD, FIELD).
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE evt 0 FIELD a n FIELD b n ;STRUCTURE" EV
DECL-EVENT:COUNT 4 T=                                 \ DECL + ARITY + two FIELD events
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:ARITY? T-TRUE
2 DECL-EVENT:FIELD? T-TRUE
3 DECL-EVENT:FIELD? T-TRUE
2 DECL-EVENT:VAR@ DECL-EVENT:NO-VARIANT T=            \ structure fields carry no variant

\ ---------------------------------------------------------------------------
\ 4. POLICY reaches both the family record and the event stream.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE ppk 0 POLICY packed-tag FIELD x n ;STRUCTURE" EV
s" ppk" FAMID FAM-POLICY@ PACKED# T=                  \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 5. DERIVE reaches both the family record and the event stream; two features
\    on one clause are accepted, each recorded once.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE der 0 DERIVE eq hash FIELD x n ;STRUCTURE" EV
s" der" FAMID FAM-EQ? T-TRUE                          \ eq derived
s" der" FAMID FAM-HASH? T-TRUE                        \ hash derived
2 DECL-EVENT:DERIVE? T-TRUE                           \ two DERIVE events after DECL + ARITY
3 DECL-EVENT:DERIVE? T-TRUE

\ ---------------------------------------------------------------------------
\ 6. A duplicate field name rejects through the field record (E-TFAM-DUP 7102)
\    and the whole provisional declaration rolls back byte-identically.
\ ---------------------------------------------------------------------------
REG-MARK
TYPE-FIELD:COUNT PFB !
DECL-EVENT:COUNT DEVB !
s" STRUCTURE dupf 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SCHN@ RB-SCH @ T=                                     \ schema nodes retired
SUMVN@ RB-SUMV @ T=                                   \ variant cursor unchanged
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
DECL-EVENT:COUNT DEVB @ T=                            \ nothing new published

\ ---------------------------------------------------------------------------
\ 7. Grammar / name / arity / type / policy / derive / terminator rejects, each
\    at the offending token with the E-TDECL-* family code.
\ ---------------------------------------------------------------------------
s" STRUCTURE field 0 ;STRUCTURE" TRY 7110 T=                     \ reserved keyword name
s" STRUCTURE n 0 ;STRUCTURE" TRY 7110 T=                         \ single-letter type name
s" STRUCTURE Bad 0 ;STRUCTURE" TRY 7101 T=                       \ upper-case name (case)
s" STRUCTURE foo q ;STRUCTURE" TRY 7108 T=                       \ non-numeric arity
s" STRUCTURE foo 99 ;STRUCTURE" TRY 7108 T=                      \ arity above the a..z cap
s" STRUCTURE foo 0 FIELD a nope ;STRUCTURE" TRY 7109 T=          \ unresolved field type
s" STRUCTURE foo 0 VARIANT q ;VARIANT ;STRUCTURE" TRY 7107 T=    \ mixed legacy token
s" STRUCTURE foo 0 FIELD x n" TRY 7107 T=                        \ missing ;STRUCTURE
s" STRUCTURE foo 0 POLICY nope FIELD x n ;STRUCTURE" TRY 7116 T= \ unknown layout policy
s" STRUCTURE foo 0 DERIVE nope FIELD x n ;STRUCTURE" TRY 7119 T= \ unknown derive feature

\ ---------------------------------------------------------------------------
\ 8. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
\ ---------------------------------------------------------------------------
s" STRUCTURE twice 0 FIELD x n ;STRUCTURE" EV
s" STRUCTURE twice 0 FIELD x n ;STRUCTURE" TRY 7102 T=

\ ---------------------------------------------------------------------------
\ 9. Deterministic snapshot identity: an identical declaration against a fresh
\    registry (family id restored, event log reset) folds to the same identity;
\    a different declaration folds to a different one.
\ ---------------------------------------------------------------------------
REG-MARK
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC !                              \ RC holds identity A
REG-RESTORE                                           \ retire family; fresh registry
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC @ T=                           \ identical declaration -> same identity
REG-RESTORE
DECL-EVENT:RESET
s" STRUCTURE ident 0 FIELD x n FIELD y n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC @ <> T-TRUE                    \ different declaration -> different identity

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-decl-suite: failures" 1 die ;
REPORT
