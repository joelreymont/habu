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
\ registry cursor byte-identical to the pre-declaration baseline; the
\ deterministic snapshot identity is reproducible for an identical declaration
\ against a fresh registry; and — the reconciliation seam — a public STRUCTURE
\ with fields generates a working sealed FAMILY:MAKE/UNMAKE ctor package that
\ round-trips bit-identically in declaration order, while a rejected, an opaque
\ zero-field, and a private declaration each generate no ctor words.
\ A failure prints F<index> + detail; REPORT exits 1.

require test/checker-assert.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture

\ Every reject below now renders a declaration diagnostic. Capture it from the
\ start so the suite's own output stays clean; the last section turns the
\ capture into assertions about the exact rendered line.
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
variable SV0   \ variant-cursor watermark for the ctor-generation gating checks

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
s" STRUCTURE foo 24 ;STRUCTURE" TRY 7108 T=                      \ arity above the shared 23 cap
s" STRUCTURE foo 0 FIELD a nope ;STRUCTURE" TRY 7109 T=          \ unresolved field type
s" STRUCTURE foo 0 FIELD a Q ;STRUCTURE" TRY 7109 T=             \ upper-case single-letter type
s" STRUCTURE foo 0 FIELD a a ;STRUCTURE" TRY 7109 T=             \ parameter outside declared arity
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
\    a different declaration folds to a different one. These declarations are
\    PRIVATE so the in-process registry-restore trick can re-declare the same
\    family id without regenerating (and colliding on) its sealed ctor words —
\    the snapshot identity is a DECL-EVENT property (family id + events, not
\    visibility), so private and public declarations fold identically. The
\    public ctor-generation seam is proven separately in cases 10-13.
\ ---------------------------------------------------------------------------
package IDENTTEST
private
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
public
;package

\ ---------------------------------------------------------------------------
\ 10. End-to-end wiring (the ;STRUCTURE -> STRUCTURE-MAKE:GENERATE seam): a public
\     STRUCTURE with fields, declared from real syntax, generates a working sealed
\     MAKE/UNMAKE ctor package (two variant rows: make + unmake). MAKE then UNMAKE
\     is a bit-identical physical no-op that preserves declaration order.
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
s" STRUCTURE tri 0 FIELD a n FIELD b n FIELD c n ;STRUCTURE" EV
SUMVN@ SV0 @ 2 + T=                                   \ exactly two ctor variant rows generated
: TRIRT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE ;      \ callable sealed ctor words
11 22 33 TRIRT 33 T= 22 T= 11 T=                      \ declaration order + values round-trip bit-identically

\ ---------------------------------------------------------------------------
\ 11. The generated MAKE/UNMAKE are exact type inverses across a cell + byte +
\     generic field mix (the make-suite candidate whitebox check, driven here from
\     real STRUCTURE syntax): `abc` has a generic parameter field, a cell field,
\     and a byte field, and at a concrete instantiation (parameter a = n) the
\     round-trip certifies with its declaration-order field types.
\ ---------------------------------------------------------------------------
s" STRUCTURE abc 1 FIELD p a FIELD k n FIELD c char ;STRUCTURE" EV
s" ABCRT ( n n char -- n n char ) ABC:MAKE ABC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic-only structure round-trips its parameter at a concrete instantiation.
s" STRUCTURE gp 1 FIELD v a ;STRUCTURE" EV
: GPRT ( n -- n ) GP:MAKE GP:UNMAKE ;
42 GPRT 42 T=

\ The post-hook STRUCTURE parser consumes the shared declaration alphabet.  A
\ maximum-arity declaration accepts g and z while f/n/r stay scalar fields; the
\ exact inverse table is tested once in type-family-suite.f.
s" STRUCTURE sdmap 23 FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e FIELD p05 g FIELD flag f FIELD integer n FIELD real r FIELD last z ;STRUCTURE" EV
s" SDMAPRT ( n n n n n n bool n r char -- n n n n n n bool n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDMAPBAD ( n n n n n bool n n r char -- n n n n n bool n n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ 12. Constructor generation is gated (SD-MAKEABLE?): only a PUBLIC structure WITH
\     fields owns a ctor package. A rejected declaration and an opaque zero-field
\     declaration each generate NO ctor words (the variant cursor is unchanged).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
s" STRUCTURE badf 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=   \ duplicate field rejects
SUMVN@ SV0 @ T=                                       \ no ctor words from a rejected declaration
SUMVN@ SV0 !
s" STRUCTURE opaque 0 ;STRUCTURE" EV                  \ zero-field opaque one-cell family (docs/type-families.md §2.2)
SUMVN@ SV0 @ T=                                       \ an opaque family owns no MAKE/UNMAKE

\ ---------------------------------------------------------------------------
\ 13. A PRIVATE structure with fields owns no construction surface either:
\     SD-MAKEABLE? requires a public family, matching the shipped product
\     precedent (private products publish no MAKE/UNMAKE, fail-closed).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
package SDPRIVTEST
private
STRUCTURE hidden 0 FIELD x n ;STRUCTURE
public
;package
SUMVN@ SV0 @ T=                                       \ private structure generates no ctor words

\ ---------------------------------------------------------------------------
\ Reject diagnostics. Before this section existed, SD-RUN threw every code below
\ with no message at all, while the legacy PRODUCT definer printed
\ "habu: bad product declaration 'x': <reason> at 'tok'". STRUCTURE now renders
\ through render.f's TDECL-DIAG, the same producer the legacy definers use, so
\ the assertions here read the exact bytes that reach the diagnostic channel,
\ captured through the same DIAG-BUFFER! / DIAG-BUFFER$ pair
\ tools/check-core.f's CHK-DECL-CAPTURE and CHK-DECL-FLUSH use. That proves the
\ channel, not an end-to-end run of the check tool: check-core does not scan
\ STRUCTURE at all until the buffer-driven registration entry lands.
\
\ Each case asserts the WHOLE rendered line, not a substring, so a message that
\ named the wrong family, dropped the token, or picked a stale reason fails.
\ ---------------------------------------------------------------------------

\ one case per reject code this suite pins, each rendering family + reason +
\ token and rethrowing the exact code.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN Sdgcase 0 ;STRUCTURE" TRY 7101 T=
s" habu: bad structure declaration 'Sdgcase': name must be a lowercase family tail at 'Sdgcase'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN n 0 ;STRUCTURE" TRY 7110 T=
s" habu: bad structure declaration 'n': reserved name at 'n'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdup 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgdup': duplicate field name at 'z'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgar q ;STRUCTURE" TRY 7108 T=
s" habu: bad structure declaration 'sdgar': arity must be a decimal, at most 23 parameters at 'q'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgbad 0 FIELD a nosuchtype ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdgbad': unknown field type at 'nosuchtype'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgpol 0 POLICY nosuch ;STRUCTURE" TRY 7116 T=
s" habu: bad structure declaration 'sdgpol': unknown layout policy at 'nosuch'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdrv 0 DERIVE nosuch ;STRUCTURE" TRY 7119 T=
s" habu: bad structure declaration 'sdgdrv': unknown derive feature at 'nosuch'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgstray 0 VARIANT ;STRUCTURE" TRY 7107 T=
s" habu: bad structure declaration 'sdgstray': unexpected token in structure declaration at 'VARIANT'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgfn 0 FIELD make n ;STRUCTURE" TRY 7125 T=
s" habu: bad structure declaration 'sdgfn': reserved field name at 'make'" DECL-DIAG:HAS? -1 T=

\ a terminator that never arrives anchors on the family, exactly as the legacy
\ unterminated-declaration packet does (sumtype.f TDECL-PRODUCT-NOEND-BODY).
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgnoend 0 FIELD a n" TRY 7107 T=
s" habu: bad structure declaration 'sdgnoend': missing ;STRUCTURE at 'sdgnoend'"
DECL-DIAG:HAS? -1 T=

\ the self-referential FIELD. The field type resolves to the family being
\ declared, whose width is not bound until close, so the field record refuses the
\ layout (E-PF-LAYOUT). That reject used to be a bare 7127 with no message; it now
\ names the declaration, the offending field, and the reason. The legacy PRODUCT
\ definer refuses the same shape earlier, at its own recursion gate (7117), so the
\ codes differ by construction — the front end has no recursion pre-check yet.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgself 0 FIELD selffld sdgself ;STRUCTURE" TRY 7127 T=
s" habu: bad structure declaration 'sdgself': invalid field layout metadata at 'selffld'"
DECL-DIAG:HAS? -1 T=

\ no family leaks from one declaration into the next: a declaration that fails
\ before its own name is read reports an empty family, not sdgself.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN" TRY 7107 T=
s" habu: bad structure declaration '': missing name" DECL-DIAG:HAS? -1 T=
s" sdgself" DECL-DIAG:HAS? 0 T=

\ hostile declarations: a field spelled like a fragment of the message is
\ reported as the token it is, and exactly one line is emitted.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgforge 0 FIELD duplicate n FIELD duplicate n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgforge': duplicate field name at 'duplicate'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:LEN 80 T=

\ a family name spelled like one of the JSON packet's own keys keeps key and
\ value distinguishable, and a token carrying a double quote is escaped rather
\ than closing the JSON string early.
DECL-DIAG:JSON
s" STRUCTURE-DECL:SD-RUN token 0 FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
s\" \"decl\":\"structure\"" DECL-DIAG:HAS? -1 T=
s\" \"family\":\"token\"" DECL-DIAG:HAS? -1 T=
s\" \"token\":\"z\"" DECL-DIAG:HAS? -1 T=
s\" \"reason\":\"duplicate field name\"" DECL-DIAG:HAS? -1 T=

DECL-DIAG:JSON
s\" STRUCTURE-DECL:SD-RUN sdgquote 0 FIELD aq\"b n ;STRUCTURE" TRY 7101 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? -1 T=
s\" \"family\":\"sdgquote\"" DECL-DIAG:HAS? -1 T=

\ accepted declarations stay silent, and a rendered reject still leaves the
\ registry byte-identical: rendering happens after the coordinator has rolled
\ back and touches no registry cursor.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgok1 0 FIELD a n ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok2 0 ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok3 1 FIELD a a ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok4 0 DERIVE eq FIELD a n ;STRUCTURE" TRY 0 T=
DECL-DIAG:SILENT? -1 T=

\ the armed duplicate-family reason, which no case above reaches: it is armed at
\ SD-REGISTER, immediately before the registry call that can raise it.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice 0 FIELD a n ;STRUCTURE" TRY 0 T=
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice 0 FIELD b n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgtwice': duplicate family at 'sdgtwice'"
DECL-DIAG:HAS? -1 T=

\ SD-CLOSE arms no reason of its own: the generator's rejects are raised past the
\ last token this front end holds, so they are answered from the packet's code
\ table. The over-long span cap and its "..." marker belong to the shared packet
\ and are pinned once, in test/enum-decl-suite.f section 22h.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgvrsv 0 FIELD z sdgvrsv ;STRUCTURE" TRY 7127 T=
s" habu: bad structure declaration 'sdgvrsv': invalid field layout metadata at 'z'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ SD-REPLAY — registering a STRUCTURE from tokens a tool already lexed.
\
\ Before this entry existed, STRUCTURE had no registration path for a tool that
\ is reading source rather than interpreting it, so tools/check-core.f simply
\ skipped the keyword: the family was never registered, and the next declaration
\ that named it as a payload type rejected with "unknown payload type". That is
\ the live bug this closes — `bin/hb --load tools/check.f -- maki/db/promotion.f`
\ could not get past maki/db/obligation.f's `STRUCTURE evidence`.
\
\ Registration here includes the family's MAKE/UNMAKE variant rows and their
\ constructor package, because that is how a later `FAMILY:MAKE` in the same
\ source resolves. Only the rendering of those two words is skipped.
\ ---------------------------------------------------------------------------
package struct-replay-test
public

TRUSTED: RP-EV ( ptr u8 n ptr u8 n -- ) STRUCTURE-DECL:SD-REPLAY ;
TRUSTED: RP-TRY ( ptr u8 n ptr u8 n -- n ) ['] RP-EV catch ;
TRUSTED: SV-NAME$ ( n -- ptr u8 n ) SUMV-NAME$ ;
TRUSTED: CTOR-PKG$ ( n -- ptr u8 n ) SUMV-CTOR-PKG$ ;
TRUSTED: CTOR-SYM ( n -- n ) SUMV-CTOR-SYM@ ;
TRUSTED: FAM-VAR-START ( n -- n ) TFAM-VAR-START@ ;
TRUSTED: FAM-VAR-COUNT ( n -- n ) TFAM-VAR-COUNT@ ;
TRUSTED: FAM-FLD-COUNT ( n -- n ) TFAM-FLD-COUNT@ ;
TRUSTED: FAM-PRODUCT? ( n -- bool ) TFAM-PRODUCT? ;
TRUSTED: DICT-RECS ( -- n ) ndict@ ;

variable RP-DICT
: DICT-MARK ( -- ) DICT-RECS RP-DICT ! ;
: DICT-SAME ( -- ) DICT-RECS RP-DICT @ T= ;

private
;package

\ A replayed STRUCTURE registers the family, its fields, and its make/unmake
\ rows, and moves the native dictionary not at all.
struct-replay-test:DICT-MARK
s" rpsd" s" 0 FIELD one n FIELD two n ;STRUCTURE" struct-replay-test:RP-TRY 0 T=
struct-replay-test:DICT-SAME

s" rpsd" FAMID FID !
FID @ struct-replay-test:FAM-PRODUCT? -1 T=
FID @ struct-replay-test:FAM-FLD-COUNT 2 T=
FID @ FAM-SLOTS@ 2 T=
FID @ struct-replay-test:FAM-VAR-COUNT 2 T=          \ make + unmake rows registered
FID @ struct-replay-test:FAM-VAR-START SV0 !
SV0 @ struct-replay-test:SV-NAME$ s" make" CORE-STR= T-TRUE
SV0 @ 1 + struct-replay-test:SV-NAME$ s" unmake" CORE-STR= T-TRUE
SV0 @ struct-replay-test:CTOR-PKG$ s" RPSD" CORE-STR= T-TRUE

\ registration yes, generation no: no constructor symbol, and the word does not
\ resolve (1 = uncheckable; the identical live declaration answers -1, accepted).
SV0 @ struct-replay-test:CTOR-SYM 0 T=
SV0 @ 1 + struct-replay-test:CTOR-SYM 0 T=
s" S1 ( n n -- rpsd ) RPSD:MAKE" CHECK-QUIET-CANDIDATE! 1 T=

\ Header clauses replay onto the same family record.
s" rpsdh" s" 0 POLICY packed-tag DERIVE eq FIELD one n ;STRUCTURE"
struct-replay-test:RP-TRY 0 T=
s" rpsdh" FAMID FID !
FID @ FAM-POLICY@ PACKED# T=
FID @ FAM-EQ? -1 T=

\ A field typed by a family REGISTERED THROUGH AN EARLIER REPLAY resolves — this
\ is the exact shape that was broken: obligation.f declares `STRUCTURE evidence`
\ and then names `evidence` as a payload type further down the same file.
s" rpsdpay" s" 0 FIELD h rpsd ;STRUCTURE" struct-replay-test:RP-TRY 0 T=
s" rpsdpay" FAMID struct-replay-test:FAM-FLD-COUNT 1 T=

\ A malformed replayed STRUCTURE reports through the same renderer as a live one.
DECL-DIAG:PROSE
s" rpsdbad" s" 0 FIELD one nope ;STRUCTURE" struct-replay-test:RP-TRY 7109 T=
s" habu: bad structure declaration 'rpsdbad': unknown field type at 'nope'"
DECL-DIAG:HAS? -1 T=

\ A buffer with no terminator rejects through the front end's own gate.
DECL-DIAG:PROSE
s" rpsdnoend" s" 0 FIELD one n" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration 'rpsdnoend': missing ;STRUCTURE"
DECL-DIAG:HAS? -1 T=

\ A zero-length name reaches the missing-name gate.
DECL-DIAG:PROSE
s" " s" 0 FIELD one n ;STRUCTURE" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration '': missing name" DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ A rejected replay leaves the stream closed, so the next LIVE declaration reads
\ the input source again.
DECL-DIAG:PROSE
s" rpsddangle" s" 0 FIELD" struct-replay-test:RP-TRY 7107 T=
DECL-DIAG:OFF
s" STRUCTURE-DECL:SD-RUN rpsdafter 0 FIELD one n ;STRUCTURE" TRY 0 T=
s" rpsdafter" FAMID struct-replay-test:FAM-FLD-COUNT 1 T=

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-decl-suite: failures" 1 die ;
REPORT
