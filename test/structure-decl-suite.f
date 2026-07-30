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

\ A naked count is retired. The provisional family is fully rolled back, so the
\ same tail can immediately be declared with its binder head.
REG-MARK
ndict@ RC !
s" STRUCTURE pair 2 ;STRUCTURE" TRY 7107 T=
TFAMN@ RB-TFAM @ T=
TF-STR-U @ RB-STR @ T=
TF-PK-N @ RB-PK @ T=
SCHN@ RB-SCH @ T=
SUMVN@ RB-SUMV @ T=
LAY-N @ RB-LAY @ T=
SCH-ROOT-N @ RB-ROOT @ T=
PF-N @ RB-PFN @ T=
PF-COMMIT-N @ RB-PFC @ T=
ndict@ RC @ T=

\ Binder names map to schema ordinals in declaration order, not alphabet order.
TYPE-FIELD:COUNT B !
s" STRUCTURE pair<e,a> FIELD left e FIELD right a ;STRUCTURE" EV
s" pair" FAMID TFAM-ARITY@ 2 T=
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 0 T=
B @ 1 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 1 T=

\ Arity comes only from the head: unused binders remain real, while a bare head
\ and an explicit empty list are both concrete.
s" STRUCTURE phantom<e> ;STRUCTURE" EV
s" phantom" FAMID TFAM-ARITY@ 1 T=
s" STRUCTURE plain ;STRUCTURE" EV
s" plain" FAMID TFAM-ARITY@ 0 T=
s" STRUCTURE explicit<> ;STRUCTURE" EV
s" explicit" FAMID TFAM-ARITY@ 0 T=

\ ---------------------------------------------------------------------------
\ 1. A product declaration persists its family and both field rows, reachable
\    through TYPE-FIELD reflection in declaration order with the right layout.
\ ---------------------------------------------------------------------------
TFAM-N@ FID !
TYPE-FIELD:COUNT B !
s" STRUCTURE point FIELD x n FIELD y n ;STRUCTURE" EV
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
s" STRUCTURE evt FIELD a n FIELD b n ;STRUCTURE" EV
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
s" STRUCTURE ppk POLICY packed-tag FIELD x n ;STRUCTURE" EV
s" ppk" FAMID FAM-POLICY@ PACKED# T=                  \ family layout policy is packed-tag
2 DECL-EVENT:POLICY? T-TRUE                           \ a POLICY event followed DECL + ARITY
2 DECL-EVENT:VAR@ PACKED# T=                          \ its recorded code is packed-tag

\ ---------------------------------------------------------------------------
\ 5. DERIVE reaches both the family record and the event stream; two features
\    on one clause are accepted, each recorded once.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
s" STRUCTURE der DERIVE eq hash FIELD x n ;STRUCTURE" EV
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
s" STRUCTURE dupf FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
TFAMN@ RB-TFAM @ T=                                   \ family retired
SCHN@ RB-SCH @ T=                                     \ schema nodes retired
SUMVN@ RB-SUMV @ T=                                   \ variant cursor unchanged
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
DECL-EVENT:COUNT DEVB @ T=                            \ nothing new published

\ ---------------------------------------------------------------------------
\ 7. Grammar / name / binder / type / policy / derive / terminator rejects, each
\    at the offending token with the E-TDECL-* family code.
\ ---------------------------------------------------------------------------
s" STRUCTURE field ;STRUCTURE" TRY 7110 T=                     \ reserved keyword name
s" STRUCTURE n ;STRUCTURE" TRY 7110 T=                         \ single-letter type name
s" STRUCTURE Bad ;STRUCTURE" TRY 7101 T=                       \ upper-case name (case)
s" STRUCTURE foo<a ;STRUCTURE" TRY 7108 T=                       \ missing close
s" STRUCTURE foo<a,> ;STRUCTURE" TRY 7108 T=                     \ empty entry
s" STRUCTURE foo<a,a> ;STRUCTURE" TRY 7108 T=                    \ duplicate binder
s" STRUCTURE foo<A> ;STRUCTURE" TRY 7108 T=                      \ uppercase binder
s" STRUCTURE foo<é> ;STRUCTURE" TRY 7108 T=                      \ multibyte binder
s" STRUCTURE foo<f> ;STRUCTURE" TRY 7108 T=                      \ concrete bool token
s" STRUCTURE foo<n> ;STRUCTURE" TRY 7108 T=                      \ concrete integer token
s" STRUCTURE foo<r> ;STRUCTURE" TRY 7108 T=                      \ concrete real token
s" STRUCTURE foo> ;STRUCTURE" TRY 7108 T=                        \ stray close
s" STRUCTURE foo FIELD a nope ;STRUCTURE" TRY 7109 T=          \ unresolved field type
s" STRUCTURE foo FIELD a Q ;STRUCTURE" TRY 7109 T=             \ upper-case single-letter type
s" STRUCTURE foo FIELD a a ;STRUCTURE" TRY 7109 T=             \ parameter outside declared arity
s" STRUCTURE foo VARIANT q ;VARIANT ;STRUCTURE" TRY 7107 T=    \ mixed legacy token
s" STRUCTURE foo FIELD x n" TRY 7107 T=                        \ missing ;STRUCTURE
s" STRUCTURE foo POLICY nope FIELD x n ;STRUCTURE" TRY 7116 T= \ unknown layout policy
s" STRUCTURE foo DERIVE nope FIELD x n ;STRUCTURE" TRY 7119 T= \ unknown derive feature

\ ---------------------------------------------------------------------------
\ 8. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
\ ---------------------------------------------------------------------------
s" STRUCTURE twice FIELD x n ;STRUCTURE" EV
s" STRUCTURE twice FIELD x n ;STRUCTURE" TRY 7102 T=

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
s" STRUCTURE ident FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC !                              \ RC holds identity A
REG-RESTORE                                           \ retire family; fresh registry
DECL-EVENT:RESET
s" STRUCTURE ident FIELD x n ;STRUCTURE" EV
DECL-EVENT:IDENTITY RC @ T=                           \ identical declaration -> same identity
REG-RESTORE
DECL-EVENT:RESET
s" STRUCTURE ident FIELD x n FIELD y n ;STRUCTURE" EV
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
s" STRUCTURE tri FIELD a n FIELD b n FIELD c n ;STRUCTURE" EV
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
s" STRUCTURE abc<a> FIELD p a FIELD k n FIELD c char ;STRUCTURE" EV
s" ABCRT ( n n char -- n n char ) ABC:MAKE ABC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic-only structure round-trips its parameter at a concrete instantiation.
s" STRUCTURE gp<a> FIELD v a ;STRUCTURE" EV
: GPRT ( n -- n ) GP:MAKE GP:UNMAKE ;
42 GPRT 42 T=

\ The post-hook STRUCTURE parser consumes the shared declaration alphabet.  A
\ maximum-arity declaration accepts g and z while f/n/r stay scalar fields; the
\ exact inverse table is tested once in type-family-suite.f.
s" STRUCTURE sdmap<a,b,c,d,e,g,h,i,j,k,l,m,o,p,q,s,t,u,v,w,x,y,z> FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e FIELD p05 g FIELD flag f FIELD integer n FIELD real r FIELD last z ;STRUCTURE" EV
s" SDMAPRT ( n n n n n n bool n r char -- n n n n n n bool n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDMAPBAD ( n n n n n bool n n r char -- n n n n n bool n n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ ---------------------------------------------------------------------------
\ 12. Constructor generation is gated (SD-MAKEABLE?): only a PUBLIC structure WITH
\     fields owns a ctor package. A rejected declaration and an opaque zero-field
\     declaration each generate NO ctor words (the variant cursor is unchanged).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
s" STRUCTURE badf FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=   \ duplicate field rejects
SUMVN@ SV0 @ T=                                       \ no ctor words from a rejected declaration
SUMVN@ SV0 !
s" STRUCTURE opaque ;STRUCTURE" EV                  \ zero-field opaque one-cell family (docs/type-families.md §2.2)
SUMVN@ SV0 @ T=                                       \ an opaque family owns no MAKE/UNMAKE

\ ---------------------------------------------------------------------------
\ 13. A PRIVATE structure with fields owns no construction surface either:
\     SD-MAKEABLE? requires a public family, matching the shipped product
\     precedent (private products publish no MAKE/UNMAKE, fail-closed).
\ ---------------------------------------------------------------------------
SUMVN@ SV0 !
package SDPRIVTEST
private
STRUCTURE hidden FIELD x n ;STRUCTURE
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
s" STRUCTURE-DECL:SD-RUN Sdgcase ;STRUCTURE" TRY 7101 T=
s" habu: bad structure declaration 'Sdgcase': name must be a lowercase family tail at 'Sdgcase'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN n ;STRUCTURE" TRY 7110 T=
s" habu: bad structure declaration 'n': reserved name at 'n'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdup FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgdup': duplicate field name at 'z'" DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgar<a,> ;STRUCTURE" TRY 7108 T=
s" habu: bad structure declaration 'sdgar<a,>': binder list must contain unique declaration parameters at 'sdgar<a,>'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgbad FIELD a nosuchtype ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdgbad': unknown field type at 'nosuchtype'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgpol POLICY nosuch ;STRUCTURE" TRY 7116 T=
s" habu: bad structure declaration 'sdgpol': unknown layout policy at 'nosuch'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdrv DERIVE nosuch ;STRUCTURE" TRY 7119 T=
s" habu: bad structure declaration 'sdgdrv': unknown derive feature at 'nosuch'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgstray VARIANT ;STRUCTURE" TRY 7107 T=
s" habu: bad structure declaration 'sdgstray': unexpected token in structure declaration at 'VARIANT'"
DECL-DIAG:HAS? -1 T=

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgfn FIELD make n ;STRUCTURE" TRY 7125 T=
s" habu: bad structure declaration 'sdgfn': reserved field name at 'make'" DECL-DIAG:HAS? -1 T=

\ a terminator that never arrives anchors on the family, exactly as the legacy
\ unterminated-declaration packet does (sumtype.f TDECL-PRODUCT-NOEND-BODY).
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgnoend FIELD a n" TRY 7107 T=
s" habu: bad structure declaration 'sdgnoend': missing ;STRUCTURE at 'sdgnoend'"
DECL-DIAG:HAS? -1 T=

\ the self-referential FIELD. The field type resolves to the family being
\ declared, whose width is not bound until close, so the field record refuses the
\ layout (E-PF-LAYOUT). That reject used to be a bare 7127 with no message; it now
\ names the declaration, the offending field, and the reason. The legacy PRODUCT
\ definer refuses the same shape earlier, at its own recursion gate (7117), so the
\ codes differ by construction — the front end has no recursion pre-check yet.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgself FIELD selffld sdgself ;STRUCTURE" TRY 7127 T=
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
s" STRUCTURE-DECL:SD-RUN sdgforge FIELD duplicate n FIELD duplicate n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgforge': duplicate field name at 'duplicate'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:LEN 80 T=

\ a family name spelled like one of the JSON packet's own keys keeps key and
\ value distinguishable, and a token carrying a double quote is escaped rather
\ than closing the JSON string early.
DECL-DIAG:JSON
s" STRUCTURE-DECL:SD-RUN token FIELD z n FIELD z n ;STRUCTURE" TRY 7102 T=
s\" \"decl\":\"structure\"" DECL-DIAG:HAS? -1 T=
s\" \"family\":\"token\"" DECL-DIAG:HAS? -1 T=
s\" \"token\":\"z\"" DECL-DIAG:HAS? -1 T=
s\" \"reason\":\"duplicate field name\"" DECL-DIAG:HAS? -1 T=

DECL-DIAG:JSON
s\" STRUCTURE-DECL:SD-RUN sdgquote FIELD aq\"b n ;STRUCTURE" TRY 7101 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? -1 T=
s\" \"family\":\"sdgquote\"" DECL-DIAG:HAS? -1 T=

\ accepted declarations stay silent, and a rendered reject still leaves the
\ registry byte-identical: rendering happens after the coordinator has rolled
\ back and touches no registry cursor.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgok1 FIELD a n ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok2 ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok3<a> FIELD a a ;STRUCTURE" TRY 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok4 DERIVE eq FIELD a n ;STRUCTURE" TRY 0 T=
DECL-DIAG:SILENT? -1 T=

\ the armed duplicate-family reason, which no case above reaches: it is armed at
\ SD-REGISTER, immediately before the registry call that can raise it.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice FIELD a n ;STRUCTURE" TRY 0 T=
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice FIELD b n ;STRUCTURE" TRY 7102 T=
s" habu: bad structure declaration 'sdgtwice': duplicate family at 'sdgtwice'"
DECL-DIAG:HAS? -1 T=

\ SD-CLOSE arms no reason of its own: the generator's rejects are raised past the
\ last token this front end holds, so they are answered from the packet's code
\ table. The over-long span cap and its "..." marker belong to the shared packet
\ and are pinned once, in test/enum-decl-suite.f section 22h.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgvrsv FIELD z sdgvrsv ;STRUCTURE" TRY 7127 T=
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
s" rpsd" s" FIELD one n FIELD two n ;STRUCTURE" struct-replay-test:RP-TRY 0 T=
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

\ Replay consumes the same binder head and ordinal map as the live driver.
TYPE-FIELD:COUNT B !
s" rppair<e,a>" s" FIELD left e FIELD right a ;STRUCTURE"
struct-replay-test:RP-TRY 0 T=
s" rppair" FAMID TFAM-ARITY@ 2 T=
B @ TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 0 T=
B @ 1 + TYPE-FIELD:SCHEMA@ SCH-ROOT@ NODE !
NODE @ SCHEMA-PARAM? T-TRUE
NODE @ SCH-A@ 1 T=

\ Header clauses replay onto the same family record.
s" rpsdh" s" POLICY packed-tag DERIVE eq FIELD one n ;STRUCTURE"
struct-replay-test:RP-TRY 0 T=
s" rpsdh" FAMID FID !
FID @ FAM-POLICY@ PACKED# T=
FID @ FAM-EQ? -1 T=

\ A field typed by a family REGISTERED THROUGH AN EARLIER REPLAY resolves — this
\ is the exact shape that was broken: obligation.f declares `STRUCTURE evidence`
\ and then names `evidence` as a payload type further down the same file.
s" rpsdpay" s" FIELD h rpsd ;STRUCTURE" struct-replay-test:RP-TRY 0 T=
s" rpsdpay" FAMID struct-replay-test:FAM-FLD-COUNT 1 T=

\ A malformed replayed STRUCTURE reports through the same renderer as a live one.
DECL-DIAG:PROSE
s" rpsdbad" s" FIELD one nope ;STRUCTURE" struct-replay-test:RP-TRY 7109 T=
s" habu: bad structure declaration 'rpsdbad': unknown field type at 'nope'"
DECL-DIAG:HAS? -1 T=

\ A buffer with no terminator rejects through the front end's own gate.
DECL-DIAG:PROSE
s" rpsdnoend" s" FIELD one n" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration 'rpsdnoend': missing ;STRUCTURE"
DECL-DIAG:HAS? -1 T=

\ A zero-length name reaches the missing-name gate.
DECL-DIAG:PROSE
s" " s" FIELD one n ;STRUCTURE" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration '': missing name" DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ A rejected replay leaves the stream closed, so the next LIVE declaration reads
\ the input source again.
DECL-DIAG:PROSE
s" rpsddangle" s" FIELD" struct-replay-test:RP-TRY 7107 T=
DECL-DIAG:OFF
s" STRUCTURE-DECL:SD-RUN rpsdafter FIELD one n ;STRUCTURE" TRY 0 T=
s" rpsdafter" FAMID struct-replay-test:FAM-FLD-COUNT 1 T=

\ ---------------------------------------------------------------------------
\ Control words are reserved in both name positions this front end gates.
\
\ A family or field named `if` would be compiled as the control word `if`
\ wherever generated code names it. The legacy definers have always refused such
\ names (sumtype.f TDECL-RESERVED?); this front end consulted only the
\ grammar-keyword half of that list, so `STRUCTURE if FIELD x n ;STRUCTURE`
\ was accepted while the legacy spelling of the same name was refused 7110. The
\ list now lives once, in TYPE-NAME:CONTROL? (src/core/type-family.f), read here
\ through CONTROL-KW? and by field rows through PF-RESERVED?.
\
\ The family position answers 7110 (this front end's own name gate) and the
\ field position answers 7125 (the field record's gate, the same code and wording
\ `make` already produced). Three of the words never reach either gate: `?do`,
\ `+loop` and `;match` do not start with a lowercase letter, so the canonical
\ tail gate refuses them first with 7101.
\ ---------------------------------------------------------------------------
DECL-DIAG:PROSE
s" STRUCTURE if FIELD x n ;STRUCTURE"      TRY 7110 T=
s" habu: bad structure declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? -1 T=
DECL-DIAG:PROSE
s" STRUCTURE do FIELD x n ;STRUCTURE"      TRY 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE match FIELD x n ;STRUCTURE"   TRY 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE endcase FIELD x n ;STRUCTURE" TRY 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE ?do FIELD x n ;STRUCTURE"     TRY 7101 T=
DECL-DIAG:PROSE
s" STRUCTURE ;match FIELD x n ;STRUCTURE"  TRY 7101 T=

DECL-DIAG:PROSE
s" STRUCTURE sdcwf FIELD if n ;STRUCTURE" TRY 7125 T=
s" habu: bad structure declaration 'sdcwf': reserved field name at 'if'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:PROSE
s" STRUCTURE sdcwf2 FIELD x n FIELD loop n ;STRUCTURE" TRY 7125 T=
s" habu: bad structure declaration 'sdcwf2': reserved field name at 'loop'"
DECL-DIAG:HAS? -1 T=

\ The match is on the WHOLE token: a name that merely contains a control word is
\ an ordinary name and still declares, in both positions.
DECL-DIAG:PROSE
s" STRUCTURE iffy FIELD looping n FIELD thence n ;STRUCTURE" TRY 0 T=
s" iffy" FAMID struct-replay-test:FAM-FLD-COUNT 2 T=
DECL-DIAG:SILENT? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ A field may name a family that owns a linear value, and the structure then owns
\ that obligation by containment (dot habu-checker-enum-payload-9e1ae6cc).
\
\ The resolver used to refuse any family whose schemas reach a linear value. That
\ made `FIELD res WSTORE:resident` legal but `FIELD m gpt2-model` — the very same
\ resource one structure deeper — reject 7109 as an "unknown field type", about a
\ family that had just registered successfully. The two spellings carry the same
\ obligation, so refusing one of them bought no soundness; it only blocked the
\ name. What actually enforces the discipline is TFAM-CONCRETE-LINEAR?, which
\ walks a product's field schemas, follows an application node into the family it
\ names, and reports the containing family linear. That walk already recursed, so
\ it needed no change: only the refusal had to go.
\
\ These cases pin the registry side — which families read back linear, and that
\ the walk keeps recursing at every extra level of nesting. test/type-linear-suite.f
\ pins what the checker then does with such a value on a row.
\
\ The linear owner and the registry reader are owned by package SDLIN. Production
\ writes the owner that way too (maki/infer/weight-store.f owns
\ `WSTORE:resident`), so naming it as a field type also exercises the qualified
\ spelling the resolver meets in real source.
\ ---------------------------------------------------------------------------
package SDLIN
public
DEFLINEAR SDLIN:tok                                   \ the linear owner these fixtures nest
TRUSTED: LINEAR? ( n -- bool ) TFAM-CONCRETE-LINEAR? ;   \ owns one, directly or through a field
;package

\ depth 1, legal before this change: a field naming the linear con itself.
s" STRUCTURE sdlbox FIELD t SDLIN:tok FIELD k n ;STRUCTURE" TRY 0 T=
s" sdlbox" FAMID SDLIN:LINEAR? T-TRUE
s" sdlbox" FAMID FAM-SLOTS@ 2 T=

\ depth 2, the shape this dot unblocks: a field naming that linear family.
TYPE-FIELD:COUNT B !
s" STRUCTURE sdlouter FIELD inner sdlbox FIELD z n ;STRUCTURE" TRY 0 T=
s" sdlouter" FAMID SDLIN:LINEAR? T-TRUE                 \ linear by containment
s" sdlouter" FAMID FAM-SLOTS@ 3 T=                    \ the nested bundle keeps its own two cells
B @ TYPE-FIELD:NAME$ s" inner" CORE-STR= T-TRUE
B @ TYPE-FIELD:CELLS@ 2 T=                            \ the field is the whole bundle, not one cell
B @ 1 + TYPE-FIELD:SLOT@ 2 T=                         \ z sits after it

\ depth 3: the walk recurses again rather than stopping one level down.
s" STRUCTURE sdldeep FIELD d sdlouter ;STRUCTURE" TRY 0 T=
s" sdldeep" FAMID SDLIN:LINEAR? T-TRUE

\ a sum reached through a product counts the same way.
s" ENUM sdlsum 0 VARIANT hold FIELD m sdlbox ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TRY 0 T=
s" sdlsum" FAMID SDLIN:LINEAR? T-TRUE
s" STRUCTURE sdlviasum FIELD e sdlsum ;STRUCTURE" TRY 0 T=
s" sdlviasum" FAMID SDLIN:LINEAR? T-TRUE

\ the control: a chain with no linear value anywhere stays non-linear, so the
\ walk is answering about the chain and not about nesting as such.
s" STRUCTURE sdlplain FIELD v n ;STRUCTURE" TRY 0 T=
s" STRUCTURE sdlplainer FIELD inner sdlplain ;STRUCTURE" TRY 0 T=
s" sdlplainer" FAMID SDLIN:LINEAR? 0= T-TRUE

\ wrong role: the same word in the FIELD NAME position is a name, never a type,
\ so it neither resolves nor makes the structure linear.
s" STRUCTURE sdlrole FIELD sdlbox n ;STRUCTURE" TRY 0 T=
s" sdlrole" FAMID SDLIN:LINEAR? 0= T-TRUE

\ reordering: naming a family before it is declared is still an unknown type,
\ so acceptance comes from resolution and not from the spelling alone.
DECL-DIAG:PROSE
s" STRUCTURE sdlfwd FIELD m sdllater ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlfwd': unknown field type at 'sdllater'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ A name that DOES resolve says why it cannot be a field type. A parametric
\ family named bare is the one such case source can reach, and reporting it as
\ "unknown field type" sent readers looking for a declaration that was right
\ there. A name that resolves to nothing still reports unknown, which is true.
\ ---------------------------------------------------------------------------
s" STRUCTURE sdlgen<a> FIELD a a ;STRUCTURE" TRY 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdlgenuse FIELD m sdlgen ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlgenuse': field type is parametric and needs type arguments at 'sdlgen'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
\ A field may NOT name a POINTER to a linear value.
\
\ Accepting the family spelling above also let `FIELD p ptr sdlbox` through, and
\ that spelling is the opposite of the one above: a pointer is a non-owning
\ boundary, so the linearity walk stops at the pointer node and the containing
\ structure reads NON-linear. It would then copy and drop freely while a linear
\ resource sat behind the address — the resource laundered, which is exactly what
\ the containment rule exists to prevent. Nothing in the tree writes that
\ spelling, and it was legal only because no producer route happened to reach it.
\ It is refused at the declaration door instead.
\
\ One rule decides it, TFCL-NODE?, the same node walk the linearity accounting
\ already trusts. So the con spelling and the family spelling are refused alike —
\ they launder identically — and the refusal holds at every depth: `ptr ptr sdlbox`
\ rejects on the inner recursion, which anchors the diagnostic on the family that
\ actually owns the resource rather than on the outer pointer.
\
\ A pointer to something that owns nothing stays legal. `ptr n` and `ptr sdlplain`
\ are how ordinary records point at ordinary data, and refusing those would break
\ every existing record for no gain.
\ ---------------------------------------------------------------------------

\ the two spellings side by side. Naming the value owns it; pointing at it cannot.
s" STRUCTURE sdlowns FIELD m sdlbox ;STRUCTURE" TRY 0 T=
s" sdlowns" FAMID SDLIN:LINEAR? T-TRUE

DECL-DIAG:PROSE
s" STRUCTURE sdlptr FIELD p ptr sdlbox ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptr': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? -1 T=

\ the con spelling launders the same way, so the same rule refuses it, and the
\ diagnostic names the con it found rather than some enclosing family.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrcon FIELD p ptr SDLIN:tok ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptrcon': field type is a pointer to a linear value and cannot own it at 'SDLIN:tok'"
DECL-DIAG:HAS? -1 T=

\ depth: a second pointer does not launder past the rule. The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" STRUCTURE sdlptr2 FIELD p ptr ptr sdlbox ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptr2': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? -1 T=

\ reaching the resource through a nested family or through a sum is still reaching
\ it, so a pointer to either is refused for the same reason.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrdeep FIELD p ptr sdldeep ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptrdeep': field type is a pointer to a linear value and cannot own it at 'sdldeep'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:PROSE
s" STRUCTURE sdlptrsum FIELD p ptr sdlsum ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptrsum': field type is a pointer to a linear value and cannot own it at 'sdlsum'"
DECL-DIAG:HAS? -1 T=

\ a field anywhere in the body is checked, not just the first one, and the token
\ names the offending field's type rather than the declaration's first field.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrlate FIELD a n FIELD p ptr sdlbox FIELD c n ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptrlate': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? -1 T=

\ the controls. A pointer to a family that owns nothing declares, keeps its one
\ cell, and leaves the structure non-linear — so the rejects above answer the
\ POINTEE's linearity and not the word `ptr`.
s" STRUCTURE sdlptrok FIELD p ptr sdlplain FIELD k n ;STRUCTURE" TRY 0 T=
s" sdlptrok" FAMID SDLIN:LINEAR? 0= T-TRUE
s" sdlptrok" FAMID FAM-SLOTS@ 2 T=                    \ the pointer is one cell, k is the other
s" STRUCTURE sdlptrok2 FIELD p ptr ptr sdlplain ;STRUCTURE" TRY 0 T=
s" sdlptrok2" FAMID SDLIN:LINEAR? 0= T-TRUE
s" STRUCTURE sdlptrn FIELD p ptr n ;STRUCTURE" TRY 0 T=
s" sdlptrn" FAMID SDLIN:LINEAR? 0= T-TRUE

\ wrong role: the same words in the FIELD NAME position are names, never types.
\ Neither resolves, neither is refused, and neither makes the structure linear.
s" STRUCTURE sdlptrrole FIELD ptr n ;STRUCTURE" TRY 0 T=
s" sdlptrrole" FAMID SDLIN:LINEAR? 0= T-TRUE
s" STRUCTURE sdlptrrole2 FIELD sdlbox n ;STRUCTURE" TRY 0 T=
s" sdlptrrole2" FAMID SDLIN:LINEAR? 0= T-TRUE

\ hostile comments. A declaration body has NO comment syntax: its reader takes
\ plain tokens, so `(` and `\` are ordinary tokens in a type or clause position
\ and the whole declaration is malformed. Text that merely reads like a field can
\ therefore neither smuggle a type in nor be quietly skipped — and it rejects
\ 7107, a DIFFERENT code from this rule's 7109, so no verdict here is ever
\ produced by scanning prose.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrpar FIELD v n ( FIELD p ptr sdlbox ) ;STRUCTURE" TRY 7107 T=
s" habu: bad structure declaration 'sdlptrpar': unexpected token in structure declaration at '('"
DECL-DIAG:HAS? -1 T=                                  \ the paren itself was the token
DECL-DIAG:PROSE
s" STRUCTURE sdlptrbsl FIELD v n \ FIELD p ptr sdlbox" TRY 7107 T=
s" habu: bad structure declaration 'sdlptrbsl': unexpected token in structure declaration at '\'"
DECL-DIAG:HAS? -1 T=                                  \ and so was the backslash
\ and trailing text after the offending field cannot suppress the reject, because
\ the field is resolved before anything following it is read.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrtail FIELD p ptr sdlbox ( note ) ;STRUCTURE" TRY 7109 T=
s" habu: bad structure declaration 'sdlptrtail': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? -1 T=
DECL-DIAG:OFF

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" structure-decl-suite: failures" 1 die ;
REPORT
