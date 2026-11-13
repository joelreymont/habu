\ structure-decl-suite.f - behavior and rollback suite for the STRUCTURE
\ declaration front end (src/core/structure-decl.f, package STRUCTURE-DECL).
\ It asserts reject codes, rendered diagnostics, field-row counts and the
\ generated words. A failure prints F<index> and detail; T-REPORT exits 1.

require src/habu/verify-source.f
require test/replay-scope.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture
require src/lib/test.f

\ Capture every reject's declaration diagnostic from the start; the last
\ section asserts the rendered lines.
DECL-DIAG:PROSE

T-RESET

variable B   variable PFB

\ 6. A duplicate field name rejects through the field record (E-TFAM-DUP 7102)
\    and the provisional field rows roll back. hb's own type entries are read
\    first: a declaration reads every pending one before it opens.
TFAM:READ-PENDING
TYPE-FIELD:COUNT PFB !
s" STRUCTURE dupf FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired

\ 7. Grammar / name / arity / type / policy / derive / terminator rejects, each
\    at the offending token with the E-TDECL-* family code.
s" STRUCTURE field ;STRUCTURE" TEST-EVAL:RC 7110 T=            \ reserved keyword name
s" STRUCTURE n ;STRUCTURE" TEST-EVAL:RC 7110 T=                \ single-letter type name
s" STRUCTURE ptr FIELD x n ;STRUCTURE" TEST-EVAL:RC 7110 T=    \ the pointer constructor's tail
s" STRUCTURE Bad ;STRUCTURE" TEST-EVAL:RC 7101 T=              \ upper-case name (case)
s" STRUCTURE foo<q,q> ;STRUCTURE" TEST-EVAL:RC 7108 T=              \ duplicate parameter
s" STRUCTURE foo<a,> ;STRUCTURE" TEST-EVAL:RC 7108 T=             \ trailing comma in parameter list
s" STRUCTURE foo FIELD a nope ;STRUCTURE" TEST-EVAL:RC 7109 T=  \ unresolved field type
s" STRUCTURE foo FIELD a Q ;STRUCTURE" TEST-EVAL:RC 7109 T=    \ upper-case single-letter type
s" STRUCTURE foo FIELD a a ;STRUCTURE" TEST-EVAL:RC 7109 T=    \ parameter outside declared arity
NEWTYPE qreq
NEWTYPE qres
s" STRUCTURE qrollback FIELD handler [ qreq qres ] ;STRUCTURE" TEST-EVAL:RC 7107 T=
s" STRUCTURE qrollback FIELD handler [ qreq qres -- ] ;STRUCTURE" TEST-EVAL:RC 0 T=
s" QROLLBACK-RT ( [ qreq qres -- ] -- qrollback ) QROLLBACK:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" STRUCTURE foo VARIANT q ;VARIANT ;STRUCTURE" TEST-EVAL:RC 7107 T=  \ mixed legacy token
s" STRUCTURE foo FIELD x n" TEST-EVAL:RC 7107 T=               \ missing ;STRUCTURE
s" STRUCTURE foo POLICY nope FIELD x n ;STRUCTURE" TEST-EVAL:RC 7116 T= \ unknown layout policy
s" STRUCTURE foo DERIVE nope FIELD x n ;STRUCTURE" TEST-EVAL:RC 7119 T= \ unknown derive feature

\ An effect reads a bare `ptr` as the pointer constructor in every scope, so the
\ tail is reserved ahead of the global `ptr` family's duplicate check (7102),
\ and a package may not shadow it as it may shadow any other global family.
package SDPTR
s" STRUCTURE ptr FIELD x n ;STRUCTURE" TEST-EVAL:RC 7110 T=
;package

\ 8. A duplicate family name rejects (E-TFAM-DUP 7102 from TFAM-DECL).
s" STRUCTURE twice FIELD x n ;STRUCTURE" evaluate-closed
s" STRUCTURE twice FIELD x n ;STRUCTURE" TEST-EVAL:RC 7102 T=

\ 10. A public STRUCTURE with fields generates a sealed MAKE/UNMAKE ctor package;
\     MAKE then UNMAKE round-trips the fields bit-identically in declaration order.
s" STRUCTURE tri FIELD a n FIELD b n FIELD c n ;STRUCTURE" evaluate-closed
: TRIRT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE ;      \ callable sealed ctor words
11 22 33 TRIRT 33 T= 22 T= 11 T=                      \ declaration order + values round-trip bit-identically

\ 11. MAKE/UNMAKE are exact type inverses across a generic, cell and byte field
\     mix at a concrete instantiation (a = n).
s" STRUCTURE abc<a> FIELD p a FIELD k n FIELD c char ;STRUCTURE" evaluate-closed
s" ABCRT ( n n char -- n n char ) ABC:MAKE ABC:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic-only structure round-trips its parameter at a concrete instantiation.
s" STRUCTURE gp<a> FIELD v a ;STRUCTURE" evaluate-closed
: GPRT ( n -- n ) GP:MAKE GP:UNMAKE ;
42 GPRT 42 T=

\ The STRUCTURE parser consumes the shared declaration alphabet. A maximum-arity
\ declaration accepts g and z while f/n/r stay scalar fields.
s" STRUCTURE sdmap<a,b,c,d,e,g,h,i,j,k,l,m,o,p,q,s,t,u,v,w,x,y,z> FIELD p00 a FIELD p01 b FIELD p02 c FIELD p03 d FIELD p04 e FIELD p05 g FIELD flag f FIELD integer n FIELD real r FIELD last z ;STRUCTURE" evaluate-closed
s" SDMAPRT ( n n n n n n bool n r char -- n n n n n n bool n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDMAPBAD ( n n n n n bool n n r char -- n n n n n bool n n r char ) SDMAP:MAKE SDMAP:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ 12. Only a structure with fields gets a ctor package (SD-MAKEABLE?): a zero-field
\     family declares, and its MAKE is an unknown word (0).
s" STRUCTURE badf FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=  \ duplicate field rejects
s" STRUCTURE opaque ;STRUCTURE" evaluate-closed                  \ zero-field opaque one-cell family (docs/forth.md "Structures And Enums")
s" SDOPQMK ( -- opaque ) OPAQUE:MAKE" CHECK-QUIET-CANDIDATE! 0 T=

\ 13. A PRIVATE structure with fields generates FAMILY-MAKE/FAMILY-UNMAKE
\     (src/core/type-family.f TF-CTOR-PRIV$) into the declaring package's
\     private wordlist; a public family gets FAMILY:MAKE in its global ctor
\     namespace. A colon name cannot land in a private wordlist: the engine
\     routes a qualified definition to the namespace's public wid, and the
\     checker records a single-colon name as the package's public symbol.
package SDPRIVTEST
private
STRUCTURE hidden FIELD x n FIELD y n ;STRUCTURE
: HRT ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE ;      \ callable from the owning package
: HAFTER ( -- n ) 7 ;                                 \ the package still defines privately
public
: HGO ( n n -- n n ) HRT ;
: HTAIL ( -- n ) HAFTER ;
;package
11 22 SDPRIVTEST:HGO 22 T= 11 T=                      \ declaration order + values round-trip
SDPRIVTEST:HTAIL 7 T=                                 \ generation did not seal the private wordlist

\ A second package and the global scope cannot resolve them: the probe is refused
\ as an undefined word (0). Contrasts: the same probe certifies inside
\ the declaring package, and case 10's public family certifies from every scope.
package SDPRIVTEST
private
s" SDPI ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
public
;package
package SDPRIVOTHER
private
s" SDPO ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDPT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
public
;package
s" SDPG ( n n -- n n ) HIDDEN-MAKE HIDDEN-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDPGT ( n n n -- n n n ) TRI:MAKE TRI:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=

\ While its package is open, a private product's MAKE and UNMAKE refuse undefine
\ by name, E-CTOR-PROTECTED 7111; a word only sharing their prefix does not.
package SDPRIVTEST
private
s" undefine HIDDEN-MAKE" TEST-EVAL:RC 7111 T=
s" undefine HIDDEN-UNMAKE" TEST-EVAL:RC 7111 T=
: HIDDEN-SIZE ( -- n ) 3 ;                            \ the family tail, a member nobody generates
HIDDEN-SIZE 3 T=
s" undefine HIDDEN-SIZE" TEST-EVAL:RC 0 T=            \ so it is the package's own word
public
;package

\ Protection keys on what a family generates, never on its variant names. A
\ private SUM or ENUM publishes no dictionary word (src/core/sumtype.f
\ TDECL-GENERATES?: a public family or a private PRODUCT), so a FAMILY-VARIANT
\ word inside the declaring package belongs to its writer and undefines. The
\ second family's variants are spelled `make` and `unmake`, catching a guard
\ that checks the product member set without asking whether the family
\ generates.
package SDPRIVSUM
private
SUMTYPE colour VARIANT red ;VARIANT VARIANT green ;VARIANT ;SUMTYPE
: COLOUR-RED ( -- n ) 5 ;
COLOUR-RED 5 T=
s" undefine COLOUR-RED" TEST-EVAL:RC 0 T=
SUMTYPE deal VARIANT make n ;VARIANT VARIANT unmake n ;VARIANT ;SUMTYPE
: DEAL-MAKE ( -- n ) 6 ;
DEAL-MAKE 6 T=
s" undefine DEAL-MAKE" TEST-EVAL:RC 0 T=
STRUCTURE veil ;STRUCTURE                           \ an opaque product publishes no member pair
: VEIL-MAKE ( -- n ) 4 ;
VEIL-MAKE 4 T=
s" undefine VEIL-MAKE" TEST-EVAL:RC 0 T=
public
;package

\ 14. A rejected PRIVATE declaration rolls back its field rows and its words: the
\     same name declares again in the package.
package SDPRIVROLL
private
TYPE-FIELD:COUNT PFB !
s" STRUCTURE roll FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=
TYPE-FIELD:COUNT PFB @ T=                             \ committed field rows retired
STRUCTURE roll FIELD z n ;STRUCTURE                 \ the name is free again
: RRT ( n -- n ) ROLL-MAKE ROLL-UNMAKE ;
public
: RGO ( n -- n ) RRT ;
;package
9 SDPRIVROLL:RGO 9 T=                                 \ the re-declaration generated working words

\ Reject diagnostics. STRUCTURE renders through render.f's TDECL-DIAG, the
\ legacy definers' producer; the assertions read the bytes captured through
\ DIAG-BUFFER! / DIAG-BUFFER$, the pair tools/check-all-errors-core.f
\ CA-DIAG-FULL-START and CA-DIAG-FINISH use. This proves the channel, not a check-tool run. Each case
\ asserts the whole rendered line.

\ one case per reject code this suite pins, each rendering family + reason +
\ token and rethrowing the exact code.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN Sdgcase ;STRUCTURE" TEST-EVAL:RC 7101 T=
s" habu: bad structure declaration 'Sdgcase': name must be a lowercase family tail at 'Sdgcase'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN n ;STRUCTURE" TEST-EVAL:RC 7110 T=
s" habu: bad structure declaration 'n': reserved name at 'n'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdup FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=
s" habu: bad structure declaration 'sdgdup': duplicate field name at 'z'" DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgar<q,q> ;STRUCTURE" TEST-EVAL:RC 7108 T=
s" habu: bad structure declaration 'sdgar<q,q>': invalid type parameter header at 'sdgar<q,q>'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgbad FIELD a nosuchtype ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdgbad': unknown field type at 'nosuchtype'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgpol POLICY nosuch ;STRUCTURE" TEST-EVAL:RC 7116 T=
s" habu: bad structure declaration 'sdgpol': unknown layout policy at 'nosuch'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgdrv DERIVE nosuch ;STRUCTURE" TEST-EVAL:RC 7119 T=
s" habu: bad structure declaration 'sdgdrv': unknown derive feature at 'nosuch'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgstray VARIANT ;STRUCTURE" TEST-EVAL:RC 7107 T=
s" habu: bad structure declaration 'sdgstray': unexpected token in structure declaration at 'VARIANT'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgfn FIELD make n ;STRUCTURE" TEST-EVAL:RC 7125 T=
s" habu: bad structure declaration 'sdgfn': reserved field name at 'make'" DECL-DIAG:HAS? TTRUE

\ a terminator that never arrives anchors on the family, exactly as the legacy
\ unterminated-declaration packet does (sumtype.f TDECL-PRODUCT-NOEND-BODY).
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgnoend FIELD a n" TEST-EVAL:RC 7107 T=
s" habu: bad structure declaration 'sdgnoend': missing ;STRUCTURE at 'sdgnoend'"
DECL-DIAG:HAS? TTRUE

\ A self-referential field is refused during type resolution, before a width
\ query can enter the unfinished family.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgself FIELD selffld sdgself ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdgself': field type cannot recursively name its owner at 'sdgself'"
DECL-DIAG:HAS? TTRUE

\ no family leaks from one declaration into the next: a declaration that fails
\ before its own name is read reports an empty family, not sdgself.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN" TEST-EVAL:RC 7107 T=
s" habu: bad structure declaration '': missing name" DECL-DIAG:HAS? TTRUE
s" sdgself" DECL-DIAG:HAS? TFALSE

\ A field may point at its own family: three linked records walk round, MAKE and
\ UNMAKE certify, and a pointer to a pointer or to a span of itself declares.
STRUCTURE sdsnode DERIVE addr FIELD id n FIELD next ptr sdsnode ;STRUCTURE
3 TYPED-BUFFER SDS-NODES sdsnode
: SDS-LINK ( -- )
   11 0 SDS-NODES SDSNODE:ID !  22 1 SDS-NODES SDSNODE:ID !  33 2 SDS-NODES SDSNODE:ID !
   1 SDS-NODES 0 SDS-NODES SDSNODE:NEXT !
   2 SDS-NODES 1 SDS-NODES SDSNODE:NEXT !
   0 SDS-NODES 2 SDS-NODES SDSNODE:NEXT ! ;
: SDS-AT ( n -- n )                     \ the id k links on from the first record
   {: k:n :}
   0 SDS-NODES k 0 ?do SDSNODE:NEXT @ loop SDSNODE:ID @ ;
SDS-LINK
0 SDS-AT 11 T=  1 SDS-AT 22 T=  2 SDS-AT 33 T=  3 SDS-AT 11 T=
s" SDSMK ( n ptr sdsnode -- sdsnode ) SDSNODE:MAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSUN ( sdsnode -- n ptr sdsnode ) SDSNODE:UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" STRUCTURE sdsback FIELD id n FIELD back ptr ptr sdsback ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdspspan FIELD id n FIELD s ptr SPAN:span<sdspspan> ;STRUCTURE" TEST-EVAL:RC 0 T=

\ A parametric family pointing at itself: pairing two of its applications ends,
\ a generic and a concrete accessor certify, and a mismatched one is refused.
STRUCTURE sdsplist<a> DERIVE addr FIELD v a FIELD next ptr sdsplist<a> ;STRUCTURE
s" SDSPG ( ptr sdsplist<a> -- ptr sdsplist<a> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSPC ( ptr sdsplist<n> -- ptr sdsplist<n> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSPX ( ptr sdsplist<n> -- ptr sdsplist<f> ) SDSPLIST:NEXT @" CHECK-QUIET-CANDIDATE! 0 T=
STRUCTURE sdsplistn<a> DERIVE addr FIELD v a FIELD next ptr sdsplistn<n> ;STRUCTURE
s" SDSNG ( ptr sdsplistn<a> -- ptr sdsplistn<n> ) SDSPLISTN:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=
s" SDSNC ( ptr sdsplistn<n> -- ptr sdsplistn<n> ) SDSPLISTN:NEXT @" CHECK-QUIET-CANDIDATE! -1 T=

\ The declaring package's own `*` does not reach its structure's AT: another
\ package reads element 2 of three through it.
package SDSTARP
: * ( n n -- n ) + ;
public
STRUCTURE trio DERIVE addr FIELD a n FIELD b n FIELD c n ;STRUCTURE
;package
package SDSTARQ
3 TYPED-BUFFER SDSTAR-BUF SDSTARP:trio
: SDSTAR-PUT ( n n -- )
   {: v:n i:n :}
   v v 1 + v 2 + SDSTARP-TRIO:MAKE i SDSTAR-BUF ! ;
public
: SDSTAR-THIRD ( -- n )
   100 0 SDSTAR-PUT  200 1 SDSTAR-PUT  300 2 SDSTAR-PUT
   0 SDSTAR-BUF 2 SDSTARP-TRIO:AT SDSTARP-TRIO:A @ ;
;package
SDSTARQ:SDSTAR-THIRD 300 T=

\ Its own family as another family's argument is refused, a span's too, though a
\ span's base only points at its element.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdsvspan FIELD s SPAN:span<sdsvspan> ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdsvspan': field type cannot recursively name its owner at 'SPAN:span<sdsvspan>'"
DECL-DIAG:HAS? TTRUE

\ A family argument may be a pointer to the declared family; a span of pointers stored in
\ the field comes back.
STRUCTURE sdpnode DERIVE addr
   FIELD kind n
   FIELD kept SPAN:span<ptr sdpnode>
;STRUCTURE
2 TYPED-BUFFER SDP-NODES sdpnode
2 TYPED-BUFFER SDP-REFS ptr sdpnode
: SDP-KEPT ( -- n n )
   7 1 SDP-NODES SDPNODE:KIND !
   1 SDP-NODES 1 SDP-REFS !
   0 SDP-REFS 2 cells SPAN:MAKE 0 SDP-NODES SDPNODE:KEPT !
   0 SDP-NODES SDPNODE:KEPT @ SPAN-SPAN:UNMAKE {: b:ptr l:n :}
   b 1 cells + @ SDPNODE:KIND @ l ;
SDP-KEPT 2 cells T= 7 T=

\ A type left open at `;STRUCTURE` is unclosed and `ptr` with no pointee malformed; a type's
\ tokens may stand on two lines and hold blanks, as a declaration's do.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdpopen FIELD kept SPAN:span<ptr ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdpopen': unclosed field type application at 'SPAN:span<ptr'"
DECL-DIAG:HAS? TTRUE
s\" STRUCTURE-DECL:SD-RUN sdpline FIELD kept SPAN:span<ptr\n   sdpline> ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE-DECL:SD-RUN sdpspaced FIELD kept SPAN:span< ptr sdpspaced > ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdpbare FIELD kept SPAN:span<ptr> ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdpbare': malformed field type application at 'SPAN:span<ptr>'"
DECL-DIAG:HAS? TTRUE

\ A pointer to its own family is a pointer to a linear value when the family
\ holds one, in either field order: judged once the fields are committed.
package SDSELF
public
DEFLINEAR SDSELF:tok
;package
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdslnode FIELD next ptr sdslnode FIELD t SDSELF:tok ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdslnode': field type is a pointer to a linear value and cannot own it at 'sdslnode'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdslnode2 FIELD t SDSELF:tok FIELD next ptr sdslnode2 ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdslnode2': field type is a pointer to a linear value and cannot own it at 'sdslnode2'"
DECL-DIAG:HAS? TTRUE

\ hostile declarations: a field spelled like a fragment of the message is
\ reported as the token it is, and exactly one line is emitted.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgforge FIELD duplicate n FIELD duplicate n ;STRUCTURE" TEST-EVAL:RC 7102 T=
s" habu: bad structure declaration 'sdgforge': duplicate field name at 'duplicate'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:LEN 80 T=

\ a family name spelled like one of the JSON packet's own keys keeps key and
\ value distinguishable, and a token carrying a double quote is escaped rather
\ than closing the JSON string early.
DECL-DIAG:JSON
s" STRUCTURE-DECL:SD-RUN token FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=
s\" \"decl\":\"structure\"" DECL-DIAG:HAS? TTRUE
s\" \"family\":\"token\"" DECL-DIAG:HAS? TTRUE
s\" \"token\":\"z\"" DECL-DIAG:HAS? TTRUE
s\" \"reason\":\"duplicate field name\"" DECL-DIAG:HAS? TTRUE

DECL-DIAG:JSON
s\" STRUCTURE-DECL:SD-RUN sdgquote FIELD aq\"b n ;STRUCTURE" TEST-EVAL:RC 7101 T=
s\" \"token\":\"aq\\\"b\"" DECL-DIAG:HAS? TTRUE
s\" \"family\":\"sdgquote\"" DECL-DIAG:HAS? TTRUE

\ accepted declarations stay silent.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgok1 FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok2 ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok3<a> FIELD a a ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE-DECL:SD-RUN sdgok4 DERIVE eq FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE

\ the armed duplicate-family reason, which no case above reaches: it is armed at
\ SD-REGISTER, immediately before the registry call that can raise it.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgtwice FIELD b n ;STRUCTURE" TEST-EVAL:RC 7102 T=
s" habu: bad structure declaration 'sdgtwice': duplicate family at 'sdgtwice'"
DECL-DIAG:HAS? TTRUE

\ SD-CLOSE arms no reason of its own: the generator's rejects are raised past the
\ last token this front end holds, so they are answered from the packet's code
\ table. The over-long span cap and its "..." marker belong to the shared packet
\ and are pinned once, in test/enum-decl-suite.f section 22h.
DECL-DIAG:PROSE
s" STRUCTURE-DECL:SD-RUN sdgvrsv FIELD z sdgvrsv ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdgvrsv': field type cannot recursively name its owner at 'sdgvrsv'"
DECL-DIAG:HAS? TTRUE

DECL-DIAG:OFF

\ SD-REPLAY - registering a STRUCTURE from tokens a tool already lexed
\ (src/habu/verify-source.f), so a later declaration can name the family as a payload
\ type. Registration includes the MAKE/UNMAKE variant rows, their constructor
\ package and checked effects; replay emits no runtime code.
package struct-replay-test
public

\ The replay entry under `catch` on copies of both strings, so a reject answers
\ its code as TEST-EVAL:RC does for the live entry.
: RP-EV ( ptr u8 n ptr u8 n -- ) STRUCTURE-DECL:SD-REPLAY ;
: RP-TRY ( ptr u8 n ptr u8 n -- n )
   [: 2over 2over RP-EV ;] catch {: rc:n :} 2drop 2drop rc ;
: DICT-RECS ( -- n ) ndict@ ;

variable RP-DICT
: DICT-MARK ( -- ) DICT-RECS RP-DICT ! ;
: DICT-SAME ( -- ) DICT-RECS RP-DICT @ T= ;

private
;package

\ A replayed STRUCTURE registers its family: a checked MAKE call resolves while the
\ neutral pair is open, and the native dictionary does not move (DICT-SAME).
struct-replay-test:DICT-MARK
CHECKER-SCOPE-START-NEUTRAL
s" rpsd" s" FIELD one n FIELD two n ;STRUCTURE" struct-replay-test:RP-TRY 0 T=
s" S1 ( n n -- rpsd ) RPSD:MAKE" VERIFY:CANDIDATE-IN-SCOPE -1 T=
s" S1BAD ( n -- rpsd ) RPSD:MAKE" VERIFY:CANDIDATE-IN-SCOPE 0 T=
CHECKER-SCOPE-FINALIZE
struct-replay-test:DICT-SAME

\ Header clauses replay too; derived EQ calls rows only the replay recorded, so it
\ runs in the check tool's replay scope (test/replay-scope.f).
TOOL-REPLAY:OPEN
s" rpsdh" s" POLICY packed-tag DERIVE eq FIELD one n ;STRUCTURE"
struct-replay-test:RP-TRY 0 T=
TOOL-REPLAY:CLOSE

\ A field typed by a family registered through an earlier replay resolves.
s" rpsdpay" s" FIELD h rpsd ;STRUCTURE" struct-replay-test:RP-TRY 0 T=

\ A malformed replayed STRUCTURE reports through the same renderer as a live one.
DECL-DIAG:PROSE
s" rpsdbad" s" FIELD one nope ;STRUCTURE" struct-replay-test:RP-TRY 7109 T=
s" habu: bad structure declaration 'rpsdbad': unknown field type at 'nope'"
DECL-DIAG:HAS? TTRUE

\ A buffer with no terminator rejects through the front end's own gate.
DECL-DIAG:PROSE
s" rpsdnoend" s" FIELD one n" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration 'rpsdnoend': missing ;STRUCTURE"
DECL-DIAG:HAS? TTRUE

\ A zero-length name reaches the missing-name gate.
DECL-DIAG:PROSE
s" " s" FIELD one n ;STRUCTURE" struct-replay-test:RP-TRY 7107 T=
s" habu: bad structure declaration '': missing name" DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A rejected replay leaves the stream closed, so the next LIVE declaration reads
\ the input source again.
DECL-DIAG:PROSE
s" rpsddangle" s" FIELD" struct-replay-test:RP-TRY 7107 T=
DECL-DIAG:OFF
s" STRUCTURE-DECL:SD-RUN rpsdafter FIELD one n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ Control words are reserved in both name positions: a family or field named
\ `if` would compile as the control word wherever generated code names it. The
\ list lives once in TYPE-NAME:CONTROL? (src/core/type-family.f), read through
\ TYPE-NAME:FAMILY-RESERVED? and by field rows through PF-RESERVED?. The family
\ position answers 7110, the field position 7125; `?do`, `+loop` and `;match`
\ do not start with a lowercase letter, so the tail gate refuses them first
\ with 7101.
DECL-DIAG:PROSE
s" STRUCTURE if FIELD x n ;STRUCTURE"      TEST-EVAL:RC 7110 T=
s" habu: bad structure declaration 'if': reserved name at 'if'" DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE do FIELD x n ;STRUCTURE"      TEST-EVAL:RC 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE match FIELD x n ;STRUCTURE"   TEST-EVAL:RC 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE endcase FIELD x n ;STRUCTURE" TEST-EVAL:RC 7110 T=
DECL-DIAG:PROSE
s" STRUCTURE ?do FIELD x n ;STRUCTURE"     TEST-EVAL:RC 7101 T=
DECL-DIAG:PROSE
s" STRUCTURE ;match FIELD x n ;STRUCTURE"  TEST-EVAL:RC 7101 T=

DECL-DIAG:PROSE
s" STRUCTURE sdcwf FIELD if n ;STRUCTURE" TEST-EVAL:RC 7125 T=
s" habu: bad structure declaration 'sdcwf': reserved field name at 'if'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdcwf2 FIELD x n FIELD loop n ;STRUCTURE" TEST-EVAL:RC 7125 T=
s" habu: bad structure declaration 'sdcwf2': reserved field name at 'loop'"
DECL-DIAG:HAS? TTRUE

\ The match is on the WHOLE token: a name that merely contains a control word is
\ an ordinary name and still declares, in both positions.
DECL-DIAG:PROSE
s" STRUCTURE iffy FIELD looping n FIELD thence n ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:SILENT? TTRUE
DECL-DIAG:OFF

\ A field may name a linear family at any depth, and the structure then owns it
\ (TFAM-CONCRETE-LINEAR?); test/type-linear-suite.f checks how such values are used.
package SDLIN
public
DEFLINEAR SDLIN:tok                                   \ the linear owner these fixtures nest
;package

\ depth 1: a field naming the linear con itself.
s" STRUCTURE sdlbox FIELD t SDLIN:tok FIELD k n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ depth 2: a field naming that linear family.
TYPE-FIELD:COUNT B !
s" STRUCTURE sdlouter FIELD inner sdlbox FIELD z n ;STRUCTURE" TEST-EVAL:RC 0 T=
B @ TYPE-FIELD:NAME$ s" inner" CORE-STR= TTRUE
B @ TYPE-FIELD:CELLS@ 2 T=                            \ the field is the whole bundle, not one cell
B @ 1 + TYPE-FIELD:SLOT@ 2 T=                         \ z sits after it

\ depth 3: the walk recurses again rather than stopping one level down.
s" STRUCTURE sdldeep FIELD d sdlouter ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a sum reached through a product counts the same way.
s" ENUM sdlsum VARIANT hold FIELD m sdlbox ;VARIANT VARIANT none FIELD c n ;VARIANT ;ENUM" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlviasum FIELD e sdlsum ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a nested chain with no linear value anywhere declares too.
s" STRUCTURE sdlplain FIELD v n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlplainer FIELD inner sdlplain ;STRUCTURE" TEST-EVAL:RC 0 T=

\ wrong role: the same word in the FIELD NAME position is a name, never a type.
s" STRUCTURE sdlrole FIELD sdlbox n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ reordering: naming a family before it is declared is still an unknown type,
\ so acceptance comes from resolution and not from the spelling alone.
DECL-DIAG:PROSE
s" STRUCTURE sdlfwd FIELD m sdllater ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlfwd': unknown field type at 'sdllater'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A name that resolves says why it cannot be a field type: a parametric family
\ named bare. A name that resolves to nothing reports unknown.
s" STRUCTURE sdlgen<a> FIELD a a ;STRUCTURE" TEST-EVAL:RC 0 T=
DECL-DIAG:PROSE
s" STRUCTURE sdlgenuse FIELD m sdlgen ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlgenuse': field type is parametric and needs type arguments at 'sdlgen'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ A field may not name a pointer to a linear value. A pointer is a non-owning
\ boundary, so the linearity walk stops at the pointer node and the structure
\ would read non-linear, copying and dropping freely with a linear resource
\ behind the address. TFCL-NODE?, the node walk the linearity accounting
\ trusts, refuses the con and family spellings alike at every depth:
\ `ptr ptr sdlbox` rejects on the inner recursion, anchoring the diagnostic on
\ the family that owns the resource. A pointer to something that owns nothing
\ (`ptr n`, `ptr sdlplain`) stays legal.

\ the two spellings side by side. Naming the value owns it; pointing at it cannot.
s" STRUCTURE sdlowns FIELD m sdlbox ;STRUCTURE" TEST-EVAL:RC 0 T=

DECL-DIAG:PROSE
s" STRUCTURE sdlptr FIELD p ptr sdlbox ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptr': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? TTRUE

\ the con spelling launders the same way, so the same rule refuses it, and the
\ diagnostic names the con it found rather than some enclosing family.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrcon FIELD p ptr SDLIN:tok ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptrcon': field type is a pointer to a linear value and cannot own it at 'SDLIN:tok'"
DECL-DIAG:HAS? TTRUE

\ depth: a second pointer does not launder past the rule. The inner recursion
\ rejects first, so the token names the family that owns the resource.
DECL-DIAG:PROSE
s" STRUCTURE sdlptr2 FIELD p ptr ptr sdlbox ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptr2': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? TTRUE

\ reaching the resource through a nested family or through a sum is still reaching
\ it, so a pointer to either is refused for the same reason.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrdeep FIELD p ptr sdldeep ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptrdeep': field type is a pointer to a linear value and cannot own it at 'sdldeep'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
s" STRUCTURE sdlptrsum FIELD p ptr sdlsum ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptrsum': field type is a pointer to a linear value and cannot own it at 'sdlsum'"
DECL-DIAG:HAS? TTRUE

\ a field anywhere in the body is checked, not just the first one, and the token
\ names the offending field's type rather than the declaration's first field.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrlate FIELD a n FIELD p ptr sdlbox FIELD c n ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptrlate': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? TTRUE

\ `ptr` lets a field name its own family only inside the pointee: by value after
\ it, or in the declaration after a refused pointee, the name is refused 7109.
s" STRUCTURE sdlpquot FIELD q [ ptr sdlpquot sdlpquot -- ] ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" STRUCTURE sdlpleak FIELD next ptr Q ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" STRUCTURE sdlpafter FIELD me sdlpafter ;STRUCTURE" TEST-EVAL:RC 7109 T=

\ the controls. A pointer to a family that owns nothing declares, so the rejects
\ above answer the POINTEE's linearity and not the word `ptr`.
s" STRUCTURE sdlptrok FIELD p ptr sdlplain FIELD k n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrok2 FIELD p ptr ptr sdlplain ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrn FIELD p ptr n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ wrong role: the same words in the FIELD NAME position are names, never types.
\ Neither resolves and neither is refused.
s" STRUCTURE sdlptrrole FIELD ptr n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" STRUCTURE sdlptrrole2 FIELD sdlbox n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ hostile comments. A declaration body has no comment syntax: `(` and `\` are
\ ordinary tokens, so the declaration is malformed and rejects 7107, not this
\ rule's 7109.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrpar FIELD v n ( FIELD p ptr sdlbox ) ;STRUCTURE" TEST-EVAL:RC 7107 T=
s" habu: bad structure declaration 'sdlptrpar': unexpected token in structure declaration at '('"
DECL-DIAG:HAS? TTRUE                                  \ the paren itself was the token
DECL-DIAG:PROSE
s" STRUCTURE sdlptrbsl FIELD v n \ FIELD p ptr sdlbox" TEST-EVAL:RC 7107 T=
s" habu: bad structure declaration 'sdlptrbsl': unexpected token in structure declaration at '\'"
DECL-DIAG:HAS? TTRUE                                  \ and so was the backslash
\ and trailing text after the offending field cannot suppress the reject, because
\ the field is resolved before anything following it is read.
DECL-DIAG:PROSE
s" STRUCTURE sdlptrtail FIELD p ptr sdlbox ( note ) ;STRUCTURE" TEST-EVAL:RC 7109 T=
s" habu: bad structure declaration 'sdlptrtail': field type is a pointer to a linear value and cannot own it at 'sdlbox'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:OFF

\ 15. DERIVE addr: one accessor per field plus AT / BYTES / CELLS, in the
\     family's spelling and wordlist, generated at commit because an accessor
\     takes a committed field id (src/core/structure-decl.f). The checker
\     derives each accessor's family, offset, extent, role and schema from the
\     field id, so the declared output type is judged.
DECL-DIAG:PROSE                                       \ this section rejects too: keep the output clean
STRUCTURE sdaddr
   DERIVE addr
   FIELD a n
   FIELD p ptr u8
   FIELD c n
;STRUCTURE
2 TYPED-BUFFER SDA-BUF sdaddr

SDADDR:BYTES 24 T=                                    \ the size words BEGIN-STRUCTURE published
SDADDR:CELLS 3 T=

: SDA-W ( n ptr u8 n n -- ) {: av:n pv:ptr cv:n i:n :} av pv cv SDADDR:MAKE i SDA-BUF ! ;
: SDA-A ( n -- n ) SDA-BUF SDADDR:A @ ;
: SDA-C ( n -- n ) SDA-BUF SDADDR:C @ ;
: SDA-P ( n -- ptr u8 ) SDA-BUF SDADDR:P @ ;
: SDA-UA ( n -- n ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:ptr cv:n :} av ;
: SDA-UC ( n -- n ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:ptr cv:n :} cv ;
: SDA-UP ( n -- ptr u8 ) SDA-BUF @ SDADDR:UNMAKE {: av:n pv:ptr cv:n :} pv ;

10 NULL-PTR 30 0 SDA-W
40 NULL-PTR 60 1 SDA-W
\ a field read through an accessor equals the same field read through UNMAKE,
\ for a scalar field, a pointer field, and the last field of the record
0 SDA-A 0 SDA-UA T=
0 SDA-C 0 SDA-UC T=
0 SDA-P 0 SDA-UP = TTRUE
1 SDA-A 40 T=
1 SDA-C 60 T=

\ AT strides by the committed width and PRESERVES the family, so the accessor of
\ the same family reads the next record; the stride is signed both ways.
: SDA-NEXT ( -- n ) 0 SDA-BUF 1 SDADDR:AT SDADDR:A @ ;
: SDA-PREV ( -- n ) 1 SDA-BUF -1 SDADDR:AT SDADDR:C @ ;
SDA-NEXT 40 T=
SDA-PREV 30 T=

\ the declared output type is the FIELD's type: a pointer field projects as
\ `ptr ptr u8` and nothing else, and a scalar field does not coerce.
s" SDAX1 ( ptr sdaddr -- ptr ptr u8 ) SDADDR:P" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAX2 ( ptr sdaddr -- ptr n ) SDADDR:P" CHECK-QUIET-CANDIDATE! 0 T=
s" SDAX3 ( ptr sdaddr -- ptr n ) SDADDR:A" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAX4 ( ptr sdaddr -- ptr r ) SDADDR:A" CHECK-QUIET-CANDIDATE! 0 T=
\ and the accessor of one family is not the accessor of another
s" STRUCTURE sdaddr2 DERIVE addr FIELD a n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDAX5 ( ptr sdaddr2 -- ptr n ) SDADDR:A" CHECK-QUIET-CANDIDATE! 0 T=

\ a GENERIC family projects at the caller's instantiation
STRUCTURE sdagen<a>
   DERIVE addr
   FIELD v a
   FIELD k n
;STRUCTURE
1 TYPED-BUFFER SDAG-BUF sdagen<n>
: SDAG-W ( n n n -- ) {: vv:n kv:n i:n :} vv kv SDAGEN:MAKE i SDAG-BUF ! ;
: SDAG-V ( n -- n ) SDAG-BUF SDAGEN:V @ ;
77 88 0 SDAG-W
0 SDAG-V 77 T=
s" SDAG1 ( ptr sdagen<a> -- ptr a ) SDAGEN:V" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAG2 ( ptr sdagen<a> -- ptr n ) SDAGEN:V" CHECK-QUIET-CANDIDATE! 0 T=

\ ...but a type argument that is itself a FAMILY is not an instantiation of the
\ accessor at all, whatever its width: the parameter binds no family term
\ (measured: `expected: ptr sdagen<a> actual: ptr sdagen<sdaw1>`), so the call
\ is refused at the accessor's own token before any address arithmetic is judged.
STRUCTURE sdaw1 FIELD z n ;STRUCTURE                 \ one cell
STRUCTURE sdaw2 FIELD z n FIELD w n ;STRUCTURE       \ two cells
s" SDAW1 ( ptr sdagen<sdaw1> -- ptr sdaw1 ) SDAGEN:V" CHECK-QUIET-CANDIDATE! 0 T=
s" SDAW2 ( ptr sdagen<sdaw2> -- ptr n ) SDAGEN:K" CHECK-QUIET-CANDIDATE! 0 T=

\ `record-at` scales by the caller's count, so it refuses a pointee whose
\ instantiated width is not the committed width: an argument wider than one
\ cell moves every field past the first, which F:AT's baked multiplier and an
\ accessor's baked offset assume away. A one-cell argument is not wide, so the
\ refusal is about width, not about a family argument.
s" SDAW3 ( ptr sdagen<sdaw2> n -- ptr sdagen<sdaw2> ) record-at" CHECK-QUIET-CANDIDATE! 0 T=
s" SDAW4 ( ptr sdagen<sdaw1> n -- ptr sdagen<sdaw1> ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
s" SDAW5 ( ptr sdaddr n -- ptr sdaddr ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
\ off a layout pointee the plain row answers, and it is `cells +` on any pointer.
s" SDAW6 ( ptr u8 n -- ptr u8 ) record-at" CHECK-QUIET-CANDIDATE! -1 T=
\ the wide instantiation itself stays a perfectly ordinary type: what is refused
\ is the address surface over it, not the pointer.
s" SDAW7 ( ptr sdagen<sdaw2> -- n ) drop 0" CHECK-QUIET-CANDIDATE! -1 T=

\ A field may hold an option of a multi-cell STRUCTURE: option's cell parameter
\ takes the layout-kind argument, as a variant payload's does. SOME and NONE go
\ through the generated accessor and read back by MATCH, and the fields around
\ the wide one keep their values. The options are built by words that declare
\ option<sdqpair>: a bare OPTION:NONE or OPTION:SOME inside a body stays the
\ open one-cell option<a> and does not meet the wide site.
STRUCTURE sdqpair FIELD x n FIELD y n ;STRUCTURE
: SDQ-ABSENT ( -- option<sdqpair> ) OPTION:NONE ;
: SDQ-PRESENT ( n n -- option<sdqpair> ) SDQPAIR:MAKE OPTION:SOME ;
: SDQ-OPEN ( option<sdqpair> -- n )
   MATCH option
      none OF -1 ENDOF
      some OF SDQPAIR:UNMAKE {: xv:n yv:n :} xv 100 * yv + ENDOF
   ;MATCH ;
s" STRUCTURE sdqrec DERIVE addr FIELD k n FIELD res option<sdqpair> FIELD z n ;STRUCTURE"
TEST-EVAL:RC 0 T=
s" 1 TYPED-BUFFER SDQ-BUF sdqrec" TEST-EVAL:RC 0 T=
s" : SDQ-INIT ( -- ) 11 SDQ-ABSENT 22 SDQREC:MAKE 0 SDQ-BUF ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-SOME ( n n -- ) SDQ-PRESENT 0 SDQ-BUF SDQREC:RES ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-NONE ( -- ) SDQ-ABSENT 0 SDQ-BUF SDQREC:RES ! ;" TEST-EVAL:RC 0 T=
s" : SDQ-READ ( -- n ) 0 SDQ-BUF SDQREC:RES @ SDQ-OPEN ;" TEST-EVAL:RC 0 T=
s" : SDQ-SIDES ( -- n ) 0 SDQ-BUF SDQREC:K @ 100 * 0 SDQ-BUF SDQREC:Z @ + ;" TEST-EVAL:RC 0 T=
s" SDQ-INIT SDQ-READ" TEST-EVAL:N -1 T=
s" 3 4 SDQ-SOME SDQ-READ" TEST-EVAL:N 304 T=
s" SDQ-SIDES" TEST-EVAL:N 1122 T=
s" SDQ-NONE SDQ-READ" TEST-EVAL:N -1 T=
s" SDQ-SIDES" TEST-EVAL:N 1122 T=
\ the same field without the addr surface
s" STRUCTURE sdqplain FIELD res option<sdqpair> ;STRUCTURE" TEST-EVAL:RC 0 T=

\ a field named like a generated member is refused AT ITS OWN TOKEN, because the
\ generator would otherwise render a name it had already defined.
s" STRUCTURE sdabad DERIVE addr FIELD at n ;STRUCTURE" TEST-EVAL:RC 7110 T=
s" STRUCTURE sdabad2 DERIVE addr FIELD make n ;STRUCTURE" TEST-EVAL:RC 7110 T=
s" STRUCTURE sdabad3 DERIVE addr FIELD cells n ;STRUCTURE" TEST-EVAL:RC 7110 T=
\ without the addr surface the same names are ordinary fields
s" STRUCTURE sdaok FIELD at n FIELD cells n ;STRUCTURE" TEST-EVAL:RC 0 T=
\ and a family with no field has no address to take
s" STRUCTURE sdaempty DERIVE addr ;STRUCTURE" TEST-EVAL:RC 7119 T=

\ the generated words are the family's own: a stray tail in its reserved
\ constructor package is still refused, and an accessor cannot be undefined.
s" undefine SDADDR:A" TEST-EVAL:RC 7111 T=
s" undefine SDADDR:AT" TEST-EVAL:RC 7111 T=

\ a PRIVATE family takes the private spelling, in the declaring package only.
package SDAPRIV
private
STRUCTURE hrec
   DERIVE addr
   FIELD x n
   FIELD y n
;STRUCTURE
1 TYPED-BUFFER HB hrec
: HW ( n n n -- ) {: xv:n yv:n i:n :} xv yv HREC-MAKE i HB ! ;
: HX ( n -- n ) HB HREC-X @ ;
: HY ( n -- n ) HB HREC-Y @ ;
: HNEXT ( n -- ptr SDAPRIV:hrec ) HB 0 HREC-AT ;
s" HXI ( ptr hrec -- ptr n ) HREC-X" CHECK-QUIET-CANDIDATE! -1 T=   \ certifies HERE
public
: HGO ( -- n n n ) 5 6 0 HW 0 HX 0 HY HREC-BYTES ;
;package
SDAPRIV:HGO 16 T= 6 T= 5 T=

package SDAOTHER
private
\ The accessor name reaches nothing here: SDAP1 is refused as an undefined word
\ (0), as its diagnostic says. Its signature names no private type, which would
\ be refused too; SDAP2 refuses for that type.
DECL-DIAG:PROSE
s" SDAP1 ( n -- n ) HREC-X" CHECK-CANDIDATE! 0 T=
s" undefined word 'HREC-X'" DECL-DIAG:HAS? TTRUE
s" SDAP2 ( ptr SDAPRIV:hrec -- n ) drop 0" CHECK-QUIET-CANDIDATE! 0 T=
public
;package

\ A private family may derive eq and hash too, in the same private spelling.
package SDAPRIVDRV
private
STRUCTURE drec
   DERIVE eq hash
   FIELD u n
;STRUCTURE
public
: DEQ ( -- bool ) 4 DREC-MAKE 4 DREC-MAKE DREC-EQ ;
: DNE ( -- bool ) 4 DREC-MAKE 5 DREC-MAKE DREC-EQ ;
: DHS ( -- bool ) 4 DREC-MAKE DREC-HASH 4 DREC-MAKE DREC-HASH = ;
;package
SDAPRIVDRV:DEQ TTRUE
SDAPRIVDRV:DNE TFALSE
SDAPRIVDRV:DHS TTRUE

\ a rejected addr declaration rolls the whole thing back, accessors included:
\ the name is free and re-declaring it publishes working words.
TYPE-FIELD:COUNT PFB !
s" STRUCTURE sdaroll DERIVE addr FIELD z n FIELD z n ;STRUCTURE" TEST-EVAL:RC 7102 T=
TYPE-FIELD:COUNT PFB @ T=
s" STRUCTURE sdaroll DERIVE addr FIELD z n ;STRUCTURE" TEST-EVAL:RC 0 T=
s" SDAR1 ( ptr sdaroll -- ptr n ) SDAROLL:Z" CHECK-QUIET-CANDIDATE! -1 T=

\ `field-project` is reserved: a package's own is refused at its definition (70,
\ `E-RESERVED-DEFINITION habu: ... reserved word cannot be a definition name:
\ field-project` on fd 2).
package SDAFP
variable V
s" : field-project ( a n -- ptr n ) drop drop V ;" TEST-EVAL:RC 70 T=
s" field-project" get-current search-wl 0= TTRUE      \ the definition did not take
;package

\ A package's own `*`, RECORD-AT, `+` or `swap`, defined before a STRUCTURE with
\ DERIVE addr, captures none of the words it makes: each reads its own field
\ and AT element. Each is undefined before the storage, whose definer spells it.
package SDACAPM
: * ( n n -- n ) + ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine *
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPM:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPR
8 constant RECORD-AT
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine RECORD-AT
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPR:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPP
: + ( n n -- n ) - ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine +
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPP:GO 2 T= 16 T= 11 T= 44 T=

package SDACAPS
: swap ( n n -- n n ) ;
STRUCTURE cp DERIVE addr FIELD a n FIELD b n ;STRUCTURE
undefine swap
2 TYPED-BUFFER CB cp
public
: GO ( -- n n n n )
   11 0 CB CP-A !  22 0 CB CP-B !  33 1 CB CP-A !  44 1 CB CP-B !
   0 CB 1 CP-AT CP-B @  0 CB CP-A @  CP-BYTES  CP-CELLS ;
;package
SDACAPS:GO 2 T= 16 T= 11 T= 44 T=

\ 16. OPAQUE is refused after a field, twice, on a fieldless family, in an ENUM and
\     outside a package; each reject keeps the field rows at their mark and renders.
: REJ-MARK ( -- ) TYPE-FIELD:COUNT PFB ! ;
: REJ-SAME ( -- ) TYPE-FIELD:COUNT PFB @ T= ;
package SDOPQ
public
STRUCTURE obox OPAQUE FIELD x n FIELD y n ;STRUCTURE
;package

\ A private family may say OPAQUE too: the placement is already private, so the
\ clause changes nothing and is admitted.
package SDOPQPRIV
private
s" STRUCTURE opriv OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T=
public
;package

\ The definer rejects, each at its token, each with the field rows at their mark.
package SDOPQREJ
public
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE olate FIELD x n OPAQUE ;STRUCTURE" TEST-EVAL:RC 7107 T=
REJ-SAME
s" habu: bad structure declaration 'olate': header clause after the first field at 'OPAQUE'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE otwice OPAQUE OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 7206 T=
REJ-SAME
s" habu: bad structure declaration 'otwice': a second OPAQUE clause in one declaration at 'OPAQUE'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE obare OPAQUE ;STRUCTURE" TEST-EVAL:RC 7107 T=
REJ-SAME
s" habu: bad structure declaration 'obare': opaque requires a family with fields at 'obare'"
DECL-DIAG:HAS? TTRUE
DECL-DIAG:PROSE
REJ-MARK
s" ENUM oshade OPAQUE VARIANT dark ;VARIANT ;ENUM" TEST-EVAL:RC 7107 T=
REJ-SAME
s" habu: bad enum declaration 'oshade': unexpected token in enum declaration at 'OPAQUE'"
DECL-DIAG:HAS? TTRUE
;package

\ Outside any package there is no private wordlist for the pair to land in: the
\ clause refuses before any name is checked, the loader goes on, the name is free.
DECL-DIAG:PROSE
REJ-MARK
s" STRUCTURE oloose OPAQUE FIELD x n ;STRUCTURE" TEST-EVAL:RC 7207 T=
REJ-SAME
s" habu: bad structure declaration 'oloose': OPAQUE requires a family declared in a package at 'OPAQUE'"
DECL-DIAG:HAS? TTRUE
s" STRUCTURE oloose FIELD x n ;STRUCTURE" TEST-EVAL:RC 0 T=

\ 17. What OPAQUE does: the family's generated words — MAKE/UNMAKE and every
\     DERIVE member — take section 13's private-family placement, bare
\     FAMILY-MEMBER in the declaring package's private wordlist, protected by
\     name, with no constructor namespace stamped; the type stays nameable from
\     any package and the declaration's own section is restored after
\     generation. A reject after generation retires the privately placed words.
package SDOPQ
public
STRUCTURE oaddr OPAQUE DERIVE addr FIELD x n FIELD y n ;STRUCTURE
STRUCTURE oeq OPAQUE DERIVE eq hash FIELD u n ;STRUCTURE
STRUCTURE oinit OPAQUE DERIVE init FIELD a n FIELD b n ;STRUCTURE
: OAFTER ( -- n ) 8 ;                                 \ defined after generation: still public
: ORT ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE ;          \ section 16's family, callable bare
: OEQT ( -- bool ) 4 OEQ-MAKE 4 OEQ-MAKE OEQ-EQ ;
: OEQF ( -- bool ) 4 OEQ-MAKE 5 OEQ-MAKE OEQ-EQ ;
: OHSH ( -- bool ) 4 OEQ-MAKE OEQ-HASH 4 OEQ-MAKE OEQ-HASH = ;
s" SDOI ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! -1 T=
s" SDOX ( ptr oaddr -- ptr n ) OADDR-X" CHECK-QUIET-CANDIDATE! -1 T=
s" SDON ( mut-view<a,b,c,init<d,oinit>> -- mut-view<a,b,c,init<d,oinit>> n ) OINIT-A@" CHECK-QUIET-CANDIDATE! -1 T=
private                                               \ the pair lives in the private wordlist
s" undefine OBOX-MAKE" TEST-EVAL:RC 7111 T=           \ protected by name there
s" undefine OBOX-UNMAKE" TEST-EVAL:RC 7111 T=
public
;package
11 22 SDOPQ:ORT 22 T= 11 T=
SDOPQ:OAFTER 8 T=
SDOPQ:OEQT TTRUE
SDOPQ:OEQF TFALSE
SDOPQ:OHSH TTRUE

\ Another package names the type and reaches none of the words, under any
\ spelling: the probe is refused as an undefined word (0), as section 13's SDPO is.
package SDOPQOTHER
private
s" SDOO ( n n -- n n ) OBOX-MAKE OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOQ ( n n -- n n ) SDOPQ-OBOX:MAKE SDOPQ-OBOX:UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOC ( n n -- n n ) SDOPQ:OBOX-MAKE SDOPQ:OBOX-UNMAKE" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOE ( SDOPQ:oeq SDOPQ:oeq -- bool ) SDOPQ-OEQ:EQ" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOA ( ptr SDOPQ:oaddr -- ptr n ) SDOPQ-OADDR:X" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOB ( mut-view<a,b,c,init<d,SDOPQ:oinit>> -- mut-view<a,b,c,init<d,SDOPQ:oinit>> n ) OINIT-A@" CHECK-QUIET-CANDIDATE! 0 T=
s" SDOT ( SDOPQ:obox -- SDOPQ:obox )" CHECK-QUIET-CANDIDATE! -1 T=
public
;package

\ A reject after generation (DERIVE init refuses a non-canonical record at
\ commit, 7119, after MAKE/UNMAKE were placed privately) retires the privately
\ placed words: the name is free and re-declaring it works.
package SDOPQ
public
REJ-MARK
s" STRUCTURE oroll<a> OPAQUE DERIVE init FIELD value a FIELD tail n ;STRUCTURE" TEST-EVAL:RC 7119 T=
REJ-SAME
STRUCTURE oroll OPAQUE FIELD z n ;STRUCTURE
: ORR ( n -- n ) OROLL-MAKE OROLL-UNMAKE ;
;package
9 SDOPQ:ORR 9 T=

\ A reject inside generation: a private OCLASH-X already holds the name DERIVE
\ addr generates for field x, so the declaration refuses (7110) after its
\ MAKE/UNMAKE were placed. The colliding word still answers; once it is undefined
\ the same declaration succeeds and its pair round-trips.
package SDOPQ
private
: OCLASH-X ( -- n ) 77 ;
public
REJ-MARK
s" STRUCTURE oclash OPAQUE DERIVE addr FIELD x n ;STRUCTURE" TEST-EVAL:RC 7110 T=
REJ-SAME
private
OCLASH-X 77 T=
undefine OCLASH-X
public
STRUCTURE oclash OPAQUE DERIVE addr FIELD x n ;STRUCTURE
: OCR ( n -- n ) OCLASH-MAKE OCLASH-UNMAKE ;
;package
17 SDOPQ:OCR 17 T=

T-REPORT
