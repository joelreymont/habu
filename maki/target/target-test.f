\ maki/target/target-test.f - target descriptor and nominal-id regressions.

require lib/test.f
require test/checker-assert.f
require maki/target/target.f

package TARGET-TEST

variable BASE-N

: YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: ALT-DESC ( n -- TARGET:descriptor ) {: caps:n :}
   TARGET:ISA-PTX 87 32 1024 49152 caps TARGET:DESCRIPTOR ;

: ROUND ( CAD-KIND:target-id -- CAD-KIND:target-id )
   TARGET:DESCRIPTOR@ TARGET:RESOLVE ;

: ALIAS ( -- CAD-KIND:target-id )
   s" orin-nx" TARGET:SM87 TARGET:DESCRIPTOR@ TARGET:REGISTER ;

: ALT ( -- CAD-KIND:target-id )
   s" sm_87-nobarrier" TARGET:CAP-ALL TARGET:CAP-BARRIER invert and
   ALT-DESC TARGET:REGISTER ;

: BAD-ISA ( -- )
   s" bad-isa" 2 87 32 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:REGISTER drop ;

: BAD-WARP ( -- )
   s" bad-warp" TARGET:ISA-PTX 87 48 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:REGISTER drop ;

: BAD-LABEL ( -- )
   s" " TARGET:SM87 TARGET:DESCRIPTOR@ TARGET:REGISTER drop ;

: UNKNOWN ( -- )
   TARGET:ISA-PTX 89 32 1024 49152 TARGET:CAP-PTX
   TARGET:DESCRIPTOR TARGET:RESOLVE drop ;

: CAP-DESC ( n -- TARGET:descriptor )   \ distinct arch per fill registration
   TARGET:ISA-PTX swap 32 1024 49152 TARGET:CAP-PTX TARGET:DESCRIPTOR ;

: CAP-FILL ( -- )   \ fill the append-only registry to its cap (TGT-CAP = 16)
   16 TARGET:COUNT - 0 ?do
      s" cap-fill" 100 i + CAP-DESC TARGET:REGISTER drop
   loop ;

: CAP-17TH ( -- )
   s" cap-over" 900 CAP-DESC TARGET:REGISTER drop ;

\ ---- payload-order fixture -----------------------------------------------------
\ Six DESC-CK-legal facts, one distinct value per descriptor payload cell, so a
\ pair of cells exchanged in the declaration shows up as two wrong readings
\ instead of cancelling out. REGISTER interns by content, so calling ORD-ID again
\ returns the same id without appending a second row.
: ORD-CAP-SET ( -- n )   TARGET:CAP-PTX TARGET:CAP-BF16 or ;

: ORD-DESC ( -- TARGET:descriptor )
   TARGET:ISA-PTX 121 16 64 32768 ORD-CAP-SET
   TARGET:DESCRIPTOR ;

: ORD-ID ( -- CAD-KIND:target-id )
   s" b4-order" ORD-DESC TARGET:REGISTER ;

: ORD-ISA ( -- n )       ORD-ID TARGET:ISA@ ;
: ORD-ARCH ( -- n )      ORD-ID TARGET:ARCH@ ;
: ORD-WARP ( -- n )      ORD-ID TARGET:WARP@ ;
: ORD-THREADS ( -- n )   ORD-ID TARGET:THREADS@ ;
: ORD-SHARED ( -- n )    ORD-ID TARGET:SHARED@ ;
: ORD-CAPS ( -- n )      ORD-ID TARGET:CAPS@ ;

\ the registry-free duals: the same two cells read straight off an un-interned
\ descriptor (a descriptor is a layout value, so it never crosses to top level)
: ORD-DESC-SHARED ( -- n )   ORD-DESC TARGET:DESC-SHARED@ ;
: ORD-DESC-CAPS ( -- n )     ORD-DESC TARGET:DESC-CAPS@ ;

\ ---- named-payload reflection --------------------------------------------------
\ TARGET:descriptor and TARGET:id-result are declared through the unified ENUM
\ front end in full mode, so each payload cell is a named FIELD published as a
\ type-registry field row keyed (family, variant). Six cells of the same type is
\ exactly where an exchanged pair hides: the values below cannot see it, because
\ every accessor is positional, so the field NAME to payload SLOT mapping is
\ pinned directly. The words used are the read-only registry axioms
\ tools/public-signatures-core.f already reads (no trust boundary; they cannot
\ mutate anything).
\
\ A family is identified by its tail plus the constructor package its variants
\ carry, which is exactly the (package, tail) pair that owns family identity, so
\ the pins below name the family they pin rather than guessing from shape.
: FAM-TAIL? ( n ptr u8 n -- bool ) {: fam:n na:ptr nu:n :}
   fam TFAM-NAME$ na nu STR= ;

: FAM-CTOR? ( n ptr u8 n -- bool ) {: fam:n pa:ptr pu:n :}
   fam TFAM-VAR-COUNT@ 0 <= if false exit then
   fam TFAM-VAR-START@ SUMV-CTOR-PKG$ pa pu STR= ;

: FAM-HIT? ( n ptr u8 n ptr u8 n -- bool ) {: fam:n ta:ptr tu:n pa:ptr pu:n :}
   fam ta tu FAM-TAIL? fam pa pu FAM-CTOR? and ;

: FAM-N ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}   \ how many registered families answer to this (tail, constructor package)
   0
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if 1+ then
   loop ;

: FAM-ID ( ptr u8 n ptr u8 n -- n ) {: ta:ptr tu:n pa:ptr pu:n :}   \ its family id, or -1
   TFAM-N@ 0 ?do
      i ta tu pa pu FAM-HIT? if i unloop exit then
   loop -1 ;

: DESC-TAIL$ ( -- ptr u8 n )   s" descriptor" ;
: DESC-CTOR$ ( -- ptr u8 n )   s" TARGET-DESCRIPTOR" ;
: IDR-TAIL$ ( -- ptr u8 n )    s" id-result" ;
: IDR-CTOR$ ( -- ptr u8 n )    s" TARGET-ID--RESULT" ;

\ FAM-ID answers -1 for a family that is not registered, and the registry readers
\ take a live id, so every read below refuses the sentinel first: a missing family
\ has to report a wrong number, never read a record that is not there. The upper
\ bound needs no guard - FAM-ID only ever returns an index it walked.
: FAM-VARS ( n -- n ) {: fam:n :}                     \ variant count, or -1 when the family is missing
   fam 0 < if -1 exit then
   fam TFAM-VAR-COUNT@ ;

: FAM-WIDTH ( n -- n ) {: fam:n :}                    \ cell width, or -1 when the family is missing
   fam 0 < if -1 exit then
   fam TFAM-WIDTH@ ;

: FAM-VAR ( n n -- n ) {: fam:n k:n :}                \ the family's k-th variant id, or -1
   fam FAM-VARS k <= if -1 exit then
   fam TFAM-VAR-START@ k + ;

: VAR-NAME$ ( n -- ptr u8 n ) {: var:n :}             \ variant name, or a name no declaration can spell
   var 0 < if s" <missing>" exit then
   var SUMV-NAME$ ;

: DESC-FAM ( -- n )   DESC-TAIL$ DESC-CTOR$ FAM-ID ;
: DESC-VAR ( -- n )   DESC-FAM 0 FAM-VAR ;            \ the sole `value` variant
: IDR-FAM ( -- n )    IDR-TAIL$ IDR-CTOR$ FAM-ID ;
: IDR-VAR ( -- n )    IDR-FAM 0 FAM-VAR ;             \ the `ok` variant

: FLD-SLOT ( n n ptr u8 n -- n )                      \ payload slot of a named field, -1 when the variant has no such field
   TYPE-FIELD:FIND 0= if drop -1 exit then
   TYPE-FIELD:SLOT@ ;

: DESC-SLOT ( ptr u8 n -- n ) {: na:ptr nu:n :}       \ slot of a named descriptor payload cell
   DESC-FAM DESC-VAR na nu FLD-SLOT ;

: VAR-FLD-N ( n n -- n ) {: fam:n var:n :}            \ committed field rows this variant owns
   0
   TYPE-FIELD:COUNT 0 ?do
      i TYPE-FIELD:FAMILY@ fam = i TYPE-FIELD:VARIANT@ var = and if 1+ then
   loop ;

T-RESET

TARGET:COUNT BASE-N !
TARGET:SM87 TARGET:LABEL$ s" sm_87" T$=
TARGET:SM87 TARGET:ISA@ TARGET:ISA-PTX T=
TARGET:SM87 TARGET:ARCH@ 87 T=
TARGET:SM87 TARGET:WARP@ 32 T=
TARGET:SM87 TARGET:THREADS@ 1024 T=
TARGET:SM87 TARGET:SHARED@ 49152 T=
TARGET:SM87 TARGET:CAPS@ TARGET:CAP-MMA and 0<> TTRUE
TARGET:SM87 TARGET:FACTS$
s" isa=1|arch=87|warp=32|threads=1024|shared=49152|caps=127" T$=

TARGET:SM87 ROUND TARGET:SM87 TARGET:EQUAL? TTRUE
ALIAS
dup TARGET:SM87 TARGET:EQUAL? TTRUE
TARGET:DIGEST@ TARGET:SM87 TARGET:DIGEST@ = TTRUE
TARGET:COUNT BASE-N @ T=
TARGET:SM87 TARGET:LABEL$ s" sm_87" T$=

ALT
dup TARGET:SM87 TARGET:EQUAL? TFALSE
TARGET:DIGEST@ TARGET:SM87 TARGET:DIGEST@ = TFALSE
TARGET:COUNT BASE-N @ 1+ T=

' BAD-ISA E-TARGET-FACT TTHROWS
' BAD-WARP E-TARGET-FACT TTHROWS
' BAD-LABEL E-TARGET-LABEL TTHROWS
' UNKNOWN E-TARGET-UNKNOWN TTHROWS

s" TGT-OK ( CAD-KIND:target-id -- CAD-KIND:target-id ) TARGET:VALIDATE" YES
s" TGT-ROUND ( CAD-KIND:target-id -- CAD-KIND:target-id ) TARGET:DESCRIPTOR@ TARGET:RESOLVE" YES
s" TGT-KW ( CAD-KIND:target-id ptr u8 n -- n ) TARGET:KEY>WIRE" YES            \ content-key encode
s" TGT-TOOL ( CAD-KIND:toolchain-id -- ptr u8 n ) TARGET:LABEL$" NO
s" TGT-ART ( CAD-KIND:artifact-id -- ptr u8 n ) TARGET:LABEL$" NO
s" TGT-XKW ( CAD-KIND:artifact-id ptr u8 n -- n ) TARGET:KEY>WIRE" NO          \ a foreign id cannot encode
s" TARGET:RAW>TARGET-ID" 0 search-wl 0= TTRUE

\ ---- the generated constructors: exact spelling + exact effect -----------------
\ Both families are declared through the unified ENUM front end in full mode, so
\ these are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing: the checker answers 1
\ (uncheckable) for a name it cannot resolve and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types.
s" TC-DV ( n n n n n n -- TARGET:descriptor ) TARGET-DESCRIPTOR:VALUE" YES
s" TC-OK ( CAD-KIND:target-id -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:OK" YES
s" TC-WW ( -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:WRONG-WIDTH" YES
s" TC-UNK ( -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:UNKNOWN" YES
\ Forge negatives on the descriptor payload: the six cells are mandatory and
\ exact, the result is not a bare scalar, and a pointer role cannot stand in for a
\ fact cell.
s" TC-DV5 ( n n n n n -- TARGET:descriptor ) TARGET-DESCRIPTOR:VALUE" NO
s" TC-DV7 ( n n n n n n n -- TARGET:descriptor ) TARGET-DESCRIPTOR:VALUE" NO
s" TC-DVB ( n n n n n n -- n ) TARGET-DESCRIPTOR:VALUE" NO
s" TC-DVR ( ptr u8 n n n n n -- TARGET:descriptor ) TARGET-DESCRIPTOR:VALUE" NO
\ Forge negatives on the id-result ok payload: a raw cell cannot fill it, the
\ result is not a bare scalar, the payload is mandatory, and a same-width FOREIGN
\ identity role cannot stand in for the target id.
s" TC-RAW ( n -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:OK" NO
s" TC-BARE ( CAD-KIND:target-id -- n ) TARGET-ID--RESULT:OK" NO
s" TC-NONE ( -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:OK" NO
s" TC-FGN ( CAD-KIND:artifact-id -- TARGET:id-result<CAD-KIND:target-id> ) TARGET-ID--RESULT:OK" NO

\ ---- the six named payload cells sit at the six declared slots ------------------
\ Exactly one registered family answers to each (tail, constructor package) pair,
\ so the slot pins below are about TARGET's own families and nothing else.
DESC-TAIL$ DESC-CTOR$ FAM-N 1 T=
IDR-TAIL$ IDR-CTOR$ FAM-N 1 T=
DESC-FAM FAM-VARS 1 T=                          \ descriptor has the one `value` variant
DESC-VAR VAR-NAME$ s" value" T$=
DESC-FAM FAM-WIDTH 7 T=                         \ six payload cells plus one tag cell
DESC-FAM DESC-VAR VAR-FLD-N 6 T=                \ and exactly six named cells, no more
s" isa" DESC-SLOT 0 T=
s" arch" DESC-SLOT 1 T=
s" warp" DESC-SLOT 2 T=
s" threads" DESC-SLOT 3 T=
s" shared" DESC-SLOT 4 T=
s" caps" DESC-SLOT 5 T=
s" facts" DESC-SLOT -1 T=                       \ an undeclared name resolves to no slot
IDR-FAM FAM-VARS 3 T=                           \ ok, wrong-width, unknown
IDR-VAR VAR-NAME$ s" ok" T$=
IDR-FAM 1 FAM-VAR VAR-NAME$ s" wrong-width" T$=
IDR-FAM 2 FAM-VAR VAR-NAME$ s" unknown" T$=
IDR-FAM FAM-WIDTH 2 T=                          \ one payload cell plus one tag cell
IDR-FAM IDR-VAR s" id" FLD-SLOT 0 T=            \ ok carries its id at slot 0
IDR-FAM IDR-VAR VAR-FLD-N 1 T=
IDR-FAM IDR-FAM 1 FAM-VAR VAR-FLD-N 0 T=        \ wrong-width carries no payload
IDR-FAM IDR-FAM 2 FAM-VAR VAR-FLD-N 0 T=        \ unknown carries no payload

\ ---- the six cells round-trip through the production path in declared order -----
\ One distinct value per cell, written through TARGET:DESCRIPTOR + TARGET:REGISTER
\ and read back through the public accessors and the canonical serialization, so
\ any cell that lands in another cell's slot reports another cell's value.
ORD-ISA TARGET:ISA-PTX T=
ORD-ARCH 121 T=
ORD-WARP 16 T=
ORD-THREADS 64 T=
ORD-SHARED 32768 T=
ORD-CAPS ORD-CAP-SET T=
ORD-DESC-SHARED 32768 T=                        \ the registry-free duals agree
ORD-DESC-CAPS ORD-CAP-SET T=
ORD-ID TARGET:FACTS$
s" isa=1|arch=121|warp=16|threads=64|shared=32768|caps=5" T$=
ORD-ID ROUND ORD-ID TARGET:EQUAL? TTRUE         \ and the whole bundle interns back to itself

public

\ desc-twin and idr-twin are the two migrated families' SHAPES under different
\ names: same arity, same variants in the same order, same named payload cells.
\ They exist only so the negatives below can prove descriptor and decode-result
\ identity is NOMINAL - two identically shaped ENUM families never unify, in
\ either direction. They have to be public: a private family publishes no
\ constructors at all, and the positive controls build through the twins' own
\ constructors, so neither negative can pass by being unresolvable rather than
\ ill-typed.
ENUM desc-twin 0
   VARIANT value
      FIELD isa n
      FIELD arch n
      FIELD warp n
      FIELD threads n
      FIELD shared n
      FIELD caps n
   ;VARIANT
;ENUM

ENUM idr-twin 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

s" TC-TWIN ( n n n n n n -- desc-twin ) TARGET--TEST-DESC--TWIN:VALUE" YES
s" TC-TWIN-X1 ( n n n n n n -- desc-twin ) TARGET-DESCRIPTOR:VALUE" NO
s" TC-TWIN-X2 ( n n n n n n -- TARGET:descriptor ) TARGET--TEST-DESC--TWIN:VALUE" NO
s" TC-ITWIN ( CAD-KIND:target-id -- idr-twin<CAD-KIND:target-id> ) TARGET--TEST-IDR--TWIN:OK" YES
s" TC-ITWIN-X1 ( CAD-KIND:target-id -- idr-twin<CAD-KIND:target-id> ) TARGET-ID--RESULT:OK" NO
s" TC-ITWIN-X2 ( CAD-KIND:target-id -- TARGET:id-result<CAD-KIND:target-id> ) TARGET--TEST-IDR--TWIN:OK" NO

\ capacity: the seventeenth distinct descriptor rejects. The registry is
\ process-global and append-only, so this fill runs LAST; no later maki/test.f
\ suite registers targets (sched-key-test runs earlier).
CAP-FILL
TARGET:COUNT 16 T=
' CAP-17TH E-TARGET-CAP TTHROWS

;package

\ Nominal-id corruption seam: a bad CAD-KIND:target-id is only mintable via the
\ private refinement, so reopen the owning package for the E-TARGET-ID
\ negatives (qualified lookup is public-only; TT- names are test-owned).
package TARGET

: TT-ID-NEG ( -- )  -1 RAW>TARGET-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  99 RAW>TARGET-ID DESCRIPTOR@ drop ;

' TT-ID-NEG E-TARGET-ID TTHROWS
' TT-ID-BIG E-TARGET-ID TTHROWS

;package

\ § 23.9 foreign-id wire codec: round-trip every registered target + fail-closed
\ decode. Reopen the owner package for LE-PUT / registry internals (an out-of-range
\ wire raw is only forgeable inside the owning package).
package TARGET

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-WIRE-RT ( CAD-KIND:target-id -- n )        \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:target-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY registered target round-trips
   TGT-N @ 0 ?do
      i RAW>TARGET-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   TGT-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
create TT-SHA CK-BYTES allot

: TT-CKEY-RT ( CAD-KIND:target-id -- n )       \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:target-id :}
   TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-ALL ( -- n )                         \ 0 iff EVERY registered target key round-trips
   TGT-N @ 0 ?do
      i RAW>TARGET-ID TT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-CKEY-WIDTH ( -- n )                       \ an 8-byte buffer decodes as wrong-width
   TT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-FILL-FF ( -- )                            \ 32 bytes no registered descriptor can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  TT-WBUF k +  c!
      1+
   repeat drop ;

: TT-CKEY-UNKNOWN ( -- n )                     \ a 32-byte non-registered key decodes as unknown
   TT-FILL-FF
   TT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(facts), NOT the raw index
   0 RAW>TARGET-ID {: id:CAD-KIND:target-id :}
   id FACTS$ TT-SHA SHA256
   id TT-WBUF TT-WCAP KEY>WIRE drop
   TT-WBUF TT-SHA CK-EQ? if 0 else 1 then ;

TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-CKEY-ALL 0 T=
TT-CKEY-WIDTH 2 T=
TT-CKEY-UNKNOWN 3 T=
TT-CKEY-IS-SHA 0 T=

;package

T-REPORT
