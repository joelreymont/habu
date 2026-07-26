\ model-provenance-test.f - MODELPROV acceptance (inference leaf S3c).
\
\ Legs, all through the public package surface (this file never reopens
\ MODELPROV):
\   1. a pin mints over a real artifact on disk, renders to canonical hexadecimal
\      text, and two independent pins over identical inputs compare equal. The
\      two expected renderings are PINNED published identities: the whole point
\      of a provenance key is that a manifest written last week still matches a
\      pin computed today, so a changed rendering is a finding, not noise. The
\      artifact digest itself is anchored independently - the fixture files are
\      the published FIPS-180 SHA-256 test vectors for "abc" and for the empty
\      input - so the artifact leg does not rest on this engine agreeing with
\      itself.
\   2. the fold is inspected STRUCTURALLY, by walking the real preimage rows
\      CONTENT-KEY:BUF$ exposes: nine tag- and length-delimited rows in the
\      declared order, each with its exact bytes - the preimage schema version
\      first, then the adapter, residency, and packing tag texts among the rest.
\      That is what proves those typed values reach the key at all. The version
\      row is checked byte by byte against the expected little-endian cell
\      encoding rather than against the module's own fold word, so the test does
\      not certify the encoding by re-running it. It is also the only proof for
\      the adapter, because MODEL:adapter currently has exactly ONE variant
\      (hf-gpt2), so a "two different adapters differ" runtime comparison cannot
\      be constructed yet; when a second adapter variant lands, add it here.
\   3. key sensitivity: flipping any one input flips the key - each label, each
\      supplied digest, the artifact bytes, the residency, the packing - plus
\      three hostile cases: swapping the two labels (reordering), swapping the
\      configuration and tokenizer digests (wrong role), and moving a byte across
\      the label boundary ("ab"+"c" versus "a"+"bc", which only differ if the
\      library really length-delimits each row). Then, separately, FOUR
\      single-cell-difference cases: two pins whose digests differ in exactly one
\      of the four cells must compare unequal. Whole-input cases cannot catch a
\      comparison narrowed to three cells, because they differ in all four.
\   4. every refusal as its named throw, with each accept/reject boundary checked
\      from BOTH sides - the printable-label range at $7E and $7F and at a bad
\      leading byte, the hexadecimal ranges at their neighbour characters
\      ('/', ':', backtick, 'g'), the label length at 128 and 129, and a digest
\      whose only bad character is the 64th - plus a proof that a refused pin
\      leaves no residue behind.
\   5. the artifact PATH, which is the one input the package hands to a core
\      primitive: at the core path cap it is accepted, one byte over it is a
\      named refusal from that primitive rather than a process exit, a negative
\      length is refused by the byte-length role before PIN is entered, and a raw
\      cell cannot reach the path-length slot at all (checker reject).
\   6. checker negatives: a raw n or a foreign MDLCFG:cfg-proof in the proof slot
\      rejects, raw cells cannot pose as the digest, the private mint and the
\      package-private helpers are unresolvable outside the package, a cfgkey
\      cannot be compared as a pin and a pin cannot be compared as a cfgkey, and
\      the three typed convention values cannot be permuted in PIN's argument row.
\   7. the KNOWN FORGERY GAP, pinned as an executable fact rather than a comment:
\      the generated MODELPROV-MPROV:UNMAKE is reachable outside the package
\      today, and this suite DEMONSTRATES the inverse the module header warns
\      about - one donor pin plus stored hexadecimal text reconstructs a pin that
\      compares equal to an unrelated one. Both the checker verdict and the
\      demonstration are asserted as they behave TODAY, so when the sealed
\      destructure / linear UNMAKE capability
\      (habu-checker-sealed-destructure-d967fc03) lands, this leg fails and forces
\      the header caveat to be retired instead of quietly outliving the gap.
\   8. the package seal itself refuses new definitions into MODELPROV.

require lib/prelude.f
require lib/fs.f
require lib/fs-mutate.f
require lib/cad-num-arithmetic.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require test/checker-assert.f
require maki/infer/model-config.f
require maki/infer/model-provenance.f

package MODELPROV-TEST

$61 constant LOW-A                 \ 'a', the filler byte for generated labels
$62 constant LOW-B                 \ 'b'
$09 constant TAB                   \ a control byte no label may carry
$20 constant SPACE                 \ the printable range starts one above this
$7E constant TILDE                 \ the highest byte a label may carry
$7F constant DEL                   \ one above it: must be refused
$2F constant SLASH                 \ one below '0'
$3A constant COLON                 \ one above '9'
$60 constant BACKTICK              \ one below 'a'
$67 constant LOW-G                 \ one above 'f'
$41 constant UPPER-A               \ upper case is not canonical digest text
$54 constant TEXT-TAG              \ CONTENT-KEY's text-row tag byte
9 constant ROWS-WANTED             \ rows the declared preimage must have
8 constant CELL-BYTES              \ bytes one folded cell occupies
1 constant SCHEMA-WANT             \ the mprov preimage version this suite pins
64 constant HEX-LEN
128 constant LABEL-CAP             \ mirrors the module's private LABEL-MAX
255 constant PATH-AT-CAP           \ longest path src/core/util.f PATHZ holds with its NUL
256 constant PATH-OVER-CAP         \ one byte more: PATHZ must refuse it
-1 constant NEG-LEN                \ used to write the NUL one byte BEFORE the buffer
-100000000 constant BIG-NEG-LEN    \ used to fault the process outright

\ src/core/util.f defines E-PATH-RANGE, but util.f is the FIRST core prefix file
\ and loads before the check hook exists, so its constants are not nameable from
\ checked user code (unlike src/core/bytes.f E-BYTE-RANGE, which loads after the
\ hook). The ABI value is pinned here, so changing the core code reds this suite
\ instead of passing silently.
7134 constant E-PATH-CORE
-7798 constant E-MP-LEN            \ fixture: the byte-length role refused a path length

create HEX HEX-LEN allot           \ KEY-HEX landing pad
create HEX2 HEX-LEN allot          \ second landing pad, for pin-versus-pin text
create LBUF 300 allot              \ generated label bytes
create DGBUF 80 allot              \ one mutated digest
create SUBJ-OUT $400 allot
create SUBJ-ERR $400 allot

variable RW-OFF                    \ preimage walk cursor
variable RW-N

\ ---- checker-candidate verdict assertions ------------------------------------
: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

\ ---- the validated path-length role ------------------------------------------
\ The shape every real caller has now that PIN takes CAD-NUM:byte-len: validate
\ the raw cell through the library and throw a named code on refusal, so a
\ negative length cannot reach PIN, let alone the core path primitive.
: >PLEN ( n -- CAD-NUM:byte-len )
   CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                              negative OF E-MP-LEN throw ENDOF
      zero OF E-MP-LEN throw ENDOF              overflow OF E-MP-LEN throw ENDOF
      underflow OF E-MP-LEN throw ENDOF         bad-alignment OF E-MP-LEN throw ENDOF
      misaligned OF E-MP-LEN throw ENDOF
   ;MATCH ;

: ROLE-REFUSES? ( n -- bool )      \ does the production role constructor refuse it?
   CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result
      ok OF drop false ENDOF                   negative OF true ENDOF
      zero OF true ENDOF                        overflow OF true ENDOF
      underflow OF true ENDOF                   bad-alignment OF true ENDOF
      misaligned OF true ENDOF
   ;MATCH ;

\ ---- the fixture artifact set, in a per-run temporary directory ---------------
\ Fixed /tmp names race with a concurrent suite, so the tree is created per run
\ (the repository TMPDIR-MKDIR convention) and removed at the end.
create ROOT FS-PATH-CAP allot   variable ROOT-U
create ART-P FS-PATH-CAP allot  variable ART-PU
create ALT-P FS-PATH-CAP allot  variable ALT-PU
create GONE-P FS-PATH-CAP allot variable GONE-PU
create LONG-P FS-PATH-CAP allot variable LONG-PU
create OVER-P FS-PATH-CAP allot variable OVER-PU

: ART-RAW ( -- ptr u8 n )   ART-P ART-PU @ ;
: ALT-RAW ( -- ptr u8 n )   ALT-P ALT-PU @ ;
: LONG-RAW ( -- ptr u8 n )  LONG-P LONG-PU @ ;

: ART-PATH ( -- ptr u8 CAD-NUM:byte-len )   ART-P  ART-PU @ >PLEN ;
: ALT-PATH ( -- ptr u8 CAD-NUM:byte-len )   ALT-P  ALT-PU @ >PLEN ;
: GONE-PATH ( -- ptr u8 CAD-NUM:byte-len )  GONE-P GONE-PU @ >PLEN ;
: LONG-PATH ( -- ptr u8 CAD-NUM:byte-len )  LONG-P LONG-PU @ >PLEN ;
: OVER-PATH ( -- ptr u8 CAD-NUM:byte-len )  OVER-P OVER-PU @ >PLEN ;

: ART-DG ( -- ptr u8 n )
   s" ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" ;
: ALT-DG ( -- ptr u8 n )
   s" e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" ;

: FILL-LABEL ( n -- ptr u8 n ) {: u:n :}
   u 0 ?do LOW-A LBUF i + c! loop
   LBUF u ;

\ A path of exactly `total` bytes inside the fixture root, so the core path cap
\ can be probed from both sides with real paths.
: BUILD-PATH ( ptr u8 n -- n ) {: dst:ptr total:n :}
   total ROOT-U @ - 1 - FILL-LABEL {: na:ptr nu:n :}
   ROOT ROOT-U @ na nu dst JOIN-PATH ;

: MK-ROOT ( -- )
   s" habu-modelprov" TMPDIR-MKDIR {: r:ptr ru:n :}
   r ROOT ru BYTE-COPY  ru ROOT-U !
   r ru s" art.bin" ART-P JOIN-PATH ART-PU !
   r ru s" alt.bin" ALT-P JOIN-PATH ALT-PU !
   r ru s" absent.bin" GONE-P JOIN-PATH GONE-PU !
   LONG-P PATH-AT-CAP BUILD-PATH LONG-PU !
   OVER-P PATH-OVER-CAP BUILD-PATH OVER-PU ! ;

: WRITE-FIXTURES ( -- )
   MK-ROOT
   ART-RAW s" abc" WRITE-ALL                   \ the "abc" vector
   ALT-RAW s" abc" drop 0 WRITE-ALL            \ the empty-input vector
   LONG-RAW s" abc" WRITE-ALL ;                \ the same bytes at a cap-length path

: CLEANUP ( -- )
   ROOT ROOT-U @ REMOVE-TREE ;

\ ---- the canonical inputs ------------------------------------------------------
: FAM ( -- ptr u8 n )  s" openai-community/gpt2" ;
: REV ( -- ptr u8 n )  s" 11c5a3d5811f50298f278a704980280950aedb10" ;
: CFG ( -- ptr u8 n )
   s" 00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff" ;
: TKZ ( -- ptr u8 n )
   s" ffeeddccbbaa99887766554433221100ffeeddccbbaa99887766554433221100" ;

\ The two pinned published identities. These were re-derived when the preimage
\ schema version row was added as row 0 - the conscious bump this file's header
\ calls for, and the reason MODELPROV:SCHEMA exists: the version row is what
\ makes the next such change visible instead of silent.
: GOLD-M ( -- ptr u8 n )
   s" 6f64cfe57f7b76a5200a30410a1eb5272bf3bad0414e9d8144054a596436fccd" ;
: GOLD-A ( -- ptr u8 n )
   s" b822a7f4249d1d8db8a4ae8374317f27809b8b60ab80109359fcc3eec2ad807f" ;

\ ---- pin builders: one axis varies, everything else is canonical ---------------
: MAPPED-PIN ( -- MODELPROV:mprov )
   FAM REV CFG TKZ ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:MAPPED MODELPROV-PACKING:AS-STORED
   MODELPROV:PIN ;

: ALLOC-PIN ( -- MODELPROV:mprov )
   FAM REV CFG TKZ ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:ALLOCATED MODELPROV-PACKING:TRANSPOSED
   MODELPROV:PIN ;

: P-LABELS ( ptr u8 n ptr u8 n -- MODELPROV:mprov )   \ family, revision vary
   CFG TKZ ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:MAPPED MODELPROV-PACKING:AS-STORED
   MODELPROV:PIN ;

: P-DIGESTS ( ptr u8 n ptr u8 n -- MODELPROV:mprov )  \ config, tokenizer vary
   {: cfg:ptr cfgu:n tkz:ptr tkzu:n :}
   FAM REV cfg cfgu tkz tkzu ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:MAPPED MODELPROV-PACKING:AS-STORED
   MODELPROV:PIN ;

: P-ART ( ptr u8 CAD-NUM:byte-len ptr u8 n -- MODELPROV:mprov )   \ path, digest vary
   {: pth:ptr pthu:CAD-NUM:byte-len dg:ptr dgu:n :}
   FAM REV CFG TKZ pth pthu dg dgu
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:MAPPED MODELPROV-PACKING:AS-STORED
   MODELPROV:PIN ;

: P-RES ( WSTORE:residency -- MODELPROV:mprov )       \ residency varies
   {: res:WSTORE:residency :}
   FAM REV CFG TKZ ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 res MODELPROV-PACKING:AS-STORED
   MODELPROV:PIN ;

: P-PACK ( MODELPROV:packing -- MODELPROV:mprov )     \ packing varies
   {: pk:MODELPROV:packing :}
   FAM REV CFG TKZ ART-PATH ART-DG
   MODEL-ADAPTER:HF-GPT2 WSTORE-RESIDENCY:MAPPED pk
   MODELPROV:PIN ;

\ ---- rendering helpers ---------------------------------------------------------
: HEX-OF ( MODELPROV:mprov -- ptr u8 n )
   HEX HEX-LEN MODELPROV:KEY-HEX drop
   HEX HEX-LEN ;

: HEX2-OF ( MODELPROV:mprov -- ptr u8 n )
   HEX2 HEX-LEN MODELPROV:KEY-HEX drop
   HEX2 HEX-LEN ;

\ ---- generated labels and mutated digests --------------------------------------
: BYTE-LABEL ( n -- ptr u8 n ) {: c:n :}       \ 'a', c, 'b' - the bad byte inside
   LOW-A LBUF 0 + c!
   c LBUF 1 + c!
   LOW-B LBUF 2 + c!
   LBUF 3 ;

: LEAD-LABEL ( n -- ptr u8 n ) {: c:n :}       \ c, 'a', 'b' - the bad byte FIRST
   c LBUF 0 + c!
   LOW-A LBUF 1 + c!
   LOW-B LBUF 2 + c!
   LBUF 3 ;

: TILDE-CAP ( -- ptr u8 n )                    \ LABEL-CAP bytes, the last a '~'
   LABEL-CAP FILL-LABEL 2drop
   TILDE LBUF LABEL-CAP 1 - + c!
   LBUF LABEL-CAP ;

: DG-WITH ( ptr u8 n n n -- ptr u8 n ) {: a:ptr u:n p:n c:n :}
   a DGBUF u BYTE-COPY
   c DGBUF p + c!
   DGBUF u ;

\ ---- 1. a pin mints, renders, and agrees with itself ---------------------------
: T-MINT ( -- )
   s" a pin over the fixture artifact renders its pinned identity" T-LABEL
   MAPPED-PIN HEX-OF GOLD-M T$=
   ALLOC-PIN HEX-OF GOLD-A T$=
   s" two independent pins over identical inputs are the same identity" T-LABEL
   MAPPED-PIN MAPPED-PIN MODELPROV:MPROV= TTRUE
   MAPPED-PIN HEX-OF MAPPED-PIN HEX2-OF T$=
   s" pins over different inputs are different identities" T-LABEL
   MAPPED-PIN ALLOC-PIN MODELPROV:MPROV= TFALSE
   s" the preimage version is published read-only" T-LABEL
   MODELPROV:SCHEMA SCHEMA-WANT T= ;

\ ---- 2. the preimage, walked as rows off the production buffer ------------------
\ Every CONTENT-KEY text row is (tag, length, bytes), so the walk below decodes
\ the real preimage instead of searching it for substrings.
: ROW-LEN ( n -- n )               \ length byte of the row starting at this offset
   {: off:n :}
   CONTENT-KEY:BUF$ drop  off 1 + BYTE+ c@ ;

: ROW-NEXT ( -- )
   RW-OFF @ ROW-LEN {: len:n :}
   RW-OFF @ 2 + len + RW-OFF ! ;

: ROW-SEEK ( n -- )
   0 RW-OFF !
   0 ?do ROW-NEXT loop ;

: PREIMAGE-LEN ( -- n )
   CONTENT-KEY:BUF$ {: a:ptr u:n :}
   u ;

: ROW-COUNT ( -- n )
   0 RW-OFF !  0 RW-N !
   begin RW-OFF @ PREIMAGE-LEN < while
      1 RW-N +!
      ROW-NEXT
   repeat
   RW-N @ ;

: ROW-TAG ( n -- n )
   ROW-SEEK
   CONTENT-KEY:BUF$ drop RW-OFF @ BYTE+ c@ ;

: ROW$ ( n -- ptr u8 n )
   ROW-SEEK
   CONTENT-KEY:BUF$ drop  RW-OFF @ 2 + BYTE+
   RW-OFF @ ROW-LEN ;

: T-ROW-TAGS ( -- )
   ROWS-WANTED 0 ?do i ROW-TAG TEXT-TAG T= loop ;

\ Row 0 must be the version as one folded cell: eight bytes, little-endian, so
\ the low byte carries the version and the other seven are zero. Spelled out
\ byte by byte on purpose - re-running the module's own fold word here would
\ certify nothing about the encoding a manifest will live with.
: T-VERSION-ROW ( -- )
   0 ROW$ {: a:ptr u:n :}
   u CELL-BYTES T=
   a c@ SCHEMA-WANT T=
   CELL-BYTES 1 ?do a i + c@ 0 T= loop ;

: T-PREIMAGE ( -- )
   s" the mapped pin folds nine declared rows in order, version first" T-LABEL
   MAPPED-PIN drop
   ROW-COUNT ROWS-WANTED T=
   T-ROW-TAGS
   T-VERSION-ROW
   1 ROW$ FAM T$=
   2 ROW$ REV T$=
   3 ROW$ CFG T$=
   4 ROW$ TKZ T$=
   5 ROW$ ART-DG T$=                       \ the digest PIN computed off the file
   6 ROW$ s" hf-gpt2" T$=
   7 ROW$ s" mapped" T$=
   8 ROW$ s" as-stored" T$=
   s" the other residency and packing variants fold their own tags" T-LABEL
   ALLOC-PIN drop
   ROW-COUNT ROWS-WANTED T=
   T-VERSION-ROW
   7 ROW$ s" allocated" T$=
   8 ROW$ s" transposed" T$= ;

\ ---- 3. sensitivity: whole inputs, then single digest cells ---------------------
: T-SENSITIVE ( -- )
   s" flipping any single input flips the key" T-LABEL
   MAPPED-PIN  s" openai-community/gpt2-medium" REV P-LABELS  MODELPROV:MPROV= TFALSE
   MAPPED-PIN  FAM s" 11c5a3d5811f50298f278a704980280950aedb11" P-LABELS
      MODELPROV:MPROV= TFALSE
   MAPPED-PIN  TKZ TKZ P-DIGESTS  MODELPROV:MPROV= TFALSE
   MAPPED-PIN  CFG CFG P-DIGESTS  MODELPROV:MPROV= TFALSE
   MAPPED-PIN  ALT-PATH ALT-DG P-ART  MODELPROV:MPROV= TFALSE
   MAPPED-PIN  WSTORE-RESIDENCY:ALLOCATED P-RES  MODELPROV:MPROV= TFALSE
   MAPPED-PIN  MODELPROV-PACKING:TRANSPOSED P-PACK  MODELPROV:MPROV= TFALSE
   s" swapping the two labels is a different artifact set (reordering)" T-LABEL
   FAM REV P-LABELS  REV FAM P-LABELS  MODELPROV:MPROV= TFALSE
   s" swapping the two digests is a different artifact set (wrong role)" T-LABEL
   CFG TKZ P-DIGESTS  TKZ CFG P-DIGESTS  MODELPROV:MPROV= TFALSE
   s" a byte moved across the label boundary cannot alias" T-LABEL
   s" ab" s" c" P-LABELS  s" a" s" bc" P-LABELS  MODELPROV:MPROV= TFALSE ;

\ One digest cell differs and nothing else: the case a comparison narrowed to
\ three cells survives. FORGE-CELL is the documented UNMAKE/MAKE inverse (leg 7),
\ used here as the only available instrument for building such a pair. `which`
\ outside 0..3 changes nothing, which is the untouched round-trip control.
: FORGE-CELL ( MODELPROV:mprov n -- MODELPROV:mprov ) {: which:n :}
   MODELPROV-MPROV:UNMAKE {: tok:MODELPROV:prov-proof :}
   MODELPROV-PROVKEY:UNMAKE {: c0:n c1:n c2:n c3:n :}
   which 0 = if c0 1 xor else c0 then
   which 1 = if c1 1 xor else c1 then
   which 2 = if c2 1 xor else c2 then
   which 3 = if c3 1 xor else c3 then
   MODELPROV-PROVKEY:MAKE
   tok MODELPROV-MPROV:MAKE ;

: T-CELLWISE ( -- )
   s" pins differing in exactly ONE digest cell are unequal (all four cells)" T-LABEL
   4 0 ?do
      MAPPED-PIN  MAPPED-PIN i FORGE-CELL  MODELPROV:MPROV= TFALSE
   loop
   s" an untouched round trip through the same words stays equal" T-LABEL
   MAPPED-PIN  MAPPED-PIN 4 FORGE-CELL  MODELPROV:MPROV= TTRUE ;

\ ---- 4. named refusals, each boundary from both sides ---------------------------
: RJ-FAM-EMPTY ( -- )
   LBUF 0  REV P-LABELS drop ;
: RJ-REV-EMPTY ( -- )
   FAM  LBUF 0 P-LABELS drop ;
: RJ-FAM-LONG ( -- )
   LABEL-CAP 1 + FILL-LABEL  REV P-LABELS drop ;
: RJ-REV-LONG ( -- )
   FAM  LABEL-CAP 1 + FILL-LABEL P-LABELS drop ;
: RJ-FAM-SPACE ( -- )
   s" gpt2 small"  REV P-LABELS drop ;
: RJ-FAM-CTRL ( -- )
   TAB BYTE-LABEL  REV P-LABELS drop ;
: RJ-FAM-DEL ( -- )
   DEL BYTE-LABEL  REV P-LABELS drop ;
: RJ-FAM-LEAD-SPACE ( -- )
   SPACE LEAD-LABEL  REV P-LABELS drop ;
: RJ-FAM-LEAD-DEL ( -- )
   DEL LEAD-LABEL  REV P-LABELS drop ;
: RJ-CFG-SHORT ( -- )
   s" 00112233445566778899aabbccddeeff00112233445566778899aabbccddeef" TKZ
   P-DIGESTS drop ;
: RJ-CFG-LONG ( -- )
   s" 00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff0" TKZ
   P-DIGESTS drop ;
: RJ-CFG-UPPER ( -- )
   CFG 8 UPPER-A DG-WITH TKZ P-DIGESTS drop ;
: RJ-CFG-SLASH ( -- )
   CFG 0 SLASH DG-WITH TKZ P-DIGESTS drop ;
: RJ-CFG-COLON ( -- )
   CFG 5 COLON DG-WITH TKZ P-DIGESTS drop ;
: RJ-CFG-BACKTICK ( -- )
   CFG 9 BACKTICK DG-WITH TKZ P-DIGESTS drop ;
: RJ-CFG-G ( -- )
   CFG 1 LOW-G DG-WITH TKZ P-DIGESTS drop ;
: RJ-CFG-LAST ( -- )                            \ only the 64th character is bad
   CFG HEX-LEN 1 - SLASH DG-WITH TKZ P-DIGESTS drop ;
: RJ-TKZ-LAST ( -- )
   CFG  TKZ HEX-LEN 1 - LOW-G DG-WITH  P-DIGESTS drop ;
: RJ-WANT-LAST ( -- )
   ART-PATH  ART-DG HEX-LEN 1 - COLON DG-WITH  P-ART drop ;
: RJ-ART-MISSING ( -- )
   GONE-PATH ART-DG P-ART drop ;
: RJ-ART-WRONG ( -- )
   ALT-PATH ART-DG P-ART drop ;             \ the empty file cannot be "abc"
: RJ-ART-SWAPPED ( -- )
   ART-PATH ALT-DG P-ART drop ;             \ nor can "abc" be the empty file
: RJ-KEY-BUF ( -- )
   MAPPED-PIN HEX HEX-LEN 1 - MODELPROV:KEY-HEX drop ;

: T-LABEL-REJECTS ( -- )
   s" every malformed label is a named refusal" T-LABEL
   [: RJ-FAM-EMPTY ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-REV-EMPTY ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-LONG ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-REV-LONG ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-SPACE ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-CTRL ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-DEL ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-LEAD-SPACE ;] MODELPROV:E-PROV-TEXT TTHROWSQ
   [: RJ-FAM-LEAD-DEL ;] MODELPROV:E-PROV-TEXT TTHROWSQ ;

: T-DIGEST-REJECTS ( -- )
   s" every malformed digest is a named refusal, including only-the-64th" T-LABEL
   [: RJ-CFG-SHORT ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-LONG ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-UPPER ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-SLASH ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-COLON ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-BACKTICK ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-G ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-CFG-LAST ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-TKZ-LAST ;] MODELPROV:E-PROV-HEX TTHROWSQ
   [: RJ-WANT-LAST ;] MODELPROV:E-PROV-HEX TTHROWSQ ;

: T-REJECTS ( -- )
   T-LABEL-REJECTS
   T-DIGEST-REJECTS
   s" the artifact itself is verified, not taken on trust" T-LABEL
   [: RJ-ART-MISSING ;] MODELPROV:E-PROV-ARTIFACT TTHROWSQ
   [: RJ-ART-WRONG ;] MODELPROV:E-PROV-DIGEST TTHROWSQ
   [: RJ-ART-SWAPPED ;] MODELPROV:E-PROV-DIGEST TTHROWSQ
   s" a rendering buffer that cannot hold the key raises" T-LABEL
   [: RJ-KEY-BUF ;] MODELPROV:E-PROV-BUF TTHROWSQ
   s" the accepting side of every boundary still accepts" T-LABEL
   LABEL-CAP FILL-LABEL REV P-LABELS drop
   TILDE-CAP REV P-LABELS drop
   TILDE BYTE-LABEL REV P-LABELS drop
   s" a refused pin leaves no residue behind" T-LABEL
   MAPPED-PIN HEX-OF GOLD-M T$= ;

\ ---- 5. the artifact path, the one input handed to a core primitive ------------
: RJ-PATH-OVER ( -- )
   OVER-PATH ART-DG P-ART drop ;
: RJ-PATH-NEG ( -- )
   NEG-LEN >PLEN {: bad:CAD-NUM:byte-len :}
   ART-P bad ART-DG P-ART drop ;
: RJ-PATH-BIG-NEG ( -- )
   BIG-NEG-LEN >PLEN {: bad:CAD-NUM:byte-len :}
   ART-P bad ART-DG P-ART drop ;

: T-PATH ( -- )
   s" a path at the core cap is accepted and pins the same artifact set" T-LABEL
   LONG-PU @ PATH-AT-CAP T=
   MAPPED-PIN  LONG-PATH ART-DG P-ART  MODELPROV:MPROV= TTRUE
   s" one byte over the cap is a named refusal, not a process exit" T-LABEL
   OVER-PU @ PATH-OVER-CAP T=
   [: RJ-PATH-OVER ;] E-PATH-CORE TTHROWSQ
   s" no negative length becomes a byte-length role, so PIN is unreachable" T-LABEL
   NEG-LEN ROLE-REFUSES? TTRUE
   BIG-NEG-LEN ROLE-REFUSES? TTRUE
   PATH-AT-CAP ROLE-REFUSES? TFALSE
   [: RJ-PATH-NEG ;] E-MP-LEN TTHROWSQ
   [: RJ-PATH-BIG-NEG ;] E-MP-LEN TTHROWSQ ;

\ ---- 6. checker negatives -------------------------------------------------------
: T-SURFACE ( -- )
   s" the public surface resolves (controls)" T-LABEL
   s" MPP-MAKE ( MODELPROV:provkey MODELPROV:prov-proof -- MODELPROV:mprov ) MODELPROV-MPROV:MAKE"
      ACCEPTED
   s" MPP-PROVKEY ( n n n n -- MODELPROV:provkey ) MODELPROV-PROVKEY:MAKE" ACCEPTED
   s" MPP-PIN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n MODEL:adapter WSTORE:residency MODELPROV:packing -- MODELPROV:mprov ) MODELPROV:PIN"
      ACCEPTED
   s" MPP-EQ ( MODELPROV:mprov MODELPROV:mprov -- bool ) MODELPROV:MPROV=" ACCEPTED
   s" MPP-HEX ( MODELPROV:mprov ptr u8 n -- MODELPROV:mprov ) MODELPROV:KEY-HEX" ACCEPTED
   s" MPP-PACK ( -- MODELPROV:packing ) MODELPROV-PACKING:AS-STORED" ACCEPTED
   s" MPP-SCHEMA ( -- n ) MODELPROV:SCHEMA" ACCEPTED
   s" a raw cell cannot reach the path-length slot of the core path primitive" T-LABEL
   s" MPN-PLEN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n MODEL:adapter WSTORE:residency MODELPROV:packing -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" MPN-PLEN2 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-off ptr u8 n MODEL:adapter WSTORE:residency MODELPROV:packing -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" a pin cannot be forged from cells, and the proof cannot be minted outside" T-LABEL
   s" MPN-RAW ( MODELPROV:provkey n -- MODELPROV:mprov ) MODELPROV-MPROV:MAKE" REJECTED
   s" MPN-CELLS ( n n n n MODELPROV:prov-proof -- MODELPROV:mprov ) MODELPROV-MPROV:MAKE"
      REJECTED
   s" MPN-MINT ( -- MODELPROV:prov-proof ) MODELPROV:MINT-PROV-PROOF" UNRESOLVED
   s" MPN-MINT2 ( -- MODELPROV:prov-proof ) MINT-PROV-PROOF" UNRESOLVED
   s" the configuration and artifact identity domains do not collapse" T-LABEL
   s" MPN-CROSS ( MODELPROV:provkey MDLCFG:cfg-proof -- MODELPROV:mprov ) MODELPROV-MPROV:MAKE"
      REJECTED
   s" MPN-KEYSWAP ( MDLCFG:cfgkey MDLCFG:cfgkey -- bool ) MODELPROV:MPROV=" REJECTED
   s" MPN-KEYSWAP2 ( MODELPROV:mprov MODELPROV:mprov -- bool ) MDLCFG:CFGKEY=" REJECTED
   s" MPN-PKSWAP ( MDLCFG:cfgkey MDLCFG:cfgkey -- bool ) MODELPROV-PROVKEY:EQ" REJECTED
   s" MPN-KEYRAW ( n n -- bool ) MODELPROV:MPROV=" REJECTED
   s" the three typed convention values cannot be permuted" T-LABEL
   s" MPN-P1 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n MODEL:adapter MODELPROV:packing WSTORE:residency -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" MPN-P2 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n WSTORE:residency WSTORE:residency MODELPROV:packing -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" MPN-P3 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n MODEL:family WSTORE:residency MODELPROV:packing -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" MPN-P4 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 CAD-NUM:byte-len ptr u8 n MODEL:adapter WSTORE:residency n -- MODELPROV:mprov ) MODELPROV:PIN"
      REJECTED
   s" the validation, fold, and projection helpers stay behind the seal" T-LABEL
   s" MPN-LABEL ( ptr u8 n -- ) MODELPROV:LABEL-OK" UNRESOLVED
   s" MPN-HEXOK ( ptr u8 n -- ) MODELPROV:HEX-OK" UNRESOLVED
   s" MPN-VERIFY ( ptr u8 CAD-NUM:byte-len ptr u8 n -- ) MODELPROV:VERIFY-ARTIFACT" UNRESOLVED
   s" MPN-FOLDT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) MODELPROV:FOLD-TEXTS" UNRESOLVED
   s" MPN-FOLDG ( MODEL:adapter WSTORE:residency MODELPROV:packing -- ) MODELPROV:FOLD-TAGS"
      UNRESOLVED
   s" MPN-FOLDN ( n -- ) MODELPROV:FOLD-N" UNRESOLVED
   s" MPN-FINAL ( -- MODELPROV:mprov ) MODELPROV:KEY-FINAL" UNRESOLVED
   s" MPN-CELLSP ( MODELPROV:mprov -- n n n n ) MODELPROV:MP-CELLS" UNRESOLVED
   s" MPN-MPKEY ( MODELPROV:mprov -- MODELPROV:provkey ) MODELPROV:MP-KEY" UNRESOLVED
   s" MPN-PLENERASE ( CAD-NUM:byte-len -- n ) MODELPROV:PLEN>N" UNRESOLVED
   s" MODELPROV publishes no text-to-pin parser and no version setter" T-LABEL
   s" MPN-PARSE ( ptr u8 n -- MODELPROV:mprov ) MODELPROV:PIN-FROM-HEX" UNRESOLVED
   s" MPN-SCHEMASET ( n -- ) MODELPROV:SCHEMA!" UNRESOLVED ;

\ ---- 7. the known forgery gap, pinned as it behaves today -----------------------
\ The module header claims KEY-HEX has no inverse INSIDE this package. That is
\ true, and it is not unforgeability. Everything below is ordinary checked code in
\ another package using only PUBLIC generated words, and it works today.
: HEXBYTE@ ( ptr u8 n -- n ) {: a:ptr b:n :}
   a b 2 * BYTE+ c@ CONTENT-KEY:HEX-NIB 4 lshift
   a b 2 * 1 + BYTE+ c@ CONTENT-KEY:HEX-NIB or ;

: HEXCELL@ ( ptr u8 n -- n ) {: a:ptr j:n :}
   0 CELL-BYTES 0 ?do
      a  j CELL-BYTES * i +  HEXBYTE@  i 8 * lshift  or
   loop ;

\ A donor pin (for its proof token) plus stored hexadecimal text yields a pin
\ carrying the digest that text encodes. No trusted word, no package reopen.
: FORGE-FROM-HEX ( MODELPROV:mprov ptr u8 n -- MODELPROV:mprov ) {: h:ptr hu:n :}
   MODELPROV-MPROV:UNMAKE {: tok:MODELPROV:prov-proof :}
   drop                                        \ discard the donor's own digest
   h 0 HEXCELL@  h 1 HEXCELL@  h 2 HEXCELL@  h 3 HEXCELL@
   MODELPROV-PROVKEY:MAKE
   tok MODELPROV-MPROV:MAKE ;

: T-KNOWN-GAP ( -- )
   s" KNOWN GAP: the generated UNMAKE is reachable outside the package TODAY" T-LABEL
   s" MPX-UNMAKE ( MODELPROV:mprov -- MODELPROV:provkey MODELPROV:prov-proof ) MODELPROV-MPROV:UNMAKE"
      ACCEPTED
   s" MPX-REMAKE ( MODELPROV:provkey MODELPROV:prov-proof -- MODELPROV:mprov ) MODELPROV-MPROV:MAKE"
      ACCEPTED
   s" KNOWN GAP: one donor pin plus stored text reconstructs an equal pin" T-LABEL
   MAPPED-PIN  ALLOC-PIN GOLD-M FORGE-FROM-HEX  MODELPROV:MPROV= TTRUE
   s" so a pin proves integrity against accident, never possession; when the" T-LABEL
   s" sealed-destructure capability lands this leg fails and the caveat retires" T-LABEL
   MODELPROV:SCHEMA SCHEMA-WANT T= ;

\ ---- 8. the seal ----------------------------------------------------------------
: T-SEALED ( -- )
   s" the package seal refuses new definitions into MODELPROV" T-LABEL
   s" package MODELPROV : MP-FORGE ( -- MODELPROV:prov-proof ) MINT-PROV-PROOF ; ;package"
   SUBJ-OUT $400 >LEN SUBJ-ERR $400 >LEN 2000 >MS SUBJECT:RUN
   ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED=
   LEN>N drop
   LEN>N drop ;

public

\ Runs AFTER ;package (the weight-store-test arrangement): the SUBJECT child
\ forks from the running process, so no package may be open when T-SEALED
\ evaluates `package MODELPROV` in the child.
: RUN ( -- )
   T-RESET
   WRITE-FIXTURES
   T-MINT
   T-PREIMAGE
   T-SENSITIVE
   T-CELLWISE
   T-REJECTS
   T-PATH
   T-SURFACE
   T-KNOWN-GAP
   T-SEALED
   CLEANUP
   T-REPORT ;

;package

MODELPROV-TEST:RUN
