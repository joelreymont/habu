\ safetensors-test.f - focused coverage for the safetensors load-session loader
\ (maki/infer/safetensors.f, epic habu-epic-gb10-uma-391d12e8). Run standalone:
\ bin/hb --load maki/infer/safetensors-test.f
\
\ Fixtures are BUILT in Forth. The valid leg writes a tiny synthetic safetensors
\ file to disk and loads it back through the real mmap path; every malformed leg
\ builds an in-memory image and asserts the loader rejects it with its NAMED
\ SAFET:E-* code. JSON is written with backtick placeholders for the double
\ quotes (an `s"` literal cannot contain `"`); BUILD rewrites ` -> " while
\ assembling the header. The capacity leg generates a header with one more
\ tensor than the loader accepts, so the rollback path is exercised for real.
\
\ The ownership half of the suite is what makes this loader different from the
\ process-global registry it replaced: two sessions are parsed INTERLEAVED and
\ each census keeps answering about its own file, a failing session leaves a
\ live census byte-identical, and the linear session/census/mapping tokens are
\ proved un-duplicable, un-droppable, and un-reusable by feeding bad definitions
\ to the checker itself. The presence-gated real leg parses HuggingFace GPT-2
\ (openai-community/gpt2) and asserts the tensor census.
\
\ The mapping half adds the other question this loader has to answer: after
\ DETACH-MAPPING hands the file mapping to its own owner, does the census still
\ describe its tensors and refuse to hand out their bytes? Those legs assert
\ both directions on the same census, in both disposal orders, and read the same
\ tensor bytes back through the mapping at the MAP-OFFSET? frame to prove the
\ two offset frames are not interchangeable.

require lib/test.f
require test/checker-assert.f
require maki/infer/safetensors.f

package SAFET-TEST

34 constant DQ                                  \ "
96 constant BT                                  \ ` placeholder for " in s" literals
44 constant COMMA
48 constant ZERO-CH
123 constant LBRACE
125 constant RBRACE
10 constant TEN

1024 constant IMG-CAP
create IMG-A IMG-CAP allot   variable LEN-A
create IMG-B IMG-CAP allot   variable LEN-B
create NAME-BUF 64 allot
create DATA-BUF 64 allot

variable SEEN-LEN
variable SEEN-B0

\ ---- what the whole-mapping body saw ---------------------------------------
144 constant SYNTH-HDR                          \ J-VALID's header JSON byte length
create MAP-BUF 64 allot
variable MAP-OFF                                \ mapping-base offset the body reads from
variable MAP-TAKE                               \ how many bytes it copies out
variable SEEN-MAP-LEN

\ ---- image builder ---------------------------------------------------------
\ dst dstcap json$ datacount -> image byte length. Layout: 8-byte little-endian
\ header length, the backtick-rewritten header JSON, then `dcount` data bytes
\ whose value is their index.
: BUILD ( ptr u8 n ptr u8 n n -- n )
   {: da:ptr dcap:n ja:ptr ju:n dcount:n :}
   8 ju + dcount + dcap > if E-STR-CAPACITY throw then
   ju 0 ?do
      ja i + c@ dup BT = if drop DQ then
      da 8 i + + c!
   loop
   8 0 ?do  ju i 8 * rshift $FF and  da i +  c!  loop
   dcount 0 ?do  i $FF and  da 8 ju + i + +  c!  loop
   8 ju + dcount + ;

: BUILD-A ( ptr u8 n n -- ) {: ja:ptr ju:n dcount:n :}
   IMG-A IMG-CAP ja ju dcount BUILD LEN-A ! ;

: BUILD-B ( ptr u8 n n -- ) {: ja:ptr ju:n dcount:n :}
   IMG-B IMG-CAP ja ju dcount BUILD LEN-B ! ;

: A$ ( -- ptr u8 n )  IMG-A LEN-A @ ;
: B$ ( -- ptr u8 n )  IMG-B LEN-B @ ;

\ ---- option assertions -----------------------------------------------------
\ A NONE where a value was required is a test FAILURE reported at the lookup, not
\ a -1 mapped into the comparison: a sentinel here could silently coincide with
\ an expected value and hide the regression.
: MISSING ( -- )
   s" required option was NONE" T-LABEL
   0 0= 0= TTRUE ;

: OPT= ( option<n> n -- )                       \ assert SOME(want)
   {: want:n :}
   MATCH option
      none OF MISSING ENDOF
      some OF want T= ENDOF
   ;MATCH ;

: OPT-NONE ( option<n> -- )                     \ assert NONE
   MATCH option
      none OF 0 ENDOF
      some OF drop 1 ENDOF
   ;MATCH 0 T= ;

: OPT-VAL ( option<n> -- n )                    \ the value; a NONE fails the assertion here
   MATCH option
      none OF MISSING -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ UNMAP-MAPPING answers a result, not a throw, so an `err` where the fixture
\ required a clean cleanup is reported at the call, exactly like MISSING above.
\ The err arm itself is a failing munmap on a well-formed mapping, which the
\ public surface cannot produce on purpose; what IS proved statically below is
\ that no caller can drop the union or read it as a bare number.
: CLEANUP-ERR ( n -- )
   drop
   s" cleanup result was err, not ok" T-LABEL
   0 0= 0= TTRUE ;

: RES-OK= ( result<n,n> n -- )                  \ assert ok(want)
   {: want:n :}
   MATCH result
      ok  OF want T= ENDOF
      err OF CLEANUP-ERR ENDOF
   ;MATCH ;

: ID-OF ( SAFET:census ptr u8 n -- SAFET:census n )
   SAFET:FIND OPT-VAL ;

\ CHECK-QUIET-CANDIDATE! verdicts: -1 accepted, 0 rejected on a type error,
\ 1 uncheckable (a token the dictionary cannot resolve at all). The three
\ assertions below keep those apart, so "private" is proved by non-resolution
\ and "illegal" by a real type error - and ACCEPTED positive controls prove the
\ harness does resolve this package's public names.
: NO-LEAK ( -- )                                \ nothing mapped and no owner left over
   SAFET-MAP:LIVE 0 T=
   SAFET:LIVE-OWNERS 0 T= ;

: REJECTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: UNRESOLVED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 1 T= ;

: ACCEPTED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- fixture headers -------------------------------------------------------
: J-VALID ( -- ptr u8 n )                       \ F32 [2,2] + BF16 [4], __metadata__ skipped
   s" {`__metadata__`:{`format`:`pt`},`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`BF16`,`shape`:[4],`data_offsets`:[16,24]}}" ;

: J-ALPHA ( -- ptr u8 n )                       \ model A: one F32 [2,2] named alpha
   s" {`alpha`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]}}" ;

: J-BETA ( -- ptr u8 n )                        \ model B: one BF16 [4] named beta
   s" {`beta`:{`dtype`:`BF16`,`shape`:[4],`data_offsets`:[0,8]}}" ;

: J-BADJSON ( -- ptr u8 n )   s" { bad" ;

\ ---- malformed images: each rejected with its named code -------------------
: R-TRUNC ( -- )                                \ image shorter than the 8-byte length
   IMG-A 4 SAFET:LOAD-SPAN SAFET:RELEASE ;

: R-HDRBIG ( -- )                               \ header length runs past the image
   8 0 ?do 1000 i 8 * rshift $FF and IMG-A i + c! loop
   IMG-A 8 SAFET:LOAD-SPAN SAFET:RELEASE ;

: R-SPAN ( ptr u8 n n -- )                      \ build then load, expecting a throw
   BUILD-A A$ SAFET:LOAD-SPAN SAFET:RELEASE ;

: R-BADJSON ( -- )    J-BADJSON 0 R-SPAN ;
: R-VALNOTOBJ ( -- )  s" {`a`:5}" 0 R-SPAN ;
: R-OOR ( -- )        s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,64]}}" 16 R-SPAN ;
\ begin after end. The span is in range and correctly aligned, and its length
\ would only look wrong once the size check ran, so this leg is what pins the
\ begin>end test to E-OFFSETS instead of letting it fall through to E-SHAPE.
: R-REVERSED ( -- )   s" {`a`:{`dtype`:`F32`,`shape`:[1],`data_offsets`:[8,4]}}" 12 R-SPAN ;
: R-OVERLAP ( -- )    s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,16]},`b`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[8,24]}}" 24 R-SPAN ;
: R-SIZE ( -- )       s" {`a`:{`dtype`:`F32`,`shape`:[2,2],`data_offsets`:[0,8]}}" 8 R-SPAN ;
: R-MISALIGN ( -- )   s" {`a`:{`dtype`:`BF16`,`shape`:[1],`data_offsets`:[0,2]},`b`:{`dtype`:`F32`,`shape`:[1],`data_offsets`:[2,6]}}" 6 R-SPAN ;
: R-BADDTYPE ( -- )   s" {`a`:{`dtype`:`F99`,`shape`:[1],`data_offsets`:[0,4]}}" 4 R-SPAN ;
: R-MISSING ( -- )    s" {`a`:{`dtype`:`F32`,`data_offsets`:[0,4]}}" 4 R-SPAN ;
: R-NEGDIM ( -- )     s" {`a`:{`dtype`:`F32`,`shape`:[-1],`data_offsets`:[0,4]}}" 4 R-SPAN ;

: R-SHAPE-OVERFLOW ( -- )
   s" {`a`:{`dtype`:`F32`,`shape`:[4294967296,2147483648],`data_offsets`:[0,0]}}" 0 R-SPAN ;

: R-BYTES-OVERFLOW ( -- )
   s" {`a`:{`dtype`:`F32`,`shape`:[2305843009213693952],`data_offsets`:[0,0]}}" 0 R-SPAN ;

: TEST-MALFORMED ( -- )
   s" every malformed header throws its named code" T-LABEL
   [: R-TRUNC ;]          SAFET:E-HEADER  TTHROWSQ
   [: R-HDRBIG ;]         SAFET:E-HEADER  TTHROWSQ
   [: R-BADJSON ;]        SAFET:E-JSON    TTHROWSQ
   [: R-VALNOTOBJ ;]      SAFET:E-JSON    TTHROWSQ
   [: R-OOR ;]            SAFET:E-OFFSETS TTHROWSQ
   [: R-REVERSED ;]       SAFET:E-OFFSETS TTHROWSQ
   [: R-OVERLAP ;]        SAFET:E-OFFSETS TTHROWSQ
   [: R-SIZE ;]           SAFET:E-SHAPE   TTHROWSQ
   [: R-MISALIGN ;]       SAFET:E-ALIGN   TTHROWSQ
   [: R-BADDTYPE ;]       SAFET:E-DTYPE   TTHROWSQ
   [: R-MISSING ;]        SAFET:E-FIELD   TTHROWSQ
   [: R-NEGDIM ;]         SAFET:E-SHAPE   TTHROWSQ
   [: R-SHAPE-OVERFLOW ;] SAFET:E-SHAPE   TTHROWSQ
   [: R-BYTES-OVERFLOW ;] SAFET:E-SHAPE   TTHROWSQ ;

\ ---- the valid synthetic file, loaded through the real mmap path -----------
: SYNTH-PATH ( -- ptr u8 n )  s" /tmp/hb-st-synth.safetensors" ;

: CLEANUP ( -- )  SYNTH-PATH FS-PATHZ unlink drop ;

: SEE-TENSOR ( SAFET:census ptr u8 n -- SAFET:census )   \ scoped zero-copy body
   {: a:ptr u:n :}
   u SEEN-LEN !
   a c@ SEEN-B0 ! ;

: CHECK-VALID ( SAFET:census -- SAFET:census )
   SAFET:COUNT 2 T=                             \ __metadata__ not counted
   s" a" ID-OF {: ia:n :}
   ia 0 < 0= TTRUE
   ia SAFET:RANK? 2 OPT=
   ia 0 SAFET:DIM? 2 OPT=   ia 1 SAFET:DIM? 2 OPT=
   ia 2 SAFET:DIM? OPT-NONE                     \ axis past the rank
   ia SAFET:DTYPE? SAFET:DT-F32 OPT=
   ia SAFET:NBYTES? 16 OPT=
   ia SAFET:BEGIN? 0 OPT=
   ia SAFET:END? 16 OPT=
   s" b" ID-OF {: ib:n :}
   ib 0 < 0= TTRUE
   ib SAFET:DTYPE? SAFET:DT-BF16 OPT=
   ib SAFET:RANK? 1 OPT=
   ib 0 SAFET:DIM? 4 OPT=
   ib SAFET:NBYTES? 8 OPT=
   ib SAFET:BEGIN? 16 OPT=
   s" absent" SAFET:FIND OPT-NONE               \ unknown name -> NONE, never -1
   99 SAFET:DTYPE? OPT-NONE                     \ id past the census -> NONE
   -1 SAFET:NBYTES? OPT-NONE
   ia [: SEE-TENSOR ;] SAFET:WITH-TENSOR 16 OPT=
   SEEN-LEN @ 16 T=  SEEN-B0 @ 0 T=             \ a's data starts at data byte 0
   ib [: SEE-TENSOR ;] SAFET:WITH-TENSOR 8 OPT=
   SEEN-LEN @ 8 T=   SEEN-B0 @ 16 T=            \ b's data starts at data byte 16
   99 [: SEE-TENSOR ;] SAFET:WITH-TENSOR OPT-NONE
   ib NAME-BUF 64 SAFET:COPY-NAME? 1 OPT=
   NAME-BUF c@ 98 T=                            \ "b"
   ib NAME-BUF 0 SAFET:COPY-NAME? OPT-NONE      \ destination too small
   ib DATA-BUF 64 SAFET:COPY-DATA? 8 OPT=       \ clamped to the tensor length
   DATA-BUF c@ 16 T=
   ia DATA-BUF 4 SAFET:COPY-DATA? 4 OPT=        \ clamped to the destination
   SAFET:MAP-LEN LEN-A @ T= ;

\ LOAD's own fault paths. These enter through the real file entry point, not
\ LOAD-SPAN: taking the image is where a fresh session can still throw, so a
\ mistake here strands the session block with nothing left to close it. Every
\ leg asserts the named code AND that the failure released everything.
: SHORT-PATH ( -- ptr u8 n )  s" /tmp/hb-st-short.safetensors" ;
: ABSENT-PATH ( -- ptr u8 n ) s" /tmp/hb-st-does-not-exist.safetensors" ;

: LOAD-ABSENT ( -- )
   ABSENT-PATH SAFET:LOAD SAFET:RELEASE ;

: LOAD-SHORT ( -- )                             \ a real file with only 4 bytes in it
   0 IMG-A 0 + c!  1 IMG-A 1 + c!  2 IMG-A 2 + c!  3 IMG-A 3 + c!
   SHORT-PATH IMG-A 4 WRITE-ALL
   SHORT-PATH SAFET:LOAD SAFET:RELEASE ;

: TEST-LOAD-FAULTS ( -- )
   s" LOAD on a missing path throws and closes its session" T-LABEL
   [: LOAD-ABSENT ;] E-FS-STAT TTHROWSQ
   NO-LEAK
   s" LOAD on a file under 8 bytes throws and closes its session" T-LABEL
   [: LOAD-SHORT ;] SAFET:E-HEADER TTHROWSQ
   NO-LEAK
   SHORT-PATH FS-PATHZ unlink drop ;

: TEST-VALID-FILE ( -- )
   s" a synthetic file round-trips through the real mmap path" T-LABEL
   J-VALID 24 BUILD-A
   SYNTH-PATH A$ WRITE-ALL
   NO-LEAK
   SYNTH-PATH SAFET:LOAD
   SAFET-MAP:LIVE 1 T=                          \ the census holds exactly one mapping
   CHECK-VALID
   SAFET:RELEASE
   NO-LEAK
   CLEANUP ;

\ ---- interleaved sessions --------------------------------------------------
: CHECK-ALPHA ( SAFET:census -- SAFET:census )
   SAFET:COUNT 1 T=
   s" alpha" ID-OF {: ia:n :}
   ia 0 < 0= TTRUE
   ia SAFET:DTYPE? SAFET:DT-F32 OPT=
   ia SAFET:NBYTES? 16 OPT=
   ia SAFET:RANK? 2 OPT=
   ia 0 SAFET:DIM? 2 OPT=
   s" beta" SAFET:FIND OPT-NONE ;

: CHECK-BETA ( SAFET:census -- SAFET:census )
   SAFET:COUNT 1 T=
   s" beta" ID-OF {: ib:n :}
   ib 0 < 0= TTRUE
   ib SAFET:DTYPE? SAFET:DT-BF16 OPT=
   ib SAFET:NBYTES? 8 OPT=
   ib SAFET:RANK? 1 OPT=
   ib 0 SAFET:DIM? 4 OPT=
   s" alpha" SAFET:FIND OPT-NONE ;

: BUILD-PAIR ( -- )
   J-ALPHA 16 BUILD-A
   J-BETA 8 BUILD-B ;

: TEST-INTERLEAVED ( -- )
   s" two sessions parse interleaved without cross-talk" T-LABEL
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT                    \ ( sA )
   SAFET:OPEN B$ SAFET:ADOPT                    \ ( sA sB )
   SAFET:PARSE                                  \ B parses first
   swap SAFET:PARSE                             \ ( sB sA ) then A
   SAFET:DETACH                                 \ ( sB cA )
   swap SAFET:DETACH                            \ ( cA cB )
   CHECK-BETA
   swap CHECK-ALPHA                             \ ( cB cA )
   swap CHECK-BETA                              \ ( cA cB ) still B's data
   SAFET:RELEASE                                \ ( cA )
   CHECK-ALPHA                                  \ A survives B's release
   SAFET:RELEASE ;

\ ---- a failing session cannot disturb a live census ------------------------
: TEST-FAILED-ISOLATION ( -- )
   s" a failed session leaves a live census byte-identical" T-LABEL
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT SAFET:PARSE SAFET:DETACH     \ ( cA )
   CHECK-ALPHA
   J-BADJSON 0 BUILD-B
   SAFET:OPEN B$ SAFET:ADOPT                              \ ( cA sB )
   [: SAFET:PARSE ;] catch {: code:n :}
   code SAFET:E-JSON T=
   SAFET:CLOSE                                            \ close after failure
   CHECK-ALPHA                                            \ unchanged
   SAFET:RELEASE ;

: ABSORB-PARSE ( SAFET:session -- SAFET:session )   \ an inner catch that swallows the fault
   [: SAFET:PARSE ;] catch drop ;

: TEST-NESTED-CATCH ( -- )
   s" a caught parse failure nests inside another catch" T-LABEL
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT SAFET:PARSE SAFET:DETACH      \ ( cA )
   J-BADJSON 0 BUILD-B
   SAFET:OPEN B$ SAFET:ADOPT                               \ ( cA sB )
   [: ABSORB-PARSE ;] catch {: nested:n :}
   nested 0 T=                                             \ the inner catch absorbed it
   SAFET:CLOSE
   CHECK-ALPHA
   SAFET:RELEASE ;

\ ---- ordering rules --------------------------------------------------------
\ Each attempt runs in a STACK-PRESERVING quotation, so the caught throw leaves
\ the session still owned and the fixture can CLOSE it. That matters here: a
\ linear owner abandoned by a throw is invisible to the checker, and the
\ NO-LEAK assertion below would see it.
: TRY-DETACH ( SAFET:session -- SAFET:session )
   \ DETACH must throw. If the guard is ever lost and it succeeds, the census is
   \ released and a fresh session replaces it so the fixture stays balanced - the
   \ E-ORDER assertion is what reports the regression.
   SAFET:DETACH SAFET:RELEASE SAFET:OPEN ;

: BAD-DETACH-UNPARSED ( -- )
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT
   [: TRY-DETACH ;] catch {: code:n :}
   SAFET:CLOSE
   code 0 <> if code throw then ;

: BAD-PARSE-NO-IMAGE ( -- )
   SAFET:OPEN
   [: SAFET:PARSE ;] catch {: code:n :}
   SAFET:CLOSE
   code 0 <> if code throw then ;

: BAD-IMAGE-TWICE ( -- )
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT
   [: B$ SAFET:ADOPT ;] catch {: code:n :}
   SAFET:CLOSE
   code 0 <> if code throw then ;

\ A second MAP-FILE must be refused BEFORE the file is mapped, otherwise the
\ rejected mapping would be stranded with no owner to unmap it - which is
\ exactly what SAFET-MAP:LIVE reports.
: BAD-MAP-TWICE ( -- )
   J-VALID 24 BUILD-A
   SYNTH-PATH A$ WRITE-ALL
   SAFET:OPEN SYNTH-PATH SAFET:MAP-FILE
   [: SYNTH-PATH SAFET:MAP-FILE ;] catch {: code:n :}
   SAFET:CLOSE
   code 0 <> if code throw then ;

: TEST-ORDER ( -- )
   s" steps taken out of order throw E-ORDER" T-LABEL
   [: BAD-DETACH-UNPARSED ;] SAFET:E-ORDER TTHROWSQ
   [: BAD-PARSE-NO-IMAGE ;]  SAFET:E-ORDER TTHROWSQ
   [: BAD-IMAGE-TWICE ;]     SAFET:E-ORDER TTHROWSQ
   [: BAD-MAP-TWICE ;]       SAFET:E-ORDER TTHROWSQ
   s" a refused step strands neither a mapping nor an owner" T-LABEL
   NO-LEAK
   CLEANUP ;

\ ---- capacity rollback -----------------------------------------------------
\ One tensor more than the loader accepts. Each entry is
\ "tN":{"dtype":"F32","shape":[0],"data_offsets":[0,0]} - zero-length tensors so
\ the image stays header-only and no pair of spans overlaps.
$30000 constant BIG-CAP
create BIG BIG-CAP allot
variable BIG-LEN

: BIG-C ( n -- )
   BIG-LEN @ BIG-CAP >= if E-STR-CAPACITY throw then
   BIG BIG-LEN @ + c!
   BIG-LEN @ 1+ BIG-LEN ! ;

: BIG-S ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do a i + c@ BIG-C loop ;

: BIG-Q$ ( ptr u8 n -- )                        \ "text"
   DQ BIG-C BIG-S DQ BIG-C ;

: BIG-DIGITS ( n -- )
   dup TEN >= if dup TEN / recurse then
   TEN mod ZERO-CH + BIG-C ;

: BIG-TENSOR ( n -- )
   DQ BIG-C s" t" BIG-S BIG-DIGITS DQ BIG-C
   s" :{" BIG-S
   s" dtype" BIG-Q$ s" :" BIG-S s" F32" BIG-Q$ s" ," BIG-S
   s" shape" BIG-Q$ s" :[0]," BIG-S
   s" data_offsets" BIG-Q$ s" :[0,0]}" BIG-S ;

: BIG-HEADER ( n -- ) {: cnt:n :}
   8 BIG-LEN !                                  \ reserve the length slot
   LBRACE BIG-C
   cnt 0 ?do
      i 0 > if COMMA BIG-C then
      i BIG-TENSOR
   loop
   RBRACE BIG-C
   BIG-LEN @ 8 - {: hlen:n :}
   8 0 ?do  hlen i 8 * rshift $FF and  BIG i +  c!  loop ;

: BAD-CAPACITY ( -- )
   SAFET:MAX-TENSORS 1+ BIG-HEADER
   BIG BIG-LEN @ SAFET:LOAD-SPAN SAFET:RELEASE ;

: TEST-CAPACITY ( -- )
   s" one tensor past the capacity rolls the whole load back" T-LABEL
   [: BAD-CAPACITY ;] SAFET:E-CAP TTHROWSQ
   s" a capacity failure leaves a fresh session usable" T-LABEL
   BUILD-PAIR
   SAFET:OPEN A$ SAFET:ADOPT SAFET:PARSE SAFET:DETACH
   CHECK-ALPHA
   SAFET:RELEASE
   s" the capacity limit itself parses" T-LABEL
   SAFET:MAX-TENSORS BIG-HEADER
   BIG BIG-LEN @ SAFET:LOAD-SPAN
   SAFET:COUNT SAFET:MAX-TENSORS T=
   SAFET:RELEASE ;

\ ---- two independent model lifetimes ---------------------------------------
: TEST-TWO-MODELS ( -- )
   s" two censuses live and are released in either order" T-LABEL
   BUILD-PAIR
   A$ SAFET:LOAD-SPAN                           \ ( cA )
   B$ SAFET:LOAD-SPAN                           \ ( cA cB )
   CHECK-BETA
   swap CHECK-ALPHA                             \ ( cB cA )
   SAFET:RELEASE                                \ release A first
   CHECK-BETA
   SAFET:RELEASE ;

\ ---- the mapping-owning and metadata-only disposal outcomes ----------------
: TEST-DISPOSAL ( -- )
   s" a mapped census unmaps on release; an adopted image is left alone" T-LABEL
   J-VALID 24 BUILD-A
   SYNTH-PATH A$ WRITE-ALL
   SYNTH-PATH SAFET:LOAD                        \ this census owns its mapping
   SAFET:COUNT 2 T=
   SAFET-MAP:LIVE 1 T=
   SAFET:RELEASE
   NO-LEAK                                      \ outcome one: the mapping was unmapped
   CLEANUP
   J-ALPHA 16 BUILD-A
   LEN-A @ {: before:n :}
   IMG-A c@ {: byte0:n :}
   A$ SAFET:LOAD-SPAN                           \ this census owns no mapping
   SAFET-MAP:LIVE 0 T=
   CHECK-ALPHA
   SAFET:RELEASE
   NO-LEAK                                      \ outcome two: metadata only, nothing unmapped
   LEN-A @ before T=                            \ the caller's image survives release
   IMG-A c@ byte0 T=
   A$ SAFET:LOAD-SPAN                           \ and is still loadable
   CHECK-ALPHA
   SAFET:RELEASE ;

\ ---- the mapping seam ------------------------------------------------------
\ The synthetic file's layout is known exactly: an 8-byte little-endian length
\ prefix, then SYNTH-HDR bytes of header JSON, then the data section. So tensor
\ "a" (data_offsets [0,16]) starts at mapping byte 8+144+0 = 152 and "b"
\ ([16,24]) at 8+144+16 = 168, while BEGIN? keeps reporting 0 and 16. Those two
\ frames are what this section pins: a MAP-OFFSET? that answered the
\ data-section frame would report 0 and 16 here and fail on the first assertion,
\ and the byte reads below show what reading a mapping at the wrong frame
\ actually lands on.
: SEE-MAPPING ( SAFET:mapping ptr u8 n -- SAFET:mapping )   \ scoped whole-mapping body
   {: a:ptr u:n :}
   u SEEN-MAP-LEN !
   a MAP-OFF @ BYTE+  MAP-BUF  MAP-TAKE @  BYTE-COPY ;

\ The byte a mapping holds at one mapping-base offset. Used only where the
\ mapping spans the whole IMG-A image, so WITH-MAPPING's reported length is
\ LEN-A and asserting it here also proves the body saw the entire mapping.
: MAP-BYTE-AT ( SAFET:mapping n -- SAFET:mapping n )
   {: off:n :}
   off MAP-OFF !  1 MAP-TAKE !
   $FF MAP-BUF c!                               \ poison: only the body may overwrite it
   [: SEE-MAPPING ;] SAFET:WITH-MAPPING LEN-A @ OPT=
   MAP-BUF c@ ;

: CHECK-MAP-OFFSETS ( SAFET:census -- SAFET:census )
   SAFET:HDR-LEN SYNTH-HDR T=
   s" a" ID-OF {: ia:n :}
   s" b" ID-OF {: ib:n :}
   ia SAFET:MAP-OFFSET? 152 OPT=
   ib SAFET:MAP-OFFSET? 168 OPT=
   ia SAFET:BEGIN? 0 OPT=                       \ the data-section frame, unchanged
   ib SAFET:BEGIN? 16 OPT=
   ia SAFET:MAP-OFFSET? OPT-VAL {: again:n :}   \ repeated calls agree
   again 152 T=
   ib SAFET:MAP-OFFSET? 168 OPT=
   99 SAFET:MAP-OFFSET? OPT-NONE                \ id past the census -> NONE, never an offset
   -1 SAFET:MAP-OFFSET? OPT-NONE ;

\ Everything a census must still answer once its mapping has left, and
\ everything it must refuse. The poison assertions prove the scoped body is not
\ run at all, rather than run over a stale address.
: CHECK-DETACHED-CENSUS ( SAFET:census -- SAFET:census )
   SAFET:COUNT 2 T=
   s" a" ID-OF {: ia:n :}
   s" b" ID-OF {: ib:n :}
   ia SAFET:DTYPE? SAFET:DT-F32 OPT=
   ib SAFET:DTYPE? SAFET:DT-BF16 OPT=
   ia SAFET:RANK? 2 OPT=
   ia 0 SAFET:DIM? 2 OPT=   ia 1 SAFET:DIM? 2 OPT=
   ia SAFET:NBYTES? 16 OPT=  ib SAFET:NBYTES? 8 OPT=
   ia SAFET:BEGIN? 0 OPT=   ib SAFET:END? 24 OPT=
   ia SAFET:MAP-OFFSET? 152 OPT=                \ the frame outlives the mapping's owner
   ib SAFET:MAP-OFFSET? 168 OPT=
   s" absent" SAFET:FIND OPT-NONE
   ib NAME-BUF 64 SAFET:COPY-NAME? 1 OPT=       \ names live in the census's own arena
   NAME-BUF c@ 98 T=
   SAFET:MAP-LEN 0 T=                           \ it records no image any more
   -1 SEEN-LEN !                                \ poison: no scoped body may run
   ia [: SEE-TENSOR ;] SAFET:WITH-TENSOR OPT-NONE
   ib [: SEE-TENSOR ;] SAFET:WITH-TENSOR OPT-NONE
   SEEN-LEN @ -1 T=
   $FF DATA-BUF c!
   ia DATA-BUF 64 SAFET:COPY-DATA? OPT-NONE
   ib DATA-BUF 64 SAFET:COPY-DATA? OPT-NONE
   DATA-BUF c@ $FF T= ;

: BUILD-SYNTH ( -- )
   J-VALID 24 BUILD-A
   SYNTH-PATH A$ WRITE-ALL ;

: TEST-MAP-OFFSET ( -- )
   s" the mapping frame matches the known synthetic layout" T-LABEL
   BUILD-SYNTH
   NO-LEAK
   SYNTH-PATH SAFET:LOAD
   SAFET-MAP:LIVE 1 T=
   SAFET:LIVE-OWNERS 1 T=
   CHECK-MAP-OFFSETS
   SAFET:RELEASE
   NO-LEAK
   CLEANUP ;

: TEST-DETACH-MAPPING ( -- )
   s" detaching a mapping moves ownership without mapping anything new" T-LABEL
   BUILD-SYNTH
   SYNTH-PATH SAFET:LOAD                        \ ( c )
   s" b" ID-OF {: ib:n :}
   ib DATA-BUF 64 SAFET:COPY-DATA? 8 OPT=       \ b's bytes while the census still owns them
   DATA-BUF c@ {: want-b0:n :}
   SAFET:DETACH-MAPPING                         \ ( c m )
   SAFET-MAP:LIVE 1 T=                          \ the kernel mapping count does not move
   SAFET:LIVE-OWNERS 2 T=                       \ but there are two owners now
   s" the mapping reads the same bytes at the MAP-OFFSET? frame" T-LABEL
   152 MAP-BYTE-AT 0 T=                         \ tensor a's first byte
   168 MAP-BYTE-AT want-b0 T=                   \ tensor b's, byte-identical to COPY-DATA?
   8 MAP-BYTE-AT LBRACE T=                      \ mapping byte 8 opens the header JSON
   0 MAP-BYTE-AT SYNTH-HDR T=                   \ and byte 0 is the header-length prefix -
                                                \ what BEGIN?'s frame would have pointed at
   SEEN-MAP-LEN @ LEN-A @ T=                    \ the body saw the whole mapping
   s" the census keeps its metadata and hands out no bytes" T-LABEL
   swap                                         \ ( m c )
   CHECK-DETACHED-CENSUS
   SAFET:RELEASE                                \ ( m ) the census is disposed first
   SAFET-MAP:LIVE 1 T=                          \ releasing it did NOT unmap
   SAFET:LIVE-OWNERS 1 T=
   s" the mapping outlives the census and unmaps exactly once" T-LABEL
   152 MAP-BYTE-AT 0 T=                         \ still readable with the census gone
   SAFET:UNMAP-MAPPING LEN-A @ RES-OK=
   SAFET-MAP:LIVE 0 T=
   NO-LEAK
   CLEANUP ;

: TEST-DETACH-TWICE ( -- )
   s" a second detach hands out no second claim on the same bytes" T-LABEL
   BUILD-SYNTH
   SYNTH-PATH SAFET:LOAD                        \ ( c )
   SAFET:DETACH-MAPPING                         \ ( c m1 )
   swap SAFET:DETACH-MAPPING                    \ ( m1 c m2 )
   SAFET-MAP:LIVE 1 T=                          \ still exactly one kernel mapping
   SAFET:LIVE-OWNERS 3 T=                       \ census + two mapping owners
   0 MAP-OFF !  1 MAP-TAKE !
   -1 SEEN-MAP-LEN !                            \ poison: the second mapping has no bytes
   [: SEE-MAPPING ;] SAFET:WITH-MAPPING OPT-NONE
   SEEN-MAP-LEN @ -1 T=
   SAFET:UNMAP-MAPPING 0 RES-OK=                \ and gives nothing back to the kernel
   SAFET-MAP:LIVE 1 T=                          \ the real mapping is untouched
   SAFET:RELEASE                                \ ( m1 ) metadata only
   SAFET-MAP:LIVE 1 T=
   SAFET:UNMAP-MAPPING LEN-A @ RES-OK=          \ the FIRST mapping still owns it
   SAFET-MAP:LIVE 0 T=
   NO-LEAK
   CLEANUP ;

: TEST-DETACH-ADOPTED ( -- )
   s" a mapping detached from an adopted image unmaps nothing" T-LABEL
   J-ALPHA 16 BUILD-A
   LEN-A @ {: before:n :}
   IMG-A c@ {: byte0:n :}
   A$ SAFET:LOAD-SPAN                           \ ( c ) owns no mapping
   SAFET-MAP:LIVE 0 T=
   SAFET:DETACH-MAPPING                         \ ( c m )
   SAFET-MAP:LIVE 0 T=
   SAFET:LIVE-OWNERS 2 T=
   8 MAP-BYTE-AT LBRACE T=                      \ the caller's image is readable through it
   swap                                         \ ( m c )
   -1 SEEN-LEN !
   s" alpha" ID-OF [: SEE-TENSOR ;] SAFET:WITH-TENSOR OPT-NONE   \ but not through the census
   SEEN-LEN @ -1 T=
   SAFET:COUNT 1 T=                             \ which still describes its tensor
   s" alpha" SAFET:FIND OPT-VAL SAFET:NBYTES? 16 OPT=
   SAFET:RELEASE                                \ ( m )
   SAFET:UNMAP-MAPPING 0 RES-OK=                \ nothing was given back
   NO-LEAK
   LEN-A @ before T=                            \ the caller's image survived untouched
   IMG-A c@ byte0 T=
   A$ SAFET:LOAD-SPAN CHECK-ALPHA SAFET:RELEASE ;

: TEST-DETACH-FAILED ( -- )
   s" a failed load leaves a live census able to detach its mapping" T-LABEL
   BUILD-SYNTH
   SYNTH-PATH SAFET:LOAD                        \ ( c ) the live, mapped census
   J-BADJSON 0 BUILD-B
   SAFET:OPEN B$ SAFET:ADOPT                    \ ( c sB )
   [: SAFET:PARSE ;] catch {: code:n :}
   code SAFET:E-JSON T=
   SAFET:CLOSE                                  \ ( c )
   SAFET-MAP:LIVE 1 T=                          \ the rejection took nothing from it
   SAFET:LIVE-OWNERS 1 T=
   CHECK-MAP-OFFSETS                            \ and it still answers both frames
   SAFET:DETACH-MAPPING                         \ ( c m ) the retry succeeds
   swap CHECK-DETACHED-CENSUS
   SAFET:RELEASE                                \ ( m )
   SAFET:UNMAP-MAPPING LEN-A @ RES-OK=
   NO-LEAK
   CLEANUP ;

\ ---- checker-enforced lifetime rules ---------------------------------------
: TEST-LINEAR-OWNERSHIP ( -- )
   s" a session cannot be duplicated, dropped, or stored" T-LABEL
   s" STT-BAD-SESSION-DUP ( SAFET:session -- SAFET:session SAFET:session ) dup" REJECTED
   s" STT-BAD-SESSION-DROP ( SAFET:session -- ) drop" REJECTED
   s" STT-BAD-SESSION-STORE ( SAFET:session ptr n -- ) !" REJECTED
   s" a census cannot be duplicated, dropped, or stored" T-LABEL
   s" STT-BAD-CENSUS-DUP ( SAFET:census -- SAFET:census SAFET:census ) dup" REJECTED
   s" STT-BAD-CENSUS-DROP ( SAFET:census -- ) drop" REJECTED
   s" STT-BAD-CENSUS-STORE ( SAFET:census ptr n -- ) !" REJECTED
   s" detach, close and release consume their owner" T-LABEL
   s" STT-BAD-DETACH-KEEPS ( SAFET:session -- SAFET:session SAFET:census ) SAFET:DETACH" REJECTED
   s" STT-BAD-CLOSE-KEEPS ( SAFET:session -- SAFET:session ) SAFET:CLOSE" REJECTED
   s" STT-BAD-RELEASE-KEEPS ( SAFET:census -- SAFET:census ) SAFET:RELEASE" REJECTED
   s" detach and close are exactly once" T-LABEL
   s" STT-BAD-DOUBLE-DETACH ( SAFET:session -- SAFET:census SAFET:census ) SAFET:DETACH SAFET:DETACH" REJECTED
   s" STT-BAD-DOUBLE-CLOSE ( SAFET:session -- ) SAFET:CLOSE SAFET:CLOSE" REJECTED
   s" STT-BAD-DETACH-AFTER-CLOSE ( SAFET:session -- SAFET:census ) SAFET:CLOSE SAFET:DETACH" REJECTED
   s" STT-BAD-CLOSE-AFTER-DETACH ( SAFET:session -- SAFET:census ) SAFET:DETACH SAFET:CLOSE" REJECTED
   s" a census is released exactly once and never unmapped twice" T-LABEL
   s" STT-BAD-DOUBLE-RELEASE ( SAFET:census -- ) SAFET:RELEASE SAFET:RELEASE" REJECTED
   s" STT-BAD-RELEASE-THEN-READ ( SAFET:census -- n ) SAFET:RELEASE SAFET:COUNT" REJECTED ;

: TEST-MAPPING-OWNERSHIP ( -- )
   s" a mapping cannot be duplicated, dropped, or stored" T-LABEL
   s" STT-BAD-MAPPING-DUP ( SAFET:mapping -- SAFET:mapping SAFET:mapping ) dup" REJECTED
   s" STT-BAD-MAPPING-DROP ( SAFET:mapping -- ) drop" REJECTED
   s" STT-BAD-MAPPING-STORE ( SAFET:mapping ptr n -- ) !" REJECTED
   s" a detach keeps its census and an unmap consumes its mapping" T-LABEL
   s" STT-BAD-DETACH-EATS-CENSUS ( SAFET:census -- SAFET:mapping ) SAFET:DETACH-MAPPING" REJECTED
   s" STT-BAD-UNMAP-KEEPS ( SAFET:mapping -- SAFET:mapping result<n,n> ) SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-DOUBLE-UNMAP ( SAFET:mapping -- result<n,n> result<n,n> ) SAFET:UNMAP-MAPPING SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-UNMAP-THEN-READ ( SAFET:mapping -- result<n,n> SAFET:mapping option<n> ) SAFET:UNMAP-MAPPING SAFET:WITH-MAPPING" REJECTED
   s" the three owner tokens are not interchangeable" T-LABEL
   s" STT-BAD-MAPPING-RELEASE ( SAFET:mapping -- ) SAFET:RELEASE" REJECTED
   s" STT-BAD-MAPPING-CLOSE ( SAFET:mapping -- ) SAFET:CLOSE" REJECTED
   s" STT-BAD-CENSUS-UNMAP ( SAFET:census -- result<n,n> ) SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-SESSION-DETACH-MAP ( SAFET:session -- SAFET:session SAFET:mapping ) SAFET:DETACH-MAPPING" REJECTED
   s" STT-BAD-MAPPING-COUNT ( SAFET:mapping -- SAFET:mapping n ) SAFET:COUNT" REJECTED
   s" a cleanup result cannot be dropped or read as a bare number" T-LABEL
   s" STT-BAD-UNMAP-IGNORED ( SAFET:mapping -- ) SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-UNMAP-RAW ( SAFET:mapping -- n ) SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-UNMAP-ARITH ( SAFET:mapping -- n ) SAFET:UNMAP-MAPPING 1 +" REJECTED
   s" no mapping reader answers without its mapping" T-LABEL
   s" STT-BAD-AMBIENT-UNMAP ( -- result<n,n> ) SAFET:UNMAP-MAPPING" REJECTED
   s" STT-BAD-AMBIENT-DETACH ( -- SAFET:mapping ) SAFET:DETACH-MAPPING" REJECTED
   s" the mapping surface really does resolve (control)" T-LABEL
   s" STT-OK-DETACH ( SAFET:census -- SAFET:census SAFET:mapping ) SAFET:DETACH-MAPPING" ACCEPTED
   s" STT-OK-UNMAP ( SAFET:mapping -- result<n,n> ) SAFET:UNMAP-MAPPING" ACCEPTED
   s" STT-OK-MAP-OFFSET ( SAFET:census n -- SAFET:census option<n> ) SAFET:MAP-OFFSET?" ACCEPTED
   s" the mapping record and the no-image state stay private" T-LABEL
   s" STT-BAD-MINT-MAPPING ( ptr u8 -- SAFET:mapping ) SAFET:MINT-MAPPING" UNRESOLVED
   s" STT-BAD-MAPPING-REC ( SAFET:mapping -- SAFET:mapping ptr n ) SAFET:MAPPING>REC" UNRESOLVED
   s" STT-BAD-TAKE-MAPPING ( SAFET:mapping -- ptr n ) SAFET:TAKE-MAPPING" UNRESOLVED
   s" STT-BAD-HAS-IMG ( SAFET:census -- SAFET:census bool ) SAFET:HAS-IMG?" UNRESOLVED
   s" STT-BAD-BYTES-OK ( SAFET:census n -- SAFET:census bool ) SAFET:BYTES-OK?" UNRESOLVED
   s" no mapping word hands back a raw pointer" T-LABEL
   s" STT-BAD-MAP-BASE ( SAFET:mapping -- SAFET:mapping ptr u8 ) SAFET:MAP-BASE" UNRESOLVED
   s" an offset reader returns option, never a -1 sentinel" T-LABEL
   s" STT-BAD-MAP-OFFSET-RAW ( SAFET:census n -- SAFET:census n ) SAFET:MAP-OFFSET?" REJECTED
   s" STT-BAD-MAP-OFFSET-SENTINEL ( SAFET:census n -- SAFET:census bool ) SAFET:MAP-OFFSET? -1 =" REJECTED ;

: TEST-NO-AMBIENT-STATE ( -- )
   s" no census reader answers without its census" T-LABEL
   s" STT-BAD-AMBIENT-COUNT ( -- n ) SAFET:COUNT" REJECTED
   s" STT-BAD-AMBIENT-FIND ( ptr u8 n -- n ) SAFET:FIND" REJECTED
   s" STT-BAD-AMBIENT-DTYPE ( n -- n ) SAFET:DTYPE?" REJECTED
   s" STT-BAD-AMBIENT-NBYTES ( n -- n ) SAFET:NBYTES?" REJECTED
   s" STT-BAD-AMBIENT-DIM ( n n -- n ) SAFET:DIM?" REJECTED
   s" a session is not a census and a census is not a session" T-LABEL
   s" STT-BAD-SESSION-READ ( SAFET:session -- SAFET:session n ) SAFET:COUNT" REJECTED
   s" STT-BAD-CENSUS-PARSE ( SAFET:census -- SAFET:census ) SAFET:PARSE" REJECTED
   s" STT-BAD-CENSUS-CLOSE ( SAFET:census -- ) SAFET:CLOSE" REJECTED
   s" STT-BAD-SESSION-RELEASE ( SAFET:session -- ) SAFET:RELEASE" REJECTED ;

: TEST-SEALED-REPRESENTATION ( -- )
   s" the public surface really does resolve (control)" T-LABEL
   s" STT-OK-COUNT ( SAFET:census -- SAFET:census n ) SAFET:COUNT" ACCEPTED
   s" STT-OK-WITH ( SAFET:census n -- SAFET:census option<n> ) SAFET:NBYTES?" ACCEPTED
   s" the block and token representation stays private" T-LABEL
   s" STT-BAD-MINT ( ptr u8 -- SAFET:session ) SAFET:MINT-SESSION" UNRESOLVED
   s" STT-BAD-BLOCK ( SAFET:session -- SAFET:session ptr n ) SAFET:SESSION>BLOCK" UNRESOLVED
   s" STT-BAD-TAKE ( SAFET:session -- ptr n ) SAFET:TAKE-SESSION" UNRESOLVED
   s" STT-BAD-RETYPE ( SAFET:session -- SAFET:census ) SAFET:SESSION>CENSUS" UNRESOLVED
   s" STT-BAD-CENSUS-BLOCK ( SAFET:census -- SAFET:census ptr n ) SAFET:CENSUS>BLOCK" UNRESOLVED
   s" STT-BAD-BYTES ( ptr n -- ptr u8 ) SAFET:BLOCK>BYTES" UNRESOLVED
   s" STT-BAD-MAP-N>PTR ( n -- ptr u8 ) SAFET-MAP:N>PTR" UNRESOLVED
   s" no public word hands back a raw pointer" T-LABEL
   s" STT-BAD-RAW-DATA ( SAFET:census n -- SAFET:census ptr u8 ) SAFET:DATA-PTR" UNRESOLVED
   s" STT-BAD-RAW-BASE ( SAFET:census -- SAFET:census ptr u8 ) SAFET:BASE" UNRESOLVED ;

: TEST-OPTION-DISCIPLINE ( -- )
   s" an id-addressed reader returns option, never a -1 sentinel" T-LABEL
   s" STT-BAD-FIND-SENTINEL ( SAFET:census ptr u8 n -- SAFET:census bool ) SAFET:FIND -1 =" REJECTED
   s" STT-BAD-DTYPE-RAW ( SAFET:census n -- SAFET:census n ) SAFET:DTYPE? 1 +" REJECTED
   s" STT-BAD-RANK-RAW ( SAFET:census n -- SAFET:census n ) SAFET:RANK?" REJECTED ;

\ ---- presence-gated real artifact (HF gpt2 model.safetensors) --------------
: REAL-PATH ( -- ptr u8 n )  s" gpt2-model/model.safetensors" ;

: CHECK-REAL ( SAFET:census -- SAFET:census )
   s" real gpt2 tensors=" type SAFET:COUNT dup . cr
   160 T=
   s" wte.weight" ID-OF {: w:n :}
   w 0 < 0= TTRUE
   w SAFET:RANK? 2 OPT=
   w 0 SAFET:DIM? 50257 OPT=
   w 1 SAFET:DIM? 768 OPT=
   w SAFET:DTYPE? SAFET:DT-F32 OPT=
   w SAFET:NBYTES? 154389504 OPT=               \ 50257*768*4
   s" wpe.weight" ID-OF {: p:n :}
   p 0 SAFET:DIM? 1024 OPT=   p 1 SAFET:DIM? 768 OPT=
   s" h.0.attn.c_attn.weight" ID-OF {: c:n :}
   c 0 SAFET:DIM? 768 OPT=                      \ Conv1D: [in, out], NOT [out, in]
   c 1 SAFET:DIM? 2304 OPT= ;

: TEST-REAL ( -- )
   REAL-PATH SAFET:PRESENT? 0= if
      s" safetensors: gpt2-model/model.safetensors absent -> real-artifact leg SKIPPED (run maki/examples/nanogpt/fetch-gpt2-model.sh)" type cr
      0 0= TTRUE exit
   then
   s" the real gpt2 checkpoint publishes its full census" T-LABEL
   REAL-PATH SAFET:LOAD
   CHECK-REAL
   SAFET:RELEASE ;

: RUN ( -- )
   T-RESET
\ NO-LEAK after every leg that owns something: a leg that forgets a mapping or an
\ owner is reported where it happened, not three tests later.
   TEST-MALFORMED         NO-LEAK
   TEST-LOAD-FAULTS       NO-LEAK
   TEST-VALID-FILE        NO-LEAK
   TEST-INTERLEAVED       NO-LEAK
   TEST-FAILED-ISOLATION  NO-LEAK
   TEST-NESTED-CATCH      NO-LEAK
   TEST-ORDER             NO-LEAK
   TEST-CAPACITY          NO-LEAK
   TEST-TWO-MODELS        NO-LEAK
   TEST-DISPOSAL          NO-LEAK
   TEST-MAP-OFFSET        NO-LEAK
   TEST-DETACH-MAPPING    NO-LEAK
   TEST-DETACH-TWICE      NO-LEAK
   TEST-DETACH-ADOPTED    NO-LEAK
   TEST-DETACH-FAILED     NO-LEAK
   TEST-LINEAR-OWNERSHIP
   TEST-MAPPING-OWNERSHIP
   TEST-NO-AMBIENT-STATE
   TEST-SEALED-REPRESENTATION
   TEST-OPTION-DISCIPLINE
   TEST-REAL
   s" the whole suite released every mapping it took" T-LABEL
   NO-LEAK
   T-REPORT ;

RUN

;package
