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
\ live census byte-identical, and the linear session/census tokens are proved
\ un-duplicable, un-droppable, and un-reusable by feeding bad definitions to the
\ checker itself. The presence-gated real leg parses HuggingFace GPT-2
\ (openai-community/gpt2) and asserts the tensor census.

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
   TEST-LINEAR-OWNERSHIP
   TEST-NO-AMBIENT-STATE
   TEST-SEALED-REPRESENTATION
   TEST-OPTION-DISCIPLINE
   TEST-REAL
   s" the whole suite released every mapping it took" T-LABEL
   NO-LEAK
   T-REPORT ;

RUN

;package
