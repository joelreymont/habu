\ maki/competitive-store.f - the checked canonical codec + sealed store records for
\ BENCH competitive-comparison values (dot habu-persist-typed-bench-2d15efa2, epic
\ habu-v2-competitive-evidence-5d07d471).
\
\ Competitive evidence used to be persisted as raw strings plus untyped numeric
\ fields, so a writer could bypass the BENCH comparison schema or replay a row under
\ the wrong policy, kind, or key (the historical Habu-FP32-vs-Triton-TF32 confusion,
\ maki/competitive-report.f). This file closes that: a persisted comparison is a
\ CANONICAL row derived from the typed value alone, and reading one back either yields
\ the exact typed comparison or a NAMED reject - persisted bytes are untrusted input.
\
\ WRITER SEAM (stated choice: package BENCH, self-contained store).
\ The store rows for BENCH values live in package BENCH alongside the types and the
\ single canonical render owner (maki/competitive-report.f). The one on-disk-encoding
\ owner is BENCH-ENCODE-GBPS/-GFLOPS here; the raw file appender BENCH-ROW-APPEND is
\ package-BENCH-PRIVATE, so - exactly like the R7 store seal sealed EVID-PUT-G /
\ SK-PUT-DURABLE to package-MAKI-private (maki/store.f, maki/store-replay.f) - nothing
\ outside this package can plant a raw competitive row: a cross-package / qualified
\ BENCH:BENCH-ROW-APPEND does not resolve (verdict 1, proven in the sibling test with a
\ BENCH:BENCH-GET read control). The ONLY public writers are the typed BENCH-PUT-GBPS /
\ BENCH-PUT-GFLOPS, which accept a typed comparison and DERIVE everything else. We put
\ the seam in package BENCH (not package MAKI) because the value TYPES and their sole
\ canonical render (RENDER-GBPS/-GFLOPS) are package-BENCH; keeping the codec here reuses
\ that single render (no wire duplication) and keeps the seal a same-package private, and
\ it touches maki/store.f only through its PUBLIC root discipline (STORE-ROOT+ /
\ STORE-RESET), never its private wire writers - the CAD store's row schemas stay untouched.
\
\ DERIVATION (write). From the typed comparison we derive, in order:
\   - its exact KEY = the canonical bench/v1 render (RENDER-GBPS/-GFLOPS). Every field
\     participates, so the whole render IS the content-addressed key (sched-key's render
\     discipline: a different field is a different key). The store latest-wins on it.
\   - the SCHEMA version = bench/v1 (recorded, so a version bump is a rehydrate reject).
\   - the content DIGEST = an FNV-1a 64-bit hash over the canonical render, 16 hex digits.
\   - the PROMOTION-EVIDENCE field = the COMPARABLE? verdict (comparable|incomparable):
\     the in-scope promotion linkage. A competitive comparison is licensed as promotion
\     evidence ONLY if the two sides share a numeric policy (GBPS-COMPARABLE? /
\     GFLOPS-COMPARABLE?, maki/numpolicy.f witnesses); the historical FP32-vs-TF32
\     confusion is exactly an incomparable pair. So the row records whether the value is
\     promotion-licensed, and rehydration re-derives it and rejects a row whose recorded
\     verdict is stale (E-BENCH-ROW-PROMO). The FULLER cross-artifact linkage (binding a
\     row to a specific ART:promoted decision via the sealed POLICY:granted grant) needs
\     the store-seal internals (maki/evidence/*) out of this dot's scope; this file
\     implements the recorded-evidence check precisely and leaves the artifact binding to
\     the promotion lane.
\ The persisted row is  <canonical-render>|schema=bench/v1|spol=<p>|bpol=<p>|unit=<u>|
\ digest=<hex16>|promo=<comparable|incomparable> . The policy/unit fields are recorded
\ REDUNDANTLY (they also live inside the render key): the read boundary cross-checks the
\ two independent copies, so a row relabelled under a different policy/kind key than it
\ was measured under is a named reject, not a silent launder.
\
\ REHYDRATION (read) treats the row bytes as untrusted and fails closed on every axis,
\ each a distinct named throw (never a silent skip):
\   duplicate-field    E-BENCH-ROW-DUP     a field label appears twice
\   malformed          E-BENCH-ROW-FIELDS  wrong field count / missing marker / bad integer
\   bad label          E-BENCH-ROW-LABEL   a field is not its expected label at its slot
\   bad token          E-BENCH-ROW-TOKEN   an enum/absence value is outside its closed domain
\   wrong-schema       E-BENCH-ROW-SCHEMA  the schema tag / field is not bench/v1
\   cross-kind         E-BENCH-ROW-KIND    a gbps row under a gflops key (or meta unit disagrees)
\   cross-policy       E-BENCH-ROW-POLICY  the recorded policy disagrees with the derived key
\   digest-mismatch    E-BENCH-ROW-DIGEST  the recorded digest is not the canonical render's
\   noncanonical       E-BENCH-ROW-CANON   the raw key bytes are not the canonical re-render
\   stale-promotion    E-BENCH-ROW-PROMO   the recorded promotion verdict is not COMPARABLE?'s
\ A valid row round-trips: persist -> rehydrate -> re-persist is byte-for-byte (the
\ committed golden in the sibling test). The direct raw-writer bypass is a checker reject
\ (BENCH-ROW-APPEND is unresolvable across the package seal), not a runtime code.
\
\ Fail closed: an oversized built row / read buffer is a named throw. maki -> habu only;
\ competitive-store owns -5310..-5320 (report layer owns -5257..-5258).

require lib/prelude.f
require lib/string.f
require lib/fmt.f
require maki/store.f                \ STORE-ROOT+ / STORE-RESET root discipline (PUBLIC) - no wire reuse
require maki/numpolicy.f            \ NPOL:dom, NPOL:NAME - the recorded/derived numeric policy
require maki/competitive-report.f   \ the typed comparisons + their SINGLE canonical render owner

-5310 constant E-BENCH-STORE-FULL   \ built row or read buffer exceeds its capacity
-5311 constant E-BENCH-ROW-FIELDS   \ persisted row: field count / missing marker / non-numeric value
-5312 constant E-BENCH-ROW-LABEL    \ a field is not its expected label at its slot
-5313 constant E-BENCH-ROW-TOKEN    \ an enum / absence value token is outside its closed domain
-5314 constant E-BENCH-ROW-DUP      \ a field label appears more than once (duplicate field)
-5315 constant E-BENCH-ROW-SCHEMA   \ the schema tag / schema field is not bench/v1
-5316 constant E-BENCH-ROW-KIND     \ throughput-unit / kind mismatch (gbps row under a gflops key)
-5317 constant E-BENCH-ROW-POLICY   \ the recorded numeric policy disagrees with the derived key
-5318 constant E-BENCH-ROW-DIGEST   \ the recorded content digest is not the canonical render's
-5319 constant E-BENCH-ROW-CANON    \ the raw key bytes are not the canonical re-render
-5320 constant E-BENCH-ROW-PROMO    \ the recorded promotion verdict is not the COMPARABLE? verdict

package BENCH
private

\ ---- framing constants + buffers --------------------------------------------
1024   constant BST-ROW-CAP         \ one built row (render + metadata)
$40000 constant BST-READ-CAP        \ whole competitive.rows read buffer (append-only cap)
$7C    constant BST-PIPE            \ '|'
$0A    constant BST-NL              \ newline
$3D    constant BST-EQ              \ '='
17     constant BST-FIELD-N         \ 11 render fields + 6 metadata fields
16     constant BST-DIG-N           \ digest hex digits (64-bit FNV-1a)

create BST-KEY   BST-ROW-CAP allot   variable BST-KEY-U    \ canonical render (the key), stable
create BST-ROW   BST-ROW-CAP allot   variable BST-ROW-U    \ built full row (encode result)
create BST-IN    BST-ROW-CAP allot   variable BST-IN-U     \ untrusted input row, stable copy
create BST-CAN   BST-ROW-CAP allot   variable BST-CAN-U    \ canonical re-render (decode integrity)
create BST-LINE  BST-ROW-CAP allot   variable BST-LINE-U   \ matched store line (get result)
create BST-READ  BST-READ-CAP allot  variable BST-READ-U   \ competitive.rows read buffer
create BST-DIG   BST-DIG-N allot                            \ digest hex text
create BST-PATH  FS-PATH-CAP allot   variable BST-PATH-U   \ resolved competitive.rows path
create BST-NLB   1 allot   BST-NL BST-NLB c!                \ single-newline append source

\ ---- FNV-1a 64-bit content digest over bytes --------------------------------
$cbf29ce484222325 constant BST-FNV-BASIS
$100000001b3       constant BST-FNV-PRIME

: BST-FNV ( ptr u8 n -- n ) {: a:ptr u:n :}
   BST-FNV-BASIS
   u 0 ?do  a i + c@ xor  BST-FNV-PRIME *  loop ;
: BST-HEXNIB ( n -- n )  $F and dup 10 < if $30 + else $37 + then ;
: BST-DIGEST! ( ptr u8 n -- )                       \ FNV -> BST-DIG hex, MSB first
   BST-FNV {: h:n :}
   BST-DIG-N 0 ?do  h  BST-DIG-N 1- i - 4 * rshift BST-HEXNIB  BST-DIG i + c!  loop ;
: BST-DIG$ ( -- ptr u8 n )  BST-DIG BST-DIG-N ;

\ ---- promotion-evidence token (the COMPARABLE? verdict) ----------------------
: BST-PROMO$ ( bool -- ptr u8 n )  if s" comparable" else s" incomparable" then ;

\ ---- resolved store path (reuses maki/store.f's PUBLIC root discipline) -------
: BST-PATH$ ( -- ptr u8 n )                         \ ensure root + join competitive.rows
   MAKI:STORE-ROOT+ s" competitive.rows" BST-PATH JOIN-PATH BST-PATH-U !
   BST-PATH BST-PATH-U @ ;

\ ---- SEALED raw appender (package-BENCH-private: the store seal) --------------
: BENCH-ROW-APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   BST-PATH$ a u APPEND-FILE
   BST-PATH$ BST-NLB 1 APPEND-FILE ;

\ ---- stable holds -----------------------------------------------------------
: BST-KEY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   a BST-KEY u BYTE-COPY  u BST-KEY-U ! ;
: BST-KEY$ ( -- ptr u8 n )  BST-KEY BST-KEY-U @ ;
: BST-ROW! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   a BST-ROW u BYTE-COPY  u BST-ROW-U ! ;
: BST-ROW$ ( -- ptr u8 n )  BST-ROW BST-ROW-U @ ;
: BST-CAN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   a BST-CAN u BYTE-COPY  u BST-CAN-U ! ;
: BST-CAN$ ( -- ptr u8 n )  BST-CAN BST-CAN-U @ ;
: BST-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   a BST-IN u BYTE-COPY  u BST-IN-U ! ;

\ ---- per-comparison policy extraction (via the typed UNMAKE) ------------------
\ product values ride the stack (they cannot bind to locals); only the enum/int
\ fields bind. Both unmakers drop the two throughput readings and return spol/bpol.
: GBPS-POLICIES ( comparison-gbps -- NPOL:dom NPOL:dom )
   BENCH-COMPARISON--GBPS:UNMAKE GBPS-DROP GBPS-DROP
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state spol:NPOL:dom bpol:NPOL:dom :}
   spol bpol ;
: GFLOPS-POLICIES ( comparison-gflops -- NPOL:dom NPOL:dom )
   BENCH-COMPARISON--GFLOPS:UNMAKE GFLOPS-DROP GFLOPS-DROP
   {: wl:workload sh:shape pr:protocol bl:baseline cache:cache-state spol:NPOL:dom bpol:NPOL:dom :}
   spol bpol ;

\ ---- metadata suffix render (the key is already the SB head) ------------------
: BST-META+ ( bool -- ) {: comparable:bool :}       \ spol bpol are appended by the caller wrappers
   s" |digest=" SB-APPEND BST-DIG$ SB-APPEND
   s" |promo="  SB-APPEND comparable BST-PROMO$ SB-APPEND ;
\ Metadata copies carry DISTINCT labels (mspol/mbpol/munit) from the render's own
\ spol/bpol/unit, so the two independent encodings never collide in the duplicate-field
\ scan yet the read boundary can cross-check them axis by axis.
: BST-POL+ ( NPOL:dom NPOL:dom -- ) {: spol:NPOL:dom bpol:NPOL:dom :}
   s" |mspol=" SB-APPEND spol NPOL:NAME SB-APPEND
   s" |mbpol=" SB-APPEND bpol NPOL:NAME SB-APPEND ;

public

\ ---- the ONLY public writers: typed comparison -> derived canonical row -------
: BENCH-ENCODE-GBPS ( comparison-gbps -- ptr u8 n )
   dup RENDER-GBPS BST-KEY!                          \ derive exact key = canonical render
   BST-KEY$ BST-DIGEST!                              \ derive content digest over the key
   dup GBPS-COMPARABLE? {: comparable:bool :}        \ derive promotion-evidence verdict
   GBPS-POLICIES {: spol:NPOL:dom bpol:NPOL:dom :}   \ derive recorded policy fields
   SB-RESET
   BST-KEY$ SB-APPEND
   s" |schema=bench/v1" SB-APPEND
   spol bpol BST-POL+
   s" |munit=gbps" SB-APPEND
   comparable BST-META+
   SB$ BST-ROW! BST-ROW$ ;
: BENCH-ENCODE-GFLOPS ( comparison-gflops -- ptr u8 n )
   dup RENDER-GFLOPS BST-KEY!
   BST-KEY$ BST-DIGEST!
   dup GFLOPS-COMPARABLE? {: comparable:bool :}
   GFLOPS-POLICIES {: spol:NPOL:dom bpol:NPOL:dom :}
   SB-RESET
   BST-KEY$ SB-APPEND
   s" |schema=bench/v1" SB-APPEND
   spol bpol BST-POL+
   s" |munit=gflops" SB-APPEND
   comparable BST-META+
   SB$ BST-ROW! BST-ROW$ ;

: BENCH-PUT-GBPS   ( comparison-gbps -- )    BENCH-ENCODE-GBPS   BENCH-ROW-APPEND ;
: BENCH-PUT-GFLOPS ( comparison-gflops -- )  BENCH-ENCODE-GFLOPS BENCH-ROW-APPEND ;

\ ---- store reset (test/session seam; delegates to the shared root) -----------
: BENCH-STORE-RESET ( -- )  MAKI:STORE-RESET ;

private

\ ---- store scan: latest-wins whole-key match, returns the full matched line ---
variable BST-Q-KA   variable BST-Q-KU   variable BST-Q-FOUND

: BST-READ-FILE ( -- )                              \ competitive.rows -> BST-READ (missing = empty)
   BST-PATH$ {: pa:ptr pu:n :}
   pa pu FILE? 0= if 0 BST-READ-U ! exit then
   pa pu FILE-SIZE BST-READ-CAP > if E-BENCH-STORE-FULL throw then
   pa pu BST-READ BST-READ-CAP READ-ALL BST-READ-U ! ;

: BST-LINE-END ( n -- n ) {: off:n :}               \ offset of the newline ending this line (or EOF)
   off begin dup BST-READ-U @ < while
      dup BST-READ + c@ BST-NL = if exit then
      1+
   repeat ;

: BST-Q-MATCH? ( ptr u8 n -- bool ) {: la:ptr lu:n :}   \ line begins with <query-key> '|'
   lu BST-Q-KU @ 1+ < if false exit then
   la BST-Q-KU @  BST-Q-KA @ BST-Q-KU @  STR= 0= if false exit then
   la BST-Q-KU @ + c@ BST-PIPE = ;

: BST-LINE! ( ptr u8 n -- ) {: la:ptr lu:n :}
   lu BST-ROW-CAP > if E-BENCH-STORE-FULL throw then
   la BST-LINE lu BYTE-COPY  lu BST-LINE-U ! ;

: BST-SCAN-AT ( n -- n ) {: off:n :}
   off BST-LINE-END {: ed:n :}
   ed off > if
      BST-READ off +  ed off -  2dup BST-Q-MATCH? if
         BST-LINE!  -1 BST-Q-FOUND !
      else 2drop then
   then
   ed 1+ ;

public

\ BENCH-GET returns the LATEST full stored row for a canonical key (append-only
\ latest-wins). The read control for the store-seal bypass regression.
: BENCH-GET ( ptr u8 n -- ptr u8 n bool ) {: ka:ptr ku:n :}
   ka BST-Q-KA !  ku BST-Q-KU !  0 BST-Q-FOUND !
   BST-READ-FILE
   0 begin dup BST-READ-U @ < while BST-SCAN-AT repeat drop
   BST-Q-FOUND @ 0= if BST-LINE 0 false exit then
   BST-LINE BST-LINE-U @ true ;

private

\ ---- field access over the stable BST-IN copy --------------------------------
: BST-PIPE-AT ( n -- n ) {: off:n :}                \ next '|' at/after off (or length)
   off begin dup BST-IN-U @ < while
      dup BST-IN + c@ BST-PIPE = if exit then
      1+
   repeat ;

: BST-FIELD-START ( n -- n ) {: k:n :}              \ byte offset where field k begins
   0                                                \ advance past k '|' delimiters from 0
   k 0 ?do
      BST-PIPE-AT
      dup BST-IN-U @ >= if E-BENCH-ROW-FIELDS throw then
      1+
   loop ;

: BST-FIELD ( n -- ptr u8 n ) {: k:n :}             \ the k-th '|' field slice
   k BST-FIELD-START {: start:n :}
   start BST-PIPE-AT {: ed:n :}
   BST-IN start +  ed start - ;

: BST-PIPES ( -- n )                                \ '|' count in BST-IN
   0  0 begin dup BST-IN-U @ < while
      dup BST-IN + c@ BST-PIPE = if swap 1+ swap then
      1+
   repeat drop ;
: BST-NFIELDS ( -- n )  BST-PIPES 1+ ;

: BST-FIELD-VALUE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: fa:ptr fu:n la:ptr lu:n :}
   fa fu la lu STARTS-WITH? 0= if E-BENCH-ROW-LABEL throw then
   fa lu +  fu lu - ;

: BST-EQ-IDX ( ptr u8 n -- n )                      \ index of '=' in a field, or -1
   BST-EQ INDEX-OF MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;
: BST-FIELD-LABEL ( ptr u8 n -- ptr u8 n ) {: fa:ptr fu:n :}   \ label = bytes before '=' (or whole)
   fa fu BST-EQ-IDX {: i:n :}
   i 0 < if fa fu else fa i then ;
: BST-FIELD-LABEL@ ( n -- ptr u8 n )  BST-FIELD BST-FIELD-LABEL ;

\ ---- duplicate-field scan (a repeated label is a reject) ----------------------
: BST-DUP-AT ( n -- ) {: k:n :}                     \ field k's label must not equal any earlier field's
   k BST-FIELD-LABEL@ {: la:ptr lu:n :}
   k 0 ?do
      la lu  i BST-FIELD-LABEL@  STR= if E-BENCH-ROW-DUP throw then
   loop ;
: BST-DUP-SCAN ( -- )
   BST-NFIELDS {: n:n :}
   n 0 ?do i BST-DUP-AT loop ;

\ ---- token -> typed family (fail closed on an out-of-domain token) ------------
: BST>WORKLOAD ( ptr u8 n -- workload ) {: a:ptr u:n :}
   a u s" saxpy" STR= if BENCH-WORKLOAD:SAXPY exit then
   a u s" gemm"  STR= if BENCH-WORKLOAD:GEMM  exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>SHAPE ( ptr u8 n -- shape ) {: a:ptr u:n :}
   a u s" n1m"    STR= if BENCH-SHAPE:N1M    exit then
   a u s" sq2048" STR= if BENCH-SHAPE:SQ2048 exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>PROTOCOL ( ptr u8 n -- protocol ) {: a:ptr u:n :}
   a u s" cuda-events" STR= if BENCH-PROTOCOL:CUDA-EVENTS exit then
   a u s" wallclock"   STR= if BENCH-PROTOCOL:WALLCLOCK   exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>BASELINE ( ptr u8 n -- baseline ) {: a:ptr u:n :}
   a u s" triton" STR= if BENCH-BASELINE:TRITON exit then
   a u s" cublas" STR= if BENCH-BASELINE:CUBLAS exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>CACHE ( ptr u8 n -- cache-state ) {: a:ptr u:n :}
   a u s" cold" STR= if BENCH-CACHE--STATE:COLD exit then
   a u s" warm" STR= if BENCH-CACHE--STATE:WARM exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>ABSENCE ( ptr u8 n -- absence ) {: a:ptr u:n :}
   a u s" not-measured" STR= if BENCH-ABSENCE:NOT-MEASURED exit then
   a u s" device-gated" STR= if BENCH-ABSENCE:DEVICE-GATED exit then
   a u s" waived"       STR= if BENCH-ABSENCE:WAIVED       exit then
   E-BENCH-ROW-TOKEN throw ;
: BST>NPOL ( ptr u8 n -- NPOL:dom ) {: a:ptr u:n :}
   a u s" exact" STR= if NPOL-DOM:EXACT     exit then
   a u s" ulp"   STR= if NPOL-DOM:ULP       exit then
   a u s" rel"   STR= if NPOL-DOM:RELATIVE  exit then
   a u s" emp"   STR= if NPOL-DOM:EMPIRICAL exit then
   E-BENCH-ROW-TOKEN throw ;

: BST-INT ( ptr u8 n -- n )                         \ a numeric reading (fail closed)
   STR>NUMBER? MATCH option
      none OF E-BENCH-ROW-FIELDS throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ ---- identity decode (shared: the seven nominal / policy render fields) --------
: BST-DECODE-IDENTITY ( -- workload shape protocol baseline cache-state NPOL:dom NPOL:dom )
   1 BST-FIELD s" wl="    BST-FIELD-VALUE BST>WORKLOAD
   2 BST-FIELD s" sh="    BST-FIELD-VALUE BST>SHAPE
   3 BST-FIELD s" pr="    BST-FIELD-VALUE BST>PROTOCOL
   4 BST-FIELD s" bl="    BST-FIELD-VALUE BST>BASELINE
   5 BST-FIELD s" cache=" BST-FIELD-VALUE BST>CACHE
   6 BST-FIELD s" spol="  BST-FIELD-VALUE BST>NPOL
   7 BST-FIELD s" bpol="  BST-FIELD-VALUE BST>NPOL ;

\ ---- per-unit throughput reading (integer or na:<absence>) --------------------
: BST>GBPS ( ptr u8 n -- gbps ) {: a:ptr u:n :}
   a u s" na:" STARTS-WITH? if a 3 + u 3 - BST>ABSENCE GBPS-ABSENT exit then
   a u BST-INT >GBPS ;
: BST>GFLOPS ( ptr u8 n -- gflops ) {: a:ptr u:n :}
   a u s" na:" STARTS-WITH? if a 3 + u 3 - BST>ABSENCE GFLOPS-ABSENT exit then
   a u BST-INT >GFLOPS ;

\ ---- structural + cross-axis checks (shared) ---------------------------------
: BST-CHECK-COUNT ( -- )  BST-NFIELDS BST-FIELD-N <> if E-BENCH-ROW-FIELDS throw then ;
: BST-CHECK-SCHEMA-TAG ( -- )                       \ render field 0 is the bench/v1 schema tag
   0 BST-FIELD s" bench/v1" STR= 0= if E-BENCH-ROW-SCHEMA throw then ;
: BST-CHECK-META-SCHEMA ( -- )                      \ metadata schema field == bench/v1
   11 BST-FIELD s" schema=" BST-FIELD-VALUE s" bench/v1" STR= 0= if E-BENCH-ROW-SCHEMA throw then ;
: BST-CHECK-KIND ( ptr u8 n -- ) {: ea:ptr eu:n :}  \ expected unit token; key + meta must both match
   8  BST-FIELD s" unit=" BST-FIELD-VALUE ea eu STR= 0= if E-BENCH-ROW-KIND throw then
   14 BST-FIELD s" munit=" BST-FIELD-VALUE ea eu STR= 0= if E-BENCH-ROW-KIND throw then ;
: BST-CHECK-POLICY ( -- )                           \ recorded policy == derived-key policy
   6  BST-FIELD s" spol=" BST-FIELD-VALUE  12 BST-FIELD s" mspol=" BST-FIELD-VALUE
      STR= 0= if E-BENCH-ROW-POLICY throw then
   7  BST-FIELD s" bpol=" BST-FIELD-VALUE  13 BST-FIELD s" mbpol=" BST-FIELD-VALUE
      STR= 0= if E-BENCH-ROW-POLICY throw then ;
: BST-KEYSLICE$ ( -- ptr u8 n )                     \ raw bytes before "|schema=" = the render key
   11 BST-FIELD-START {: s:n :}
   BST-IN  s 1- ;
: BST-CHECK-DIGEST ( -- )                           \ BST-DIG (FNV of the re-render) == recorded digest
   BST-DIG$  15 BST-FIELD s" digest=" BST-FIELD-VALUE  STR= 0= if E-BENCH-ROW-DIGEST throw then ;
: BST-CHECK-CANON ( ptr u8 n -- ) {: ca:ptr cu:n :} \ raw key bytes == the canonical re-render
   BST-KEYSLICE$ ca cu STR= 0= if E-BENCH-ROW-CANON throw then ;
: BST-CHECK-PROMO ( bool -- )                       \ recorded promo == the COMPARABLE? verdict
   BST-PROMO$  16 BST-FIELD s" promo=" BST-FIELD-VALUE  STR= 0= if E-BENCH-ROW-PROMO throw then ;

\ ---- shared prologue: untrusted row -> validated structure + cross-checks ------
: BST-DECODE-PROLOGUE ( ptr u8 n ptr u8 n -- )      \ ( row expected-unit -- )   leaves nothing
   {: ea:ptr eu:n :}                                \ expected-unit token (gbps|gflops)
   BST-IN!
   BST-DUP-SCAN
   BST-CHECK-COUNT
   BST-CHECK-SCHEMA-TAG
   BST-CHECK-META-SCHEMA
   ea eu BST-CHECK-KIND
   BST-CHECK-POLICY ;

public

\ ---- rehydration: an untrusted persisted row -> the typed comparison ----------
\ Deepest checks run against the fully-decoded value: digest + noncanonical are proven
\ over the canonical re-render (RENDER-*), stale-promotion over the derived COMPARABLE?.
: BENCH-DECODE-GBPS ( ptr u8 n -- comparison-gbps )
   s" gbps" BST-DECODE-PROLOGUE
   BST-DECODE-IDENTITY
   9  BST-FIELD s" subj=" BST-FIELD-VALUE BST>GBPS
   10 BST-FIELD s" base=" BST-FIELD-VALUE BST>GBPS
   COMPARE-GBPS
   dup RENDER-GBPS BST-CAN!
   BST-CAN$ BST-DIGEST!  BST-CHECK-DIGEST
   BST-CAN$ BST-CHECK-CANON
   dup GBPS-COMPARABLE? BST-CHECK-PROMO ;
: BENCH-DECODE-GFLOPS ( ptr u8 n -- comparison-gflops )
   s" gflops" BST-DECODE-PROLOGUE
   BST-DECODE-IDENTITY
   9  BST-FIELD s" subj=" BST-FIELD-VALUE BST>GFLOPS
   10 BST-FIELD s" base=" BST-FIELD-VALUE BST>GFLOPS
   COMPARE-GFLOPS
   dup RENDER-GFLOPS BST-CAN!
   BST-CAN$ BST-DIGEST!  BST-CHECK-DIGEST
   BST-CAN$ BST-CHECK-CANON
   dup GFLOPS-COMPARABLE? BST-CHECK-PROMO ;

;package
