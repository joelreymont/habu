\ maki/competitive-evidence-store.f - the DURABLE typed store codec for cevid/v1
\ per-side competitive EVIDENCE rows (dot habu-persist-typed-cevid-6f08452c;
\ follow-on from habu-v2-competitive-evidence-5d07d471, which landed the schema).
\
\ CONCERN: crash-safe, content-keyed PUBLICATION of ONE cevid/v1 evidence row and its
\ fresh-process rehydration - a DIFFERENT concern from maki/competitive-evidence.f (the
\ in-memory ring-pool schema + canonical render). This file adds only the persistence
\ codec: it reopens package CEVID and reuses that file's SINGLE canonical render
\ (RENDER) as the on-disk key, so the wire format is never duplicated.
\
\ PRECEDENTS. It blends the two landed store idioms:
\   - maki/competitive-store.f (bench/v1): the KEY is the canonical render - every field
\     participates, so the whole render IS the content-addressed key. Persisted bytes are
\     untrusted input; a rehydrated row either yields the exact typed value or a named
\     reject. The versioned "|schema=" envelope tags the wire so a version bump rejects.
\   - maki/db/diff-case-store.f (CASESTORE): crash-safe ATOMIC-WRITE-FILE (temp+rename)
\     one file per record, content-addressed by SHA-256, with typed ok/absent/malformed
\     LOAD verdicts (a bespoke per-package sum, never a value+flag sentinel) and an
\     embedded-identity integrity check (the file at this key really is this row).
\
\ ---- KEY COMPOSITION (mirrors competitive-store's canonical-render key) ----------------
\ ENCODE derives the KEY = RENDER (the canonical cevid/v1 row: cevid/v1|wl=..|rev=..|
\ sh=..|pol=..|tgt=..|comp=..|cache=..|pr=..|bl=..|lat=..|thr=..|byt=..|launch=..|mem=..|
\ energy=..). EVERY field participates, including cache-state - so cold vs warm is a
\ DIFFERENT key and a DIFFERENT durable file (the T-PATH-DISTINCT proof). The STORE KEY is
\ SHA-256(render); the file is <root>/rows/<hex(store-key)>. A fresh process that rebuilds
\ the identical evidence derives a byte-identical render, the same SHA-256, and finds the
\ record.
\
\ ---- THE RECORD (the versioned envelope over the render) -------------------------------
\ ENCODE emits  <canonical-render>|schema=cevid/v1 . The render already carries field 0
\ "cevid/v1" as its own schema tag; the trailing "|schema=" field is the redundant wire
\ version tag competitive-store's read boundary cross-checks (a version bump is a reject).
\ No separate content digest is embedded: unlike competitive-store's shared append-only
\ file (where the key is not the filename), here the SHA-256 FILENAME already commits to
\ the content, and DECODE's canonical re-render proves the stored bytes are canonical - a
\ redundant checksum would add nothing.
\
\ ---- CRASH-SAFE WRITE ------------------------------------------------------------------
\ PUT derives the key, encodes the whole row, and publishes with ATOMIC-WRITE-FILE
\ (lib/fs-mutate.f temp+rename): a reader ever sees the file absent or the COMPLETE row,
\ never a torn write. Content-addressing makes a re-PUT byte-identical (idempotent).
\ DURABILITY BOUNDARY (shared with the sibling stores, dotted there): rename gives
\ atomicity + PROCESS-crash recovery; a native fsync/dir-sync for power-loss durability is
\ the remaining capability.
\
\ ---- LOAD (fresh-process rehydration + integrity) --------------------------------------
\ LOAD rebuilds the query render from an evidence handle, re-derives the store key, and
\ returns the typed load-result: absent (no durable file), malformed (a present file that
\ fails structural decode / canonicalisation / the content-path identity check), or
\ ok<evidence> (the rehydrated typed handle). DECODE is the standalone untrusted-bytes
\ codec, also returning the verdict sum; its structural throws (schema / fields / label /
\ token / canon, plus the schema's own reading throws E-CEVID-UNIT / E-CEVID-CAP) are
\ mapped to `malformed` at the catch boundary (the maki/db/diff-suite.f DECODE idiom), and
\ any UNMAPPED code is re-thrown - IO / width / root failures never masquerade as
\ malformed. Root and width failures are the only reserved throws.
\
\ Fail closed: an oversized row / read buffer and an empty/over-cap root are named throws;
\ every untrusted-bytes forgery is the `malformed` verdict, never a silent accept.
\ maki -> habu only; competitive-evidence-store owns -5422..-5428.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/competitive-evidence.f   \ reopen package CEVID: RENDER + the typed row + E-CEVID-* codes

-5422 constant E-CEVIDST-ROOT     \ resolved store root path empty or over the path cap
-5423 constant E-CEVIDST-FULL     \ a built row / read buffer exceeds its capacity
-5424 constant E-CEVIDST-FIELDS   \ decode: field count / missing pipe-marker / non-numeric reading value
-5425 constant E-CEVIDST-LABEL    \ decode: a field is not its expected label at its slot
-5426 constant E-CEVIDST-TOKEN    \ decode: an enum / absence / unit token is outside its closed domain
-5427 constant E-CEVIDST-SCHEMA   \ decode: the schema tag / schema field is not cevid/v1
-5428 constant E-CEVIDST-CANON    \ decode: raw key bytes are not the canonical re-render (or content-path mismatch)

package CEVID
public

\ Typed LOAD outcome: ok carries the rehydrated typed evidence handle in its `evidence`
\ field; absent is no durable file; malformed is a present file that fails structural decode /
\ canonicalisation / the content-path identity check. A bespoke per-package sum (the
\ diff-case-store shape), never a value+flag sentinel.
\
\ Declared through the unified ENUM front end in full mode (the arity after the name selects
\ it), so the payload is a named FIELD rather than a positional one. The name is this file's
\ own: the LOAD, DECODE and LOAD-surface contracts all write the outcome as `ok<evidence>`.
\ `FIELD evidence evidence` reads as a repetition but is not one - FIELD takes a name and then
\ a type, and the carried type here is CEVID's own `evidence` product, which is spelled bare
\ inside this package. The generated CEVID-LOAD--RESULT:OK / :ABSENT / :MALFORMED constructors
\ and every MATCH site are unchanged, because both the spellings and the payload binding order
\ derive from the package, the family tail and the declaration order - none of which the mode
\ touches. Note the tail `load-result` is shared with CASESTORE (maki/db/diff-case-store.f):
\ the two families are distinct and never unify, which the suite pins in both directions.
ENUM load-result 0
   VARIANT ok FIELD evidence evidence ;VARIANT
   VARIANT absent ;VARIANT
   VARIANT malformed ;VARIANT
;ENUM

private

\ ---- readable wrappers over the generated constructor spellings -----------------
: LR-OK ( evidence -- load-result )    CEVID-LOAD--RESULT:OK ;
: LR-ABSENT ( -- load-result )         CEVID-LOAD--RESULT:ABSENT ;
: LR-MALFORMED ( -- load-result )      CEVID-LOAD--RESULT:MALFORMED ;

\ ---- framing constants + fixed widths -------------------------------------------
32   constant CKW                  \ SHA-256 content-key width
64   constant HEX-W                \ hex rendering of a 32-byte key
1024 constant CEV-ROW-CAP          \ one built row (render + schema field)
1024 constant CEV-IN-CAP           \ untrusted input row read buffer
17   constant CEV-FIELD-N          \ 16 render fields (0..15) + 1 "schema=" metadata field
$7C  constant CEV-PIPE             \ '|'
$3A  constant CEV-COLON            \ ':'

\ ---- store buffers (all fixed; only lengths live in variables) ------------------
create ROOT-BUF FS-PATH-CAP allot   variable ROOT-U
create SUB-BUF  FS-PATH-CAP allot   variable SUB-U
create PATH-BUF FS-PATH-CAP allot   variable PATH-U
create HEXBUF   HEX-W allot
create KEYBUF   CKW allot                        \ scratch: a row's store key (SHA-256 of the render)
create CEV-KEY  CEV-ROW-CAP allot   variable CEV-KEY-U   \ canonical render (the key), stable
create CEV-ROW  CEV-ROW-CAP allot   variable CEV-ROW-U   \ built full row (encode result), stable
create CEV-IN   CEV-IN-CAP  allot   variable CEV-IN-U    \ untrusted input row, stable copy
create CEV-QR   CEV-ROW-CAP allot   variable CEV-QR-U    \ LOAD query render (content-path check), stable
variable ROOT-SET                                 \ -1 once the root is pinned / resolved
variable LD-EV                                    \ decoded evidence slot (returned through the catch boundary)

\ ---- store root: HABU_CEVID_STORE or a private default --------------------------
: DEFAULT$ ( -- ptr u8 n )   s" tmp/cevid-store" ;

: ENV$ ( -- ptr u8 n )
   s" HABU_CEVID_STORE" GETENV dup 0 > if exit then
   2drop DEFAULT$ ;

: ROOT-SET! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= u FS-PATH-CAP > or if E-CEVIDST-ROOT throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !  true ROOT-SET ! ;

: ROOT-RESOLVE ( -- )
   ROOT-SET @ if exit then
   ENV$ ROOT-SET! ;

public
\ ROOT! pins the store root (a test / fresh-process child driver points it at a private dir).
: ROOT! ( ptr u8 n -- )   ROOT-SET! ;

\ ROOT$ resolves (env or default, once) and returns the store root path.
: ROOT$ ( -- ptr u8 n )   ROOT-RESOLVE ROOT-BUF ROOT-U @ ;

private
\ ---- path builders (distinct buffers so one path survives building the next) ----
: SUB$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}      \ <root>/<name> -> SUB-BUF
   ROOT$ a u SUB-BUF JOIN-PATH SUB-U !
   SUB-BUF SUB-U @ ;

: ROW-PATH$ ( ptr u8 -- ptr u8 n ) {: kp:ptr :}      \ <root>/rows/<hex(store-key)> -> PATH-BUF
   kp HEXBUF SHA256>HEX
   s" rows" SUB$ HEXBUF HEX-W PATH-BUF JOIN-PATH PATH-U !
   PATH-BUF PATH-U @ ;

: ENSURE-DIRS ( -- )
   ROOT$ MAKE-DIRS
   s" rows" SUB$ MAKE-DIRS ;

\ ---- stable holds ---------------------------------------------------------------
: CEV-KEY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CEV-ROW-CAP > if E-CEVIDST-FULL throw then
   a CEV-KEY u BYTE-COPY  u CEV-KEY-U ! ;
: CEV-KEY$ ( -- ptr u8 n )  CEV-KEY CEV-KEY-U @ ;
: CEV-ROW! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CEV-ROW-CAP > if E-CEVIDST-FULL throw then
   a CEV-ROW u BYTE-COPY  u CEV-ROW-U ! ;
: CEV-ROW$ ( -- ptr u8 n )  CEV-ROW CEV-ROW-U @ ;
: CEV-IN! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CEV-IN-CAP > if E-CEVIDST-FULL throw then
   a CEV-IN u BYTE-COPY  u CEV-IN-U ! ;
: CEV-QR! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CEV-ROW-CAP > if E-CEVIDST-FULL throw then
   a CEV-QR u BYTE-COPY  u CEV-QR-U ! ;
: CEV-QR$ ( -- ptr u8 n )  CEV-QR CEV-QR-U @ ;

\ ---- field access over the stable CEV-IN copy (the competitive-store framing) ---
: CEV-PIPE-AT ( n -- n ) {: off:n :}                 \ next '|' at/after off (or length)
   off begin dup CEV-IN-U @ < while
      dup CEV-IN + c@ CEV-PIPE = if exit then
      1+
   repeat ;

: CEV-FIELD-START ( n -- n ) {: k:n :}               \ byte offset where field k begins
   0                                                 \ advance past k '|' delimiters from 0
   k 0 ?do
      CEV-PIPE-AT
      dup CEV-IN-U @ >= if E-CEVIDST-FIELDS throw then
      1+
   loop ;

: CEV-FIELD ( n -- ptr u8 n ) {: k:n :}              \ the k-th '|' field slice
   k CEV-FIELD-START {: start:n :}
   start CEV-PIPE-AT {: ed:n :}
   CEV-IN start +  ed start - ;

: CEV-PIPES ( -- n )                                 \ '|' count in CEV-IN
   0  0 begin dup CEV-IN-U @ < while
      dup CEV-IN + c@ CEV-PIPE = if swap 1+ swap then
      1+
   repeat drop ;
: CEV-NFIELDS ( -- n )  CEV-PIPES 1+ ;

: CEV-FIELD-VALUE ( ptr u8 n ptr u8 n -- ptr u8 n ) {: fa:ptr fu:n la:ptr lu:n :}
   fa fu la lu STARTS-WITH? 0= if E-CEVIDST-LABEL throw then
   fa lu +  fu lu - ;

: CEV-KEYSLICE$ ( -- ptr u8 n )                      \ raw bytes before "|schema=" = the render key
   CEV-FIELD-N 1- CEV-FIELD-START {: s:n :}
   CEV-IN  s 1- ;

\ ---- token -> typed family (fail closed on an out-of-domain token; inverts the ----
\ ---- competitive-evidence.f WL$/SH$/PR$/BL$/CACHE$/ABS$/REV$/COMP$/U-NAME wires) --
: CEV>WORKLOAD ( ptr u8 n -- BENCH:workload ) {: a:ptr u:n :}
   a u s" saxpy" STR= if BENCH-WORKLOAD:SAXPY exit then
   a u s" gemm"  STR= if BENCH-WORKLOAD:GEMM  exit then
   E-CEVIDST-TOKEN throw ;
: CEV>REVISION ( ptr u8 n -- revision ) {: a:ptr u:n :}
   a u s" saxpy-v4"            STR= if CEVID-REVISION:SAXPY-V4     exit then
   a u s" mm-cp-async-blocked" STR= if CEVID-REVISION:MM-BLOCKED   exit then
   a u s" mmm-wide-b-m4-s1"    STR= if CEVID-REVISION:MMM-WIDE-B   exit then
   a u s" triton-jit"          STR= if CEVID-REVISION:TRITON-JIT   exit then
   a u s" triton-autotuned"    STR= if CEVID-REVISION:TRITON-TUNED exit then
   E-CEVIDST-TOKEN throw ;
: CEV>SHAPE ( ptr u8 n -- BENCH:shape ) {: a:ptr u:n :}
   a u s" n1m"    STR= if BENCH-SHAPE:N1M    exit then
   a u s" sq2048" STR= if BENCH-SHAPE:SQ2048 exit then
   E-CEVIDST-TOKEN throw ;
: CEV>NPOL ( ptr u8 n -- NPOL:dom ) {: a:ptr u:n :}
   a u s" exact" STR= if NPOL-DOM:EXACT     exit then
   a u s" ulp"   STR= if NPOL-DOM:ULP       exit then
   a u s" rel"   STR= if NPOL-DOM:RELATIVE  exit then
   a u s" emp"   STR= if NPOL-DOM:EMPIRICAL exit then
   E-CEVIDST-TOKEN throw ;
: CEV>TARGET ( ptr u8 n -- CAD-KIND:target-id ) {: a:ptr u:n :}
   a u s" sm_87" STR= if TARGET:SM87 exit then      \ the only corpus target (orin-nx sm_87)
   E-CEVIDST-TOKEN throw ;
: CEV>COMPILER ( ptr u8 n -- compiler ) {: a:ptr u:n :}
   a u s" ptxas-12.6"   STR= if CEVID-COMPILER:HABU-PTXAS exit then
   a u s" triton-3.5.1" STR= if CEVID-COMPILER:TRITON-351 exit then
   E-CEVIDST-TOKEN throw ;
: CEV>CACHE ( ptr u8 n -- BENCH:cache-state ) {: a:ptr u:n :}
   a u s" cold" STR= if BENCH-CACHE--STATE:COLD exit then
   a u s" warm" STR= if BENCH-CACHE--STATE:WARM exit then
   E-CEVIDST-TOKEN throw ;
: CEV>PROTOCOL ( ptr u8 n -- BENCH:protocol ) {: a:ptr u:n :}
   a u s" cuda-events" STR= if BENCH-PROTOCOL:CUDA-EVENTS exit then
   a u s" wallclock"   STR= if BENCH-PROTOCOL:WALLCLOCK   exit then
   E-CEVIDST-TOKEN throw ;
: CEV>BASELINE ( ptr u8 n -- BENCH:baseline ) {: a:ptr u:n :}
   a u s" triton" STR= if BENCH-BASELINE:TRITON exit then
   a u s" cublas" STR= if BENCH-BASELINE:CUBLAS exit then
   E-CEVIDST-TOKEN throw ;
: CEV>ABSENCE ( ptr u8 n -- BENCH:absence ) {: a:ptr u:n :}
   a u s" not-measured" STR= if BENCH-ABSENCE:NOT-MEASURED exit then
   a u s" device-gated" STR= if BENCH-ABSENCE:DEVICE-GATED exit then
   a u s" waived"       STR= if BENCH-ABSENCE:WAIVED       exit then
   E-CEVIDST-TOKEN throw ;
: CEV>UNIT ( ptr u8 n -- unit ) {: a:ptr u:n :}
   a u s" ns"     STR= if CEVID-UNIT:U-NS     exit then
   a u s" ms"     STR= if CEVID-UNIT:U-MS     exit then
   a u s" gflops" STR= if CEVID-UNIT:U-GFLOPS exit then
   a u s" gbps"   STR= if CEVID-UNIT:U-GBPS   exit then
   a u s" bytes"  STR= if CEVID-UNIT:U-BYTES  exit then
   a u s" count"  STR= if CEVID-UNIT:U-COUNT  exit then
   a u s" watts"  STR= if CEVID-UNIT:U-WATTS  exit then
   E-CEVIDST-TOKEN throw ;

: CEV-INT ( ptr u8 n -- n )                          \ a numeric reading value (fail closed)
   STR>NUMBER? MATCH option
      none OF E-CEVIDST-FIELDS throw ENDOF
      some OF ENDOF
   ;MATCH ;

: CEV-COLON-AT ( ptr u8 n -- n ) {: a:ptr u:n :}     \ index of the first ':' (or the length)
   0 begin dup u < while
      dup a + c@ CEV-COLON = if exit then
      1+
   repeat ;

\ ---- reading decode: "na:<absence>" or "<value>:<unit>" -> reading ---------------
\ VAL-CK (an over-cap forged value) + the field setters' category check (a wrong-category
\ forged unit) are the schema's own throws E-CEVID-CAP / E-CEVID-UNIT; DECODE maps both to
\ the `malformed` verdict at the catch boundary.
: CEV>READING ( ptr u8 n -- reading ) {: a:ptr u:n :}
   a u s" na:" STARTS-WITH? if
      a 3 + u 3 - CEV>ABSENCE CEVID-READING:RD-NA exit
   then
   a u CEV-COLON-AT {: ci:n :}
   ci u >= if E-CEVIDST-FIELDS throw then            \ a present reading must carry ':'
   a ci CEV-INT VAL-CK
   a ci 1+ +  u ci 1+ -  CEV>UNIT
   CEVID-READING:RD-AT ;

\ ---- structural checks ----------------------------------------------------------
: CEV-CHECK-COUNT ( -- )
   CEV-NFIELDS CEV-FIELD-N <> if E-CEVIDST-FIELDS throw then ;
: CEV-CHECK-SCHEMA-TAG ( -- )                        \ render field 0 is the cevid/v1 schema tag
   0 CEV-FIELD s" cevid/v1" STR= 0= if E-CEVIDST-SCHEMA throw then ;
: CEV-CHECK-META-SCHEMA ( -- )                       \ the trailing metadata schema field == cevid/v1
   CEV-FIELD-N 1- CEV-FIELD s" schema=" CEV-FIELD-VALUE s" cevid/v1" STR= 0=
   if E-CEVIDST-SCHEMA throw then ;

\ ---- identity decode (the nine nominal / policy / target render fields) ----------
: CEV-DECODE-IDENTITY ( -- BENCH:workload revision BENCH:shape NPOL:dom CAD-KIND:target-id compiler BENCH:cache-state BENCH:protocol BENCH:baseline )
   1 CEV-FIELD s" wl="    CEV-FIELD-VALUE CEV>WORKLOAD
   2 CEV-FIELD s" rev="   CEV-FIELD-VALUE CEV>REVISION
   3 CEV-FIELD s" sh="    CEV-FIELD-VALUE CEV>SHAPE
   4 CEV-FIELD s" pol="   CEV-FIELD-VALUE CEV>NPOL
   5 CEV-FIELD s" tgt="   CEV-FIELD-VALUE CEV>TARGET
   6 CEV-FIELD s" comp="  CEV-FIELD-VALUE CEV>COMPILER
   7 CEV-FIELD s" cache=" CEV-FIELD-VALUE CEV>CACHE
   8 CEV-FIELD s" pr="    CEV-FIELD-VALUE CEV>PROTOCOL
   9 CEV-FIELD s" bl="    CEV-FIELD-VALUE CEV>BASELINE ;

\ ---- reading decode: rebuild the six readings onto the fresh handle -------------
\ Each setter re-imposes its field's unit CATEGORY, so a forged wrong-category unit is the
\ schema's E-CEVID-UNIT throw (mapped to malformed), not a silent accept.
: CEV-APPLY-READINGS ( evidence -- evidence )
   10 CEV-FIELD s" lat="    CEV-FIELD-VALUE CEV>READING LAT!
   11 CEV-FIELD s" thr="    CEV-FIELD-VALUE CEV>READING THR!
   12 CEV-FIELD s" byt="    CEV-FIELD-VALUE CEV>READING BYT!
   13 CEV-FIELD s" launch=" CEV-FIELD-VALUE CEV>READING LAUNCH!
   14 CEV-FIELD s" mem="    CEV-FIELD-VALUE CEV>READING MEM!
   15 CEV-FIELD s" energy=" CEV-FIELD-VALUE CEV>READING ENERGY! ;

\ ---- DECODE-RUN: the throwing inverse of ENCODE over the stable CEV-IN ------------
\ Rebuilds the typed evidence, then proves the stored render key IS the canonical render of
\ the rebuilt handle (a non-canonical spelling is E-CEVIDST-CANON).
: DECODE-RUN ( -- evidence )
   CEV-CHECK-COUNT
   CEV-CHECK-SCHEMA-TAG
   CEV-CHECK-META-SCHEMA
   CEV-DECODE-IDENTITY ROW
   CEV-APPLY-READINGS
   dup RENDER CEV-KEYSLICE$ STR= 0= if E-CEVIDST-CANON throw then ;

\ ---- LOAD-RUN: DECODE-RUN + the content-path identity check ----------------------
\ The file at SHA-256(query-render) must decode to the SAME render (a file planted at this
\ content key under different bytes is E-CEVIDST-CANON -> malformed).
: LOAD-RUN ( -- evidence )
   DECODE-RUN
   dup RENDER CEV-QR$ STR= 0= if E-CEVIDST-CANON throw then ;

\ ---- catch boundary: map the decode-domain throws to `malformed`, re-throw the rest
: CEV-MALFORMED? ( n -- bool )
   dup E-CEVIDST-SCHEMA =
   over E-CEVIDST-FIELDS = or
   over E-CEVIDST-LABEL  = or
   over E-CEVIDST-TOKEN  = or
   over E-CEVIDST-CANON  = or
   over E-CEVID-UNIT     = or
   over E-CEVID-CAP      = or
   nip ;

: DEC-VERDICT ( n -- load-result )
   dup 0= if drop LD-EV @ >EV LR-OK exit then
   dup CEV-MALFORMED? if drop LR-MALFORMED exit then
   throw ;

public

\ ---- the canonical codec: typed evidence <-> byte-stable versioned row -----------
\ ENCODE derives the key (RENDER, stashed stable) then wraps it in the "|schema=" envelope.
: ENCODE ( evidence -- ptr u8 n )
   RENDER CEV-KEY!
   SB-RESET
   CEV-KEY$ SB-APPEND
   s" |schema=cevid/v1" SB-APPEND
   SB$ CEV-ROW! CEV-ROW$ ;

\ DECODE treats the bytes as untrusted and returns ok<evidence> | malformed (never absent -
\ absence is a file-presence concept). Structural throws become the malformed verdict.
: DECODE ( ptr u8 n -- load-result ) {: a:ptr u:n :}
   u CEV-IN-CAP > if E-CEVIDST-FULL throw then
   a u CEV-IN!
   [: DECODE-RUN EV> LD-EV ! ;] catch DEC-VERDICT ;

\ ---- durable store surface (the diff-case-store shape) ---------------------------
\ PUT durably publishes one evidence row, keyed by SHA-256(render). Crash-safe (temp+rename)
\ and idempotent (a re-PUT is byte-identical).
: PUT ( evidence -- )
   ENCODE {: ra:ptr ru:n :}                          \ stashes CEV-KEY (render), returns the row
   ENSURE-DIRS
   CEV-KEY$ KEYBUF SHA256
   KEYBUF ROW-PATH$ {: pa:ptr pu:n :}
   pa pu ra ru ATOMIC-WRITE-FILE ;

\ HAS? is true once a durable file exists for an evidence's content key.
: HAS? ( evidence -- bool )
   RENDER CEV-KEY!  CEV-KEY$ KEYBUF SHA256
   KEYBUF ROW-PATH$ FILE? ;

\ PATH$ is the content-addressed absolute path of an evidence's durable row (valid until the
\ next path build). A pure function of the evidence's canonical render.
: PATH$ ( evidence -- ptr u8 n )
   RENDER CEV-KEY!  CEV-KEY$ KEYBUF SHA256
   KEYBUF ROW-PATH$ ;

\ LOAD rehydrates an evidence's durable row: absent (no file) / malformed (present but the
\ file fails decode / canonicalisation / the content-path identity check) / ok<evidence>.
: LOAD ( evidence -- load-result )
   RENDER CEV-QR!                                    \ the query render (stable) for the content-path check
   CEV-QR$ KEYBUF SHA256
   KEYBUF ROW-PATH$ {: pa:ptr pu:n :}
   pa pu FILE? 0= if LR-ABSENT exit then
   pa pu FILE-SIZE CEV-IN-CAP > if E-CEVIDST-FULL throw then
   pa pu CEV-IN CEV-IN-CAP READ-ALL CEV-IN-U !
   [: LOAD-RUN EV> LD-EV ! ;] catch DEC-VERDICT ;

\ ---- test-only store lifecycle --------------------------------------------------
\ ROOT+ resolves and CREATES the store root + rows dir, returning the root path.
: ROOT+ ( -- ptr u8 n )   ENSURE-DIRS ROOT$ ;

\ RESET removes the whole store tree (test-only); a missing store is a no-op.
: RESET ( -- )
   ROOT$ {: a:ptr u:n :}
   u 0 > if a u EXISTS? if a u REMOVE-TREE then then ;

;package
