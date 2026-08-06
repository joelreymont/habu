\ maki/train-state.f - checked training-state checkpoint codec (save/load for resume).
\
\ nanoGPT training-state checkpointing persists what a trainer needs to RESUME a
\ run bit-identically: the model parameters, the optimizer moments, and the step
\ counter. This is a DIFFERENT concern from maki/checkpoint.f, which is gradient
\ (activation) checkpointing - the lower-memory recompute of the SAME step, not
\ cross-run persistence. This file owns only the on-disk codec; the trainer
\ (maki/adam-train.f) registers which buffers make up its resume state.
\
\ ---- store-layer verification (dot demanded it before writing a new format) ----
\ None of the existing store layers round-trips FLOAT BUFFERS bit-identically:
\   maki/store.f          line-oriented TEXT rows of a key + integer/enum fields;
\                         torn-tail rejection (E-STORE-TORN) but no per-byte
\                         checksum and no float payload. Not a buffer store.
\   maki/competitive-store.f  typed BENCH rows with an FNV-1a-64 canonical digest -
\                         a good INTEGRITY pattern to reuse, but the payload is
\                         enum/integer fields, never float buffers.
\   maki/golden-artifact.f    DOES serialize float buffers, but as 9-decimal text
\                         compared under a TOLERANCE (GA-DECIMALS / GA-WITHIN?).
\                         Decimal text is lossy for f64 and tolerance is not
\                         bit-exact, so a resume built on it would not be
\                         bit-identical. Wrong tool for resume.
\ Verdict: no existing layer fits, so this codec writes the RAW 64-bit cells (Habu
\ floats ARE cells; @/! move them losslessly, maki/array.f) and REUSES the proven
\ FNV-1a-64 integrity construction from maki/competitive-store.f and the store-root
\ discipline (STORE-ROOT+) from maki/store.f. It reuses over reinventing where a
\ fit existed, and adds only the bit-exact float-buffer frame nothing else provides.
\
\ ---- on-disk frame (all raw 64-bit cells; the file never leaves the host) ------
\   cell 0            magic  ("HBTSTAT1")
\   cell 1            version
\   cell 2            ncells (payload cell count)
\   cell 3 .. 3+N     payload (registered segments, in registration order)
\   cell 3+N          FNV-1a-64 digest over cells [0, 3+N)
\ A registered segment is a (base-address, cell-count) span; save copies buffer
\ cells into the payload, load copies them back. Determinism: save is a pure copy
\ + deterministic hash, so save->load->save is byte-identical.
\
\ ---- fail closed (each a distinct named throw; nothing partially loaded) --------
\ LOAD reads the whole file into a scratch image and VALIDATES every axis BEFORE it
\ writes a single cell into any registered buffer, so a failed load leaves the
\ trainer state exactly as it was (atomic):
\   missing file                       E-TSC-MISSING
\   truncated / size not header+payload+digest for the declared ncells / unaligned
\                                      E-TSC-TRUNC
\   bad magic or version               E-TSC-MAGIC
\   flipped byte (digest mismatch)     E-TSC-DIGEST
\   declared ncells != registered schema total (wrong-shape / different sizes)
\                                      E-TSC-SHAPE
\   segment table or image over capacity  E-TSC-CAP
\ Digest is checked BEFORE shape so an intact-but-wrong-shape file fails as SHAPE
\ while a corrupted one fails as DIGEST. maki -> habu only; owns -5213..-5218.

require lib/prelude.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require maki/store.f

-5213 constant E-TSC-MISSING   \ load: the checkpoint file does not exist
-5214 constant E-TSC-TRUNC     \ file size is not header+payload+digest for the declared cell count / unaligned
-5215 constant E-TSC-MAGIC     \ magic or version cell is not this codec's
-5216 constant E-TSC-SHAPE     \ declared payload cell count disagrees with the registered segment schema
-5217 constant E-TSC-DIGEST    \ content digest mismatch (a flipped byte)
-5218 constant E-TSC-CAP       \ segment table / image exceeds capacity

package MAKI
private

\ ---- frame constants --------------------------------------------------------
$4842545354415431 constant TSC-MAGIC     \ "HBTSTAT1" packed into one cell
1    constant TSC-VERSION
3    constant TSC-HDR-CELLS               \ magic + version + ncells
64   constant TSC-SEG-MAX                 \ registered segments
4096 constant TSC-PAY-MAX                 \ payload cells (image = header + payload + digest)
TSC-HDR-CELLS TSC-PAY-MAX + 1 + cells constant TSC-CAP-BYTES

create TSC-IMG     TSC-CAP-BYTES allot
create TSC-SEG-PTR TSC-SEG-MAX cells allot
create TSC-SEG-LEN TSC-SEG-MAX cells allot
variable TSC-NSEG                          \ registered segment count
variable TSC-TOTAL                         \ sum of registered segment cell counts
variable TSC-CUR                           \ image read/write cursor (cell index)

create TSC-PATH-BUF FS-PATH-CAP allot   variable TSC-PATH-U
create TSC-DIR-BUF  FS-PATH-CAP allot   variable TSC-DIR-U
create TSC-FILE-BUF 96 allot            variable TSC-FILE-U

\ ---- FNV-1a-64 content digest over bytes (same construction as the
\ competitive-store row digest: a bit flip anywhere changes the hash) -----------
$cbf29ce484222325 constant TSC-FNV-BASIS
$100000001b3       constant TSC-FNV-PRIME
: TSC-FNV ( ptr u8 n -- n ) {: a:ptr u:n :}
   TSC-FNV-BASIS
   u 0 ?do  a i + c@ xor  TSC-FNV-PRIME *  loop ;

\ ---- image cell accessors (raw cells: bit copy, representation-agnostic) -------
: TSC-CELL! ( n n -- )  cells TSC-IMG + ! ;       \ ( val cell-index -- )
: TSC-CELL@ ( n -- n )  cells TSC-IMG + @ ;       \ ( cell-index -- val )

\ ---- path: <store-root>/ckpt/<name>.ckpt ------------------------------------
: TSC-FILE$ ( ptr u8 n -- ptr u8 n ) {: na:ptr nu:n :}     \ "<name>.ckpt"
   nu 6 + 96 > if E-TSC-CAP throw then
   na TSC-FILE-BUF nu BYTE-COPY
   s" .ckpt" {: sa:ptr su:n :}
   sa TSC-FILE-BUF nu + su BYTE-COPY
   nu su + TSC-FILE-U !
   TSC-FILE-BUF TSC-FILE-U @ ;
: TSC-DIR$ ( -- ptr u8 n )                                 \ <store-root>/ckpt (root ensured)
   STORE-ROOT+ s" ckpt" TSC-DIR-BUF JOIN-PATH TSC-DIR-U !
   TSC-DIR-BUF TSC-DIR-U @ ;

public
: TSC-PATH$ ( ptr u8 n -- ptr u8 n ) {: na:ptr nu:n :}     \ resolved checkpoint path (test corruption seam)
   TSC-DIR$ {: da:ptr du:n :}
   na nu TSC-FILE$ {: fa:ptr fu:n :}
   da du fa fu TSC-PATH-BUF JOIN-PATH TSC-PATH-U !
   TSC-PATH-BUF TSC-PATH-U @ ;
: TSC-EXISTS? ( ptr u8 n -- bool )  TSC-PATH$ FILE? ;

\ ---- segment registration (both save and load are driven by this schema) ------
: TSC-BEGIN ( -- )  0 TSC-NSEG !  0 TSC-TOTAL ! ;
: TSC-SEG ( ptr a n -- ) {: base:ptr cnt:n :}
   TSC-NSEG @ TSC-SEG-MAX >= if E-TSC-CAP throw then
   TSC-TOTAL @ cnt + TSC-PAY-MAX > if E-TSC-CAP throw then
   base TSC-SEG-PTR TSC-NSEG @ cells + !
   cnt  TSC-SEG-LEN TSC-NSEG @ cells + !
   TSC-TOTAL @ cnt + TSC-TOTAL !
   TSC-NSEG @ 1+ TSC-NSEG ! ;
private

\ ---- save: build the image from the registered segments -----------------------
: TSC-PUT-SEG ( ptr n n -- ) {: base:ptr cnt:n :}
   cnt 0 ?do
      base i cells + @  TSC-CUR @ TSC-CELL!
      TSC-CUR @ 1+ TSC-CUR !
   loop ;
: TSC-BUILD ( -- n )                                       \ image byte length
   TSC-MAGIC   0 TSC-CELL!
   TSC-VERSION 1 TSC-CELL!
   TSC-TOTAL @ 2 TSC-CELL!
   TSC-HDR-CELLS TSC-CUR !
   TSC-NSEG @ 0 ?do
      TSC-SEG-PTR i cells + @  TSC-SEG-LEN i cells + @  TSC-PUT-SEG
   loop
   TSC-HDR-CELLS TSC-TOTAL @ + {: nhp:n :}                 \ header + payload cell count
   TSC-IMG nhp cells TSC-FNV  nhp TSC-CELL!                \ digest at cell nhp
   nhp 1+ cells ;

\ ---- load: read + validate (atomic: no segment touched until all checks pass) --
: TSC-READ ( ptr u8 n -- n ) {: na:ptr nu:n :}             \ whole file -> TSC-IMG, return byte length
   na nu TSC-EXISTS? 0= if E-TSC-MISSING throw then
   na nu TSC-PATH$ {: pa:ptr pu:n :}
   pa pu FILE-SIZE TSC-CAP-BYTES > if E-TSC-CAP throw then
   pa pu TSC-IMG TSC-CAP-BYTES READ-ALL ;
: TSC-VALIDATE ( n -- ) {: nbytes:n :}
   nbytes TSC-HDR-CELLS 1+ cells < if E-TSC-TRUNC throw then    \ smaller than header + digest
   0 TSC-CELL@ TSC-MAGIC   <> if E-TSC-MAGIC throw then
   1 TSC-CELL@ TSC-VERSION <> if E-TSC-MAGIC throw then
   2 TSC-CELL@ {: ncells:n :}
   ncells 0 < if E-TSC-TRUNC throw then
   ncells TSC-PAY-MAX > if E-TSC-TRUNC throw then
   TSC-HDR-CELLS ncells + 1+ cells nbytes <> if E-TSC-TRUNC throw then   \ declared size == actual
   TSC-IMG TSC-HDR-CELLS ncells + cells TSC-FNV
      TSC-HDR-CELLS ncells + TSC-CELL@ <> if E-TSC-DIGEST throw then     \ flipped byte
   ncells TSC-TOTAL @ <> if E-TSC-SHAPE throw then ;                     \ wrong shape (intact file)

\ ---- restore: copy the validated payload back into the registered segments -----
: TSC-GET-SEG ( ptr n n -- ) {: base:ptr cnt:n :}
   cnt 0 ?do
      TSC-CUR @ TSC-CELL@  base i cells + !
      TSC-CUR @ 1+ TSC-CUR !
   loop ;
: TSC-RESTORE ( -- )
   TSC-HDR-CELLS TSC-CUR !
   TSC-NSEG @ 0 ?do
      TSC-SEG-PTR i cells + @  TSC-SEG-LEN i cells + @  TSC-GET-SEG
   loop ;

public
\ TSC-SAVE writes the registered segments to <root>/ckpt/<name>.ckpt.
: TSC-SAVE ( ptr u8 n -- ) {: na:ptr nu:n :}
   TSC-BUILD {: nbytes:n :}
   TSC-DIR$ MAKE-DIRS
   na nu TSC-PATH$ TSC-IMG nbytes WRITE-ALL ;

\ TSC-LOAD validates the file fully, then restores it into the registered segments;
\ any failure throws BEFORE a segment is touched (a failed load is a no-op).
: TSC-LOAD ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu TSC-READ TSC-VALIDATE
   TSC-RESTORE ;

;package
