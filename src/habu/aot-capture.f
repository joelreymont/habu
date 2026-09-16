\ aot-capture.f — host-only AOT-REPL capture (metabuild build step).
\
\ Scans the metabuild host's freshly-compiled words for inter-word call sites
\ (one direct `BL imm26` each, habu2.f LCEMITBL), reverse-looks-up each callee's
\ dict NAME, and builds the four AOT buffers (src/habu/aot-decl.f) that EMIT-AOT-SEED bakes
\ into bin/hb: a code blob, N dict records (xt/end blob-relative, inline name),
\ a call-site relocation table (blob-offset -> callee name-pool ref), and a name
\ pool. At boot EM-SEED-AOT copies the blob, registers the records, and re-encodes
\ each call site's imm26 to the callee's address in THAT engine (via LFIND) —
\ so the captured code needs no address to match host vs bin/hb.
\
\ Canonicalization: after recording a call site the BL's imm26 in the blob is ZEROED
\ (bare `bl #0`), so the baked blob is deterministic across builds (the host address
\ delta is ASLR-varying and is re-patched at boot anyway).
\
\ Loaded ONLY in the stdin metabuild (after habu2.f, before stdin.f). NOT baked
\ into bin/hb — host build-time meta-words over the host dictionary. Raw dict/code
\ boundary is confined to the TRUSTED:/TRUST casts below (no `0 set-check` span, so
\ the checked build stays fail-closed through the image writer).

require src/habu/address-cells.f
require src/habu/code-span.f

package AOT-CAPTURE

\ The buffers this file fills are package AOT-BUF's public surface
\ (src/habu/aot-decl.f), read here by their bare names; the import closes with
\ the package.
using AOT-BUF

\ --- raw dict/code boundary casts (host build-time only). AOT-DBASE names only
\ the dictionary record region; live engine registries are under AOT-LIVE-DATA. ---
\ The casts expose record addresses, byte views, and record cells for reverse lookup.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: AOT-DBASE ( -- ptr n ) dbase@ ;
TRUSTED: AOT-DBASE-N ( -- n ) dbase@ ;
TRUSTED: AOT-DATA-N ( -- n ) data-base ;
TRUSTED: AOT-A>U8 ( ptr n -- ptr u8 ) ;
TRUSTED: AOT-N>U8 ( n -- ptr u8 ) ;
: AOT-LIVE-DATA ( -- ptr n ) data-base ;
: AOT-CELL@ ( ptr n -- n ) @ ;
: AOT-N-C! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store a full cell as 8 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c!
   v 32 rshift p 4 + c!  v 40 rshift p 5 + c!  v 48 rshift p 6 + c!  v 56 rshift p 7 + c! ;
: AOT-P32! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 32 bits as 4 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;

\ --- host dictionary record k (48 bytes): field readers (ptr-first byte offsets) ---
: AOT-REC ( n -- ptr n ) 48 * AOT-DBASE swap + ;
: AOT-RXT ( ptr n -- n ) AOT-CELL@ ;                          \ [0] code entry (xt)
: AOT-RLEN ( ptr n -- n ) 8 + AOT-CELL@ ;                     \ raw [8]: encoded code length or package private WID
: AOT-RBODY ( ptr n -- n ) AOT-RLEN dup CODE-SPAN:CHECK CODE-SPAN:BODY ;
: AOT-RBYTES ( ptr n -- n ) AOT-RLEN CODE-SPAN:BYTES ;
: AOT-RFLAGS ( ptr n -- n ) 16 + AOT-CELL@ ;                  \ [16] flags | name len
: AOT-RNLEN ( ptr n -- n ) AOT-RFLAGS $0003FFFFFFFFFFFF and ;   \ = DNAME-LEN-MASK (top 14 bits are flags + DNAME-MIN-IN + DKIND)
: AOT-REXT? ( ptr n -- bool ) AOT-RFLAGS $2000000000000000 and 0= 0= ;
: AOT-RNPTR ( ptr n -- ptr u8 )
   dup AOT-REXT? if 24 + AOT-CELL@ AOT-N>U8 else AOT-A>U8 24 + then ;
: AOT-RWID ( ptr n -- n ) 40 + AOT-CELL@ ;                    \ [40] wordlist or -1 package sentinel

\ --- 32-bit little-endian code word; direct `BL imm26` call recognise + decode.
\ Every statically known native call is now one BL (habu2.f LCEMITBL); the callee's
\ absolute host address = the site's original code address + sign-extended(imm26)*4.
\ The site sits in the copied blob buffer, so its original address is AOT-CODE-B0
\ (the capture-time code base) + the byte offset of the site within the blob.
\ ACAP-TGT decodes imm26, which B and BL share, so it reads both.
: ACAP-W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
: ACAP-TGT ( ptr u8 -- n ) {: p:ptr :}            \ absolute callee address of the BL at p
   p ACAP-W32@ $3FFFFFF and  $2000000 xor $2000000 -  2 lshift    \ sign-extended imm26 * 4
   AOT-CODE-B0 @  p AOT-BLOB-BUF@ -  +  + ;                       \ + (B0 + site blob offset)
: ACAP-CALL? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FC000000 and $94000000 = ;
: ACAP-BRANCH? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FC000000 and $14000000 = ;
\ The window's own code, as the copy holds it. The blob moves rigidly — the seed
\ copies it whole and the merge appends it whole — so a branch that lands inside
\ it keeps its displacement and needs no name.
: ACAP-IN-CODE? ( n -- bool ) {: t:n :}
   t AOT-CODE-B0 @ < if 0 0= 0= exit then
   t AOT-CODE-B0 @ AOT-BLOB-LEN @ + < ;
: ACAP-ZERO-IMM ( ptr u8 -- ) {: p:ptr :}         \ zero the imm26 -> bare `bl #0`, for build determinism
   p ACAP-W32@ $FC000000 and  p AOT-P32! ;

\ --- reverse lookup: absolute call target (host xt) -> its dict record index ---
\ WHY THIS IS AN INDEX AND NOT A SCAN. Both callers ask once PER SITE - the BL
\ scan below for every call in the copied blob, ACAP-OUT-CHAIN for every recorded
\ address chain the window's DATA span does not hold - so a walk of the whole
\ dictionary per question is quadratic in the size of the captured window. On the
\ REPL window this file was written for that is 217 sites against 8,545 records
\ (1.9M record reads, 7.6 ms); on the compiler chain it is 13,674 sites against
\ 14,481 records, which is 198M reads and 1.16 s of every `install --force`.
\ The engine's own name lookup had this disease and this cure, but its hash index
\ cannot answer THIS question: it is keyed on a name and a wordlist (habu1.f
\ C-HIDX-HASH) and what a call site knows is a code address. Hence a local index.
\
\ AN XT DOES NOT NAME A RECORD, and the tie-break is not a detail. `EXPORT` gives
\ one body a SECOND record under a second name (habu2.f C-EXPORT), so two records
\ can carry one xt, and the scan this replaces answered the LOWEST of them. The
\ index reproduces that exactly rather than approximately: records are inserted in
\ ascending order and an insert whose probe chain already holds a record with the
\ same xt does nothing, so an xt's entry is the first index that ever carried it.
\
\ THE ANSWER IS VERIFIED, NEVER TRUSTED. A slot holds a record index and the probe
\ re-reads that record's live [0] before accepting it, so a wrong slot can only
\ make the probe walk on - it can never rename a call site. Staleness is refused
\ rather than tolerated: the build stamps the ndict it indexed and every lookup
\ checks it, so an index built for a different dictionary - or not built at all,
\ since the stamp starts at 0 and ndict never is - ends the build instead of
\ quietly reporting live call sites unresolved.
DICT-CAP 2 * constant ACAP-TIDX-SLOTS          \ 2x the dictionary bound, so the load stays at
create ACAP-TIDX ACAP-TIDX-SLOTS 4 * allot     \ or under half and a probe always meets an empty
variable ACAP-TIDX-N                           \ slot. u32 slots: 0 empty, else record index + 1
variable ACAP-TIDX-ND                          \ the ndict this index was built for (0 = none)
variable ACAP-TS                               \ probe cursor

: ACAP-TIDX@ ( n -- n ) 4 * ACAP-TIDX + ACAP-W32@ ;
: ACAP-TIDX! ( n n -- ) {: v:n s:n :} v  s 4 * ACAP-TIDX +  AOT-P32! ;
: ACAP-TIDX-STEP ( -- ) ACAP-TS @ 1+ ACAP-TIDX-SLOTS 1 - and ACAP-TS ! ;

\ An entry's slot. Xts are instruction-aligned so the low two bits carry nothing;
\ the rest goes through Knuth's multiplicative hash and the product's high half is
\ folded down, so bodies laid out a constant distance apart do not share a chain.
: ACAP-TIDX-HASH ( n -- n ) {: xt:n :}
   xt 2 rshift 2654435761 *  $FFFFFFFF and {: g:n :}
   g 16 rshift  g xor  ACAP-TIDX-SLOTS 1 - and ;

\ The linear reference the index replaces. It is the specification of the answer
\ and ACAP-TIDX-PROVE below holds the index against it on the live dictionary at
\ every capture; nothing on the capture's hot path calls it.
: ACAP-TGT>SCAN ( n -- n ) {: tgt:n :}
   ndict@ 0 ?do
      i AOT-REC AOT-RXT tgt = if i unloop exit then
   loop
   -1 ;

: ACAP-TGT>REC ( n -- n ) {: tgt:n :}
   ndict@ ACAP-TIDX-ND @ <> if
      s" aot-capture: target index was not built for the live dictionary" 74 die
   then
   tgt ACAP-TIDX-HASH ACAP-TS !
   ACAP-TIDX-SLOTS 0 ?do
      ACAP-TS @ ACAP-TIDX@ {: e:n :}
      e 0= if -1 unloop exit then                       \ empty slot: the xt is in no record
      e 1- AOT-REC AOT-RXT tgt = if e 1- unloop exit then
      ACAP-TIDX-STEP
   loop
   -1 ;

: ACAP-TIDX-INS ( n -- ) {: k:n :}             \ index record k; the lowest index per xt wins
   k AOT-REC AOT-RXT {: xt:n :}
   xt ACAP-TIDX-HASH ACAP-TS !
   ACAP-TIDX-SLOTS 0 ?do
      ACAP-TS @ ACAP-TIDX@ {: e:n :}
      e 0= if
         k 1+ ACAP-TS @ ACAP-TIDX!
         ACAP-TIDX-N @ 1+ ACAP-TIDX-N !
         unloop exit
      then
      e 1- AOT-REC AOT-RXT xt = if unloop exit then     \ a lower index already carries this xt
      ACAP-TIDX-STEP
   loop
   s" aot-capture: target index full" 74 die ;

: ACAP-TIDX-BUILD ( -- )                       \ index the live dictionary, ascending
   0 ACAP-TIDX-ND !
   ndict@ DICT-CAP > if
      s" aot-capture: dictionary above the target index bound" 74 die
   then
   ACAP-TIDX-SLOTS 0 ?do 0 i ACAP-TIDX! loop
   0 ACAP-TIDX-N !
   ndict@ 0 ?do i ACAP-TIDX-INS loop
   ndict@ ACAP-TIDX-ND ! ;

variable ACAP-TIDX-MM                          \ index/scan disagreement count
\ A value no record carries must answer -1, and the witness's own absence is
\ established by the linear scan rather than assumed.
: ACAP-TIDX-ABSENT ( n -- ) {: w:n :}
   w ACAP-TGT>SCAN 0 >= if
      s" aot-capture: target-index absence witness is a live record" 74 die
   then
   w ACAP-TGT>REC 0 >= if 1 ACAP-TIDX-MM +! then ;

\ Some ordinary record's code entry. A package record's [0] is a raw WID, not an
\ entry, so the near-miss witness below has to come from an ordinary one to be
\ off instruction alignment rather than off nothing.
: ACAP-NO-ORDINARY ( -- )
   s" aot-capture: dictionary holds no ordinary record" 74 die ;
: ACAP-TIDX-CODE-XT ( -- n )
   ndict@ 0 ?do
      i AOT-REC AOT-RWID -1 <> if i AOT-REC AOT-RXT unloop exit then
   loop
   ACAP-NO-ORDINARY ;

\ THE PROOF IS O(ndict) AND IT IS NOT A SAMPLE. For every record k the index must
\ answer some j with xt(j) = xt(k) and j <= k. Take the smallest index m carrying
\ a given xt: its answer j obeys j <= m and xt(j) = xt(m), and m is the smallest
\ index with that xt, so j >= m and therefore j = m. The scan returns exactly m,
\ so agreeing on every record's OWN xt is agreeing on every xt there is - which is
\ why no quadratic cross-check is run and none is needed. The two witnesses close
\ the other half of the answer, the absent one.
: ACAP-TIDX-PROVE ( -- )
   0 ACAP-TIDX-MM !
   ndict@ 0 ?do
      i AOT-REC AOT-RXT {: xt:n :}
      xt ACAP-TGT>REC {: j:n :}
      j 0 < if 1 ACAP-TIDX-MM +! else
         j i > if 1 ACAP-TIDX-MM +! then
         j AOT-REC AOT-RXT xt <> if 1 ACAP-TIDX-MM +! then
      then
   loop
   AOT-DATA-N ACAP-TIDX-ABSENT                          \ far: no code entry is a DATA address
   ACAP-TIDX-CODE-XT 2 + ACAP-TIDX-ABSENT               \ near: a real entry, off alignment
   ACAP-TIDX-MM @ 0= 0= if
      s" aot-capture: TARGET INDEX MISMATCH count=" type ACAP-TIDX-MM @ . cr
      s" aot-capture: target index disagrees with the dictionary scan" 74 die
   then ;

\ --- deduped name pool: entries are [len:u8][name bytes]; ADD returns the entry
\ byte offset (points at the len byte). Records and call-reloc rows both reference
\ names by this offset, so each distinct callee/word name is stored exactly once. ---
variable ACAP-EQ                                             \ pool-compare mismatch accumulator
: ACAP-POOL-EQ? ( ptr u8 n n -- bool ) {: a:ptr u:n e:n :}   \ pool entry e has len u and bytes == a?
   0 ACAP-EQ !
   AOT-NAMES-BUF@ e + c@ u = if
      u 0 ?do
         a i + c@  AOT-NAMES-BUF@ e 1+ + i + c@  = 0= if 1 ACAP-EQ ! then
      loop
   else 1 ACAP-EQ ! then
   ACAP-EQ @ 0= ;
variable ACAP-PP                                             \ pool scan cursor
\ The linear reference the pool index replaces. ACAP-NIDX-SELFTEST holds the index
\ against it; nothing on the capture's hot path calls it.
: ACAP-POOL-SCAN ( ptr u8 n -- n ) {: a:ptr u:n :}           \ entry off, or -1 if absent
   0 ACAP-PP !
   begin ACAP-PP @ AOT-NAMES-LEN @ < while
      a u ACAP-PP @ ACAP-POOL-EQ? if ACAP-PP @ exit then
      AOT-NAMES-BUF@ ACAP-PP @ + c@ 1+ ACAP-PP @ + ACAP-PP !
   repeat
   -1 ;

\ --- the pool's index: name bytes -> entry offset -----------------------------
\ SAME DISEASE, SAME CURE, ONE EXTRA FACT. The pool is DEDUPED - ACAP-POOL-ADD
\ asks before every add and only writes on absence - so a name has at most one
\ entry and "the first match" and "the match" are one answer, which is why the
\ index needs no tie-break where the target index does. What the walk cost was is
\ the walk: on the compiler chain, 18,737 asks against a pool growing to 37,779
\ bytes, 0.29 s in ACAP-COMPACT-RECS alone.
\
\ ITS BOUND IS THE DICTIONARY'S, AND THAT IS DERIVED, NOT ESTIMATED. Every name
\ that reaches ACAP-POOL-ADD is some host record's name: ACAP-ADD-SITE passes the
\ callee record's, ACAP-COMPACT-RECS the captured record's own, ACAP-ADD-XTSITE
\ the named word's. Distinct entries therefore cannot outnumber the dictionary,
\ and 2x DICT-CAP slots hold the load at or under half. A producer that adds a
\ name from somewhere else must re-derive that; the refusal below is what it meets
\ if it does not.
\
\ THE POOL AND ITS INDEX ARE CLEARED BY ONE WORD. Several places empty the pool -
\ the capture's own reset and both ends of each build-time self-test below - and an
\ index left holding offsets into emptied bytes would answer a hit the scan cannot
\ see. So no site sets AOT-NAMES-LEN to zero any more; ACAP-POOL-RESET is the only
\ writer of the pair, and the coupling is structural rather than remembered.
DICT-CAP 2 * constant ACAP-NIDX-SLOTS
create ACAP-NIDX ACAP-NIDX-SLOTS 4 * allot                   \ u32: 0 empty, else entry off + 1
variable ACAP-NIDX-N                                         \ entries in the pool
variable ACAP-NH                                             \ name-hash accumulator
variable ACAP-NS                                             \ probe cursor

: ACAP-NIDX@ ( n -- n ) 4 * ACAP-NIDX + ACAP-W32@ ;
: ACAP-NIDX! ( n n -- ) {: v:n s:n :} v  s 4 * ACAP-NIDX +  AOT-P32! ;
: ACAP-NIDX-STEP ( -- ) ACAP-NS @ 1+ ACAP-NIDX-SLOTS 1 - and ACAP-NS ! ;

\ FNV-1a over the name bytes, the same key derivation the engine's dictionary
\ index uses (habu1.f C-HIDX-HASH), with the 32-bit result's high half folded down
\ before the mask so short names do not crowd one end of the table.
: ACAP-NIDX-HASH ( ptr u8 n -- n ) {: a:ptr u:n :}
   2166136261 ACAP-NH !
   u 0 ?do
      ACAP-NH @  a i + c@ xor  16777619 *  $FFFFFFFF and  ACAP-NH !
   loop
   ACAP-NH @ {: g:n :}
   g 16 rshift  g xor  ACAP-NIDX-SLOTS 1 - and ;

: ACAP-POOL-RESET ( -- )                                     \ the pool and its index, together
   0 AOT-NAMES-LEN !
   ACAP-NIDX-SLOTS 0 ?do 0 i ACAP-NIDX! loop
   0 ACAP-NIDX-N ! ;

: ACAP-POOL-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}           \ entry off, or -1 if absent
   a u ACAP-NIDX-HASH ACAP-NS !
   ACAP-NIDX-SLOTS 0 ?do
      ACAP-NS @ ACAP-NIDX@ {: e:n :}
      e 0= if -1 unloop exit then                            \ empty slot: the name has no entry
      a u e 1- ACAP-POOL-EQ? if e 1- unloop exit then
      ACAP-NIDX-STEP
   loop
   -1 ;

\ Every entry the pool holds must be the answer the index gives for its own
\ bytes. It is one probe per entry, so it runs over the REAL pool at the end of
\ every capture rather than over a fixture - which is where the thousands of names
\ that actually share a slot are.
variable ACAP-NIDX-PM                                        \ pool-proof mismatch count
: ACAP-NIDX-PROVE ( -- )
   0 ACAP-NIDX-PM !  0 ACAP-PP !
   begin ACAP-PP @ AOT-NAMES-LEN @ < while
      AOT-NAMES-BUF@ ACAP-PP @ 1+ +  AOT-NAMES-BUF@ ACAP-PP @ + c@
      ACAP-POOL-FIND ACAP-PP @ <> if 1 ACAP-NIDX-PM +! then
      AOT-NAMES-BUF@ ACAP-PP @ + c@ 1+ ACAP-PP @ + ACAP-PP !
   repeat
   ACAP-NIDX-PM @ 0= 0= if
      s" aot-capture: POOL INDEX MISMATCH count=" type ACAP-NIDX-PM @ . cr
      s" aot-capture: pool index does not answer its own entries" 74 die
   then ;

: ACAP-NIDX+ ( n -- ) {: off:n :}                            \ index an entry ACAP-POOL-ADD just wrote
   ACAP-NIDX-N @ DICT-CAP >= if
      s" aot-capture: name pool holds more entries than the dictionary" 74 die
   then
   AOT-NAMES-BUF@ off 1+ +  AOT-NAMES-BUF@ off + c@  ACAP-NIDX-HASH ACAP-NS !
   ACAP-NIDX-SLOTS 0 ?do
      ACAP-NS @ ACAP-NIDX@ 0= if
         off 1+ ACAP-NS @ ACAP-NIDX!
         ACAP-NIDX-N @ 1+ ACAP-NIDX-N !
         unloop exit
      then
      ACAP-NIDX-STEP
   loop
   s" aot-capture: name pool index full" 74 die ;

: ACAP-POOL-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}            \ deduped entry off (points at len byte)
   u 255 > if s" aot-capture: name too long for pool" 74 die then
   a u ACAP-POOL-FIND dup 0 >= if exit then drop
   AOT-NAMES-LEN @ 1+ u + AOT-NAMES-CAP > if s" aot-capture: name pool overflow" 74 die then
   AOT-NAMES-LEN @ 1+ u + AOT-NAMES-RESERVE
   AOT-NAMES-LEN @ {: off:n :}
   u  AOT-NAMES-BUF@ off + c!                                \ [len]
   u 0 ?do a i + c@  AOT-NAMES-BUF@ off 1+ + i + c!  loop    \ [bytes]
   off 1+ u + AOT-NAMES-LEN !
   off ACAP-NIDX+
   off ;

\ --- call-site reloc rows: blob-off u32 + name-off u32 (into pool) + scope u32 ---
\ The first two fields are u32 and neither carries a range check of its own,
\ because neither value can reach one: a blob offset is an index into a blob the
\ copy already refused past AOT-BLOB-CAP, and a pool offset is an index into a
\ pool ACAP-POOL-ADD already refused past AOT-NAMES-CAP. Both caps are megabytes
\ below 2^32, so the refusals that exist are the whole bound. The u16 fields these
\ replaced DID need their own checks, because their bound (64 KiB) sat below the
\ buffer caps and nothing else would have caught a crossing.
\ The SCOPE is the wordlist the seed searches the name in, and ACAP-SITE-SCOPE
\ below is the only producer of a value for it.
: ACAP-SITE-ROW ( n -- ptr u8 ) SITE-ROW * AOT-SITE-BUF@ + ;
: ACAP-ADD-SITE ( n ptr u8 n n -- ) {: boff:n a:ptr u:n w:n :}
   AOT-SITE-N @ AOT-SITE-MAX >= if s" aot-capture: too many call sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-SITE-N @ ACAP-SITE-ROW {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!  w r 8 + AOT-P32!
   AOT-SITE-N @ 1+ AOT-SITE-N ! ;

\ --- records: copy host record (48 bytes), rebase ordinary [0] xt to blob offset ---
: ACAP-REC-DST ( n -- ptr u8 ) 48 * AOT-REC-BUF@ swap + ;
: ACAP-ADD-REC ( n n -- ) {: k:n bstart:n :}
   k AOT-REC AOT-RWID DICT-WL:RETIRED = if exit then
   AOT-REC-N @ AOT-REC-MAX >= if s" aot-capture: too many records" 74 die then
   k AOT-REC AOT-A>U8 {: src:ptr :}
   AOT-REC-N @ ACAP-REC-DST {: d:ptr :}
   48 0 ?do src i + c@  d i + c!  loop                        \ verbatim 48-byte copy
   k AOT-REC AOT-RWID -1 <> if
      k AOT-REC AOT-RXT bstart -  d AOT-N-C!                  \ ordinary [0] = xt - blob-start
   then                                                        \ package [0]/[8] are raw u32 WID roles
   AOT-REC-N @ 1+ AOT-REC-N ! ;

\ --- compact AOT-CREC-ROW records: blob-off-or-package-public u32 + code-len-or-
\ package-private u32 + name-off u32 + (flags u8 | min-in u8<<8 | dkind u8<<16)
\ u32 + wid u32. Built from the
\ verbatim 48B records; each record's inline name is added to the deduped pool.
\ EM-AOT-REGISTER-RECS expands each compact record
\ back to the full 48B dict record at boot. All the constant/derivable fields
\ (flags nibble, DNAME-MIN-IN byte, DKIND pair, wid, name length, and the
\ [24..40) inline-name zero padding) are asserted or reconstructed; the ACAP-PROVE-RECS
\ pass then proves the expansion is byte-identical. The wid is a full u32
\ (matching the verbatim [40] cell's checked u32 domain) so wordlist IDs above
\ 255 round-trip through the seed -- the field was a truncating u8. The min-in
\ byte (record [16] bits 52-59, dot habu-habu-certified-words-84e84eaf) rides
\ the former pad byte so certified arity survives the seed round-trip. ---
: ACAP-CREC-DST ( n -- ptr u8 ) AOT-CREC-ROW * AOT-REC-MAX 48 * +  AOT-REC-BUF@ swap + ;
: ACAP-REC48@ ( -- ptr u8 ) AOT-REC-MAX 48 * AOT-REC-MAX AOT-CREC-ROW * +  AOT-REC-BUF@ swap + ;

\ A 48B record's EXT bit, read off the copy rather than the live dictionary.
: ACAP-REC-EXT? ( ptr u8 -- bool ) {: v:ptr :}
   v 20 + ACAP-W32@ 28 rshift 2 and 0= 0= ;

\ A record's name bytes. An inline name (up to DNAME-INL = 16) sits in the record
\ at [24]. A longer one does not: the engine's own definer writes it at CP, inside
\ the code region (habu2.f C-STORE-NAME), and puts that address in the cell at
\ [24]. Either kind goes into the deduped pool from here, so the seed reads every
\ name the same way whatever its length -- which is what makes an EXT-named word
\ capturable at all. It used to be refused outright, and the compiler chain has
\ 45 records the refusal would have thrown out.
: ACAP-REC-NAME ( ptr u8 bool -- ptr u8 ) {: v:ptr ext:bool :}
   ext 0= if v 24 + exit then
   v 24 + ACAP-W32@  v 28 + ACAP-W32@ 32 lshift or  AOT-N>U8 ;
\ Audit (c): every wid a record carries is a window coordinate. Wid 0, the global
\ wordlist, is the only number that names the same wordlist in two processes; a
\ wordlist the window did not create has no counterpart the seed could rebase it
\ to, so it is refused here rather than registered into whatever the target keeps
\ at that number. A package's private slot is 0 when it has none.
: ACAP-WID-IN? ( n -- bool ) {: w:n :}
   w 0= if 0 0= exit then
   w AOT-WID-W0 @ < if 0 0= 0= exit then
   w AOT-WID-W0 @ AOT-WID-SPAN @ + < ;

: ACAP-?WID ( ptr u8 n -- ) {: v:ptr w:n :}
   w ACAP-WID-IN? if exit then
   s" aot-capture: window record " type
   v  v ACAP-REC-EXT?  ACAP-REC-NAME  v 16 + ACAP-W32@  type
   s"  names wordlist " type w .
   s" , which its window did not create; the window allocated [" type
   AOT-WID-W0 @ .
   s" ," type AOT-WID-W0 @ AOT-WID-SPAN @ + .
   s" )" type cr
   s" aot-capture: captured wid outside the window" 74 die ;

: ACAP-AUDIT-WIDS ( -- )
   AOT-REC-N @ 0 ?do
      i ACAP-REC-DST {: v:ptr :}
      v CELL-VIEW AOT-RWID -1 = if
         v v ACAP-W32@ ACAP-?WID
         v v 8 + ACAP-W32@ ACAP-?WID
      else
         v v 40 + ACAP-W32@ ACAP-?WID
      then
   loop ;


\ --- blob copy ---
: ACAP-COPY-BLOB ( n n -- ) {: bstart:n bend:n :}
   bstart AOT-CODE-B0 !                          \ capture-time code base: ACAP-TGT decodes BL sites against it
   bend bstart - {: len:n :}
   len 0 < if s" aot-capture: negative blob span" 74 die then
   len AOT-BLOB-CAP > if s" aot-capture: blob exceeds buffer" 74 die then
   len 0 ?do bstart AOT-N>U8 i + c@  AOT-BLOB-BUF@ i + c!  loop
   len AOT-BLOB-LEN ! ;

\ --- the prelude band: what a captured word may call, and what it may hold ----
\ WHOSE DICTIONARY THE SEED RESOLVES IN. A captured call site travels as a NAME
\ and EM-SEED-AOT LFINDs it in the engine it is booting, so every callee a window
\ word has must be a word THAT engine carries: either a word of the capturing
\ process that the target's own prefix defines too, or a word inside the window,
\ which the seed registers before it patches anything. A capture running in a
\ booted bin/hb has a third kind, and it is the one this band exists for - the
\ files the capture tool loads to be ABLE to capture. Those words exist in the
\ capturing engine and in no target, so a call into them bakes a name that LFIND
\ cannot answer, and the failure lands at the boot of a shipped binary rather than
\ at the build that made it.
\
\ THE MARKS ARE THE BAND. The producer records the record index and the DATA
\ cursor as they stood when its prelude began; the window's own rstart and d0 are
\ where that prelude ended. What lies between is the prelude, and what lies below
\ is the engine the capture is running in - whose names the target shares because
\ the target is built from the same prefix. The band is two-sided because a
\ prelude word can reach a window word two ways: as the target of a call, and as
\ an ADDRESS a window word holds, which the DATA relocation would then rebase into
\ a pointer at nothing. Both audits run over the full recorded populations, and
\ both refuse by naming the window word that carries the site.
\
\ THE HOST DECLARES AN EMPTY BAND, and that is a statement rather than a default:
\ the metabuild host compiles its whole prefix from the same sources the engine it
\ writes will carry, so it has no prelude of its own and its marks are the window's
\ own start (src/habu/stdin.f CAPTURE-REPL). Declaring is mandatory - a capture
\ that never called PRELUDE-MARK does not know which of its words the target has,
\ and refuses instead of guessing.
variable ACAP-PRE-R      \ first record index of the prelude band
variable ACAP-PRE-D      \ first DATA address of the prelude band
variable ACAP-MARKED?    \ the band was declared for this capture
variable ACAP-W-B0                       \ the window's code base, latched at CAPTURE
variable ACAP-W-R0  variable ACAP-W-R1   \ its record span
variable ACAP-W-D0                       \ its first DATA address

public

\ Declare where the capturing process's own prelude begins: the record index and
\ the DATA cursor as they stood before the capture tool loaded anything. A
\ producer with no prelude passes the window's own start, which is an empty band.
: PRELUDE-MARK ( n n -- ) {: r:n d:n :}
   r ACAP-PRE-R !  d ACAP-PRE-D !  0 0= ACAP-MARKED? ! ;

private

\ The window record whose compiled code holds this blob offset, or -1. Used only
\ on a refusal path, so a linear walk of the window is the right shape: it needs
\ no index, no proof that an index answers what it answers, and no reset.
\ A package record ([40] = -1) carries WID roles in [0]/[8] rather than a code
\ span, so it can hold no offset and is skipped.
: ACAP-REC-AT ( n -- n ) {: boff:n :}
   ACAP-W-B0 @ boff + {: a:n :}
   ACAP-W-R1 @ ACAP-W-R0 @ ?do
      i AOT-REC AOT-RWID -1 <> if
         i AOT-REC AOT-RXT a <=
         i AOT-REC AOT-RXT i AOT-REC AOT-RBYTES + a > and if i unloop exit then
      then
   loop
   -1 ;

: ACAP-NAME. ( n -- ) {: k:n :}
   k 0 < if s" <no record>" type exit then
   k AOT-REC AOT-RNPTR  k AOT-REC AOT-RNLEN  type ;

\ Audit (a): every call a window word makes. A callee below the prelude mark is a
\ word of the booting engine and the target's prefix defines it; a callee inside
\ the window is registered by the seed before the patch pass runs. Anything else
\ is a name the target has not got, and the capture ends here rather than baking
\ it.
: ACAP-SITE-BAND ( n n -- ) {: boff:n k:n :}
   k ACAP-PRE-R @ < if exit then
   k ACAP-W-R0 @ >= k ACAP-W-R1 @ < and if exit then
   s" aot-capture: window word " type boff ACAP-REC-AT ACAP-NAME.
   s"  at blob offset " type boff .
   s" calls " type k ACAP-NAME.
   s" , which the booting engine has and no target does" type cr
   s" aot-capture: window call into the prelude band" 74 die ;

\ Audit (b) is the DATA half, and it is a SENTENCE ADDED TO AN EXISTING REFUSAL
\ rather than a second one. A recorded address the window's spans do not place is
\ already refused below (ACAP-UNCLASSIFIED), fail-closed, for every value; what
\ the band adds is WHICH KIND of address it is, and that is the difference between
\ a diagnostic a reader can act on and a number. The kind that matters to a
\ capture running in a booted engine is the middle one: an address allotted after
\ this process started and before the window opened belongs to the capture tool's
\ own prelude, exists in no target, and would be rebased into a pointer at
\ whatever the seeded engine put at that offset.
\ Writing it as a second refusal was tried and refuted: with a forged window base
\ (test/aot-wid-suite.f HABU_AOT_D0_SKEW moves d0 past the span) every real window
\ address falls into the band, so the second refusal took the first one's only
\ producer and the tree lost a tested stop. One refusal, one die line, and the
\ band in the diagnostic keeps both.
: ACAP-BAND. ( n -- ) {: v:n :}
   v ACAP-PRE-D @ < if s" below this process's own start" type exit then
   v ACAP-W-D0 @ < if s" in the prelude band, which no target carries" type exit then
   s" above the window's DATA span" type ;

\ --- what wordlist the seed will search a callee's name in ---------------------
\ A bare name is not an identity: measured on the chain, SLOT@ lives in five
\ wordlists at once and in none of them globally. So a site carries a SCOPE, and
\ four kinds of scope are all there are. A wid below FIRST-DYNAMIC-WID is a
\ layout.f CONSTANT and names the same wordlist in every engine, exactly as wid 0
\ does. A wid inside the window is a coordinate the seed rebases like a record's.
\ A wid the window did not create is this engine's own number for a package the
\ target numbers its own way, so the NAME carries the scope instead - qualified
\ with the package's own name, which the seed resolves through the qualifier path
\ a compile uses. Anything left is refused by name.
$3A constant ACAP-QUAL-SEP                        \ ':' - the separator LFIND's qualifier scan looks for
256 constant ACAP-QUAL-CAP
create ACAP-QUAL-BUF ACAP-QUAL-CAP allot

\ The package row whose PUBLIC ([0]) or PRIVATE ([8]) wordlist is w, or -1. One
\ walk for both halves: the two differ by a field and nothing else, and a second
\ copy of the walk is a second place for the namespace-record shape to drift.
: ACAP-PKG-ROW ( n bool -- n ) {: w:n pub:bool :}
   ndict@ 0 ?do
      i AOT-REC AOT-RWID DICT-WL:NAMESPACE = if
         pub if i AOT-REC AOT-RXT else i AOT-REC AOT-RLEN then
         w = if i unloop exit then
      then
   loop
   -1 ;

: ACAP-PKG-PUB ( n -- n ) {: w:n :}               \ the package row publishing wid w, or -1
   w 0 0= ACAP-PKG-ROW ;

: ACAP-QUAL$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: pa:ptr pu:n wa:ptr wu:n :}
   pu wu + 1+ ACAP-QUAL-CAP > if
      s" aot-capture: qualified callee name exceeds the buffer" 74 die
   then
   pu 0 ?do pa i + c@  ACAP-QUAL-BUF i + c!  loop
   ACAP-QUAL-SEP ACAP-QUAL-BUF pu + c!
   wu 0 ?do wa i + c@  ACAP-QUAL-BUF pu 1+ + i + c!  loop
   ACAP-QUAL-BUF  pu wu + 1+ ;

\ A prefix CODE entry travels only under a resolving global or public name.
\ Its record and namespace must predate the tooling cut. Aliases sharing an
\ entry are tried separately because the first record can be private or retired.
: ACAP-PREFIX-NAME? ( n n -- ptr u8 n bool ) {: k:n target:n :}
   k 0 < k ACAP-PRE-R @ >= or if NULL$ false exit then
   k AOT-REC {: rec:ptr :}
   rec AOT-RXT target <> if NULL$ false exit then
   rec AOT-RWID {: wid:n :}
   wid 0 < if NULL$ false exit then
   rec AOT-RNPTR rec AOT-RNLEN {: name:ptr size:n :}
   size 0= if NULL$ false exit then
   wid 0= if name size else
      wid FIRST-DYNAMIC-WID < if NULL$ false exit then
      wid ACAP-PKG-PUB {: pkg:n :}
      pkg 0 < pkg ACAP-PRE-R @ >= or if NULL$ false exit then
      pkg AOT-REC {: owner:ptr :}
      owner AOT-RNPTR owner AOT-RNLEN DICT-WL:NAMESPACE XREF-FIND-WL-INDEX
      dup 0 < swap ACAP-PRE-R @ >= or if NULL$ false exit then
      owner AOT-RNPTR owner AOT-RNLEN name size ACAP-QUAL$
   then {: a:ptr u:n :}
   a u XREF-FIND-INDEX {: resolved:n :}
   resolved 0 < resolved ACAP-PRE-R @ >= or if NULL$ false exit then
   resolved AOT-REC AOT-RXT target <> if NULL$ false exit then
   a u true ;


: ACAP-TARGET-NAME? ( n -- ptr u8 n bool ) {: target:n :}
   target ACAP-TGT>REC target ACAP-PREFIX-NAME? if true exit then 2drop
   ACAP-PRE-R @ 0 ?do
      i target ACAP-PREFIX-NAME? if true unloop exit then 2drop
   loop
   NULL$ false ;

variable ACAP-P

\ A private word of a pre-window package is the one callee no scope can carry:
\ the qualifier reaches a package's PUBLIC wordlist only. It is also unreachable -
\ a caller in that package's private scope is itself a record of that package, and
\ ACAP-?WID refuses a window record whose wid the window did not create - so this
\ names the next producer of one rather than a case that arrives.
: ACAP-REFUSE-SCOPE ( n n -- ) {: k:n w:n :}
   s" aot-capture: window word " type ACAP-P @ ACAP-REC-AT ACAP-NAME.
   s"  calls " type k ACAP-NAME.
   s"  in wordlist " type w .
   s" , which its window did not create and no package publishes" type cr
   s" aot-capture: call site into a wordlist the seed cannot name" 74 die ;

: ACAP-SITE-SCOPE ( n -- ptr u8 n n ) {: k:n :}   \ callee record -> name, scope
   k AOT-REC AOT-RNPTR  k AOT-REC AOT-RNLEN {: a:ptr u:n :}
   k AOT-REC AOT-RWID {: w:n :}
   w 0 >= w FIRST-DYNAMIC-WID < and if a u w exit then
   w ACAP-WID-IN? if a u w exit then
   w ACAP-PKG-PUB {: p:n :}
   p 0 < if k w ACAP-REFUSE-SCOPE then
   p AOT-REC AOT-RNPTR  p AOT-REC AOT-RNLEN  a u ACAP-QUAL$  WID-QUAL ;

\ --- audit (f): every checked window word's signature travels ------------------
\
\ WHY THIS AUDIT EXISTS. An AOT seed puts a word in the runtime dictionary and
\ nothing in the checker's record set, so a `:` definition naming a seeded word
\ dies E-UNDEFINED at that token in the shipped engine even though the engine can
\ call it. The artifact therefore carries the window's SIGNATURES, collected by
\ the checker while the window is open (src/core/checker.f, armed by
\ src/habu/aot-arm.f OPEN). This is the check that the collection is COMPLETE.
\
\ IT IS NOT A COUNT COMPARISON. The collection happens at three producers, and
\ that list is an enumeration - a fourth producer added later would be silently
\ missed. So this asks the question per RECORD: for every record the window
\ compiled, if the checker knows an effect for it then the pool must carry its
\ signature, and if it does not the capture ends here by name. That turns the
\ enumeration into something a gate refuses rather than something a comment
\ claims. Measured over the compiler chain when it was written: 6892 records = 94
\ package records + 6798 checked words, all 6798 carried; the first version of
\ the collection left 17 behind and this is what named them.
\
\ THE TWO EXEMPT ROLES ARE ROLES, NOT NAMES. A package record and a retired
\ record are identified by their WORDLIST - DICT-WL:NAMESPACE and
\ DICT-WL:RETIRED - so no spelling can put a word in either class.
\
\ A WORD WITH NO CHECKER EFFECT IS NOT A FINDING. A `0 set-check` span in a
\ window compiles real runtime words the checker never recorded; they are
\ uncallable from checked code in THIS engine and stay exactly as uncallable in
\ the seeded one, so there is nothing to carry and nothing to refuse.

\ The scope half of the key SYM-FIND uses - ( pkg$, public?, found? ) - taken
\ from the record's own WORDLIST and not from whatever package is open. An empty
\ package name is the global scope, which is the key the checker interns a global
\ under. A wid no package row claims is refused rather than guessed at: answering
\ "global" for it would ask about a different word.
\
\ THE LAST ANSWER IS KEPT because the walk above is linear in the dictionary and
\ this is asked once per window record - 6892 times over the compiler chain, and
\ its records arrive grouped by package, so one memo cell turns 6892 walks into
\ 94. Keyed on the wid itself, so it cannot answer for a different one.
variable ACAP-PKG-MEMO-W                          \ the wid the memo answers for, 0 = none
variable ACAP-PKG-MEMO-ROW                        \ ... and the package row it answered
variable ACAP-PKG-MEMO-PUB

: ACAP-PKG-LOOKUP ( n -- n bool ) {: w:n :}       \ package row and public?, row -1 if none
   w ACAP-PKG-MEMO-W @ = if
      ACAP-PKG-MEMO-ROW @ ACAP-PKG-MEMO-PUB @ 0 <> exit
   then
   w 0 0= ACAP-PKG-ROW {: p:n :}
   p 0 >= if
      w ACAP-PKG-MEMO-W !  p ACAP-PKG-MEMO-ROW !  -1 ACAP-PKG-MEMO-PUB !
      p 0 0= exit
   then
   w 0 0= 0= ACAP-PKG-ROW {: q:n :}
   w ACAP-PKG-MEMO-W !  q ACAP-PKG-MEMO-ROW !  0 ACAP-PKG-MEMO-PUB !
   q 0 0= 0= ;

: ACAP-REC-PKG ( n -- ptr u8 n bool bool ) {: w:n :}
   w 0= if s" "  0 0= 0=  0 0= exit then
   w ACAP-PKG-LOOKUP {: p:n pub:bool :}
   p 0 < if s" "  0 0= 0=  0 0= 0= exit then
   p AOT-REC AOT-RNPTR  p AOT-REC AOT-RNLEN  pub  0 0= ;

\ --- which records the image ships --------------------------------------------
\
\ WHAT GOES, AND WHY IT CAN. A package's PRIVATE word is unreachable by name in
\ the engine this image becomes: no source can qualify into a private wordlist,
\ the interpreter never finds one, and since the call sites carry a record index
\ instead of a name (habu2.f EMIT-AOT-SITES) nothing in the payload looks one up
\ either. Its record - 20 bytes here, 48 in the booted dictionary, and its name
\ in the pool - therefore buys nothing any caller can use. THE CODE STAYS: the
\ word is still called, by a displacement the blob carries.
\
\ WHAT STAYS, AS A RULE AND NOT A LIST. A record ships when the payload ITSELF
\ names it, and the payload says so in three tables of its own: the boot-run
\ entry list and the named code sites, which the seed resolves by name at boot,
\ and the address cells, where a DATA cell holding a word's ENTRY is the engine
\ reaching that word through a cell. Asking those three tables cannot drift the
\ way a hand-written keep-list drifts. Package rows always ship: they carry the
\ wordlist roles the seed rebases and the sealed-WID gate reads.
variable ACAP-BP

: ACAP-POOL$ ( n -- ptr u8 n ) {: noff:n :}
   AOT-NAMES-BUF@ noff 1+ +  AOT-NAMES-BUF@ noff + c@ ;

: ACAP-XTOFF-ENTRY? ( n -- bool ) {: off:n :}
   AOT-WINDOW:XTOFF-N @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i AOT-WINDOW:XTOFF-ROW * + 4 + ACAP-W32@ {: tgt:n :}
      tgt AOT-WINDOW:XTOFF-KIND-MASK and 0= if
         tgt AOT-WINDOW:XTOFF-VALUE-MASK and off 1+ = if true unloop exit then
      then
   loop
   false ;

: ACAP-XTSITE-NAMES? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   AOT-XTSITE:N @ 0 ?do
      AOT-XTSITE:BUF@ i 8 * + 4 + ACAP-W32@ ACAP-POOL$ a u CORE-STR=CI if
         true unloop exit
      then
   loop
   false ;

: ACAP-BOOTRUN-AT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   AOT-BOOTRUN-BUF@ ACAP-BP @ + c@ {: len:n :}
   len 0= if false exit then
   AOT-BOOTRUN-BUF@ ACAP-BP @ + 1+  len  a u CORE-STR=CI ;

: ACAP-BOOTRUN-NAMES? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 ACAP-BP !
   begin ACAP-BP @ AOT-BOOTRUN-LEN @ < while
      a u ACAP-BOOTRUN-AT? if true exit then
      AOT-BOOTRUN-BUF@ ACAP-BP @ + c@ 1+ ACAP-BP @ + ACAP-BP !
   repeat
   false ;

: ACAP-PRIVATE? ( n -- bool ) {: w:n :}
   w 0= if false exit then
   w ACAP-REC-PKG {: pa:ptr pu:n pub:bool found:bool :}
   found 0= if false exit then
   pub 0= ;

: ACAP-REC-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k ACAP-REC-DST {: v:ptr :}
   v  v ACAP-REC-EXT?  ACAP-REC-NAME  v 16 + ACAP-W32@ ;

\ ---- the keep-set -------------------------------------------------------------
\ FOUR NAMES THE DERIVED RULE CANNOT SEE, each because something OUTSIDE the
\ payload resolves it by name. The list is short, explicit and reviewed as
\ policy, the way tools/manifest-lint.f states the engine's entry points: a rule
\ that cannot see a use is better than a rule quietly widened until it can.
\
\ NSTR:IMPORT-ROWS - tools/native-build-core.f TARGET-IMPORTER reaches it by name
\ through the shipped dictionary, because the alternatives are a public wrapper,
\ which test/compiler/native-string.f forbids outright, or a fixed engine cell.
\ That file's own comment named this dot as the one that has to carry the entry,
\ before this dot ran. The drift guard is the build's own refusal, "native-build:
\ literal importer missing", the moment the entry stops matching.
\
\ MEM:WB-DEPTH, MEM:WB-BUFFERS, MEM:WB-LENGTHS - lib/memory-test.f asserts that a
\ completed WITH-BYTES left no frame behind: the depth is back to zero and both
\ frame buffers are released, which index 0 refusing proves. The property is real
\ and nothing public observes it, so the audited suite reads the cells. This is
\ the weaker of the four reasons - a white-box suite over a BAKED package is
\ asking the shipped image for something the shipped image should not have to
\ answer - and the end state is that block moving to where it is compiled beside
\ what it audits, after which these three go.
: ACAP-KEEP-PKG? ( n ptr u8 n -- bool ) {: k:n pa:ptr pu:n :}
   k ACAP-REC-DST 40 + ACAP-W32@ ACAP-REC-PKG {: na:ptr nu:n pub:bool found:bool :}
   found 0= if false exit then
   na nu pa pu CORE-STR=CI ;

: ACAP-KEEP? ( n -- bool ) {: k:n :}
   k ACAP-REC-NAME$ {: a:ptr u:n :}
   a u s" IMPORT-ROWS" CORE-STR=CI  k s" NSTR" ACAP-KEEP-PKG? and if true exit then
   k s" MEM" ACAP-KEEP-PKG? 0= if false exit then
   a u s" WB-DEPTH" CORE-STR=CI if true exit then
   a u s" WB-BUFFERS" CORE-STR=CI if true exit then
   a u s" WB-LENGTHS" CORE-STR=CI ;

: ACAP-NAMED? ( n -- bool ) {: k:n :}
   k ACAP-REC-DST {: v:ptr :}
   v CELL-VIEW AOT-RWID -1 = if true exit then
   v 40 + ACAP-W32@ ACAP-PRIVATE? 0= if true exit then
   v ACAP-W32@ ACAP-XTOFF-ENTRY? if true exit then
   k ACAP-KEEP? if true exit then
   k ACAP-REC-NAME$ {: a:ptr u:n :}
   a u ACAP-XTSITE-NAMES? if true exit then
   a u ACAP-BOOTRUN-NAMES? ;

\ THE ROW STAYS, THE NAME GOES. The row is 20 bytes here and 48 in the booted
\ dictionary and it carries the word's code span, which is not a name and is not
\ optional: src/habu/aot-lib.f walks the shipped records to retarget every
\ PC-relative branch when hb-build shakes an application out of this image, and a
\ displacement that lands in a span no record covers has nowhere to go ("aot:
\ PC-relative target removed or outside closure"). The NAME is what makes a word
\ reachable, and that is what a stripped row loses: its pool entry becomes the
\ empty name, so no lookup in the booted engine can return it - not the
\ interpreter, not a reopened package, not XREF-FIND. The build writes every name
\ it stripped to <image>.names beside the engine (tools/native-build-core.f), so
\ a tool that has to name this code still can.
create ACAP-NAMED-BIT AOT-REC-MAX cells allot   \ per record: 1 kept its name, 0 stripped
variable ACAP-REC-ALL

: ACAP-COMPACT-ONE ( n -- ) {: k:n :}
   k ACAP-REC-DST {: v:ptr :}                              \ verbatim 48B record
   v CELL-VIEW AOT-RWID -1 = {: pkg:bool :}
   v 4 + ACAP-W32@ 0= 0= if s" aot-capture: rec blob-off exceeds u32" 74 die then
   v 12 + ACAP-W32@ 0= 0= if s" aot-capture: rec end exceeds u32" 74 die then
   pkg 0= if
      v 8 + ACAP-W32@ CODE-SPAN:CHECK
      v 44 + ACAP-W32@ 0= 0= if s" aot-capture: rec wid exceeds u32" 74 die then
   then
   v 20 + ACAP-W32@ 28 rshift $F and {: flags:n :}         \ flag nibble ([16] bits 60-63)
   v 20 + ACAP-W32@ 20 rshift $FF and {: minin:n :}        \ DNAME-MIN-IN byte ([16] bits 52-59)
   v 20 + ACAP-W32@ 18 rshift 3 and {: dkind:n :}          \ DKIND pair ([16] bits 50-51)
   v 20 + ACAP-W32@ $0003FFFF and 0= 0= if s" aot-capture: rec [16] stray high bits" 74 die then
   v ACAP-REC-EXT? {: ext:bool :}                          \ name out of line (DNAME-EXT)
   v 16 + ACAP-W32@ {: len:n :}                            \ name length ([16] low word)
   ext 0= len 16 > and if s" aot-capture: rec name too long for inline" 74 die then
   pkg if $FFFFFFFF else v 40 + ACAP-W32@ then {: wid:n :} \ package marker or full ordinary u32 WID
   v ACAP-W32@ {: start:n :}  v 8 + ACAP-W32@ {: clen:n :}
   k ACAP-NAMED? {: named:bool :}
   named if v ext ACAP-REC-NAME len else s" " then ACAP-POOL-ADD {: noff:n :}
   named if flags else flags 13 and then {: rflags:n :}    \ stripped rows are inline and empty: clear DNAME-EXT
   named if 1 else 0 then  k cells ACAP-NAMED-BIT + !
   k ACAP-CREC-DST {: c:ptr :}                             \ 20B: start u32 + len u32 + name-off u32 + flags u8 + min-in u8 + dkind u8 + wid u32
   start c AOT-P32!  clen c 4 + AOT-P32!  noff c 8 + AOT-P32!
   rflags  minin 8 lshift or  dkind 16 lshift or  c 12 + AOT-P32!   \ one store so the spare byte is written zero
   wid c 16 + AOT-P32! ;

: ACAP-COMPACT-RECS ( -- )
   AOT-REC-N @ ACAP-REC-ALL !
   ACAP-REC-ALL @ 0 ?do
      i ACAP-COMPACT-ONE
   loop ;

\ Expand a compact AOT-CREC-ROW record to a 48B dict record image -- the field
\ reconstruction EM-AOT-REGISTER-RECS runs at boot. Ordinary [0] remains a blob
\ offset for the build-time inverse proof; boot adds CP. Package [0]/[8] stay raw.
\ ONE CELL IS NOT MODELLED, AND CANNOT BE. For an EXT-named record the boot pass
\ stores the RUNTIME address of the pool entry's bytes in [24], and that address
\ exists only in the engine being booted -- the same reason the code literals
\ travel b0-relative. So the model leaves [24..32) zero for those records and
\ ACAP-PROVE-RECS proves the NAME rather than the pointer: the pooled name the
\ seed will hand the record is the host record's own name, byte for byte. The
\ pointer's proof is a boot, and it is a direct one - EM-AOT-BOOTRUN resolves an
\ entry word through LFIND, which reads exactly this cell for an EXT name.
: ACAP-EXPAND-REC ( ptr u8 ptr u8 -- ) {: c:ptr s:ptr :}      \ c=compact record, s=48B out
   c ACAP-W32@ s AOT-N-C!                                     \ [0..8) = blob-off or package public WID
   c 4 + ACAP-W32@ s 8 + AOT-N-C!                             \ [8..16) = code len or package private WID
   c 8 + ACAP-W32@ {: noff:n :}                               \ name-off u32
   AOT-NAMES-BUF@ noff + c@ {: len:n :}                       \ len = pool[entry]
   c 12 + c@ {: flags:n :}
   c 13 + c@ {: minin:n :}
   c 14 + c@ {: dkind:n :}
   flags 60 lshift  minin 52 lshift or  dkind 50 lshift or  len or  s 16 + AOT-N-C!   \ [16] = flags<<60 | min-in<<52 | dkind<<50 | len
   0 s 24 + AOT-N-C!  0 s 32 + AOT-N-C!                       \ zero [24..40)
   flags 2 and 0= if                                          \ inline name: the bytes live in the record
      len 0 ?do  AOT-NAMES-BUF@ noff 1+ + i + c@  s 24 + i + c!  loop
   then
   c 16 + ACAP-W32@ dup $FFFFFFFF = if drop -1 then
   s 40 + AOT-N-C! ;                                          \ package marker sign-extends; ordinary wid stays u32
variable ACAP-RECMM                                           \ record-proof mismatch count
\ The pooled name a record will resolve to at boot IS the name the host record
\ carries: same length byte, same bytes. This is what stands in for comparing an
\ EXT record's [24] cell, which holds two different addresses for the same name.
: ACAP-PROVE-NAME ( ptr u8 ptr u8 -- ) {: c:ptr v:ptr :}      \ c=compact row, v=verbatim record
   c 8 + ACAP-W32@ {: noff:n :}
   v 16 + ACAP-W32@ {: len:n :}
   v  v ACAP-REC-EXT?  ACAP-REC-NAME {: nm:ptr :}
   AOT-NAMES-BUF@ noff + c@ len = 0= if 1 ACAP-RECMM +! then
   len 0 ?do
      nm i + c@  AOT-NAMES-BUF@ noff 1+ + i + c@  = 0= if 1 ACAP-RECMM +! then
   loop ;
\ A STRIPPED ROW IS PROVED DIFFERENTLY, AND SAYS SO. Its name fields are meant
\ to differ from the record's: [16]'s low word is 0 where the record has a
\ length, DNAME-EXT is clear, and [24..40) holds no inline bytes. So the walk
\ skips [16..40) for such a row and ACAP-PROVE-STRIPPED asserts what the row
\ MUST say instead - an empty pooled name and no EXT - which is the property the
\ booted engine's lookup depends on. Every other byte, the code span included,
\ is still compared field for field.
: ACAP-PROVE-STRIPPED ( ptr u8 -- ) {: c:ptr :}
   c 8 + ACAP-W32@ {: noff:n :}
   AOT-NAMES-BUF@ noff + c@ 0= 0= if 1 ACAP-RECMM +! then
   c 12 + c@ 2 and 0= 0= if 1 ACAP-RECMM +! then ;
: ACAP-PROVE-RECS ( -- )                                      \ fail-closed: expand==verbatim, field-for-field
   0 ACAP-RECMM !
   ACAP-REC48@ {: s:ptr :}
   AOT-REC-N @ 0 ?do
      i ACAP-CREC-DST {: c:ptr :}
      c s ACAP-EXPAND-REC                                     \ rebuild 48B from compact
      i ACAP-REC-DST {: v:ptr :}                              \ the record this row was made from
      v ACAP-REC-EXT? {: ext:bool :}
      i cells ACAP-NAMED-BIT + @ 0= {: stripped:bool :}
      48 0 ?do
         ext  i 24 >= and  i 32 < and
         stripped  i 16 >= and  i 40 < and  or
         0= if
            s i + c@  v i + c@  = 0= if
               ACAP-RECMM @ 12 < if
                  s" record " type j . s"  byte " type i .
                  s"  expected " type s i + c@ . s"  actual " type v i + c@ .
                  s"  name " type v ext ACAP-REC-NAME v 16 + ACAP-W32@ type cr
               then
               1 ACAP-RECMM +!
            then
         then
      loop
      stripped if c ACAP-PROVE-STRIPPED else
         ext if c v ACAP-PROVE-NAME then                      \ ... and the name stands in for it
      then
   loop
   ACAP-RECMM @ 0= 0= if
      s" aot-capture: RECORD EXPANSION MISMATCH count=" type ACAP-RECMM @ . cr
      s" aot-capture: compact record expansion != verbatim 48B" 74 die
   then ;

variable ACAP-SIG-KNOWN                            \ window records the checker knows an effect for
variable ACAP-SIG-EXEMPT                           \ package, retired, and unrecorded records

: ACAP-REFUSE-SIG-WID ( n n -- ) {: k:n w:n :}
   s" aot-capture: window record " type k ACAP-NAME.
   s"  is in wordlist " type w .
   s" , which no package record claims" type cr
   s" aot-capture: a window record's wordlist names no package" 74 die ;

: ACAP-REFUSE-SIG ( n -- ) {: k:n :}
   s" aot-capture: the checker knows an effect for window word " type k ACAP-NAME.
   s"  and the captured signature pool carries none" type cr
   s" aot-capture: a checked window word's signature was not captured" 74 die ;

\ --- and the same walk emits the rows -----------------------------------------
\ THE COPY IS THE AUDIT'S WALK, not a second pass over the store. Every row the
\ artifact carries is one the audit just proved belongs to a word this window
\ compiled, so the artifact cannot name a word the target engine has not got.
\ Copying the store instead put 723 extra rows in (the capture tool's own
\ post-window words) and a signature for an absent word is worse than a missing
\ one: a definition naming it certifies and then calls nothing.
\
\ THE ROW IS COPIED, NOT REBUILT. It is four u32 - name, signature and package as
\ offsets into the pool's string arena, then visibility - and the arena travels
\ verbatim beside it, so nothing is re-interned and no offset is recomputed. The
\ checker owns that format and stays its only reader.
: ACAP-SIG-OVERFLOW ( -- )
   s" aot-capture: the window has more checked words than the signature buffer holds" 74 die ;

: ACAP-SIG-ROW+ ( n -- ) {: at:n :}
   AOT-SIG-N @ 1 + AOT-SIG-MAX > if ACAP-SIG-OVERFLOW then
   AOT-ARM:PAYLOAD-SPANS 2drop {: rows:ptr bytes:n :}
   at 0 < at bytes > or if ACAP-SIG-OVERFLOW then
   SIG-ROW bytes at - > if ACAP-SIG-OVERFLOW then
   SIG-ROW 0 ?do
      rows at + i + c@
      AOT-SIG-BUF@ AOT-SIG-N @ SIG-ROW * + i + c!
   loop
   AOT-SIG-N @ 1 + AOT-SIG-N ! ;

: ACAP-?SIG ( n -- ) {: k:n :}
   k AOT-REC AOT-RWID {: w:n :}
   w DICT-WL:NAMESPACE = if ACAP-SIG-EXEMPT @ 1 + ACAP-SIG-EXEMPT ! exit then
   w DICT-WL:RETIRED = if ACAP-SIG-EXEMPT @ 1 + ACAP-SIG-EXEMPT ! exit then
   w ACAP-REC-PKG {: pa:ptr pu:n pub:bool ok:bool :}
   ok 0= if k w ACAP-REFUSE-SIG-WID then
   k AOT-REC AOT-RNPTR k AOT-REC AOT-RNLEN {: na:ptr nu:n :}
   pa pu pub na nu AOT-ARM:PAYLOAD-LOOKUP {: row:n known:bool :}
   known 0= if
      ACAP-SIG-EXEMPT @ 1 + ACAP-SIG-EXEMPT ! exit
   then
   ACAP-SIG-KNOWN @ 1 + ACAP-SIG-KNOWN !
   row 0= if k ACAP-REFUSE-SIG then
   row 1 - ACAP-SIG-ROW+ ;

\ Carry the owner's complete frozen graph/name arena. Rows index it by offset,
\ so the copy preserves every graph edge and name without rewriting either.
: ACAP-SIG-STRINGS ( -- )
   AOT-ARM:PAYLOAD-SPANS {: rows:ptr rowu:n str:ptr n:n :}
   \ Admit the known sidecar extent before reserving. The finished capture
   \ charges its remaining registry/body sections through the same budget.
   56 AOT-SIG-N @ SIG-ROW AOT-SECTION:+ROWS n AOT-SECTION:+RAW drop
   n AOT-SIG-STR-RESERVE
   n 0 > if str AOT-SIG-STR-BUF@ n BYTE-COPY then
   n AOT-SIG-STR-LEN ! ;

\ The type registry the signatures resolve against, written by the registry that
\ owns those records; this file carries the bytes and never reads them.
: ACAP-SIG-REGISTRY ( -- )
   AOT-REG-BUF@ AOT-REG-CAP AOT-ARM:PAYLOAD-REG-SAVE AOT-REG-LEN ! ;

\ IT RUNS LAST OF THE AUDITS, after the call and address scans. Those ask whether
\ what travels is SOUND - a name the target has, an address the target can place;
\ this asks whether it is COMPLETE. A window that is unsound is unsound whether
\ or not its signatures were collected, so the more fundamental refusal has to be
\ the one a reader sees. Running it earlier masked them (measured:
\ test/aot-band-data.f's address refusal came back as an uncarried signature).
\
\ NOTHING TESTS THE ARMING SEPARATELY, and that is not an omission: a window
\ compiled with the store disarmed has an empty pool, and the walk below then
\ names its FIRST checked word as uncarried. A guard asking "is the store armed
\ NOW" would be asking at the wrong moment anyway - a capture tool closes the
\ signature window (AOT-ARM:SIG-CLOSE) before it captures, so armed-now is false
\ on the correct path.
\ WHERE THE WINDOW'S TYPES END, and why it is a different question from where its
\ SIGNATURES end. The rows that travel are chosen by the walk below - one per
\ record of THIS window - so a store holding more than the window says is
\ harmless: nothing outside [R0,R1) is ever copied. The registry has no such
\ walk. Its delta is two high-waters subtracted, so the moment it is read is the
\ whole of what it means, and that moment is here: whatever the registry has
\ grown by the time a capture runs is what that capture declares.
\
\ UNLESS THE WINDOW WAS CLOSED EARLY, which one caller must do.
\ tools/aot-chain-capture.f loads its own assembler and artifact writer AFTER its
\ window and before it captures, and those declare families of their own; read
\ here, the delta would carry them (measured: the chain declares 70 families and
\ a capture-time read counted more, and the seeded engine then refused its own
\ registry). That tool closes at the window instead (AOT-ARM:SIG-CLOSE), which
\ disarms the collection, and an already-closed window keeps the end it declared.
: ACAP-AUDIT-SIGS ( -- )
   0 ACAP-SIG-KNOWN !  0 ACAP-SIG-EXEMPT !  0 AOT-SIG-N !
   0 ACAP-PKG-MEMO-W !  0 ACAP-PKG-MEMO-ROW !  0 ACAP-PKG-MEMO-PUB !
   ACAP-W-R1 @ ACAP-W-R0 @ ?do i ACAP-?SIG loop
   ACAP-SIG-STRINGS
   ACAP-SIG-REGISTRY ;

\ --- audit (d): the row resolves the way the seed will ask ---------------------
\ The question EM-AOT-PATCH-SITES asks at the boot of the engine this capture is
\ baked into, asked here of the row that was just written - the pooled name and
\ the stored scope, through the engine's own find. A wrong scope (the whole of
\ dot 9d7d8e72: a packaged callee searched globally), a wrong pool offset and a
\ shadowed name all fail at the build that made the artifact instead of at that
\ boot, where the exit says only that a name was not found.
\ ONE SCOPE CANNOT BE ASKED FROM HERE and is carried by what makes it safe
\ instead: `search-wl` refuses OWNER-API-PRI-WID, the sealed engine-helper
\ wordlist that the seed's own routine does search, so a site there is required to
\ name a PRE-WINDOW record - one of the engine's own baked helpers - whose wid is
\ a layout constant and therefore the same number in the target.
: ACAP-XREF-XT ( ptr n -- n )
   dup XREF-FOUND? if XREF-START exit then drop 0 ;

: ACAP-SITE-XT ( ptr u8 n n -- n ) {: a:ptr u:n w:n :}
   w WID-QUAL = if a u XREF-FIND ACAP-XREF-XT exit then
   a u w XREF-FIND-WL ACAP-XREF-XT ;

: ACAP-REFUSE-SITE ( n n ptr u8 n n -- ) {: s:n k:n a:ptr u:n w:n :}
   s" aot-capture: call site " type s .
   s" bakes the name " type a u type
   s"  in scope " type w .
   s" , which does not resolve to " type k ACAP-NAME.
   s"  in this engine" type cr
   s" aot-capture: a call site's name does not resolve the way the seed asks" 74 die ;

: ACAP-?SITE ( n n -- ) {: s:n k:n :}
   s ACAP-SITE-ROW {: r:ptr :}
   r 4 + ACAP-W32@ {: noff:n :}
   AOT-NAMES-BUF@ noff 1+ +  AOT-NAMES-BUF@ noff + c@ {: a:ptr u:n :}
   r 8 + ACAP-W32@ {: w:n :}
   w OWNER-API-PRI-WID = if
      k ACAP-PRE-R @ < if exit then
      s k a u w ACAP-REFUSE-SITE
   then
   a u w ACAP-SITE-XT  k AOT-REC AOT-RXT = if exit then
   s k a u w ACAP-REFUSE-SITE ;

\ --- scan the copied blob for call sites; record + canonicalize each ---
\ TWO OPCODES REACH A WORD FROM OUTSIDE IT AND BOTH TRAVEL BY NAME. A BL is
\ every ordinary call. A B is ordinary control flow, inside the word that emitted
\ it, where the rigid blob move keeps it exact — except for one producer:
\ LDOESPATCH plants `b D` at a created word's RET, and D is the does>-clause of
\ the DEFINING word. When that definer sits outside the window, its displacement
\ measures against code the target does not have, and three chain words came to
\ branch into the middle of PATHZ that way (dot
\ habu-merged-engine-nmigrate-c970bf04). So an OUT-OF-WINDOW B is a site, and the
\ rule over it is total rather than tolerant: it must name a record ENTRY. The
\ clause carries one now (habu2.f J-DOES), and a branch that resolves to no
\ record is refused BY NAME here — not counted into AOT-UNRES-N the way an
\ unresolved BL is, because a call to a word the capture kept in source is a word
\ nobody baked, while a branch with no name is a jump into whatever the delta
\ lands on. The in-window Bs stay verbatim: 4852 of them on the compiler chain,
\ and making each a name lookup would buy nothing a rigid move does not give.
: ACAP-SITE-ADD ( n -- ) {: k:n :}
   ACAP-P @ k ACAP-SITE-BAND                          \ ... and the target has this name
   k ACAP-SITE-SCOPE {: a:ptr u:n w:n :}
   ACAP-P @ a u w ACAP-ADD-SITE
   AOT-SITE-N @ 1- k ACAP-?SITE
   AOT-BLOB-BUF@ ACAP-P @ +        ACAP-ZERO-IMM ;     \ one 4-byte site
: ACAP-SITE-HERE ( -- )
   AOT-BLOB-BUF@ ACAP-P @ + ACAP-TGT {: t:n :}
   t ACAP-IN-CODE? if exit then
   t ACAP-TGT>REC {: k:n :}
   k 0 < if
      s" aot-capture: call target has no dictionary record" 74 die
   then
   k ACAP-SITE-ADD ;
: ACAP-REFUSE-BRANCH ( n -- ) {: t:n :}
   s" aot-capture: window word " type ACAP-P @ ACAP-REC-AT ACAP-NAME.
   s"  at blob offset " type ACAP-P @ .
   s" branches out of the window to " type t .
   s" , which is no record's entry and so has no name the seed can resolve" type cr
   s" aot-capture: out-of-window branch to no record" 74 die ;
: ACAP-BRANCH-HERE ( -- )
   AOT-BLOB-BUF@ ACAP-P @ + ACAP-TGT {: t:n :}
   t ACAP-IN-CODE? if exit then                       \ the blob's own control flow
   t ACAP-TGT>REC {: k:n :}
   k 0 < if t ACAP-REFUSE-BRANCH then
   k ACAP-SITE-ADD ;
: ACAP-SCAN-CALLS ( -- )
   0 ACAP-P !
   begin ACAP-P @ 4 + AOT-BLOB-LEN @ <= while
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-CALL? if ACAP-SITE-HERE then
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-BRANCH? if ACAP-BRANCH-HERE then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

\ Address sites come from the emitter's relocation map, never from recognizing
\ instruction bytes or guessing whether an integer looks like an address.
\ Both compilers record each chain where they create it. A direct call retains
\ its callee's chain and creates no address literal in the caller.
\
\ DATA and code sweeps classify recorded values against the captured spans.
\ In-window addresses move with their span. Pre-window code travels by the
\ callee's name; pre-window DATA has no general target-layout mapping and is
\ refused. Explicit compile handlers can still create such a DATA reference
\ (for example `is` on a pre-window defer), so this audit remains necessary.
: ACAP-CHAIN-BIT? ( n n -- bool ) {: bstart:n boff:n :}
   bstart boff + AOT-DBASE-N - {: off:n :}
   AOT-LIVE-DATA SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + AOT-A>U8 c@
   off 2 rshift 7 and rshift 1 and 0= 0= ;

: ACAP-ADD-DSITE ( n -- ) {: boff:n :}   \ store blob offset as u32
   AOT-CSITE-N @ 0<> if s" aot-capture: DATA sites follow CODE sites" 74 die then
   AOT-DSITE-N @ dup 0< swap AOT-DSITE-MAX >= or if
      s" aot-capture: too many DATA sites" 74 die then
   AOT-DSITE-N @ 1+ AOT-DSITE-RESERVE
   boff  AOT-DSITE-N @ 4 * AOT-DSITE-BUF@ +  AOT-P32!
   AOT-DSITE-N @ 1+ AOT-DSITE-N ! ;

: ACAP-ADD-CSITE ( n -- ) {: boff:n :}   \ append (as u32) after the DATA offsets in the DSITE buffer
   AOT-DSITE-N @ {: dn:n :} AOT-CSITE-N @ {: cn:n :}
   dn 0< dn AOT-DSITE-MAX > or cn 0< or if
      s" aot-capture: invalid relocation site counts" 74 die then
   cn AOT-DSITE-MAX dn - >= if s" aot-capture: too many reloc sites" 74 die then
   dn cn + 1+ AOT-DSITE-RESERVE
   boff dn cn + 4 * AOT-DSITE-BUF@ + AOT-P32!
   AOT-CSITE-N @ 1+ AOT-CSITE-N ! ;

\ A NAMED code site: the chain at this blob offset holds the entry of the word
\ called `a u`, and the seed is to resolve that name in the engine it is booting
\ rather than rebase what the chain holds now. The value in the blob is zeroed for
\ the same reason a recorded BL's imm26 is - a captured host address is both
\ builder-dependent and wrong in the seeded engine - and zeroing it means the boot
\ patch is the only thing that can put an address there.
\ ITS PRODUCER IS ACAP-OUT-CHAIN BELOW. A code literal a window word CREATES for a
\ pre-window word (`['] X` on a prefix word) is what needs this.
\ The compile handler creates that literal in the window body (C-BTICK calls
\ C-CODE-ADDR); direct call emission does not remove it. Only a name-keyed
\ carry can answer it. An in-window code literal is NOT a candidate: rebasing it by
\ the code delta is correct and costs no lookup.
: ACAP-ADD-XTSITE ( n ptr u8 n -- ) {: boff:n a:ptr u:n :}
   AOT-XTSITE:N @ AOT-XTSITE:MAX >= if s" aot-capture: too many named code sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-XTSITE:N @ 8 * AOT-XTSITE:BUF@ + {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!
   AOT-BLOB-BUF@ boff +  0 SNAP-RELOC:SET-CHAIN                 \ no host address travels in the blob
   AOT-XTSITE:N @ 1+ AOT-XTSITE:N ! ;

\ The refusal, with the site named. A capture that cannot classify one of its own
\ recorded chains has nothing correct to bake, so it dies rather than choosing.
: ACAP-UNCLASSIFIED ( n n -- ) {: boff:n v:n :}
   s" aot-capture: window word " type boff ACAP-REC-AT ACAP-NAME.
   s"  at blob offset " type boff .
   s" carries " type v .
   s" which is " type v ACAP-BAND.
   s" and so in neither the window's DATA span nor its code span" type cr
   s" aot-capture: recorded address site outside both window spans" 74 die ;

\ A recorded chain the window's DATA span does not hold. Three outcomes, and the
\ middle one is what dot habu-widen-the-aot-089f5faf added.
\
\ IN-WINDOW CODE is left alone: ACAP-SCAN-CSITES rebases it by the code delta, and
\ the value it must preserve is (value - b0), which the second sweep stores.
\
\ A WORD'S ENTRY becomes a name-keyed row. The chain holds the code entry of a word
\ the host dictionary knows and the window does not contain, which is exactly the
\ shape a `['] X` on a PRE-WINDOW word compiles to. Its value cannot
\ be rebased -- the metabuild host recompiles the whole core prefix a second time
\ without rewinding DP, so its prefix band has no counterpart in the target and no
\ delta relates the two -- and it cannot be left, because that bakes the building
\ host's address into bin/hb. What it CAN be is what a call site already is: a NAME.
\ The name must resolve to this exact entry in the eligible prefix, using the
\ same global/public alias check as stored CODE cells. ACAP-ADD-XTSITE zeroes the
\ four lanes, so no host address is left underneath the seed's answer.
\
\ ANYTHING ELSE STILL ENDS THE BUILD, and the two classes cannot be confused. A
\ pre-window DATA address is the other thing a window word can hold that the spans
\ do not place, and it can never match here: a record's [0] is a code ENTRY, and
\ DATA-VA sits far above every address the code region can hold on either target,
\ so no DATA address equals any record's xt. Pre-window DATA is eliminated at the
\ ordinary call site instead; an explicit pre-window DATA literal still receives
\ the named refusal.
: ACAP-OUT-CHAIN ( n n n n -- ) {: boff:n v:n bstart:n bend:n :}
   v bstart >= v bend < and if exit then          \ in-window code: the CODE sweep rebases it
   v ACAP-TARGET-NAME? if
      {: name:ptr size:n :}
      boff name size ACAP-ADD-XTSITE exit
   then 2drop
   boff v ACAP-UNCLASSIFIED ;

\ The DATA half, and the totality check. Every recorded site is classified here:
\ one in the DATA span is recorded for the boot DATA-reloc pass, and every other
\ one goes to ACAP-OUT-CHAIN above, which rebases, names, or refuses it.
\ THE ZEROED LANES DO NOT DISTURB THE SECOND SWEEP. A named row's chain is left
\ holding 0, and ACAP-SCAN-CSITES asks the same in-code-span question of it, which
\ 0 fails -- so a site named here is not also rebased there. The interior words of
\ the chain are never re-examined either way: the address map carries one bit at
\ each chain's START and this walk tests that bit before it reads anything.
: ACAP-SCAN-DSITES ( n n n n -- ) {: bstart:n bend:n d0:n d1:n :}
   d0 AOT-DATA-D0 !  d1 d0 - AOT-DATA-SIZE !
   0 ACAP-P !
   begin ACAP-P @ SNAP-RELOC:ADDR-CHAIN-BYTES + AOT-BLOB-LEN @ <= while
      bstart ACAP-P @ ACAP-CHAIN-BIT? if
         AOT-BLOB-BUF@ ACAP-P @ + SNAP-RELOC:CHAINV {: v:n :}
         v d0 >= v d1 < and if
            ACAP-P @ ACAP-ADD-DSITE
         else
            ACAP-P @ v bstart bend ACAP-OUT-CHAIN
         then
      then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

\ A deferred word's trailer contains the same DATA address as its dispatch
\ code. Its magic identifies the metadata field; aliases must relocate it once.
: ACAP-DSITE-HELD? ( n -- bool ) {: site:n :}
   AOT-DSITE-N @ 0 ?do
      AOT-DSITE-BUF@ i 4 * + ACAP-W32@ site = if true unloop exit then
   loop
   false ;

: ACAP-DEFER-SITE ( n n n n n -- )
   {: k:n bstart:n bend:n d0:n d1:n :}
   k AOT-REC {: rec:ptr :}
   \ Namespace [8] is a private wordlist ID, not a dictionary code length.
   rec AOT-RWID DICT-WL:NAMESPACE = if exit then
   rec AOT-RXT rec AOT-RBODY + {: meta:n :}
   meta bstart < meta 16 + bend > or if exit then
   meta AOT-N>U8 CELL-VIEW AOT-CELL@ DEFER-MAGIC <> if exit then
   meta 8 + AOT-N>U8 CELL-VIEW AOT-CELL@ {: addr:n :}
   addr d0 < addr CELL + d1 > or if
      s" aot-capture: defer metadata outside DATA window" 74 die
   then
   meta 8 + bstart - AOT-DSITE-CELL or {: site:n :}
   site ACAP-DSITE-HELD? 0= if site ACAP-ADD-DSITE then ;

: ACAP-SCAN-DEFER-SITES ( n n n n -- )
   {: bstart:n bend:n d0:n d1:n :}
   ACAP-W-R1 @ ACAP-W-R0 @ ?do
      i bstart bend d0 d1 ACAP-DEFER-SITE
   loop ;

\ The CODE half. Its sites are the anonymous quotation entry addresses, and each
\ is canonicalized into a b0-relative offset with captureB0 = 0. The boot pass
\ rebases every recorded literal by the code delta (seedCP - captureB0), so the
\ only invariant the stored value must preserve is (value - b0). Before the JIT
\ region moved it mapped at a fixed VA and a raw absolute was builder-invariant;
\ since the move the region base is the runtime __text-relative base (ASLR-varying
\ on macOS, fixed VMBASE on Linux), so a raw absolute would make the baked blob
\ depend on the builder's live region and the seed would never reach a fixpoint.
\ Storing the offset reproduces the old relocation result byte-for-byte.
\
\ IT IS A SECOND SWEEP AND NOT A SECOND TEST, because the two site lists share one
\ buffer with every DATA offset ahead of every code offset - ACAP-ADD-CSITE
\ appends past AOT-DSITE-N - so the DATA sweep has to finish before the first code
\ offset is written.
: ACAP-SCAN-CSITES ( n n -- ) {: bstart:n bend:n :}
   0 AOT-CODE-B0 !                                      \ canonical code base 0
   0 ACAP-P !
   begin ACAP-P @ SNAP-RELOC:ADDR-CHAIN-BYTES + AOT-BLOB-LEN @ <= while
      bstart ACAP-P @ ACAP-CHAIN-BIT? if
         AOT-BLOB-BUF@ ACAP-P @ + SNAP-RELOC:CHAINV {: v:n :}
         v bstart >= v bend < and if
            ACAP-P @ ACAP-ADD-CSITE
            AOT-BLOB-BUF@ ACAP-P @ +  v bstart -  SNAP-RELOC:SET-CHAIN
         then
      then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

\ --- the captured window's DATA content ---------------------------------------
\ WHY THE BYTES HAVE TO TRAVEL. The seed used to reserve the span and copy
\ nothing, on the reading that a REPL window is all `allot`/`variable` and so all
\ zero. It is not: a TRUST row's name and signature are `s"` literals interned
\ into the DP heap, so the window carries real bytes and the seeded engine read
\ zeros where they should have been. Measured on the metabuild window before this
\ changed: 5726 bytes of span, 24 of them nonzero.
\
\ AND WHY THE SPAN MAY NOT. Carrying the whole span was the first repair and it
\ was too coarse: the compiler chain's window is 1,531,045 bytes of span holding
\ 32 bytes of content, so the engine baked 1.5 MB of zeros - 42% of the product -
\ to deliver four cells (dot habu-census-the-captured-fe5f7c49). What travels now
\ is the NON-ZERO EXTENTS, and the seed zeroes the span before it lays them in.
\
\ AND WHY ONE KIND OF BYTE MAY NOT. A declared address cell holds either an XT in
\ the BUILDING host's JIT window or a pointer in that host's captured DATA
\ window. Neither raw address belongs to the seeded engine. THE INVARIANT: a
\ declared address cell's value is owned by its declaration, never by the
\ window's bytes. Every declared cell the window contains is therefore captured
\ structurally: the seed recreates the exact null or window-relative target and
\ re-registers the cell's kind, and a non-null target the window does not place is
\ refused. A cell the window does NOT contain is a cell of the booting engine, and
\ ACAP-BAKE-DATA below says which of those the capture may describe and why.
\ The set is taken from the table and never from what a cell contains: the table
\ is written where a cell's kind is decided, which is the only place it is known.
: ACAP-ADD-XTOFF ( n n -- ) {: celloff:n meta:n :}
   AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-MAX >= if s" aot-capture: too many declared address cells" 74 die then
   AOT-WINDOW:XTOFF-N @ 1+ AOT-WINDOW:XTOFF-RESERVE
   AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-ROW * AOT-WINDOW:XTOFF-BUF@ + {: row:ptr :}
   celloff row AOT-P32!
   meta row 4 + AOT-P32!
   AOT-WINDOW:XTOFF-N @ 1+ AOT-WINDOW:XTOFF-N ! ;

: ACAP-XTOFF@ ( n -- n ) {: k:n :}
   k AOT-WINDOW:XTOFF-ROW * AOT-WINDOW:XTOFF-BUF@ + ACAP-W32@ ;

: ACAP-XTMETA@ ( n -- n ) {: k:n :}
   k AOT-WINDOW:XTOFF-ROW * AOT-WINDOW:XTOFF-BUF@ + 4 + ACAP-W32@ ;

: ACAP-XTCELL-ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;

: ACAP-XTCELL-RAW ( n -- n ) ADDRESS-CELLS:ROW@ ;

: ACAP-XTCELL-OFF ( n -- n ) ACAP-XTCELL-RAW SNAP-RELOC:XTCELL-OFF-MASK and ;

: ACAP-XTCELL-DATA? ( n -- bool )
   ACAP-XTCELL-RAW SNAP-RELOC:XTCELL-DATA-TAG and 0 <> ;

: ACAP-XTCELL-AT ( n -- ptr n ) {: k:n :}
   AOT-LIVE-DATA k ACAP-XTCELL-OFF + ;

: ACAP-XTCELL-CELL-REFUSE ( n -- ) {: off:n :}
   s" aot-capture: declared address cell outside DATA: " type off . cr
   s" aot-capture: declared address cell is outside DATA" 74 die ;

: ACAP-XTCELL-CELL-CHECK ( n -- ) {: off:n :}
   off SNAP-RELOC:XTCELL-OFF-MAX > if off ACAP-XTCELL-CELL-REFUSE then ;

\ A row that overlaps the window without lying wholly inside it would leave half a
\ host address in the baked bytes, so it ends the build rather than being skipped.
: ACAP-XTCELL-STRADDLES ( n -- ) {: woff:n :}
   s" aot-capture: declared address cell straddles the window edge at offset " type woff . cr
   s" aot-capture: declared address cell straddles the window edge" 74 die ;

: ACAP-CLASSIFY-XTCELL ( n n -- ) {: woff:n len:n :}
   woff 8 + 0 <= if exit then                         \ wholly below the window
   woff len >= if exit then                          \ wholly above the window
   woff 0 >= woff 8 + len <= and if exit then        \ wholly inside; run scan masks it
   woff ACAP-XTCELL-STRADDLES ;                       \ either edge overlaps a partial cell

: ACAP-TARGET-REFUSE ( n -- ) {: v:n :}
   s" aot-capture: declared address target outside its capture window: " type v . cr
   s" aot-capture: declared address target is not self-contained" 74 die ;

: ACAP-TARGET-OFFSET ( n n n -- n ) {: v:n lo:n hi:n :}
   v 0= if 0 exit then
   v lo >= v hi < and 0= if v ACAP-TARGET-REFUSE then
   v lo - 1+ dup AOT-WINDOW:XTOFF-VALUE-MASK > if v ACAP-TARGET-REFUSE then ;

\ The cell's declared target beside the window its KIND names: a DATA-pointer cell
\ is answered by the DATA span and an execution-token cell by the code span, which
\ is the whole reason the kind is recorded where it is decided rather than guessed
\ from the value.
: ACAP-XTCELL-TARGET ( n n n n n -- n n n ) {: k:n b0:n b1:n d0:n d1:n :}
   k ACAP-XTCELL-AT AOT-CELL@
   k ACAP-XTCELL-DATA? if d0 d1 else b0 b1 then ;

: ACAP-XTCELL-TARGET-IN? ( n n n n n -- bool )
   ACAP-XTCELL-TARGET {: v:n lo:n hi:n :}
   v lo >= v hi < and ;

: ACAP-CELL-NAMED ( n -- n ) {: target:n :}
   target ACAP-TARGET-NAME? if
      ACAP-POOL-ADD 1+ AOT-WINDOW:XTOFF-NAME-TAG or exit
   then 2drop
   target ACAP-TARGET-REFUSE ;

: ACAP-XTCELL-META ( n n n n n -- n ) {: k:n b0:n b1:n d0:n d1:n :}
   k b0 b1 d0 d1 ACAP-XTCELL-TARGET {: v:n lo:n hi:n :}
   k ACAP-XTCELL-DATA? 0= v 0<> and if
      v lo < v hi >= or if v ACAP-CELL-NAMED exit then
   then
   v 0<> v lo < v hi >= or and if
      s" aot-capture: address row " type k .
      s"  cell DATA+" type k ACAP-XTCELL-OFF .
      s"  expected range " type lo . hi . cr
   then
   v lo hi ACAP-TARGET-OFFSET
   k ACAP-XTCELL-DATA? if AOT-WINDOW:XTOFF-DATA-TAG or then ;

\ --- the window's non-zero extents --------------------------------------------
\ ONE ROW AND ITS BYTES, APPENDED TOGETHER. The bytes go into their own section in
\ ROW ORDER, so a row needs no offset into them: the decoder walks the byte
\ section with a running cursor, which is the same counts-not-stored discipline
\ the rest of the format keeps. Both buffers refuse their own overflow by name.
variable ACAP-RS      \ the open run's start, or -1 when none is open
variable ACAP-RE      \ one past the open run's last non-zero byte
variable ACAP-RP      \ the scan cursor inside one segment
variable ACAP-RQ      \ the segment cursor across the window
variable ACAP-RN      \ the next declared cell at or above ACAP-RQ
variable ACAP-RC      \ ACAP-NEXT-CELL's running minimum

: ACAP-ADD-RUN ( n n n -- ) {: d0:n off:n rl:n :}
   AOT-WINDOW:RUN-N @ AOT-WINDOW:RUN-MAX >= if
      s" aot-capture: too many window DATA runs" 74 die then
   AOT-WINDOW:RBYTES-LEN @ rl + AOT-WINDOW:RBYTES-CAP > if
      s" aot-capture: the window DATA runs exceed the AOT run-byte buffer" 74 die then
   AOT-WINDOW:RUN-N @ AOT-WINDOW:RUN-ROW * AOT-WINDOW:RUN-BUF@ + {: r:ptr :}
   off r AOT-P32!  rl r 4 + AOT-P32!
   rl 0 ?do
      d0 off + i + AOT-N>U8 c@
      AOT-WINDOW:RBYTES-BUF@ AOT-WINDOW:RBYTES-LEN @ + i + c!
   loop
   AOT-WINDOW:RBYTES-LEN @ rl + AOT-WINDOW:RBYTES-LEN !
   AOT-WINDOW:RUN-N @ 1+ AOT-WINDOW:RUN-N ! ;

: ACAP-RUN-CLOSE ( n n -- ) {: d0:n at:n :}
   ACAP-RS @ 0 < if exit then
   d0 ACAP-RS @ at ACAP-RS @ - ACAP-ADD-RUN
   -1 ACAP-RS ! ;

\ The non-zero extents of [from, to), which is one gap between declared cells.
\ Nothing outside such a gap is ever offered, which is how the cells stay out of
\ every run - and it is also why the merge below can never swallow one: a run
\ closes at every segment end, so a declared cell is a boundary no gap crosses.
\ A run ends at its last non-zero byte and reopens only after RUN-GAP-MIN zeros,
\ so shorter gaps travel inside it rather than buying a second row.
: ACAP-SCAN-SEG ( n n n -- ) {: d0:n from:n to:n :}
   -1 ACAP-RS !
   from ACAP-RE !
   from ACAP-RP !
   begin ACAP-RP @ to < while
      d0 ACAP-RP @ + AOT-N>U8 c@ 0=
      if    ACAP-RP @ ACAP-RE @ - AOT-WINDOW:RUN-GAP-MIN >=
            if d0 ACAP-RE @ ACAP-RUN-CLOSE then
      else  ACAP-RS @ 0 < if ACAP-RP @ ACAP-RS ! then
            ACAP-RP @ 1+ ACAP-RE !
      then
      ACAP-RP @ 1+ ACAP-RP !
   repeat
   d0 ACAP-RE @ ACAP-RUN-CLOSE ;

\ The lowest declared-cell offset at or above `p`, or the span when none is left.
\ Asked once per gap rather than once per byte, and it reads the table in whatever
\ order the engine registered it - so the scan needs no ordering the engine does
\ not promise. That costs a pass per gap, and the gaps are the cells plus one, so
\ the whole walk is the window's length plus the square of its declared-cell
\ count. An independently owned index can improve this without changing the
\ append order of the engine registry or the artifact rows.
: ACAP-NEXT-CELL ( n n -- n ) {: p:n len:n :}
   len ACAP-RC !
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i ACAP-XTOFF@ {: loc:n :}
      loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0<> if
         loc AOT-WINDOW:XTOFF-LOC-MASK and {: off:n :}
         off p >= off ACAP-RC @ < and if off ACAP-RC ! then
      then
   loop
   ACAP-RC @ ;

: ACAP-SCAN-RUNS ( n n -- ) {: d0:n len:n :}
   0 ACAP-RQ !
   begin ACAP-RQ @ len < while
      ACAP-RQ @ len ACAP-NEXT-CELL ACAP-RN !
      d0 ACAP-RQ @ ACAP-RN @ ACAP-SCAN-SEG
      ACAP-RN @ 8 + ACAP-RQ !
   repeat ;

\ Where the seed will find this cell: a window offset under the window tag for a
\ cell the capture carries, and a plain DATA offset for a fixed engine cell.
: ACAP-XTCELL-LOC ( n n n -- n ) {: woff:n celloff:n len:n :}
   woff 0 >= woff len < and if woff AOT-WINDOW:XTOFF-WINDOW-TAG or exit then
   celloff ;

\ WHOSE CELL IT IS, and it is not always the window's. A cell INSIDE the captured
\ DATA span travels with the window: its row carries a window-relative location,
\ the seed recreates its value and re-registers its kind, and ACAP-CLASSIFY-XTCELL
\ keeps its bytes out of every sparse run.
\
\ A CELL THE WINDOW DOES NOT CONTAIN BELONGS TO THE ENGINE THE WINDOW WAS LOADED
\ INTO, and the capture describes exactly one thing about it: an address of the
\ WINDOW that the window's load stored there. That store is a load-time effect no
\ captured byte carries, the seed has a window-relative target to write, and the
\ target engine has nothing of its own at that address - so the row travels. This
\ is how a metabuilt engine gets its checker hook: the window recompiles the
\ checker, `set-check` stores the new xt into the fixed HOOK-CELL below the
\ window, and row 0 of the table is what puts it back in the engine being written.
\
\ ANY OTHER TARGET IS STATE THE WINDOW DID NOT CREATE, and no row may travel for
\ it. A capture running in a booted engine meets two populations of them, both
\ declared before the window opened: the engine's own hook cells, holding xts the
\ booting engine's prefix installed, and one cell per declared cell of that
\ engine's OWN captured window, holding addresses in that window. Neither has a
\ correct value the capture could write - a window-relative target would name the
\ wrong address and a null would erase what the seeded engine's prefix put there -
\ and the seeded engine's own state is already right, so the cell is left alone.
\ Refusing instead is what this used to do, and it ended every capture taken
\ inside a booted engine on row 0, the engine's own HOOK-CELL (dot
\ habu-keep-declared-addr-dbd7d8d9). The refusal it kept is still reachable and
\ still fail-closed, on the cell that has no other answer: one INSIDE the window,
\ whose bytes travel and whose target the window does not place
\ (test/aot-address-cell-target-out-bad.f).
: ACAP-BAKE-DATA ( n n n n -- ) {: b0:n b1:n d0:n d1:n :}
   d1 d0 - {: len:n :}
   len AOT-WINDOW:SPAN-CAP > if
      s" aot-capture: DATA window exceeds the AOT window span cap" 74 die then
   d0 AOT-DATA-N - {: d0off:n :}
   ACAP-XTCELL-ROWS 0 ?do
      i ACAP-XTCELL-OFF {: celloff:n :}
      celloff ACAP-XTCELL-CELL-CHECK
      celloff d0off - {: woff:n :}
      woff len ACAP-CLASSIFY-XTCELL
      woff 0 >= woff len < and
      i b0 b1 d0 d1 ACAP-XTCELL-TARGET-IN? or if
         i b0 b1 d0 d1 ACAP-XTCELL-META {: meta:n :}
         woff celloff len ACAP-XTCELL-LOC
         meta ACAP-ADD-XTOFF
      then
   loop
   d0 len ACAP-SCAN-RUNS ;

\ --- boot-run list: append a top-level entry-word NAME to the 0-terminated
\ [len][name] list EM-AOT-BOOTRUN walks (LFIND + blr) after the seed installs the
\ REPL. Keeps a live trailing 0 terminator (uncounted) so the bake needs no pad.
\ The seed that walks it runs at the END OF THE ENGINE PREFIX on EVERY boot
\ (src/habu/habu2.f, AOT-BOOTRUN-CAP), so a name added here runs before the first
\ user token of a piped program, a `--load` run and a tty REPL alike. A fixture
\ that reports from inside a capture window can therefore be a batch fixture: it
\ pipes a line to the built engine and reads what the boot-run printed. A pty is
\ still what an INTERACTIVE claim needs (the entry words ask TTY? themselves), and
\ nothing else. ---
public

\ ---- the build-side name map ---------------------------------------------------
\ Every record the capture SAW, shipped or not, read back by capture order. The
\ image keeps the names it can be asked for and strips the rest, so a tool that
\ needs to name the code a stripped image carries reads the map the build wrote
\ beside it (tools/native-build-core.f writes <image>.names from these readers).
\ The verbatim 48-byte records still hold every name when the driver asks, which
\ is where the stripped ones come from.

: MAP-N ( -- n )
   ACAP-REC-ALL @ ;

: MAP-NAMED ( n -- n )                             \ 1 when the image kept the name, 0 when stripped
   cells ACAP-NAMED-BIT + @ ;

: MAP-NAME$ ( n -- ptr u8 n )
   ACAP-REC-NAME$ ;

: MAP-START ( n -- n )                             \ code blob offset, build-time
   ACAP-REC-DST ACAP-W32@ ;

: MAP-LEN ( n -- n )                               \ code length in bytes
   ACAP-REC-DST 8 + ACAP-W32@ ;

: MAP-WID ( n -- n )                               \ wordlist id, or -1 for a package row
   ACAP-REC-DST {: v:ptr :}
   v CELL-VIEW AOT-RWID -1 = if -1 exit then
   v 40 + ACAP-W32@ ;

: BOOTRUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 255 > if s" aot-capture: boot-run name too long" 74 die then
   AOT-BOOTRUN-LEN @ u + 2 + AOT-BOOTRUN-CAP > if s" aot-capture: boot-run overflow" 74 die then
   AOT-BOOTRUN-LEN @ {: off:n :}
   u  AOT-BOOTRUN-BUF@ off + c!                     \ [len]
   u 0 ?do a i + c@  AOT-BOOTRUN-BUF@ off + 1+ i + c!  loop
   off u + 1+ AOT-BOOTRUN-LEN !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;      \ live terminator (uncounted)

\ WHAT THE ROW TABLE MUST HOLD, as the two populations ACAP-BAKE-DATA writes a row
\ for, each recounted off the LIVE table with that word's own arithmetic. A producer
\ can then check the length of the table it just produced against a reading that did
\ not come from the table (tools/aot-chain-capture.f ?XTOFF, and the same predicate
\ over a real capture in test/aot-artifact-roundtrip.f), which is what catches a row
\ never written, a row written twice, and a row for a cell in neither population.
\ The two are disjoint because the classification is: a cell INSIDE the window
\ counts here however its target is placed.
\
\ DECLARED-IN is the cells inside the window's DATA span. Their bytes are kept out
\ of every sparse run, so the value the seed writes can only come from their row.
: DECLARED-IN ( n n -- n ) {: d0:n d1:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   d1 d0 - {: len:n :}
   0
   ACAP-XTCELL-ROWS 0 ?do
      i ACAP-XTCELL-OFF d0off - {: woff:n :}
      woff 0 >= woff len < and if 1+ then
   loop ;

\ TARGETED-OUT is the cells the window does NOT contain whose declared target is
\ inside their kind's span. They belong to the engine the window was loaded into
\ and travel for one reason: the window's load stored a window address there, which
\ is a load-time effect no captured byte carries. TRAPPED-BELOW asks a narrower
\ question of the same population - the CODE cells below the window, which are the
\ ones a boot-run installer has to refill - so the two are not interchangeable.
: TARGETED-OUT ( n n n n -- n ) {: b0:n b1:n d0:n d1:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   d1 d0 - {: len:n :}
   0
   ACAP-XTCELL-ROWS 0 ?do
      i ACAP-XTCELL-OFF d0off - {: woff:n :}
      woff 0 >= woff len < and 0= if
         i b0 b1 d0 d1 ACAP-XTCELL-TARGET-IN? if 1+ then
      then
   loop ;

\ How many declared address cells BELOW the window hold an address inside it.
\
\ THIS IS THE OTHER HALF OF THE BOOT-RUN CONTRACT, and the half nothing measured
\ until now. A declared cell wholly INSIDE the window is excluded from the
\ sparse DATA runs by ACAP-CLASSIFY-XTCELL, so its relocated value comes only
\ from XTOFF. A cell below the window is not captured at all - it belongs to the
\ engine the window was loaded into - and if the window's load PLANTED a window
\ address in it, then that write is a load-time effect no captured byte carries.
\ In a seeded engine the cell holds whatever the target's own prefix put there,
\ and the only thing that can put the window's routine back is a boot-run entry.
\ So every cell counted here is a boot-run row owed, and a caller that declares
\ fewer rows than this counts has an installer it has not declared.
\
\ IT ANSWERS THE CELL'S CONTENT, NOT ITS KIND, which is what makes it total: the
\ engine registers a row wherever an execution token is stored into a `defer`,
\ so an installer of a kind nobody has written yet is counted the same way. A
\ cell whose value points somewhere else is somebody else's business and is not
\ counted, so an install made after the window closes cannot be mistaken for one
\ the window made.
: TRAPPED-BELOW ( n n n -- n ) {: b0:n b1:n d0:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   0
   ACAP-XTCELL-ROWS 0 ?do
      i ACAP-XTCELL-OFF {: off:n :}
      i ACAP-XTCELL-DATA? 0= off d0off < and if
         i ACAP-XTCELL-AT AOT-CELL@ {: v:n :}
         v b0 >= v b1 < and if 1+ then
      then
   loop ;

\ What the signature audit measured, so a reader can check the capture against
\ the window instead of taking the audit's word for it: the records it found the
\ checker knows an effect for, and the ones it exempted (package rows, retired
\ rows, and words with no checker effect at all). The two sum to the window's
\ record count, which is what tools/aot-chain-capture.f prints them beside.
: SIG-KNOWN ( -- n ) ACAP-SIG-KNOWN @ ;
: SIG-EXEMPT ( -- n ) ACAP-SIG-EXEMPT @ ;

private

\ --- protected WIDs owned by this window --------------------------------------
: ACAP-PWID-IN-RANGE? ( n -- bool ) {: wid:n :}
   wid 0 < 0=  wid PROT-WID-MAX <  and ;
: ACAP-LIVE-PWID? ( n -- bool ) {: wid:n :}
   wid ACAP-PWID-IN-RANGE? 0= if 0 0= 0= exit then
   AOT-LIVE-DATA PROT-BITS-OFF + wid 3 rshift + AOT-A>U8 c@
   wid 7 and rshift 1 and 0= 0= ;
: ACAP-PWIN-ADD ( n -- ) {: rel:n :}
   rel AOT-PWIN-N @ 4 * AOT-PWIN-BUF@ + AOT-P32!
   AOT-PWIN-N @ 1+ AOT-PWIN-N ! ;

\ Capture only WIDs created in the runtime window.  Absolute build-host bits are
\ deliberately ignored: replaying them would resurrect the discarded namespace.
: ACAP-PWIN-CAPTURE ( -- )
   AOT-LIVE-DATA AOT-DBASE = if
      s" aot-capture: live DATA aliases dictionary base" 74 die
   then
   0 ACAP-LIVE-PWID? if
      s" aot-capture: protected-WID registry marks WID 0" 74 die
   then
   0 AOT-PWIN-N !
   AOT-WID-W0 @ AOT-WID-SPAN @ + {: w1:n :}
   w1 AOT-WID-W0 @ ?do
      i ACAP-LIVE-PWID? if i AOT-WID-W0 @ - ACAP-PWIN-ADD then
   loop ;

\ Capture the words in dict[rec-start, rec-end) compiled contiguously into the host
\ region [blob-start, blob-end); [d0,d1) is the REPL DATA span (create/variable).
: ACAP-RESET ( -- )
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  ACAP-POOL-RESET
   0 AOT-DSITE-N !  0 AOT-DATA-D0 !  0 AOT-DATA-SIZE !
   0 AOT-CSITE-N !  0 AOT-CODE-B0 !  0 AOT-WINDOW:XTOFF-N !
   0 AOT-WINDOW:RUN-N !  0 AOT-WINDOW:RBYTES-LEN !
   0 AOT-XTSITE:N !  0 AOT-PWIN-N !
   0 AOT-BOOTRUN-LEN !  0 AOT-BOOTRUN-BUF@ c! ;
public

\ AOT-ARM owns the captured cursors and frozen checker payload. Producers latch
\ them before loading this tool, whose own compiler dependencies must remain
\ outside the captured window.

\ The band the two audits read, latched from this capture's own arguments. The
\ marks are NOT reset with the buffers: they describe the process, and a widened
\ re-capture of the same window (test/aot-wid-build.f) is the same process.
: ACAP-BAND! ( n n n n -- ) {: bstart:n rstart:n rend:n d0:n :}
   ACAP-MARKED? @ 0= if
      s" aot-capture: capture without a declared prelude band" 74 die
   then
   ACAP-PRE-R @ rstart > if
      s" aot-capture: prelude mark above the window's first record" 74 die
   then
   ACAP-PRE-D @ d0 > if
      s" aot-capture: prelude DATA mark above the window's DATA base" 74 die
   then
   AOT-ARM:W1 @ AOT-ARM:W0 @ < if
      s" aot-capture: the window's wordlist span ends before it starts" 74 die
   then
   bstart ACAP-W-B0 !
   rstart ACAP-W-R0 !  rend ACAP-W-R1 !
   d0 ACAP-W-D0 !
   AOT-ARM:W0 @ AOT-WID-W0 !
   AOT-ARM:W1 @ AOT-ARM:W0 @ - AOT-WID-SPAN ! ;

\ Release transient mappings at their last use, immediately before DATA copy.
\ A replacement runtime in the window owns the whole registry being captured;
\ a retained runtime also owns host/writer records outside this DATA span.
: ACAP-DBUF-XT ( ptr u8 n -- n )
   XREF-FIND ACAP-XREF-XT ;

\ These operations have known effects but are resolved from the live instance.
TRUSTED: ACAP-CLEANUP-XT ( n -- [ -- ] ) ;
TRUSTED: ACAP-RANGE-XT ( n n n -- ptr u8 n [ ptr u8 n -- ] ) ;
: ACAP-RUN-XT ( n -- ) ACAP-CLEANUP-XT execute ;
: ACAP-RUN-RANGE ( n n n -- ) ACAP-RANGE-XT execute ;

: ACAP-RELEASE-DYNAMIC ( n n n n -- ) {: b0:n b1:n d0:n d1:n :}
   d1 d0 < if s" aot-capture: reversed dynamic DATA span" 74 die then
   s" DYNAMIC-STORAGE:RELEASE-ALL" ACAP-DBUF-XT {: xt:n :}
   xt b0 >= xt b1 < and if xt ACAP-RUN-XT exit then
   s" DYNAMIC-STORAGE:RELEASE-RANGE" ACAP-DBUF-XT {: range:n :}
   range 0= if
      s" aot-capture: dynamic storage has no range release" 74 die then
   d0 d1 d0 - range ACAP-RUN-RANGE ;

\ Full-runtime payload mode is explicit. Its verified checker stores travel in
\ DATA, so an empty sidecar is valid only for that captured owner and closure.
TRUSTED: ACAP-ADDRESS ( ptr u8 -- n ) ;

: ACAP-MEMBER? ( ptr u8 n ptr u8 n -- bool )
   {: pkg:ptr pkgu:n name:ptr nameu:n :}
   \ A later window may give a previously queried WID a new package owner.
   0 ACAP-PKG-MEMO-W ! 0 ACAP-PKG-MEMO-ROW ! 0 ACAP-PKG-MEMO-PUB !
   AOT-ARM:R1 @ AOT-ARM:R0 @ ?do
      i AOT-REC {: rec:ptr :}
      rec AOT-RWID dup 0 >= if
         ACAP-REC-PKG {: pa:ptr pu:n pub:bool found:bool :}
         found if
            pa pu pkg pkgu CORE-STR=CI if
               rec AOT-RNPTR rec AOT-RNLEN name nameu CORE-STR=CI if
                  0 0= unloop exit then
            then
         then
      else drop then
   loop
   0 0= 0= ;

: ACAP-PERSISTENT-OWNER ( -- )
   AOT-ARM:PAYLOAD-OWNER @ CHECKER-OWNER-ABI:BYTES
   CHECKER-OWNER-GUARD:VALIDATE {: owner:ptr :}
   owner ACAP-ADDRESS {: at:n :}
   at AOT-ARM:D0 @ < if
      s" aot-capture: persistent checker owner is outside captured DATA" 74 die then
   at AOT-ARM:D0 @ - CHECKER-OWNER-ABI:HEADER-BYTES < if
      s" aot-capture: checker owner descriptor is outside captured DATA" 74 die then
   at AOT-ARM:D1 @ > if
      s" aot-capture: persistent checker owner is outside captured DATA" 74 die then
   owner 8 - CELL-VIEW @ AOT-ARM:D1 @ at - > if
      s" aot-capture: checker owner extends beyond captured DATA" 74 die then
   owner CHECKER-OWNER-ABI:CAPTURE-OFF + CELL-VIEW @ {: prepare:n :}
   prepare AOT-ARM:B0 @ < prepare AOT-ARM:B1 @ >= or if
      s" aot-capture: checker preparation is outside captured code" 74 die then
   s" CHECKER-REG" s" DECLARATIONS" ACAP-MEMBER? 0= if
      s" aot-capture: persistent payload has no captured checker declaration owner" 74 die then
   s" PREFIX-MARK" s" CURSORS" ACAP-MEMBER? 0= if
      s" aot-capture: persistent payload has no completed core prefix" 74 die then ;

: PAYLOAD-CAPTURE ( -- )
   AOT-ARM:?FROZEN
   AOT-ARM:PAYLOAD-MODE @ 1 = if
      AOT-ARM:PAYLOAD-EXPORTED @ if exit then
      AOT-ARM:B0 @ AOT-ARM:R0 @ AOT-ARM:R1 @ AOT-ARM:D0 @ ACAP-BAND!
      ACAP-AUDIT-SIGS
      -1 AOT-ARM:PAYLOAD-EXPORTED ! exit
   then
   ACAP-PERSISTENT-OWNER
   0 ACAP-SIG-KNOWN ! 0 ACAP-SIG-EXEMPT !
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN ! ;

\ The payload membership describes precisely the bytes and records copied by
\ CAPTURE. The final DATA end may be updated after owner persistence, before
\ WINDOW$ is read; callers cannot substitute another band at the copy seam.
: ACAP-PAYLOAD-BAND? ( n n n n n n -- bool )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart AOT-ARM:B0 @ = bend AOT-ARM:B1 @ = and
   rstart AOT-ARM:R0 @ = and rend AOT-ARM:R1 @ = and
   d0 AOT-ARM:D0 @ = and d1 AOT-ARM:D1 @ = and ;

: CAPTURE ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart bend rstart rend d0 d1 ACAP-PAYLOAD-BAND? 0= if
      s" aot-capture: capture bounds differ from frozen payload window" 74 die then
   bstart rstart rend d0 ACAP-BAND!
   PAYLOAD-CAPTURE
   ACAP-RESET
   ACAP-TIDX-BUILD                              \ xt -> record index for THIS dictionary
   ACAP-TIDX-PROVE                              \ ... which answers what the scan answers
   bstart bend ACAP-COPY-BLOB
   rend rstart ?do i bstart ACAP-ADD-REC loop
   ACAP-AUDIT-WIDS
   ACAP-SCAN-CALLS
   bstart bend d0 d1 ACAP-SCAN-DSITES
   bstart bend d0 d1 ACAP-SCAN-DEFER-SITES
   bstart bend ACAP-SCAN-CSITES
   bstart bend d0 d1 ACAP-RELEASE-DYNAMIC      \ no dynamic-storage mapping may reach the bytes below
   bstart bend d0 d1 ACAP-BAKE-DATA            \ DATA bytes plus every declared address cell
   ACAP-COMPACT-RECS                            \ build 16B compact records + add record names to pool
   ACAP-PROVE-RECS                              \ fail-closed inverse proof
   ACAP-NIDX-PROVE                              \ ... and the pool index answers every entry
   ACAP-PWIN-CAPTURE                            \ only the window's own seals travel
   AOT-ARM:PAYLOAD-MODE @ 1 = AOT-SECTION:BYTES drop ;

private

\ --- host validation dump (bring-up only) ---
: ACAP-. ( -- )
   s" aot-capture: recs=" type AOT-REC-N @ . s" sites=" type AOT-SITE-N @ .
   s" blob=" type AOT-BLOB-LEN @ . s" names=" type AOT-NAMES-LEN @ . cr
   AOT-SITE-N @ 0 ?do
      i ACAP-SITE-ROW {: r:ptr :}
      r ACAP-W32@ {: boff:n :}                         \ blob-off u32
      r 4 + ACAP-W32@ {: noff:n :}                     \ name-off u32
      s"   site off=" type boff .
      s" name=" type
      AOT-NAMES-BUF@ noff 1+ +  AOT-NAMES-BUF@ noff + c@  type cr
   loop ;

\ --- build-time regression: a wordlist ID above 255 must round-trip through the
\ compact record format. Runs here in the live metabuild (the only context where
\ ACAP-* exist) BEFORE stdin.f's CAPTURE-REPL / ACAP-RESET, so the synthetic record
\ it writes into record #0 is discarded before the real capture. Fail-closed via
\ die: pre-widening ACAP-COMPACT-RECS died on wid>255; the u16->u32 wid field now
\ lets it survive, and ACAP-PROVE-RECS confirms expand(compact)==verbatim including
\ the [40] wid. ACAP-EXPAND-REC is the EXACT model of the boot-time
\ EM-AOT-REGISTER-RECS unpack, so this also guards that inverse.
: ACAP-WID-SELFTEST ( -- )
   ACAP-POOL-RESET                                  \ fresh dedup pool for the synthetic record
   0 ACAP-REC-DST {: d:ptr :}                        \ verbatim 48B dict record #0
   0 d AOT-N-C!                                      \ [0..8)   xt/blob-off = 0
   8 d 8 + AOT-N-C!                                  \ [8..16)  end = 8
   3 d 16 + AOT-N-C!                                 \ [16]     flags(0)<<60 | name-len(3)
   $434241 d 24 + AOT-N-C!                           \ [24..32) inline name "ABC" (LE)
   0 d 32 + AOT-N-C!                                 \ [32..40) inline-name zero pad
   1000 d 40 + AOT-N-C!                              \ [40..48) wid = 1000  ( > 255 )
   1 AOT-REC-N !
   ACAP-COMPACT-RECS                                 \ pack -> 16B compact
   ACAP-PROVE-RECS                                   \ expand==verbatim, field-for-field (incl [40] wid)
   0 ACAP-CREC-DST 16 + ACAP-W32@ 1000 <> if
      s" aot-capture: wid>255 self-test: compact wid corrupted" 74 die then
   8 CODE-SPAN:EXACT d 8 + AOT-N-C!
   ACAP-COMPACT-RECS ACAP-PROVE-RECS
   0 ACAP-CREC-DST 4 + ACAP-W32@ 8 CODE-SPAN:EXACT <> if
      s" aot-capture: full code span corrupted" 74 die then
   0 AOT-REC-N !  ACAP-POOL-RESET ;                 \ leave buffers clean for the real capture

\ --- build-time regression: the pool index answers what the linear pool walk
\ answers. Runs in the live metabuild BEFORE stdin.f's CAPTURE-REPL and leaves the
\ pool empty, so the real capture is unaffected. The cases are built to fool a
\ reader that matches on bytes rather than on ENTRIES: a name that is a strict
\ prefix of another, two of one length differing only in the last byte, and a name
\ whose bytes also occur INSIDE a later entry at a non-entry boundary - which is
\ the offset a substring search would return and an entry walk cannot. Each case
\ is scored twice: the linear scan against the offset the add reported (so a
\ broken reference is not mistaken for a broken index) and the index against the
\ scan. Fail-closed via die. ---
variable ACAP-NIDX-MM                                \ index disagrees with the pool scan
variable ACAP-NIDX-XM                                \ pool scan disagrees with the expected offset
: ACAP-NAME-CASE ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a u ACAP-POOL-SCAN {: s:n :}
   s want <> if 1 ACAP-NIDX-XM +! then
   a u ACAP-POOL-FIND s <> if 1 ACAP-NIDX-MM +! then ;

: ACAP-NIDX-CASES ( -- )                             \ the five entries, then every question
   s" AB" ACAP-POOL-ADD {: o1:n :}
   s" ABC" ACAP-POOL-ADD {: o2:n :}
   s" ABD" ACAP-POOL-ADD {: o3:n :}                  \ same length as ABC, last byte differs
   s" XY" ACAP-POOL-ADD {: o4:n :}
   s" AXYB" ACAP-POOL-ADD {: o5:n :}                 \ contains XY at a non-entry boundary
   o1 o2 = o1 o3 = or o1 o4 = or o1 o5 = or
   o2 o3 = or o2 o4 = or o2 o5 = or
   o3 o4 = or o3 o5 = or o4 o5 = or if
      s" aot-capture: pool self-test: distinct names share an entry" 74 die
   then
   s" ABD" o3 ACAP-NAME-CASE                         \ asked out of add order
   s" AB"  o1 ACAP-NAME-CASE
   s" XY"  o4 ACAP-NAME-CASE                         \ not the copy inside AXYB
   s" AXYB" o5 ACAP-NAME-CASE
   s" ABC" o2 ACAP-NAME-CASE
   s" ABE" -1 ACAP-NAME-CASE                         \ absent: shares a two-byte prefix
   s" A"   -1 ACAP-NAME-CASE                         \ absent: a prefix of three entries
   s" AXY" -1 ACAP-NAME-CASE                         \ absent: a prefix of AXYB
   s" ABCD" -1 ACAP-NAME-CASE ;                      \ absent: an entry plus a byte

\ THE BYTE COMPARISON NEEDS A COLLISION, AND FIVE NAMES IN 65,536 SLOTS DO NOT
\ COLLIDE. Measured: a mutation that accepts the first OCCUPIED slot without
\ comparing the name at all passed every case above and the whole battery, because
\ each of those names had its slot to itself. These three do not: AAAC and AACC
\ are one length and QZS is another, and all three hash to one slot, so a reader
\ that skips the bytes answers the first of them for all three and a reader that
\ compares only the length answers the wrong four-byte one. The slots are asserted
\ EQUAL first, so a future change to ACAP-NIDX-HASH ends the build here with this
\ line rather than quietly retiring the case.
: ACAP-NIDX-COLLIDE ( -- )
   s" AAAC" ACAP-NIDX-HASH {: h1:n :}
   s" AACC" ACAP-NIDX-HASH {: h2:n :}
   s" QZS"  ACAP-NIDX-HASH {: h3:n :}
   h1 h2 <> h1 h3 <> or if
      s" aot-capture: pool self-test: the collision fixture no longer collides" 74 die
   then
   s" AAAC" ACAP-POOL-ADD {: c1:n :}
   s" AACC" ACAP-POOL-ADD {: c2:n :}
   s" QZS"  ACAP-POOL-ADD {: c3:n :}
   c1 c2 = c1 c3 = or c2 c3 = or if
      s" aot-capture: pool self-test: colliding names share an entry" 74 die
   then
   s" AACC" c2 ACAP-NAME-CASE                        \ second on the chain, same length as the first
   s" QZS"  c3 ACAP-NAME-CASE                        \ third on the chain, a different length
   s" AAAC" c1 ACAP-NAME-CASE
   s" AACA" -1 ACAP-NAME-CASE ;                      \ absent, one byte from two of the three

: ACAP-NIDX-SELFTEST ( -- )
   ACAP-POOL-RESET
   0 ACAP-NIDX-MM !  0 ACAP-NIDX-XM !
   ACAP-NIDX-CASES
   ACAP-NIDX-COLLIDE
   AOT-NAMES-LEN @ {: len0:n :}  ACAP-NIDX-N @ {: n0:n :}
   s" ABC" ACAP-POOL-ADD  s" ABC" ACAP-POOL-SCAN <> if
      s" aot-capture: pool self-test: re-adding a name moved its entry" 74 die
   then
   AOT-NAMES-LEN @ len0 <> ACAP-NIDX-N @ n0 <> or if
      s" aot-capture: pool self-test: re-adding a name grew the pool" 74 die
   then
   ACAP-NIDX-XM @ 0= 0= if
      s" aot-capture: pool self-test: linear pool scan wrong, count=" type ACAP-NIDX-XM @ . cr
      s" aot-capture: pool scan disagrees with the recorded entry offsets" 74 die
   then
   ACAP-NIDX-MM @ 0= 0= if
      s" aot-capture: pool self-test: index/scan mismatch, count=" type ACAP-NIDX-MM @ . cr
      s" aot-capture: pool index disagrees with the pool scan" 74 die
   then
   \ THE RESET IS HALF THE CONTRACT. Emptying the pool without emptying the index
   \ leaves slots pointing at bytes the next add is about to overwrite, and the
   \ bytes are still THERE - so the stale entry compares equal and the index hands
   \ back an offset the scan cannot see. Nothing else in the tree notices: a
   \ mutation dropping the index half of ACAP-POOL-RESET passed the whole battery,
   \ because the fixture names above happen to be nobody's real word. These three
   \ questions are what make that mutation red.
   ACAP-POOL-RESET
   s" AXYB" ACAP-POOL-FIND -1 <> if
      s" aot-capture: pool self-test: reset left an entry findable in the index" 74 die
   then
   ACAP-NIDX-N @ 0= 0= if
      s" aot-capture: pool self-test: reset left the index population nonzero" 74 die
   then
   s" AB" ACAP-POOL-ADD 0= 0= if
      s" aot-capture: pool self-test: the pool did not restart at offset 0" 74 die
   then
   ACAP-POOL-RESET ;
ACAP-NIDX-SELFTEST
ACAP-WID-SELFTEST

;package
