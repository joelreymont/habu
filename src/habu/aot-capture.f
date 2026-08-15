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

package AOT-CAPTURE

\ The buffers this file fills are package AOT-BUF's public surface
\ (src/habu/aot-decl.f), read here by their bare names; the import closes with
\ the package.
using AOT-BUF

\ --- raw dict/code boundary casts (host build-time only). AOT-DBASE names only
\ the dictionary record region; live engine registries are under AOT-LIVE-DATA. ---
\ The casts expose record addresses, byte views, and record cells for reverse lookup.
\ Retirement: habu-builder-trust-rows-c5d41af6.
TRUSTED: AOT-DBASE ( -- ptr a ) dbase@ ;
TRUSTED: AOT-DBASE-N ( -- n ) dbase@ ;
TRUSTED: AOT-DATA-N ( -- n ) data-base ;
TRUSTED: AOT-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: AOT-N>U8 ( n -- ptr u8 ) ;
: AOT-LIVE-DATA ( -- ptr a ) data-base ;
: AOT-CELL@ ( ptr a -- n ) @ ;
s" AOT-CELL@" s" ptr a -- n" TRUST
: AOT-N-C! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store a full cell as 8 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c!
   v 32 rshift p 4 + c!  v 40 rshift p 5 + c!  v 48 rshift p 6 + c!  v 56 rshift p 7 + c! ;
: AOT-P32! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 32 bits as 4 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;

\ --- host dictionary record k (48 bytes): field readers (ptr-first byte offsets) ---
: AOT-REC ( n -- ptr a ) 48 * AOT-DBASE swap + ;
: AOT-RXT ( ptr a -- n ) AOT-CELL@ ;                          \ [0] code entry (xt)
: AOT-RLEN ( ptr a -- n ) 8 + AOT-CELL@ ;                     \ [8] code LENGTH (habu2.f EM-AOT-REGISTER-RECS) or package private WID
: AOT-RFLAGS ( ptr a -- n ) 16 + AOT-CELL@ ;                  \ [16] flags | name len
: AOT-RNLEN ( ptr a -- n ) AOT-RFLAGS $0003FFFFFFFFFFFF and ;   \ = DNAME-LEN-MASK (top 14 bits are flags + DNAME-MIN-IN + DKIND)
: AOT-REXT? ( ptr a -- bool ) AOT-RFLAGS $2000000000000000 and 0= 0= ;
: AOT-RNPTR ( ptr a -- ptr u8 )
   dup AOT-REXT? if 24 + AOT-CELL@ AOT-N>U8 else AOT-A>U8 24 + then ;
: AOT-RWID ( ptr a -- n ) 40 + AOT-CELL@ ;                    \ [40] wordlist or -1 package sentinel

\ --- 32-bit little-endian code word; direct `BL imm26` call recognise + decode.
\ Every statically known native call is now one BL (habu2.f LCEMITBL); the callee's
\ absolute host address = the site's original code address + sign-extended(imm26)*4.
\ The site sits in the copied blob buffer, so its original address is AOT-CODE-B0
\ (the capture-time code base) + the byte offset of the site within the blob.
: ACAP-W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
: ACAP-W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;
: ACAP-TGT ( ptr u8 -- n ) {: p:ptr :}            \ absolute callee address of the BL at p
   p ACAP-W32@ $3FFFFFF and  $2000000 xor $2000000 -  2 lshift    \ sign-extended imm26 * 4
   AOT-CODE-B0 @  p AOT-BLOB-BUF@ -  +  + ;                       \ + (B0 + site blob offset)
: ACAP-CALL? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FC000000 and $94000000 = ;
: ACAP-ZERO-IMM ( ptr u8 -- ) {: p:ptr :}         \ zero the imm26 -> bare `bl #0`, for build determinism
   p ACAP-W32@ $FC000000 and  p ACAP-W32! ;

\ Re-encode a full 64-bit value into an existing four-lane MOVZ/MOVK chain (the
\ four imm16 fields), keeping each instruction's opcode AND its destination
\ register. Used to canonicalize the code-address literals so the baked blob is
\ builder-independent. It never reads the register it preserves, which is what
\ lets it rewrite a chain the native compiler emitted into an allocator's
\ register as readily as one the engine emitted into x9.
: ACAP-SET-CHAIN ( ptr u8 n -- ) {: p:ptr val:n :}
   4 0 ?do
      p i 4 * + ACAP-W32@ $FFE0001F and                \ keep opcode + Rd
      val i 16 * rshift $FFFF and 5 lshift or           \ this lane's imm16 into bits 20-5
      p i 4 * + ACAP-W32!
   loop ;

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

\ THE TIE-BREAK NEEDS A DUPLICATE TO BE TESTED, AND THE DICTIONARY DOES NOT
\ SUPPLY ONE. Measured on this build: the metabuild host holds ZERO records
\ sharing an entry at the REPL window every `install --force` captures, and two
\ once the compiler chain is inside the window. So a mutation making the LAST
\ index win instead of the first passed the whole battery - the branch is correct
\ and, today, unreachable. EXPORT is the only producer of two records for one
\ body, so the pair below is one, made deliberately and by that same production
\ keyword. ACAP-TIDX-PROVE refuses an index built over a dictionary with no
\ duplicate at all, which is what stops this pair from being deleted as unused
\ and what will tell the next author why it is here.
: ACAP-ALIAS-SEED ( -- n ) 0 ;
public
export ACAP-ALIAS-SEED                         \ a second record, same entry, second wordlist
private

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
   ndict@ ACAP-TIDX-N @ - 0= if                         \ records inserted minus slots taken
      s" aot-capture: no two records share an entry, so the tie-break above is untested" 74 die
   then
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
   AOT-NAMES-LEN @ {: off:n :}
   u  AOT-NAMES-BUF@ off + c!                                \ [len]
   u 0 ?do a i + c@  AOT-NAMES-BUF@ off 1+ + i + c!  loop    \ [bytes]
   off 1+ u + AOT-NAMES-LEN !
   off ACAP-NIDX+
   off ;

\ --- call-site reloc rows: packed 8 bytes = blob-off u32 + name-off u32 (into pool) ---
\ Both fields are u32 and neither carries a range check of its own, because
\ neither value can reach one: a blob offset is an index into a blob the copy
\ already refused past AOT-BLOB-CAP, and a pool offset is an index into a pool
\ ACAP-POOL-ADD already refused past AOT-NAMES-CAP. Both caps are megabytes below
\ 2^32, so the refusals that exist are the whole bound. The u16 fields these
\ replaced DID need their own checks, because their bound (64 KiB) sat below the
\ buffer caps and nothing else would have caught a crossing.
: ACAP-SITE-ROW ( n -- ptr u8 ) 8 * AOT-SITE-BUF@ + ;
: ACAP-ADD-SITE ( n ptr u8 n -- ) {: boff:n a:ptr u:n :}
   AOT-SITE-N @ AOT-SITE-MAX >= if s" aot-capture: too many call sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-SITE-N @ ACAP-SITE-ROW {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!
   AOT-SITE-N @ 1+ AOT-SITE-N ! ;

\ --- records: copy host record (48 bytes), rebase ordinary [0] xt to blob offset ---
: ACAP-REC-DST ( n -- ptr u8 ) 48 * AOT-REC-BUF@ swap + ;
variable AOT-UNRES-N                          \ kept-source counter: unresolved call sites
: ACAP-ADD-REC ( n n -- ) {: k:n bstart:n :}
   AOT-REC-N @ AOT-REC-MAX >= if s" aot-capture: too many records" 74 die then
   k AOT-REC AOT-A>U8 {: src:ptr :}
   AOT-REC-N @ ACAP-REC-DST {: d:ptr :}
   48 0 ?do src i + c@  d i + c!  loop                        \ verbatim 48-byte copy
   src AOT-RWID -1 <> if
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
: ACAP-COMPACT-RECS ( -- )
   AOT-REC-N @ 0 ?do
      i ACAP-REC-DST {: v:ptr :}                              \ verbatim 48B record
      v AOT-RWID -1 = {: pkg:bool :}
      v 4 + ACAP-W32@ 0= 0= if s" aot-capture: rec blob-off exceeds u32" 74 die then
      v 12 + ACAP-W32@ 0= 0= if s" aot-capture: rec end exceeds u32" 74 die then
      pkg 0= if
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
      v ext ACAP-REC-NAME len ACAP-POOL-ADD {: noff:n :}      \ the name -> deduped pool entry
      i ACAP-CREC-DST {: c:ptr :}                             \ 20B: start u32 + len u32 + name-off u32 + flags u8 + min-in u8 + dkind u8 + wid u32
      start c AOT-P32!  clen c 4 + AOT-P32!  noff c 8 + AOT-P32!
      flags  minin 8 lshift or  dkind 16 lshift or  c 12 + AOT-P32!    \ one store so the spare byte is written zero
      wid c 16 + AOT-P32!
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
: ACAP-PROVE-RECS ( -- )                                      \ fail-closed: expand==verbatim, field-for-field
   0 ACAP-RECMM !
   ACAP-REC48@ {: s:ptr :}
   AOT-REC-N @ 0 ?do
      i ACAP-CREC-DST {: c:ptr :}
      c s ACAP-EXPAND-REC                                     \ rebuild 48B from compact
      i ACAP-REC-DST {: v:ptr :}
      v ACAP-REC-EXT? {: ext:bool :}
      48 0 ?do
         ext  i 24 >= and  i 32 < and  0= if                  \ EXT: [24..32) is the out-of-line pointer
            s i + c@  v i + c@  = 0= if 1 ACAP-RECMM +! then
         then
      loop
      ext if c v ACAP-PROVE-NAME then                         \ ... and the name stands in for it
   loop
   ACAP-RECMM @ 0= 0= if
      s" aot-capture: RECORD EXPANSION MISMATCH count=" type ACAP-RECMM @ . cr
      s" aot-capture: compact record expansion != verbatim 48B" 74 die
   then ;

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
         i AOT-REC AOT-RXT i AOT-REC AOT-RLEN + a > and if i unloop exit then
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

\ --- scan the copied blob for call sites; record + canonicalize each ---
variable ACAP-P
: ACAP-SITE-HERE ( -- )
   AOT-BLOB-BUF@ ACAP-P @ + ACAP-TGT ACAP-TGT>REC {: k:n :}
   k 0 < if 1 AOT-UNRES-N +! exit then                \ call to no dict word -> word kept-source (counted)
   ACAP-P @ k ACAP-SITE-BAND                          \ ... and the target has this name
   ACAP-P @  k AOT-REC AOT-RNPTR  k AOT-REC AOT-RNLEN  ACAP-ADD-SITE
   AOT-BLOB-BUF@ ACAP-P @ +        ACAP-ZERO-IMM ;     \ one 4-byte BL site
: ACAP-SCAN-CALLS ( -- )
   0 ACAP-P !
   begin ACAP-P @ 4 + AOT-BLOB-LEN @ <= while
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-CALL? if ACAP-SITE-HERE then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

\ --- the recorded address chains of the captured span ------------------------
\ WHERE THE SITES COME FROM, AND WHY IT IS NOT A SCAN ANY MORE. Every chain the
\ compiler builds records the region word it starts in, at the emit site, in the
\ address-literal map (src/habu/layout.f SNAP-RELOC:ADDRMAP-OFF; the producers are
\ habu2.f C-DATA-ADDR, C-DATA-ADDR-RAW and C-CODE-ADDR, and the native chain's
\ publication seam). So EXISTENCE is answered by the map and by nothing else.
\
\ AND A COPIED CHAIN IS RECORDED TOO, which is not a detail here but most of what
\ this pass finds. A `create`d data word's whole body is one chain plus the push
\ stencil, short enough that habu2.f C-CALL copies it into every caller instead of
\ calling it, so a captured window holds far more copies than originals: on the
\ metabuild window, 21 chains were created and 142 are present. The copies carry
\ their record because the inliner's copy loop reissues it (SNAP-RELOC:CARRY-SITE),
\ which is what makes a record able to replace the old scan at all - a record that
\ only covered the CREATED chains would have found 21 of 142 and left the rest
\ holding the building host's addresses.
\
\ WHAT THIS REPLACED. Two passes used to walk the blob looking for the shape of a
\ four-word MOVZ/MOVK chain into x9 and then test the value against a span to
\ decide WHETHER the word was a site at all. That was a guess twice over: a
\ compiled word may carry inline data that decodes as a move-wide chain, and an
\ ordinary integer may hold any value at all, so a scalar whose value happened to
\ land in the DATA range was indistinguishable from an address. It also could not
\ see the chains the native compiler emits, which name whichever register the
\ allocator picked rather than x9 - the defect this pass exists to close.
\
\ WHAT THE SPANS STILL DO, AND WHY THAT IS NOT THE SAME KIND OF TEST. A recorded
\ site is already known to hold a real address; the only question left is WHICH
\ address, and that is a total classification over two spans rather than a
\ recognition. [d0,d1) is the window's DATA span and [b0,b1) its code span, and
\ the two are DISJOINT BY CONSTRUCTION, not by luck: d0 and d1 are `here` either
\ side of the compile, so they lie inside the DATA region mapped MAP_FIXED at
\ DATA-VA, while b0 and b1 are `cp@` either side of it and lie inside the JIT code
\ region, which the engine maps REGION-OFF above its own __text. DATA-VA is far
\ above every address that region can hold on either target, so no value can be in
\ both spans and the two counts partition the recorded set.
\
\ AND A SITE IN NEITHER SPAN IS AN ADDRESS THE WINDOW DOES NOT CARRY - the shape a
\ PRE-WINDOW literal has, whose correct value is fixed by the prefix's own layout
\ and differs between the metabuild host and bin/hb. Rebasing it by this window's
\ delta would be wrong and skipping it leaves the host's address baked in. The band
\ is what makes the case VISIBLE at all: the old value-range scan recorded no site
\ for such a chain and said nothing, so the seeded engine read a host address in
\ silence. Which of the two things happens to it now depends on what it names, and
\ ACAP-OUT-CHAIN below is where that is decided.
\
\ PRE-WINDOW DATA IS ELIMINATED, AND THAT IS THE RULING (dot
\ habu-aot-pre-window-0b01043c). Carrying such a site was measured and REFUTED:
\ the metabuild host truncates its boot dictionary back to the first prefix file
\ and recompiles the whole core prefix a second time without rewinding DP, so
\ every pre-window DATA address a window word can hold lives in a band with no
\ counterpart in the target, and the two layouts are not even order-isomorphic -
\ there is no delta and no monotone map, and a verbatim carry is silent
\ corruption. What was eliminated instead is the way such a site got into a
\ window: AOT-ARM:OPEN tells the engine where the window starts, and the
\ compile-mode inliner then declines to COPY a body carrying a chain the window
\ cannot describe and emits its call instead (habu2.f AOT-WINDOW:EMIT-OUTSIDE), which the
\ scan above records as an ordinary call site and the seed relocates by name. So
\ that class is empty by construction and the refusal guards the next producer of
\ one rather than the one that used to arrive here every build.
\
\ PRE-WINDOW CODE IS CARRIED, AND THE DECLINE CANNOT REACH IT. A `['] X` or
\ `postpone X` naming a prefix word is not a copied body - the compile handler
\ emits the chain into the window word's own body - so there is nothing for the
\ inliner to decline. It is instead a call target that is not a BL, and it gets the
\ answer a call target gets: the name travels and the seed resolves it. That is the
\ name-keyed row above (dot habu-widen-the-aot-089f5faf).
: ACAP-CHAIN-BIT? ( n n -- bool ) {: bstart:n boff:n :}
   bstart boff + AOT-DBASE-N - {: off:n :}
   AOT-LIVE-DATA SNAP-RELOC:ADDRMAP-OFF + off 5 rshift + AOT-A>U8 c@
   off 2 rshift 7 and rshift 1 and 0= 0= ;

\ The four immediate fields of a recorded chain, whatever register they name. The
\ register is not read: the producers agree on the carrier's WIDTH, and which
\ register it writes into is the allocator's business and no longer this file's.
: ACAP-CHAINV ( ptr u8 -- n ) {: p:ptr :}
   p ACAP-W32@ 5 rshift $FFFF and
   p 4 + ACAP-W32@ 5 rshift $FFFF and 16 lshift or
   p 8 + ACAP-W32@ 5 rshift $FFFF and 32 lshift or
   p 12 + ACAP-W32@ 5 rshift $FFFF and 48 lshift or ;

: ACAP-ADD-DSITE ( n -- ) {: boff:n :}   \ store blob offset as u32
   AOT-DSITE-N @ AOT-DSITE-MAX >= if s" aot-capture: too many DATA sites" 74 die then
   boff  AOT-DSITE-N @ 4 * AOT-DSITE-BUF@ +  AOT-P32!
   AOT-DSITE-N @ 1+ AOT-DSITE-N ! ;

: ACAP-ADD-CSITE ( n -- ) {: boff:n :}   \ append (as u32) after the DATA offsets in the DSITE buffer
   AOT-DSITE-N @ AOT-CSITE-N @ + AOT-DSITE-MAX >= if s" aot-capture: too many reloc sites" 74 die then
   boff  AOT-DSITE-N @ AOT-CSITE-N @ + 4 * AOT-DSITE-BUF@ +  AOT-P32!
   AOT-CSITE-N @ 1+ AOT-CSITE-N ! ;

\ A NAMED code site: the chain at this blob offset holds the entry of the word
\ called `a u`, and the seed is to resolve that name in the engine it is booting
\ rather than rebase what the chain holds now. The value in the blob is zeroed for
\ the same reason a recorded BL's imm26 is - a captured host address is both
\ builder-dependent and wrong in the seeded engine - and zeroing it means the boot
\ patch is the only thing that can put an address there.
\ ITS PRODUCER IS ACAP-OUT-CHAIN BELOW. A code literal a window word CREATES for a
\ pre-window word (`['] X` or `postpone X` on a prefix word) is what needs this.
\ Eliminating the class the way the DATA literals were eliminated does not reach
\ it: the inliner's decline removes COPIES of such a chain, not the one the compile
\ handler emits into the window word's own body (habu2.f C-BTICK and C-POSTPONE
\ both call C-CODE-ADDR there), so the decline leaves the case standing and only a
\ carry can answer it. An in-window code literal is NOT a candidate: rebasing it by
\ the code delta is correct and costs no lookup.
: ACAP-ADD-XTSITE ( n ptr u8 n -- ) {: boff:n a:ptr u:n :}
   AOT-XTSITE:N @ AOT-XTSITE:MAX >= if s" aot-capture: too many named code sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-XTSITE:N @ 8 * AOT-XTSITE:BUF@ + {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!
   AOT-BLOB-BUF@ boff +  0 ACAP-SET-CHAIN                 \ no host address travels in the blob
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
\ shape a `['] X` or `postpone X` on a PRE-WINDOW word compiles to. Its value cannot
\ be rebased -- the metabuild host recompiles the whole core prefix a second time
\ without rewinding DP, so its prefix band has no counterpart in the target and no
\ delta relates the two -- and it cannot be left, because that bakes the building
\ host's address into bin/hb. What it CAN be is what a call site already is: a NAME.
\ The reverse lookup is the same ACAP-TGT>REC the BL scan uses, the name is read the
\ same way (AOT-RNPTR, so an EXT name travels too), and the seed resolves it with
\ the same LFIND. ACAP-ADD-XTSITE zeroes the four lanes, so no host address is left
\ underneath the answer.
\
\ ANYTHING ELSE STILL ENDS THE BUILD, and the two classes cannot be confused. A
\ pre-window DATA address is the other thing a window word can hold that the spans
\ do not place, and it can never match here: a record's [0] is a code ENTRY, and
\ DATA-VA sits far above every address the code region can hold on either target,
\ so no DATA address equals any record's xt. Pre-window DATA is eliminated at the
\ producer instead (the inliner decline, dot habu-aot-pre-window-0b01043c) and its
\ arrival here is still the named refusal.
: ACAP-OUT-CHAIN ( n n n n -- ) {: boff:n v:n bstart:n bend:n :}
   v bstart >= v bend < and if exit then          \ in-window code: the CODE sweep rebases it
   v ACAP-TGT>REC {: k:n :}
   k 0 < if boff v ACAP-UNCLASSIFIED then         \ no return: the refusal ends the build
   boff  k AOT-REC AOT-RNPTR  k AOT-REC AOT-RNLEN  ACAP-ADD-XTSITE ;

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
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-CHAINV {: v:n :}
         v d0 >= v d1 < and if
            ACAP-P @ ACAP-ADD-DSITE
         else
            ACAP-P @ v bstart bend ACAP-OUT-CHAIN
         then
      then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

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
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-CHAINV {: v:n :}
         v bstart >= v bend < and if
            ACAP-P @ ACAP-ADD-CSITE
            AOT-BLOB-BUF@ ACAP-P @ +  v bstart -  ACAP-SET-CHAIN
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
\ AND WHY ONE KIND OF BYTE MAY NOT. A `defer` compiled inside the window allots a
\ dispatch cell and registers it in the declared-address-cell table
\ (src/habu/layout.f SNAP-RELOC:XTCELL-*), and that cell holds a code address in
\ the BUILDING host. On macOS the JIT region is __text-relative and ASLR-varying,
\ so baking one would make the image depend on the run that produced it and the
\ byte fixpoint would never close - the same defect the code literals avoid by
\ being stored b0-relative. THE INVARIANT: a declared address cell's value is
\ owned by whatever declares it, never by the window's bytes. So those cells are
\ zeroed here and their offsets recorded, and the seed puts the `defer-unset`
\ trap xt of the engine it is booting into each one - the same value a freshly
\ declared cell holds, found through the same keyword lookup the compiler uses.
\ The boot-run list then installs the real vectors, which is what owns them; a
\ cell the boot-run misses dies "defer: unset execution vector" at first use
\ instead of branching to whatever the bytes held.
\ The set is taken from the table and never from what a cell contains: the table
\ is written where a cell's kind is decided, which is the only place it is known.
: ACAP-ADD-XTOFF ( n -- ) {: woff:n :}
   AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-MAX >= if s" aot-capture: too many declared address cells" 74 die then
   woff  AOT-WINDOW:XTOFF-N @ 4 * AOT-WINDOW:XTOFF-BUF@ +  AOT-P32!
   AOT-WINDOW:XTOFF-N @ 1+ AOT-WINDOW:XTOFF-N ! ;

: ACAP-ZERO-CELL ( n -- ) {: woff:n :}
   8 0 ?do 0 AOT-WINDOW:DATA-BUF@ woff + i + c! loop ;

: ACAP-COPY-DATA ( n n -- ) {: d0:n len:n :}
   len AOT-WINDOW:DATA-CAP > if s" aot-capture: DATA window exceeds the AOT data buffer" 74 die then
   len 0 ?do
      d0 i + AOT-N>U8 c@  AOT-WINDOW:DATA-BUF@ i + c!
   loop ;

\ A row that overlaps the window without lying wholly inside it would leave half a
\ host address in the baked bytes, so it ends the build rather than being skipped.
: ACAP-XTCELL-STRADDLES ( n -- ) {: woff:n :}
   s" aot-capture: declared address cell straddles the window edge at offset " type woff . cr
   s" aot-capture: declared address cell straddles the window edge" 74 die ;

: ACAP-MASK-XTCELL ( n n -- ) {: woff:n len:n :}
   woff 8 + len <= if woff ACAP-ADD-XTOFF  woff ACAP-ZERO-CELL exit then
   woff ACAP-XTCELL-STRADDLES ;

: ACAP-BAKE-DATA ( n n -- ) {: d0:n d1:n :}
   d1 d0 - {: len:n :}
   d0 len ACAP-COPY-DATA
   d0 AOT-DATA-N - {: d0off:n :}
   AOT-LIVE-DATA SNAP-RELOC:XTCELL-N-CELL + AOT-CELL@ {: rows:n :}
   rows 0 ?do
      AOT-LIVE-DATA SNAP-RELOC:XTCELL-ROWS-OFF + i cells + AOT-CELL@ d0off - {: woff:n :}
      woff 0 >= woff len < and if woff len ACAP-MASK-XTCELL then
   loop ;

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

: BOOTRUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 255 > if s" aot-capture: boot-run name too long" 74 die then
   AOT-BOOTRUN-LEN @ u + 2 + AOT-BOOTRUN-CAP > if s" aot-capture: boot-run overflow" 74 die then
   AOT-BOOTRUN-LEN @ {: off:n :}
   u  AOT-BOOTRUN-BUF@ off + c!                     \ [len]
   u 0 ?do a i + c@  AOT-BOOTRUN-BUF@ off + 1+ i + c!  loop
   off u + 1+ AOT-BOOTRUN-LEN !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;      \ live terminator (uncounted)

private

\ --- protected-WID registry capture (TFAM 2b-v): serialize the live friend-arena
\ band into AOT-PWID-BUF so EMIT-AOT-SEED bakes it and EMIT-AOT-PROT-RESTORE
\ restores it at boot. The band is a WID-indexed bitmap and the buffer is a
\ bit-for-bit image of it, so the bake is canonical: one protected SET always
\ produces one byte string, whatever order the host happened to protect them in.
\ That is what lets install --force converge byte-identically across the changeover.
\
\ SHAPE DETECTION -- this is the transitional-build hazard layout.f documents. The
\ capture runs on the METABUILD HOST, which during the changeover is the PREVIOUS,
\ table-era engine, read at the offsets the NEW layout names. The tag cell reads
\ PROT-REG-TAG on a bitmap-era host and a row count (0..PROT-WID-LEGACY-MAX) on a
\ table-era one; the two cannot be confused, because a legacy count is bounded far
\ below the tag and a table-era engine has no way to write the tag. Any third value
\ is an unknown lineage and dies rather than being guessed at. The legacy leg
\ retires once the seed has rolled past the transition (dot
\ habu-retire-the-legacy-31ad57bc). ---
: ACAP-PWID-IN-RANGE? ( n -- bool ) {: wid:n :}
   wid 0 < 0=  wid PROT-WID-MAX <  and ;
: ACAP-PWID-BYTE ( n -- ptr u8 ) 3 rshift AOT-PWID-BUF@ + ;
: ACAP-PWID-SET ( n -- ) {: wid:n :}
   wid ACAP-PWID-IN-RANGE? 0= if
      s" aot-capture: protected WID above the bitmap bound" 74 die
   then
   wid ACAP-PWID-BYTE dup c@  1 wid 7 and lshift or  swap c! ;
: ACAP-PWID-BIT? ( n -- bool ) {: wid:n :}
   wid ACAP-PWID-IN-RANGE? 0= if 0 0= 0= exit then
   wid ACAP-PWID-BYTE c@  wid 7 and rshift  1 and 0= 0= ;
: ACAP-PWID-CLEAR ( -- )
   PROT-BITS-BYTES 0 ?do 0 AOT-PWID-BUF@ i + c! loop ;
: ACAP-PWID-TAG@ ( -- n ) AOT-LIVE-DATA PROT-REG-TAG-CELL + atomic@ ;
: ACAP-PWID-SAME-SHAPE ( -- )                                  \ bitmap-era host: copy the band
   PROT-BITS-BYTES 0 ?do
      AOT-LIVE-DATA PROT-BITS-OFF + i + AOT-A>U8 c@  AOT-PWID-BUF@ i + c!
   loop ;
: ACAP-PWID-LEGACY ( n -- ) {: n:n :}                          \ table-era host: rows -> bits
   n 0 < n PROT-WID-LEGACY-MAX > or if
      s" aot-capture: unrecognised protected-WID registry shape" 74 die
   then
   n 0 ?do
      AOT-LIVE-DATA PROT-WID-LEGACY-OFF + i 4 * + AOT-A>U8 ACAP-W32@ ACAP-PWID-SET
   loop ;
\ The live-band reads below are the only place this file still reads engine DATA,
\ so the guard that they are reading DATA and not the dictionary belongs here: if
\ data-base and dbase@ ever named one region the band read would silently return
\ dictionary bytes and bake them as a protected set.
: ACAP-PWID-CAPTURE ( -- )                                     \ live band -> AOT-PWID-BUF
   AOT-LIVE-DATA AOT-DBASE = if
      s" aot-capture: live DATA aliases dictionary base" 74 die
   then
   ACAP-PWID-CLEAR
   ACAP-PWID-TAG@ {: tag:n :}
   tag PROT-REG-TAG = if ACAP-PWID-SAME-SHAPE exit then
   tag ACAP-PWID-LEGACY ;
variable ACAP-PWID-N                                           \ population accumulator
: ACAP-PWID-COUNT ( -- n )                                     \ how many WIDs are protected
   0 ACAP-PWID-N !
   PROT-WID-MAX 0 ?do i ACAP-PWID-BIT? if ACAP-PWID-N @ 1 + ACAP-PWID-N ! then loop
   ACAP-PWID-N @ ;
\ The bitmap makes the row scan's three facts structural: a bit has exactly one
\ index, the index cannot leave the band, and ACAP-PWID-SET already refused any
\ out-of-range WID a legacy host offered. The one fact left to check is that WID 0
\ is not a wordlist, and it is one bit.
: ACAP-PWID-CHECK ( -- )
   0 ACAP-PWID-BIT? if
      s" aot-capture: protected-WID registry marks WID 0" 74 die
   then ;

variable ACAP-PWID-MX                                          \ max-WID accumulator
: ACAP-PWID-MAXWID ( -- n )                                    \ largest protected WID (0 if none)
   0 ACAP-PWID-MX !
   PROT-WID-MAX 0 ?do i ACAP-PWID-BIT? if i ACAP-PWID-MX ! then loop
   ACAP-PWID-MX @ ;

\ Capture the words in dict[rec-start, rec-end) compiled contiguously into the host
\ region [blob-start, blob-end); [d0,d1) is the REPL DATA span (create/variable).
: ACAP-RESET ( -- )
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  ACAP-POOL-RESET
   0 AOT-UNRES-N !  0 AOT-DSITE-N !  0 AOT-DATA-D0 !  0 AOT-DATA-SIZE !
   0 AOT-CSITE-N !  0 AOT-CODE-B0 !  0 AOT-WINDOW:XTOFF-N !  ACAP-PWID-CLEAR
   0 AOT-XTSITE:N !
   0 AOT-BOOTRUN-LEN !  0 AOT-BOOTRUN-BUF@ c! ;
public

\ THIS FILE DOES NOT ARM THE WINDOW; AOT-ARM:OPEN (src/habu/aot-arm.f) does, and
\ it is the only word that writes AOT-WINDOW:D0-CELL/B0-CELL. Every producer calls
\ it directly - src/habu/stdin.f CAPTURE-REPL, test/aot-band-lib.f, and the chain
\ capture tool - because a capture running inside a booted engine has to arm the
\ window before its own tooling exists, and this file cannot be loaded that early
\ without putting its closure (asm.f, icode.f) in front of the compiler chain that
\ shares it. A capture-side alias for the operation would just be a second name
\ that could grow a second body.
\
\ The arming is why ACAP-UNCLASSIFIED below is a BACKSTOP rather than the
\ mechanism: with the window declared, a body holding a pre-window address is
\ called rather than copied (habu2.f AOT-WINDOW:EMIT-OUTSIDE), so the site the
\ refusal names is unreachable by construction and the refusal stands guard over
\ whatever produces one next.
\
\ THERE IS NO CLOSE, and that is the window's own shape, not an omission. The
\ capture takes a SNAPSHOT of [d0, here) at the moment it runs; every definition
\ compiled afterwards extends the same window, which is exactly what the widened
\ re-captures in test/aot-wid-build.f do. The window therefore stays open for the
\ life of the metabuild process, which exits once the image is written.

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
   bstart ACAP-W-B0 !
   rstart ACAP-W-R0 !  rend ACAP-W-R1 !
   d0 ACAP-W-D0 ! ;

: CAPTURE ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart rstart rend d0 ACAP-BAND!
   ACAP-RESET
   ACAP-TIDX-BUILD                              \ xt -> record index for THIS dictionary
   ACAP-TIDX-PROVE                              \ ... which answers what the scan answers
   bstart bend ACAP-COPY-BLOB
   rend rstart ?do i bstart ACAP-ADD-REC loop
   ACAP-SCAN-CALLS
   bstart bend d0 d1 ACAP-SCAN-DSITES
   bstart bend ACAP-SCAN-CSITES
   d0 d1 ACAP-BAKE-DATA                         \ the window's own DATA bytes, declared cells trapped
   ACAP-COMPACT-RECS                            \ build 16B compact records + add record names to pool
   ACAP-PROVE-RECS                              \ fail-closed inverse proof
   ACAP-NIDX-PROVE                              \ ... and the pool index answers every entry
   ACAP-PWID-CAPTURE                            \ serialize the protected-WID bitmap
   ACAP-PWID-CHECK ;                            \ WID 0 is never a wordlist

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

\ --- build-time regression (TFAM 2b-v): the protected-WID bitmap must round-trip a
\ WID above 255 through the AOT serialize/deserialize with no truncation, must not
\ answer for a WID it never set, and must report an exact maximum (the boot restore
\ uses it to advance WIDN past every restored WID). Runs in the live metabuild
\ BEFORE stdin.f's real AOT-CAPTURE:CAPTURE and clears the buffer afterwards, so the
\ capture is unaffected. ACAP-PWID-SET is the exact serialize the capture uses and
\ ACAP-PWID-BIT? reads the same bits EMIT-AOT-PROT-RESTORE copies at boot, so
\ this guards both directions. Fail-closed via die. ---
: ACAP-PWID-SELFTEST ( -- )
   ACAP-PWID-CLEAR
   42 ACAP-PWID-SET
   1000 ACAP-PWID-SET                                \ > 255: the truncation this must not do
   300 ACAP-PWID-SET                                 \ > 255, and in a different byte
   42 ACAP-PWID-BIT? 0= if s" aot-capture: pwid self-test: wid 42 lost" 74 die then
   1000 ACAP-PWID-BIT? 0= if s" aot-capture: pwid self-test: wid 1000 (>255) truncated" 74 die then
   300 ACAP-PWID-BIT? 0= if s" aot-capture: pwid self-test: wid 300 (>255) truncated" 74 die then
   43 ACAP-PWID-BIT? if s" aot-capture: pwid self-test: neighbour bit set" 74 die then
   1000 8 - ACAP-PWID-BIT? if s" aot-capture: pwid self-test: wrong byte set" 74 die then
   PROT-WID-MAX ACAP-PWID-BIT? if s" aot-capture: pwid self-test: bound not refused" 74 die then
   ACAP-PWID-COUNT 3 <> if s" aot-capture: pwid self-test: population wrong" 74 die then
   ACAP-PWID-MAXWID 1000 <> if s" aot-capture: pwid self-test: max-WID for WIDN advance wrong" 74 die then
   ACAP-PWID-CLEAR
   0 ACAP-PWID-COUNT <> if s" aot-capture: pwid self-test: clear left bits set" 74 die then ;
ACAP-PWID-SELFTEST


;package
