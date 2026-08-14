\ aot-capture.f — host-only AOT-REPL capture (metabuild build step).
\
\ Scans the metabuild host's freshly-compiled words for inter-word call sites
\ (one direct `BL imm26` each, habu2.f LCEMITBL), reverse-looks-up each callee's
\ dict NAME, and builds the four AOT buffers (habu2.f) that EMIT-AOT-SEED bakes
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
: AOT-CELL! ( n ptr a -- ) ! ;
s" AOT-CELL!" s" n ptr a --" TRUST
: AOT-N-C! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store a full cell as 8 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c!
   v 32 rshift p 4 + c!  v 40 rshift p 5 + c!  v 48 rshift p 6 + c!  v 56 rshift p 7 + c! ;
: AOT-P32! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 32 bits as 4 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;

\ --- host dictionary record k (48 bytes): field readers (ptr-first byte offsets) ---
: AOT-REC ( n -- ptr a ) 48 * AOT-DBASE swap + ;
: AOT-RXT ( ptr a -- n ) AOT-CELL@ ;                          \ [0] code entry (xt)
: AOT-REND ( ptr a -- n ) 8 + AOT-CELL@ ;                     \ [8] code end or package private WID
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
: ACAP-TGT>REC ( n -- n ) {: tgt:n :}
   ndict@ 0 ?do
      i AOT-REC AOT-RXT tgt = if i unloop exit then
   loop
   -1 ;

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
: ACAP-POOL-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}           \ entry off, or -1 if absent
   0 ACAP-PP !
   begin ACAP-PP @ AOT-NAMES-LEN @ < while
      a u ACAP-PP @ ACAP-POOL-EQ? if ACAP-PP @ exit then
      AOT-NAMES-BUF@ ACAP-PP @ + c@ 1+ ACAP-PP @ + ACAP-PP !
   repeat
   -1 ;
: ACAP-POOL-ADD ( ptr u8 n -- n ) {: a:ptr u:n :}            \ deduped entry off (points at len byte)
   u 255 > if s" aot-capture: name too long for pool" 74 die then
   a u ACAP-POOL-FIND dup 0 >= if exit then drop
   AOT-NAMES-LEN @ 1+ u + AOT-NAMES-CAP > if s" aot-capture: name pool overflow" 74 die then
   AOT-NAMES-LEN @ {: off:n :}
   u  AOT-NAMES-BUF@ off + c!                                \ [len]
   u 0 ?do a i + c@  AOT-NAMES-BUF@ off 1+ + i + c!  loop    \ [bytes]
   off 1+ u + AOT-NAMES-LEN !
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

\ --- scan the copied blob for call sites; record + canonicalize each ---
variable ACAP-P
: ACAP-SITE-HERE ( -- )
   AOT-BLOB-BUF@ ACAP-P @ + ACAP-TGT ACAP-TGT>REC {: k:n :}
   k 0 < if 1 AOT-UNRES-N +! exit then                \ call to no dict word -> word kept-source (counted)
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
\ AND A SITE IN NEITHER SPAN IS FATAL. It is an address the window does not carry
\ - the shape a PRE-WINDOW literal has: data allotted while the prefix loaded, so
\ below d0, whose correct value is fixed by the prefix's own DP and differs
\ between the metabuild host and bin/hb. Rebasing it by this window's delta would
\ be wrong and skipping it leaves the host's address baked in. The band is what
\ makes the case VISIBLE at all: the old value-range scan recorded no site for
\ such a chain and said nothing, so the seeded engine read a host address in
\ silence. Turning that silence into a named refusal is the improvement, and the
\ refusal correctly blocks a capture whose window cannot describe its own
\ contents.
\
\ IT IS NOW A BACKSTOP, AND THAT IS THE RULING (dot
\ habu-aot-pre-window-0b01043c). Carrying such a site was measured and REFUTED:
\ the metabuild host truncates its boot dictionary back to the first prefix file
\ and recompiles the whole core prefix a second time without rewinding DP, so
\ every pre-window address a window word can hold lives in a band with no
\ counterpart in the target, and the two layouts are not even order-isomorphic -
\ there is no delta and no monotone map, and a verbatim carry is silent
\ corruption. What was eliminated instead is the way such a site got into a
\ window: WINDOW-OPEN below tells the engine where the window starts, and the
\ compile-mode inliner then declines to COPY a body carrying a chain the window
\ cannot describe and emits its call instead (habu2.f AOT-WINDOW:EMIT-OUTSIDE), which the
\ scan above records as an ordinary call site and the seed relocates by name. So
\ the class is empty by construction and this refusal guards the next producer of
\ one rather than the one that used to arrive here every build.
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
\ ITS PRODUCER IS NOT HERE YET. A code literal a window word CREATES for a
\ pre-window word (`['] X` on a prefix word) is what needs this, and
\ ACAP-SCAN-DSITES still refuses one below. Eliminating the class the way the DATA
\ literals were eliminated does not reach it: the inliner's decline removes COPIES
\ of such a chain, not the one the compile handler emits into the window word's own
\ body. Building the name-keyed row is dot habu-widen-the-aot-089f5faf. The row and
\ its boot arm ship now because the format is baked into the engine and migrating
\ it twice would migrate every baked-code route twice. An in-window code literal is
\ NOT a candidate: rebasing it by the code delta is correct and costs no lookup.
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
   s" aot-capture: recorded address site at blob offset " type boff .
   s" carries " type v .
   s" which is in neither the window's DATA span nor its code span" type cr
   s" aot-capture: recorded address site outside both window spans" 74 die ;

\ The DATA half, and the totality check. Every recorded site is classified here:
\ one in the DATA span is recorded for the boot DATA-reloc pass, one in the code
\ span is left for the second sweep, and one in neither ends the build.
: ACAP-SCAN-DSITES ( n n n n -- ) {: bstart:n bend:n d0:n d1:n :}
   d0 AOT-DATA-D0 !  d1 d0 - AOT-DATA-SIZE !
   0 ACAP-P !
   begin ACAP-P @ SNAP-RELOC:ADDR-CHAIN-BYTES + AOT-BLOB-LEN @ <= while
      bstart ACAP-P @ ACAP-CHAIN-BIT? if
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-CHAINV {: v:n :}
         v d0 >= v d1 < and if
            ACAP-P @ ACAP-ADD-DSITE
         else
            v bstart >= v bend < and 0= if ACAP-P @ v ACAP-UNCLASSIFIED then
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
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  0 AOT-NAMES-LEN !
   0 AOT-UNRES-N !  0 AOT-DSITE-N !  0 AOT-DATA-D0 !  0 AOT-DATA-SIZE !
   0 AOT-CSITE-N !  0 AOT-CODE-B0 !  0 AOT-WINDOW:XTOFF-N !  ACAP-PWID-CLEAR
   0 AOT-XTSITE:N !
   0 AOT-BOOTRUN-LEN !  0 AOT-BOOTRUN-BUF@ c! ;
public

\ Tell the ENGINE where the window that is about to be filled begins, so its
\ compile-mode inliner can stop copying address chains the window will not be able
\ to describe (habu2.f AOT-WINDOW:EMIT-OUTSIDE; the cells are src/habu/layout.f AOT-WINDOW:D0-CELL
\ and AOT-WINDOW:B0-CELL). Called from the same three lines that latch the span, in
\ src/habu/stdin.f CAPTURE-REPL, and the two arguments are those same two cursors.
\
\ It is the one thing this file tells the engine rather than reads from it, and it
\ is what makes ACAP-UNCLASSIFIED below a BACKSTOP instead of the mechanism: a body
\ holding a pre-window address is now called rather than copied, so the site the
\ refusal names is unreachable by construction and the refusal stands guard over
\ whatever produces one next.
\
\ THERE IS NO CLOSE, and that is the window's own shape, not an omission. The
\ capture takes a SNAPSHOT of [d0, here) at the moment it runs; every definition
\ compiled afterwards extends the same window, which is exactly what the widened
\ re-captures in test/aot-wid-build.f do. The window therefore stays open for the
\ life of the metabuild process, which exits once the image is written.
: WINDOW-OPEN ( n n -- ) {: b0:n d0:n :}
   d0 AOT-LIVE-DATA AOT-WINDOW:D0-CELL + AOT-CELL!
   b0 AOT-LIVE-DATA AOT-WINDOW:B0-CELL + AOT-CELL! ;

: CAPTURE ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   ACAP-RESET
   bstart bend ACAP-COPY-BLOB
   rend rstart ?do i bstart ACAP-ADD-REC loop
   ACAP-SCAN-CALLS
   bstart bend d0 d1 ACAP-SCAN-DSITES
   bstart bend ACAP-SCAN-CSITES
   d0 d1 ACAP-BAKE-DATA                         \ the window's own DATA bytes, declared cells trapped
   ACAP-COMPACT-RECS                            \ build 16B compact records + add record names to pool
   ACAP-PROVE-RECS                              \ fail-closed inverse proof
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
   0 AOT-NAMES-LEN !                                \ fresh dedup pool for the synthetic record
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
   0 AOT-REC-N !  0 AOT-NAMES-LEN ! ;               \ leave buffers clean for the real capture
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
