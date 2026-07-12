\ aot-capture.f — host-only AOT-REPL capture (metabuild build step).
\
\ Scans the metabuild host's freshly-compiled words for inter-word call sites
\ (movz/movk/movk x16 ; blr x16 absolute chains), reverse-looks-up each callee's
\ dict NAME, and builds the four AOT buffers (habu2.f) that EMIT-AOT-SEED bakes
\ into bin/hb: a code blob, N dict records (xt/end blob-relative, inline name),
\ a call-site relocation table (blob-offset -> callee name-pool ref), and a name
\ pool. At boot EM-SEED-AOT copies the blob, registers the records, and re-encodes
\ each call site's immediates to the callee's address in THAT engine (via LFIND) —
\ so the captured code needs no address to match host vs bin/hb.
\
\ Canonicalization: after recording a call site the three movz/movk immediates in
\ the blob are ZEROED, so the baked blob is deterministic across builds (the host
\ addresses are ASLR-varying and are re-patched at boot anyway).
\
\ Loaded ONLY in the stdin metabuild (after habu2.f, before stdin.f). NOT baked
\ into bin/hb — host build-time meta-words over the host dictionary. Raw dict/code
\ boundary is confined to the TRUSTED:/TRUST casts below (no `0 set-check` span, so
\ the checked build stays fail-closed through the image writer).

\ --- raw dict/code boundary casts (host build-time only) ---
TRUSTED: AOT-DBASE ( -- ptr a ) dbase@ ;
TRUSTED: AOT-A>U8 ( ptr a -- ptr u8 ) ;
TRUSTED: AOT-N>U8 ( n -- ptr u8 ) ;
: AOT-CELL@ ( ptr a -- n ) @ ;
s" AOT-CELL@" s" ptr a -- n" TRUST
: AOT-N-C! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store a full cell as 8 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c!
   v 32 rshift p 4 + c!  v 40 rshift p 5 + c!  v 48 rshift p 6 + c!  v 56 rshift p 7 + c! ;
: AOT-P32! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 32 bits as 4 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;
: AOT-P16! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 16 bits as 2 LE bytes
   v p c!  v 8 rshift p 1+ c! ;

\ --- host dictionary record k (48 bytes): field readers (ptr-first byte offsets) ---
: AOT-REC ( n -- ptr a ) 48 * AOT-DBASE swap + ;
: AOT-RXT ( ptr a -- n ) AOT-CELL@ ;                          \ [0] code entry (xt)
: AOT-RFLAGS ( ptr a -- n ) 16 + AOT-CELL@ ;                  \ [16] flags | name len
: AOT-RNLEN ( ptr a -- n ) AOT-RFLAGS $000FFFFFFFFFFFFF and ;   \ = DNAME-LEN-MASK (top 12 bits are flags + DNAME-MIN-IN)
: AOT-REXT? ( ptr a -- bool ) AOT-RFLAGS $2000000000000000 and 0= 0= ;
: AOT-RNPTR ( ptr a -- ptr u8 )
   dup AOT-REXT? if 24 + AOT-CELL@ AOT-N>U8 else AOT-A>U8 24 + then ;

\ --- 32-bit little-endian code word; movz/movk/movk x16 ; blr x16 recognise+decode
: ACAP-W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
: ACAP-W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;
: ACAP-TGT ( ptr u8 -- n ) {: p:ptr :}
   p ACAP-W32@ 5 rshift $FFFF and
   p 4 + ACAP-W32@ 5 rshift $FFFF and 16 lshift or
   p 8 + ACAP-W32@ 5 rshift $FFFF and 32 lshift or ;
: ACAP-CALL? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FFE0001F and $D2800010 =
   p 4 + ACAP-W32@ $FFE0001F and $F2A00010 = and
   p 8 + ACAP-W32@ $FFE0001F and $F2C00010 = and
   p 12 + ACAP-W32@ $D63F0200 = and ;
: ACAP-ZERO-IMM ( ptr u8 -- ) {: p:ptr :}         \ zero the imm16, for build determinism
   p ACAP-W32@ $FFE0001F and  p ACAP-W32! ;

\ movz/movk x9 4-instruction literal chain (C-LIT) recognise + decode (DATA-addr refs)
: ACAP-LIT9? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FFE0001F and $D2800009 =
   p 4 + ACAP-W32@ $FFE0001F and $F2A00009 = and
   p 8 + ACAP-W32@ $FFE0001F and $F2C00009 = and
   p 12 + ACAP-W32@ $FFE0001F and $F2E00009 = and ;
: ACAP-LIT9V ( ptr u8 -- n ) {: p:ptr :}
   p ACAP-W32@ 5 rshift $FFFF and
   p 4 + ACAP-W32@ 5 rshift $FFFF and 16 lshift or
   p 8 + ACAP-W32@ 5 rshift $FFFF and 32 lshift or
   p 12 + ACAP-W32@ 5 rshift $FFFF and 48 lshift or ;

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

\ --- call-site reloc rows: packed 4 bytes = blob-off u16 + name-off u16 (into pool) ---
: ACAP-SITE-ROW ( n -- ptr u8 ) 4 * AOT-SITE-BUF@ + ;
: ACAP-ADD-SITE ( n ptr u8 n -- ) {: boff:n a:ptr u:n :}
   AOT-SITE-N @ AOT-SITE-MAX >= if s" aot-capture: too many call sites" 74 die then
   boff $FFFF > if s" aot-capture: call blob-off exceeds u16" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   noff $FFFF > if s" aot-capture: name pool offset exceeds u16" 74 die then
   AOT-SITE-N @ ACAP-SITE-ROW {: r:ptr :}
   boff r AOT-P16!  noff r 2 + AOT-P16!
   AOT-SITE-N @ 1+ AOT-SITE-N ! ;

\ --- records: copy host record (48 bytes), rebase [0] xt to blob offset ---
: ACAP-REC-DST ( n -- ptr u8 ) 48 * AOT-REC-BUF@ swap + ;
variable AOT-EXT-N   variable AOT-UNRES-N     \ kept-source counters: EXT names / unresolved call sites
: ACAP-ADD-REC ( n n -- ) {: k:n bstart:n :}
   AOT-REC-N @ AOT-REC-MAX >= if s" aot-capture: too many records" 74 die then
   k AOT-REC AOT-REXT? if 1 AOT-EXT-N +! then         \ EXT (long) name -> kept-source (counted)
   k AOT-REC AOT-A>U8 {: src:ptr :}
   AOT-REC-N @ ACAP-REC-DST {: d:ptr :}
   48 0 ?do src i + c@  d i + c!  loop                        \ verbatim 48-byte copy
   k AOT-REC AOT-RXT bstart -  d AOT-N-C!                     \ [0] = xt - blob-start
   AOT-REC-N @ 1+ AOT-REC-N ! ;

\ --- compact 12B records: blob-off u16 + end u16 + name-off u16 + flags u8 +
\ min-in u8 + wid u32. Built from the verbatim 48B records; each record's inline
\ name is added to the deduped pool. EM-AOT-REGISTER-RECS expands each 12B record
\ back to the full 48B dict record at boot. All the constant/derivable fields
\ (flags nibble, DNAME-MIN-IN byte, wid, name length, and the [24..40)
\ inline-name zero padding) are asserted or reconstructed; the ACAP-PROVE-RECS
\ pass then proves the expansion is byte-identical. The wid is a full u32
\ (matching the verbatim [40] cell's checked u32 domain) so wordlist IDs above
\ 255 round-trip through the seed -- the field was a truncating u8. The min-in
\ byte (record [16] bits 52-59, dot habu-habu-certified-words-84e84eaf) rides
\ the former pad byte so certified arity survives the seed round-trip. ---
: ACAP-CREC-DST ( n -- ptr u8 ) 12 * AOT-REC-MAX 48 * +  AOT-REC-BUF@ swap + ;
: ACAP-REC48@ ( -- ptr u8 ) AOT-REC-MAX 48 * AOT-REC-MAX 12 * +  AOT-REC-BUF@ swap + ;
: ACAP-COMPACT-RECS ( -- )
   AOT-REC-N @ 0 ?do
      i ACAP-REC-DST {: v:ptr :}                              \ verbatim 48B record
      v 4 + ACAP-W32@ 0= 0= if s" aot-capture: rec blob-off exceeds u32" 74 die then
      v 12 + ACAP-W32@ 0= 0= if s" aot-capture: rec end exceeds u32" 74 die then
      v 44 + ACAP-W32@ 0= 0= if s" aot-capture: rec wid exceeds u32" 74 die then
      v 20 + ACAP-W32@ 28 rshift $F and {: flags:n :}         \ flag nibble ([16] bits 60-63)
      v 20 + ACAP-W32@ 20 rshift $FF and {: minin:n :}        \ DNAME-MIN-IN byte ([16] bits 52-59)
      v 20 + ACAP-W32@ $000FFFFF and 0= 0= if s" aot-capture: rec [16] stray high bits" 74 die then
      flags 2 and 0= 0= if s" aot-capture: rec has EXT name (uncompactable)" 74 die then
      v 16 + ACAP-W32@ {: len:n :}                            \ name length ([16] low word)
      len 16 > if s" aot-capture: rec name too long for inline" 74 die then
      v 40 + ACAP-W32@ {: wid:n :}                            \ full u32 (v+44 u32-bound asserted above)
      v ACAP-W32@ {: boff:n :}  v 8 + ACAP-W32@ {: rend:n :}
      boff $FFFF > if s" aot-capture: rec blob-off exceeds u16" 74 die then
      rend $FFFF > if s" aot-capture: rec end (codelen-4) exceeds u16" 74 die then
      v 24 + len ACAP-POOL-ADD {: noff:n :}                   \ inline name -> deduped pool entry
      noff $FFFF > if s" aot-capture: rec name-off exceeds u16" 74 die then
      i ACAP-CREC-DST {: c:ptr :}                             \ 12B: blob-off u16 + end u16 + name-off u16 + flags u8 + min-in u8 + wid u32
      boff c AOT-P16!  rend c 2 + AOT-P16!  noff c 4 + AOT-P16!  flags c 6 + c!  minin c 7 + c!  wid c 8 + AOT-P32!
   loop ;

\ Expand a compact 12B record to a 48B dict record image -- the EXACT algorithm
\ EM-AOT-REGISTER-RECS runs at boot (minus the CP rebase of [0], added here as 0 so
\ the record-body comparison holds field-for-field with the pre-rebase verbatim).
: ACAP-U16@ ( ptr u8 -- n ) {: p:ptr :}  p c@  p 1+ c@ 8 lshift or ;
: ACAP-EXPAND-REC ( ptr u8 ptr u8 -- ) {: c:ptr s:ptr :}      \ c=compact 12B, s=48B out
   c ACAP-U16@ s AOT-N-C!                                     \ [0..8) = blob-off (u16 -> u64, hi=0)
   c 2 + ACAP-U16@ s 8 + AOT-N-C!                             \ [8..16) = end (u16 -> u64, hi=0)
   c 4 + ACAP-U16@ {: noff:n :}                               \ name-off u16
   AOT-NAMES-BUF@ noff + c@ {: len:n :}                       \ len = pool[entry]
   c 6 + c@ {: flags:n :}
   c 7 + c@ {: minin:n :}
   flags 60 lshift  minin 52 lshift or  len or  s 16 + AOT-N-C!   \ [16] = flags<<60 | min-in<<52 | len
   0 s 24 + AOT-N-C!  0 s 32 + AOT-N-C!                       \ zero [24..40)
   len 0 ?do  AOT-NAMES-BUF@ noff 1+ + i + c@  s 24 + i + c!  loop
   c 8 + ACAP-W32@ s 40 + AOT-N-C! ;                          \ [40..48) = wid (u32 -> u64, hi=0)
variable ACAP-RECMM                                           \ record-proof mismatch count
: ACAP-PROVE-RECS ( -- )                                      \ fail-closed: expand==verbatim, field-for-field
   0 ACAP-RECMM !
   ACAP-REC48@ {: s:ptr :}
   AOT-REC-N @ 0 ?do
      i ACAP-CREC-DST s ACAP-EXPAND-REC                       \ rebuild 48B from compact
      i ACAP-REC-DST {: v:ptr :}
      48 0 ?do  s i + c@  v i + c@  = 0= if 1 ACAP-RECMM +! then  loop
   loop
   ACAP-RECMM @ 0= 0= if
      s" aot-capture: RECORD EXPANSION MISMATCH count=" type ACAP-RECMM @ . cr
      s" aot-capture: compact record expansion != verbatim 48B" 74 die
   then ;

\ --- blob copy ---
: ACAP-COPY-BLOB ( n n -- ) {: bstart:n bend:n :}
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
   AOT-BLOB-BUF@ ACAP-P @ +        ACAP-ZERO-IMM
   AOT-BLOB-BUF@ ACAP-P @ + 4 +    ACAP-ZERO-IMM
   AOT-BLOB-BUF@ ACAP-P @ + 8 +    ACAP-ZERO-IMM ;
: ACAP-SCAN-CALLS ( -- )
   0 ACAP-P !
   begin ACAP-P @ 16 + AOT-BLOB-LEN @ <= while
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-CALL? if
         ACAP-SITE-HERE
         ACAP-P @ 16 + ACAP-P !
      else
         ACAP-P @ 4 + ACAP-P !
      then
   repeat ;

\ --- scan the blob for DATA-address literals (movz/movk x9, value in the REPL DATA
\ range [d0,d1)) and record their blob offsets for the boot DATA-reloc pass ---
: ACAP-ADD-DSITE ( n -- ) {: boff:n :}   \ store blob offset as u16 (blob < 64K)
   AOT-DSITE-N @ AOT-DSITE-MAX >= if s" aot-capture: too many DATA sites" 74 die then
   boff $FFFF > if s" aot-capture: DATA site offset exceeds u16" 74 die then
   boff  AOT-DSITE-N @ 2 * AOT-DSITE-BUF@ +  AOT-P16!
   AOT-DSITE-N @ 1+ AOT-DSITE-N ! ;
: ACAP-SCAN-DATA ( n n -- ) {: d0:n d1:n :}
   d0 AOT-DATA-D0 !  d1 d0 - AOT-DATA-SIZE !
   0 ACAP-P !
   begin ACAP-P @ 16 + AOT-BLOB-LEN @ <= while
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-LIT9? if
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-LIT9V {: v:n :}
         v d0 >= v d1 < and if ACAP-P @ ACAP-ADD-DSITE then
         ACAP-P @ 16 + ACAP-P !
      else
         ACAP-P @ 4 + ACAP-P !
      then
   repeat ;

\ --- scan the blob for CODE-address literals (movz/movk x9, value in the captured
\ code range [b0,b1)) -- anonymous quotation entry addresses -- and record their blob
\ offsets for the boot CODE-reloc pass (rebased by the code delta) ---
: ACAP-ADD-CSITE ( n -- ) {: boff:n :}   \ append (as u16) after the DATA offsets in the DSITE buffer
   AOT-DSITE-N @ AOT-CSITE-N @ + AOT-DSITE-MAX >= if s" aot-capture: too many reloc sites" 74 die then
   boff $FFFF > if s" aot-capture: CODE site offset exceeds u16" 74 die then
   boff  AOT-DSITE-N @ AOT-CSITE-N @ + 2 * AOT-DSITE-BUF@ +  AOT-P16!
   AOT-CSITE-N @ 1+ AOT-CSITE-N ! ;
: ACAP-SCAN-CODE ( n n -- ) {: b0:n b1:n :}
   b0 AOT-CODE-B0 !
   0 ACAP-P !
   begin ACAP-P @ 16 + AOT-BLOB-LEN @ <= while
      AOT-BLOB-BUF@ ACAP-P @ + ACAP-LIT9? if
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-LIT9V {: v:n :}
         v b0 >= v b1 < and if ACAP-P @ ACAP-ADD-CSITE then
         ACAP-P @ 16 + ACAP-P !
      else
         ACAP-P @ 4 + ACAP-P !
      then
   repeat ;

\ --- boot-run list: append a top-level entry-word NAME to the 0-terminated
\ [len][name] list EM-AOT-BOOTRUN walks (LFIND + blr) after the seed installs the
\ REPL. Keeps a live trailing 0 terminator (uncounted) so the bake needs no pad. ---
: ACAP-BOOTRUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 255 > if s" aot-capture: boot-run name too long" 74 die then
   AOT-BOOTRUN-LEN @ u + 2 + AOT-BOOTRUN-CAP > if s" aot-capture: boot-run overflow" 74 die then
   AOT-BOOTRUN-LEN @ {: off:n :}
   u  AOT-BOOTRUN-BUF@ off + c!                     \ [len]
   u 0 ?do a i + c@  AOT-BOOTRUN-BUF@ off + 1+ i + c!  loop
   off u + 1+ AOT-BOOTRUN-LEN !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;      \ live terminator (uncounted)

\ --- protected-WID registry capture (TFAM 2b-v): serialize the live friend-arena
\ registry (PROT-WID-N-CELL count + PROT-WID-OFF u32 table) into AOT-PWID-BUF so
\ EMIT-AOT-SEED bakes it and EM-AOT-REGISTER-PROT-WIDS restores it at boot. Each
\ WID is a full u32, so wordlist IDs above 255 round-trip with no u8 truncation.
\ In the current metabuild no producer populates the registry (item 8 will), so the
\ live count is 0 and the capture is a clean no-op; ACAP-PWID-SELFTEST proves the
\ u32 serialize/deserialize round-trip independently, exactly as ACAP-WID-SELFTEST
\ proves the record path. ---
: ACAP-PWID-SLOT ( n -- ptr u8 ) 4 * AOT-PWID-BUF@ + ;         \ slot index -> u32 byte addr
: ACAP-PWID-PUT ( n n -- ) {: v:n ix:n :}                      \ v=wid ix=slot: store as u32
   v  ix ACAP-PWID-SLOT  AOT-P32! ;
: ACAP-PWID-GET ( n -- n ) ACAP-PWID-SLOT ACAP-W32@ ;          \ slot -> wid (u32, boot-read model)
: ACAP-PWID-CAPTURE ( -- )                                     \ live registry -> AOT-PWID-BUF
   AOT-DBASE PROT-WID-N-CELL + AOT-CELL@ {: n:n :}
   n AOT-PWID-MAX > if s" aot-capture: protected-WID registry overflow" 74 die then
   n AOT-PWID-N !
   n 0 ?do
      AOT-DBASE PROT-WID-OFF + i 4 * + AOT-A>U8 ACAP-W32@       \ live table[i] (u32)
      i ACAP-PWID-PUT
   loop ;
variable ACAP-PWID-MX                                          \ max-WID accumulator
: ACAP-PWID-MAXWID ( -- n )                                    \ largest WID in AOT-PWID-BUF (0 if empty)
   0 ACAP-PWID-MX !
   AOT-PWID-N @ 0 ?do
      i ACAP-PWID-GET dup ACAP-PWID-MX @ > if ACAP-PWID-MX ! else drop then
   loop
   ACAP-PWID-MX @ ;

\ Capture the words in dict[rec-start, rec-end) compiled contiguously into the host
\ region [blob-start, blob-end); [d0,d1) is the REPL DATA span (create/variable).
: ACAP-RESET ( -- )
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  0 AOT-NAMES-LEN !
   0 AOT-EXT-N !  0 AOT-UNRES-N !  0 AOT-DSITE-N !  0 AOT-DATA-D0 !  0 AOT-DATA-SIZE !
   0 AOT-CSITE-N !  0 AOT-CODE-B0 !  0 AOT-PWID-N !
   0 AOT-BOOTRUN-LEN !  0 AOT-BOOTRUN-BUF@ c! ;
: ACAP-CAPTURE ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   ACAP-RESET
   bstart bend ACAP-COPY-BLOB
   rend rstart ?do i bstart ACAP-ADD-REC loop
   ACAP-SCAN-CALLS
   d0 d1 ACAP-SCAN-DATA
   bstart bend ACAP-SCAN-CODE
   ACAP-COMPACT-RECS                            \ build 12B compact records + add record names to pool
   ACAP-PROVE-RECS                              \ fail-closed: expand(compact)==verbatim 48B, field-for-field
   ACAP-PWID-CAPTURE ;                          \ serialize the protected-WID registry (TFAM 2b-v)

\ --- host validation dump (bring-up only) ---
: ACAP-. ( -- )
   s" aot-capture: recs=" type AOT-REC-N @ . s" sites=" type AOT-SITE-N @ .
   s" blob=" type AOT-BLOB-LEN @ . s" names=" type AOT-NAMES-LEN @ . cr
   AOT-SITE-N @ 0 ?do
      i ACAP-SITE-ROW {: r:ptr :}
      r c@  r 1+ c@ 8 lshift or {: boff:n :}           \ blob-off u16
      r 2 + c@  r 3 + c@ 8 lshift or {: noff:n :}      \ name-off u16
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
   ACAP-COMPACT-RECS                                 \ pack -> 12B compact (died here pre-fix on wid>255)
   ACAP-PROVE-RECS                                   \ expand==verbatim, field-for-field (incl [40] wid)
   0 ACAP-CREC-DST 8 + ACAP-W32@ 1000 <> if
      s" aot-capture: wid>255 self-test: compact wid corrupted" 74 die then
   0 AOT-REC-N !  0 AOT-NAMES-LEN ! ;               \ leave buffers clean for the real capture
ACAP-WID-SELFTEST

\ --- build-time regression (TFAM 2b-v): a protected-WID registry entry above 255
\ must round-trip through the u32 AOT serialize/deserialize with no truncation, and
\ the max-WID (used at boot to advance WIDN past every restored WID) must be exact.
\ Runs in the live metabuild BEFORE stdin.f's real ACAP-CAPTURE, and clears
\ AOT-PWID-N so the real (empty) capture is unaffected. Fail-closed via die:
\ ACAP-PWID-PUT is the exact serialize the capture uses; ACAP-PWID-GET is the exact
\ u32 read EM-AOT-REGISTER-PROT-WIDS runs at boot, so this guards both directions. ---
: ACAP-PWID-SELFTEST ( -- )
   0 AOT-PWID-N !
   42   0 ACAP-PWID-PUT                              \ entry 0 = 42
   1000 1 ACAP-PWID-PUT                              \ entry 1 = 1000  ( > 255 )
   300  2 ACAP-PWID-PUT                              \ entry 2 = 300   ( > 255 )
   3 AOT-PWID-N !
   0 ACAP-PWID-GET 42   <> if s" aot-capture: pwid self-test: entry 0 corrupted" 74 die then
   1 ACAP-PWID-GET 1000 <> if s" aot-capture: pwid self-test: wid 1000 (>255) truncated" 74 die then
   2 ACAP-PWID-GET 300  <> if s" aot-capture: pwid self-test: wid 300 (>255) truncated" 74 die then
   ACAP-PWID-MAXWID 1000 <> if s" aot-capture: pwid self-test: max-WID for WIDN advance wrong" 74 die then
   0 AOT-PWID-N ! ;                                  \ leave clean for the real capture
ACAP-PWID-SELFTEST
