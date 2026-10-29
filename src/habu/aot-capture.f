\ aot-capture.f - host build-time AOT capture: the window's code, records, name
\ pool and call-site rows into the AOT buffers (src/habu/aot-decl.f) that habu2.f
\ EMIT-AOT-SEED bakes into the seed. EM-SEED-AOT re-links each call site by name
\ at boot (LFIND), so captured code holds no host address and each site's imm26
\ is zeroed for determinism. ARM code reach comes from BL scans; CAPTURE-NATIVE
\ takes x86 reach from the native compiler's routine graph. Never baked in.

require src/habu/address-cells.f
require src/habu/aot-arm.f
require src/habu/aot-decl.f
require src/habu/code-span.f
require src/habu/layout.f
require src/habu/sites.f
require src/habu/terminal-call.f
require src/core/does-clause.f
require src/lib/sort.f

package AOT-CAPTURE

\ Package AOT-BUF's public surface (src/habu/aot-decl.f); the import closes with
\ the package.
using AOT-BUF

\ The raw dict/code boundary is confined to these TRUST casts. AOT-DBASE names
\ only the dictionary record region; live engine registries are under
\ AOT-LIVE-DATA.
CAST: AOT-N>U8 ( n -- ptr u8 )
: AOT-DBASE ( -- ptr n ) dbase@ AOT-N>U8 CELL-VIEW ;
: AOT-DBASE-N ( -- n ) dbase@ ;
: AOT-DATA-N ( -- n ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - ;
\ habu1.f WLFIND's hash index. xref-search-wl's row is this package's own
\ (src/habu/prims.f), as OUTER's is for outer.f WL-PROBE.
: AOT-WL-RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;
: AOT-LIVE-DATA ( -- ptr n ) data-base ;
: AOT-CELL@ ( ptr n -- n ) @ ;
: AOT-N-C! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store a full cell as 8 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c!
   v 32 rshift p 4 + c!  v 40 rshift p 5 + c!  v 48 rshift p 6 + c!  v 56 rshift p 7 + c! ;
: AOT-P32! ( n ptr u8 -- ) {: v:n p:ptr :}         \ store low 32 bits as 4 LE bytes
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;

\ Byte offsets in host dictionary record k (48 bytes).
: AOT-REC ( n -- ptr n ) 48 * AOT-DBASE swap + ;
: AOT-RXT ( ptr n -- n ) AOT-CELL@ ;                          \ [0] code entry (xt)
: AOT-RLEN ( ptr n -- n ) 8 + AOT-CELL@ ;                     \ raw [8]: encoded code length or package private WID
: AOT-RBODY ( ptr n -- n ) AOT-RLEN dup CODE-SPAN:CHECK CODE-SPAN:BODY ;
: AOT-RBYTES ( ptr n -- n ) AOT-RLEN CODE-SPAN:BYTES ;
: AOT-RFLAGS ( ptr n -- n ) 16 + AOT-CELL@ ;                  \ [16] flags | name len
: AOT-RNLEN ( ptr n -- n ) AOT-RFLAGS DNAME-LEN-MASK and ;      \ [16] name length
: AOT-REXT? ( ptr n -- bool ) AOT-RFLAGS DNAME-EXT and 0= 0= ;
: AOT-RNPTR ( ptr n -- ptr u8 )
   dup AOT-REXT? if 24 + AOT-CELL@ AOT-N>U8 else BYTE-VIEW 24 + then ;
: AOT-RWID ( ptr n -- n ) 40 + AOT-CELL@ ;                    \ [40] wordlist or -1 package sentinel

\ Direct calls: every statically known native call is one BL (habu2.f
\ LCEMITBL). A site's original address is AOT-CODE-B0 plus its blob offset.
\ ACAP-TGT decodes imm26, which B and BL share.
: ACAP-W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;
: ACAP-TGT ( ptr u8 -- n ) {: p:ptr :}            \ absolute callee address of the BL at p
   p ACAP-W32@ $3FFFFFF and  $2000000 xor $2000000 -  2 lshift    \ sign-extended imm26 * 4
   AOT-CODE-B0 @  p AOT-BLOB-BUF@ -  +  + ;                       \ + (B0 + site blob offset)
: ACAP-CALL? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FC000000 and $94000000 = ;
: ACAP-BRANCH? ( ptr u8 -- bool ) {: p:ptr :}
   p ACAP-W32@ $FC000000 and $14000000 = ;
\ Internal targets need no name: the capture compactor retargets them before the
\ seed copies the final blob.
: ACAP-IN-CODE? ( n -- bool ) {: t:n :}
   t AOT-CODE-B0 @ < if 0 0= 0= exit then
   t AOT-CODE-B0 @ AOT-BLOB-LEN @ + < ;
: ACAP-ZERO-IMM ( ptr u8 -- ) {: p:ptr :}         \ zero the imm26 -> bare `bl #0`, for build determinism
   p ACAP-W32@ $FC000000 and  p AOT-P32! ;

\ Reverse lookup: call target (host xt) -> dict record index.
\ An index, not a scan: the BL scan and ACAP-OUT-CHAIN ask once per site. As a
\ scan, the REPL window cost 217 sites x 8,545 records (7.6 ms) and the compiler
\ chain 13,674 x 14,481 (198M reads, 1.16 s of every `install --force`). The
\ engine's hash index (habu1.f C-HIDX-HASH) is keyed on name and wordlist, not
\ code address.
\ `EXPORT` gives one body a second record (habu2.f C-EXPORT), so two records can
\ carry one xt: records are inserted ascending and an insert meeting its xt does
\ nothing, so the lowest record index wins.
\ A slot holds a record index and the probe re-reads that record's live [0].
\ The build stamps the ndict it indexed and every lookup checks it, so an index
\ built for another dictionary, or never built (the stamp starts at 0), ends the
\ build instead of reporting live call sites unresolved.
DICT-CAP 2 * constant ACAP-TIDX-SLOTS          \ 2x the dictionary bound, so the load stays at
create ACAP-TIDX ACAP-TIDX-SLOTS 4 * allot     \ or under half and a probe always meets an empty
                                              \ slot. u32 slots: 0 empty, else record index + 1
variable ACAP-TIDX-ND                          \ the ndict this index was built for (0 = none)
variable ACAP-TS                               \ probe cursor

: ACAP-TIDX@ ( n -- n ) 4 * ACAP-TIDX + ACAP-W32@ ;
: ACAP-TIDX! ( n n -- ) {: v:n s:n :} v  s 4 * ACAP-TIDX +  AOT-P32! ;
: ACAP-TIDX-STEP ( -- ) ACAP-TS @ 1+ ACAP-TIDX-SLOTS 1 - and ACAP-TS ! ;

\ Xts are instruction-aligned, so the low two bits carry nothing; the product's
\ high half is folded down so bodies a constant distance apart do not share a
\ chain.
: ACAP-TIDX-HASH ( n -- n ) {: xt:n :}
   xt 2 rshift 2654435761 *  $FFFFFFFF and {: g:n :}
   g 16 rshift  g xor  ACAP-TIDX-SLOTS 1 - and ;

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
   ndict@ 0 ?do i ACAP-TIDX-INS loop
   ndict@ ACAP-TIDX-ND ! ;

\ The deduped name pool: entries are [len:u8][name bytes] at an offset that
\ records and call-site rows both reference, so each name is stored once.
variable ACAP-EQ                                             \ pool-compare mismatch accumulator
: ACAP-POOL-EQ? ( ptr u8 n n -- bool ) {: a:ptr u:n e:n :}   \ pool entry e has len u and bytes == a?
   0 ACAP-EQ !
   AOT-NAMES-BUF@ e + c@ u = if
      u 0 ?do
         a i + c@  AOT-NAMES-BUF@ e 1+ + i + c@  = 0= if 1 ACAP-EQ ! then
      loop
   else 1 ACAP-EQ ! then
   ACAP-EQ @ 0= ;
\ The pool is deduped, so a name has at most one entry and no tie-break is
\ needed. As a walk it cost 18,737 asks against a 37,779-byte pool on the
\ compiler chain, 0.29 s in ACAP-COMPACT-RECS alone.
\ The bound is derived: every name reaching ACAP-POOL-ADD is a host record's
\ (ACAP-ADD-SITE the callee's, ACAP-COMPACT-RECS the captured record's,
\ ACAP-ADD-XTSITE the named word's), so entries cannot outnumber the dictionary
\ and 2x DICT-CAP slots keep the load at most half. A producer adding a name
\ from elsewhere must re-derive that; the refusal below is what it meets.
\ ACAP-POOL-RESET clears the pool and its index together, so no index offset
\ points into emptied bytes.
DICT-CAP 2 * constant ACAP-NIDX-SLOTS
create ACAP-NIDX ACAP-NIDX-SLOTS 4 * allot                   \ u32: 0 empty, else entry off + 1
variable ACAP-NIDX-N                                         \ entries in the pool
variable ACAP-NH                                             \ name-hash accumulator
variable ACAP-NS                                             \ probe cursor

: ACAP-NIDX@ ( n -- n ) 4 * ACAP-NIDX + ACAP-W32@ ;
: ACAP-NIDX! ( n n -- ) {: v:n s:n :} v  s 4 * ACAP-NIDX +  AOT-P32! ;
: ACAP-NIDX-STEP ( -- ) ACAP-NS @ 1+ ACAP-NIDX-SLOTS 1 - and ACAP-NS ! ;

\ FNV-1a, the engine dictionary index's key derivation (habu1.f C-HIDX-HASH),
\ with the high half folded down before the mask so short names do not crowd one
\ end of the table.
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

\ Call-site rows: blob-off u32 + name-off u32 (into pool) + scope u32.
\ The u32 fields need no range check: the copy refuses past AOT-BLOB-CAP and
\ ACAP-POOL-ADD past AOT-NAMES-CAP, both megabytes below 2^32. The scope is the
\ wordlist the seed searches the name in; ACAP-SITE-SCOPE is its only producer.
: ACAP-SITE-ROW ( n -- ptr u8 ) SITE-ROW * AOT-SITE-BUF@ + ;

\ Site kinds by original instruction offset, for the reach pass: an external
\ B/BL (canonicalised to zero), an in-window code literal, raw metadata; kind 4
\ is an external call to the fatal engine primitive, with no continuation.
\ Build-host scratch, never in the payload.
DYNAMIC-BUFFER ACAP-GSITE n
: ACAP-GSITE! ( n n -- ) {: kind:n off:n :}
   off 0< if s" aot-capture: negative code-site offset" 74 die then
   off AOT-BLOB-LEN @ >= if s" aot-capture: code-site offset exceeds blob" 74 die then
   kind off 4 / ACAP-GSITE ! ;
: ACAP-GSITE@ ( n -- n ) 4 / ACAP-GSITE @ ;

: ACAP-ADD-SITE ( n ptr u8 n n -- ) {: boff:n a:ptr u:n w:n :}
   AOT-SITE-N @ AOT-SITE-MAX >= if s" aot-capture: too many call sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-SITE-N @ ACAP-SITE-ROW {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!  w r 8 + AOT-P32!
   AOT-SITE-N @ 1+ AOT-SITE-N ! ;

variable ACAP-W-B0                       \ the window's code base, latched at CAPTURE
variable ACAP-W-R0  variable ACAP-W-R1   \ its record span
variable ACAP-W-D0                       \ its first DATA address

\ Records: copy the host record (48 bytes), rebase an ordinary [0] xt to its blob offset.
: ACAP-REC-DST ( n -- ptr u8 ) 48 * AOT-REC-BUF@ swap + ;

\ A record has two indices: its dictionary index (live dictionary, a compiler's
\ map) and its capture index (its row of the verbatim table, taken by
\ ACAP-REC-DST, ACAP-NAMED-BIT, ACAP-GRAPH-LIVE? and every ACAP-REC-ALL walk).
\ ACAP-ADD-REC leaves retired records out, so the two differ past one.
DYNAMIC-BUFFER ACAP-REC-OF n                    \ window offset -> capture index, or -1
: ACAP-DICT>CAP ( n -- n ) {: idx:n :}          \ -1 outside the window or retired
   idx ACAP-W-R0 @ >=  idx ACAP-W-R1 @ <  and 0= if -1 exit then
   idx ACAP-W-R0 @ - ACAP-REC-OF @ ;

\ A record carries no file, so the definition's name is the whole locator.
\ ACAP-NAME. prints the same pair but is defined below. Three lines, one fact
\ each with the prefix (test/aot-capture-bound.f reads them as lines).
: ACAP-REC-REFUSE ( n -- ) {: k:n :}
   s" aot-capture: records captured " type AOT-REC-N @ .
   s" aot-capture: record bound " type AOT-REC-MAX .
   s" aot-capture: adding " type k AOT-REC AOT-RNPTR k AOT-REC AOT-RNLEN type cr
   s" aot-capture: too many records" 74 die ;

\ The capture index, or -1 for a retired record, which is not copied; CAPTURE
\ keeps the answer (ACAP-REC-OF).
: ACAP-ADD-REC ( n n -- n ) {: k:n bstart:n :}
   k AOT-REC AOT-RWID DICT-WL:RETIRED = if -1 exit then
   AOT-REC-N @ AOT-REC-MAX >= if k ACAP-REC-REFUSE then
   k AOT-REC BYTE-VIEW {: src:ptr :}
   AOT-REC-N @ ACAP-REC-DST {: d:ptr :}
   48 0 ?do src i + c@  d i + c!  loop                        \ verbatim 48-byte copy
   k AOT-REC AOT-RWID -1 <> if
      k AOT-REC AOT-RXT bstart -  d AOT-N-C!                  \ ordinary [0] = xt - blob-start
   then                                                        \ package [0]/[8] are raw u32 WID roles
   AOT-REC-N @
   AOT-REC-N @ 1+ AOT-REC-N ! ;

\ Compact AOT-CREC-ROW records: blob-off-or-package-public u32 +
\ code-len-or-package-private u32 + name-off u32 + (flags u8 | min-in u8<<8 |
\ dkind u8<<16) u32 + wid u32; the dkind byte holds the DKIND pair in bits 0-1,
\ DNAME-CELL in bit 2, DNAME-ALLOC in bit 3 and DNAME-REP in bit 4. Names go to
\ the deduped pool. EM-AOT-REGISTER-RECS expands each back to a 48-byte record
\ at boot, rebuilding the derivable fields (flags nibble, min-in byte, DKIND
\ pair, wid, name length, the [24..40) name padding). The wid is a full u32 so
\ wordlist IDs above 255 round-trip; the min-in byte (record [16] bits 52-59)
\ carries certified arity.
: ACAP-CREC-DST ( n -- ptr u8 ) AOT-CREC-ROW * AOT-REC-MAX 48 * +  AOT-REC-BUF@ swap + ;

: ACAP-REC-EXT? ( ptr u8 -- bool ) {: v:ptr :}
   v 20 + ACAP-W32@ 28 rshift 2 and 0= 0= ;

\ An inline name (up to DNAME-INL = 16) sits at [24]; a longer one is written
\ at CP in the code region (habu2.f C-STORE-NAME) with its address at [24]. Both
\ go into the pool, so the seed reads every name the same way.
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

\ The payload carries only the window's own wid numbering: a record ships `wid -
\ W0 + WID-REL-BASE`, wid 0 ships as 0, and the seed adds the booting engine's
\ WIDN back (habu2.f AOT-WINDOW:REBASE-WID,, base cell LWIDW0 baked as
\ WID-REL-BASE). The host's W0 belongs to the building process: one extra host
\ package moved every wid field, 3305 bytes of one image. The fields are an
\ ordinary record's [40] and a package row's [0] and [8]. The protected-wordlist
\ rows (ACAP-PWIN-CAPTURE) are relative and zero-based: that table has no marker
\ value to collide with.
: ACAP-REL-WID ( n -- n ) {: w:n :}
   w 0= if 0 exit then
   w AOT-WID-W0 @ - WID-REL-BASE + ;

\ A package row's first two cells hold its public and private WID where an
\ ordinary record holds a code span.
: ACAP-WID-FIELD ( n bool -- n ) {: w:n pkg:bool :}
   pkg if w ACAP-REL-WID else w then ;

: ACAP-?WID ( ptr u8 n -- ) {: v:ptr w:n :}
   w ACAP-WID-IN? if exit then
   s" aot-capture: window record " type
   v  v ACAP-REC-EXT?  ACAP-REC-NAME  v 16 + ACAP-W32@  type
   s"  names wordlist " type w .INT
   s" , which its window did not create; the window allocated [" type
   AOT-WID-W0 @ .INT
   s" ," type AOT-WID-W0 @ AOT-WID-SPAN @ + .INT
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


: ACAP-COPY-BLOB ( n n -- ) {: bstart:n bend:n :}
   bstart AOT-CODE-B0 !                          \ capture-time code base: ACAP-TGT decodes BL sites against it
   bend bstart - {: len:n :}
   len 0 < if s" aot-capture: negative blob span" 74 die then
   len AOT-BLOB-CAP > if s" aot-capture: blob exceeds buffer" 74 die then
   len 0 ?do bstart AOT-N>U8 i + c@  AOT-BLOB-BUF@ i + c!  loop
   len AOT-BLOB-LEN ! ;

\ A captured call site travels as a name that EM-SEED-AOT LFINDs in the booting
\ engine, so a callee must be a capturing-process word the target's prefix also
\ defines, or a window word (registered before patching). A capture in a booted
\ bin/hb has a third kind, the capture tool's own files, which exist in no
\ target: a call into them would fail at the shipped binary's boot.
\ The producer's marks (record index and DATA cursor when its prelude began)
\ and the window's rstart and d0 bound the prelude; below it is the engine the
\ target shares. The band is two-sided: a window word reaches a prelude word as
\ a call target or as an address the DATA relocation would rebase into nothing.
\ Both audits run over the full recorded populations and name the window word.
\ The metabuild host declares an empty band (src/habu/stdin.f CAPTURE-REPL): it
\ has no prelude. Declaring is mandatory: a capture that never called
\ PRELUDE-MARK refuses instead of guessing.
variable ACAP-PRE-R      \ first record index of the prelude band
variable ACAP-PRE-D      \ first DATA address of the prelude band
variable ACAP-MARKED?    \ the band was declared for this capture

public

\ The record index and DATA cursor before the capture tool loaded anything; a
\ producer with no prelude passes the window's own start.
: PRELUDE-MARK ( n n -- ) {: r:n d:n :}
   r ACAP-PRE-R !  d ACAP-PRE-D !  0 0= ACAP-MARKED? ! ;

private

\ The window record whose code holds this blob offset, or -1. Refusal path only,
\ so a linear walk. A package record ([40] = -1) carries WID roles in [0]/[8],
\ not a code span, and is skipped.
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

\ Audit (a): every call a window word makes. A callee below the prelude mark is
\ in the target's prefix; one inside the window is registered by the seed before
\ the patch pass. Anything else ends the capture.
: ACAP-SITE-BAND ( n n -- ) {: boff:n k:n :}
   k ACAP-PRE-R @ < if exit then
   k ACAP-W-R0 @ >= k ACAP-W-R1 @ < and if exit then
   s" aot-capture: window word " type boff ACAP-REC-AT ACAP-NAME.
   s"  at blob offset " type boff .INT
   s"  calls " type k ACAP-NAME.
   s" , which the booting engine has and no target does" type cr
   s" aot-capture: window call into the prelude band" 74 die ;

\ Audit (b), the DATA half, is a sentence added to ACAP-UNCLASSIFIED's refusal,
\ not a second refusal: it says which kind of address the value is, and a
\ prelude address (allotted after this process started, before the window
\ opened) would be rebased onto whatever the seeded engine put there. A second
\ refusal was refuted: with a forged window base (test/aot-wid-suite.f
\ HABU_AOT_D0_SKEW) every window address falls into the band, so it took the
\ first refusal's only producer and the tree lost a tested stop.
: ACAP-BAND. ( n -- ) {: v:n :}
   v ACAP-PRE-D @ < if s" below this process's own start" type exit then
   v ACAP-W-D0 @ < if s" in the prelude band, which no target carries" type exit then
   s" above the window's DATA span" type ;

\ A bare name is not an identity: on the chain SLOT@ lives in five wordlists and
\ none globally. A site carries one of four scopes: a wid below
\ FIRST-DYNAMIC-WID is a layout.f constant, the same in every engine, as wid 0
\ is; an in-window wid is rebased like a record's; a wid the window did not
\ create is carried by qualifying the name with its package's name, resolved
\ through the compile qualifier path. Anything left is refused by name.
$3A constant ACAP-QUAL-SEP                        \ ':' - the separator LFIND's qualifier scan looks for
256 constant ACAP-QUAL-CAP
create ACAP-QUAL-BUF ACAP-QUAL-CAP allot

\ The package row whose public ([0]) or private ([8]) wordlist is w, or -1.
: ACAP-PKG-ROW ( n bool -- n ) {: w:n pub:bool :}
   ndict@ 0 ?do
      i AOT-REC AOT-RWID DICT-WL:NAMESPACE = if
         pub if i AOT-REC AOT-RXT else i AOT-REC AOT-RLEN then
         w = if i unloop exit then
      then
   loop
   -1 ;

\ From the target index rather than the walk above, because the call scan asks
\ once per site into a package (417 of the compiler chain's 7020 capture samples
\ were that walk). A package row's [0] is its public wid and an ordinary
\ record's [0] a code entry no wid reaches, so the lowest record for w is the
\ walk's row unless it is retired, and a wid is allocated once (habu1.f
\ BWORDLIST counts up), so no live row carries a retired one's wid.
: ACAP-PKG-PUB ( n -- n ) {: w:n :}
   w ACAP-TGT>REC {: p:n :}
   p 0 < if -1 exit then
   p AOT-REC AOT-RWID DICT-WL:NAMESPACE = if p exit then
   -1 ;

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

\ Refused when no record carrying the callee's entry is in a scope the seed can
\ search: the qualifier reaches only a public wordlist, and an `undefine`d
\ record is in none (producer: a window word compiled against a word later
\ retired and replaced). A private callee with no public alias never arrives
\ here (ACAP-?WID refuses first), nor does a branch into a private definer's
\ does> clause made through its public alias: EXPORT publishes the clause with
\ the definer (habu2.f package DOES-REC, "the clause an export carries"), so
\ ACAP-SITE-REC finds the public record, also after `undefine` retired the
\ private pair.
: ACAP-REFUSE-SCOPE ( n n -- ) {: k:n w:n :}
   s" aot-capture: window word " type ACAP-P @ ACAP-REC-AT ACAP-NAME.
   s"  calls " type k ACAP-NAME.
   s"  in wordlist " type w .INT
   s" , which its window did not create and no package publishes" type cr
   s" aot-capture: call site into a wordlist the seed cannot name" 74 die ;

\ The scope column is not window-relative: a scope below FIRST-DYNAMIC-WID
\ passes through as itself (sites carry OWNER-API-PRI-WID, 2), so relative
\ offsets 1 and 2 would read as those wordlists, and an in-window scope ends the
\ build. None arises by structure: a site exists only for a callee outside the
\ blob (ACAP-SITE-HERE, ACAP-BRANCH-HERE) and below the prelude mark, so its
\ wordlist predates W0. Measured: all 12516 sites of a whole-tree image carry
\ scope 0.
: ACAP-REFUSE-WIN-SCOPE ( n n -- ) {: k:n w:n :}
   s" aot-capture: window word " type ACAP-P @ ACAP-REC-AT ACAP-NAME.
   s"  calls " type k ACAP-NAME.
   s"  in wordlist " type w .INT
   s" , which its own window created; a call site carries no window coordinate" type cr
   s" aot-capture: call site scope inside the capture window" 74 die ;

\ A layout constant (wid 0 among them) or a pre-window package's public
\ wordlist.
: ACAP-SCOPE-NAMED? ( n -- bool ) {: k:n :}
   k AOT-REC AOT-RWID {: w:n :}
   w 0 >= w FIRST-DYNAMIC-WID < and if true exit then
   w ACAP-WID-IN? if false exit then
   w ACAP-PKG-PUB 0 >= ;

\ `EXPORT` gives a body a second, public record (habu2.f C-EXPORT), so the first
\ record carrying an entry (ACAP-TGT>REC's answer) can be private. When its
\ scope is unsearchable, the site travels under the first later record of the
\ entry whose scope is, taken from below the prelude mark (a record above it is
\ in no target); with none, ACAP-SITE-SCOPE refuses by name.
: ACAP-SITE-REC ( n -- n ) {: k:n :}
   k ACAP-SCOPE-NAMED? if k exit then
   k ACAP-PRE-R @ >= if k exit then
   k AOT-REC AOT-RXT {: xt:n :}
   ACAP-PRE-R @ k 1+ ?do
      i AOT-REC AOT-RXT xt = if
         i ACAP-SCOPE-NAMED? if i unloop exit then
      then
   loop
   k ;

: ACAP-SITE-SCOPE ( n -- ptr u8 n n ) {: callee:n :}   \ callee record -> name, scope
   callee ACAP-SITE-REC {: k:n :}
   k AOT-REC AOT-RNPTR  k AOT-REC AOT-RNLEN {: a:ptr u:n :}
   k AOT-REC AOT-RWID {: w:n :}
   w 0 >= w FIRST-DYNAMIC-WID < and if a u w exit then
   w ACAP-WID-IN? if k w ACAP-REFUSE-WIN-SCOPE then
   w ACAP-PKG-PUB {: p:n :}
   p 0 < if k w ACAP-REFUSE-SCOPE then
   p AOT-REC AOT-RNPTR  p AOT-REC AOT-RNLEN  a u ACAP-QUAL$  WID-QUAL ;

\ Audit (f): every checked window word's signature travels.
\ A seeded word is in the runtime dictionary but not the checker's record set,
\ so a `:` naming it would die E-UNDEFINED; the artifact carries the window's
\ signatures, collected by the checker while the window is open
\ (src/core/checker.f, armed by src/habu/aot-arm.f OPEN). This checks the
\ collection is complete per record, not by count, because its three producers
\ are an enumeration a fourth would silently escape. Measured on the compiler
\ chain: 6892 records = 94 package records + 6798 checked words, all carried;
\ the collection's first version left 17 behind.
\ Package and retired records are exempt by wordlist (DICT-WL:NAMESPACE,
\ DICT-WL:RETIRED), never by name. A word with no checker effect (a `0
\ set-check` span) is uncallable from checked code here and in the seed, so it
\ is not a finding.

\ The scope half of SYM-FIND's key ( pkg$, public?, found? ) from the record's
\ own wordlist, not the open package; an empty package name is the global
\ scope. A wid no package row claims is refused rather than answered "global".
\ The memo turns 6892 linear walks over the compiler chain into 94, because
\ records arrive grouped by package; it is keyed on the wid.
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

\ A package's private word is unreachable by name in the image: no source can
\ qualify into a private wordlist and call sites carry a record index (habu2.f
\ EMIT-AOT-SITES), so its record (20 bytes here, 48 booted, plus its name) buys
\ nothing. Its code stays only when the reach graph finds a caller or a
\ declared code address.
\ A record ships when the payload names it: the boot-run entry list and the
\ named code sites (resolved by name at boot), and the address cells holding a
\ word's entry. Package rows always ship: they carry the wordlist roles the seed
\ rebases and the sealed-WID gate reads.
variable ACAP-BP

: ACAP-POOL$ ( n -- ptr u8 n ) {: noff:n :}
   AOT-NAMES-BUF@ noff 1+ +  AOT-NAMES-BUF@ noff + c@ ;

DYNAMIC-BUFFER ACAP-ENTRY-OFF n
variable ACAP-ENTRY-N

\ Decoded once per pass and sorted as a separate view: the artifact keeps its
\ location order and encoded values. Keeps offset+1, null zero included,
\ and excludes DATA and named targets.
: ACAP-ENTRY-BUILD ( -- )
   0 ACAP-ENTRY-N !
   AOT-WINDOW:XTOFF-N @ {: count:n :}
   count ACAP-ENTRY-OFF-RESERVE
   count 0= if exit then
   AOT-WINDOW:XTOFF-BUF@ {: rows:ptr :}
   count 0 ?do
      rows i AOT-WINDOW:XTOFF-ROW * + 4 + ACAP-W32@ {: tgt:n :}
      tgt AOT-WINDOW:XTOFF-KIND-MASK and 0= if
         tgt AOT-WINDOW:XTOFF-VALUE-MASK and ACAP-ENTRY-N @ ACAP-ENTRY-OFF !
         1 ACAP-ENTRY-N +!
      then
   loop
   ACAP-ENTRY-N @ 1 > if
      0 ACAP-ENTRY-OFF ACAP-ENTRY-N @ [: < ;] SORT:SORT!
   then ;

: ACAP-XTOFF-ENTRY? ( n -- bool )
   {: off:n :}
   ACAP-ENTRY-N @ 0= if false exit then
   0 ACAP-ENTRY-OFF {: entries:ptr :}
   off 1+ {: target:n :}
   0 ACAP-ENTRY-N @
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      entries mid cells + @ {: value:n :}
      value target = if true exit then
      value target < if mid 1+ hi else lo mid then
   repeat
   2drop
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

\ Dynamic name uses the derived rule cannot see: consumers resolve these names
\ after this engine is captured. The list is explicit policy, as
\ tools/manifest-lint.f states the engine's entry points.
\ NSTR:IMPORT-ROWS and NSTR:ATTACH-SEGMENTS are private build-driver seams
\ resolved through the shipped dictionary; the alternatives are a public
\ wrapper, which test/compiler/native-string.f forbids, or a fixed engine cell.
\ The drift guard is the build's refusal "native-build: literal importer
\ missing".
\ The FILE cache resolves TFAM:FILE-REF-RESET/USE, TFAM:FILE-IDENTITY$/FILE-FIND
\ and CHECKER-REG:FILE-CON-RESET/USE after capture without reloading their
\ sources. The definition cache binds and clears CHECKER-REG's publication
\ observer by its public names. The FILE origin codec and cache resolve their
\ owner callbacks by name when they replay or neutralize a captured window.
\ DEFER-UNSET: habu2.f C-DEFER-FIND-UNSET resolves it by name for every new
\ defer from a lookup string outside the named-site tables; without the record
\ `defer SETUP ( -- )` fails.
\ CHECKER-STORAGE-CELLS: FILE and stripped AOT load aot-data-cell.f after
\ capture, and its scan resolves this checker export by name.
: ACAP-KEEP-PKG? ( n ptr u8 n -- bool ) {: k:n pa:ptr pu:n :}
   k ACAP-REC-DST 40 + ACAP-W32@ ACAP-REC-PKG {: na:ptr nu:n pub:bool found:bool :}
   found 0= if false exit then
   na nu pa pu CORE-STR=CI ;

: ACAP-KEEP? ( n -- bool ) {: k:n :}
   k ACAP-REC-NAME$ {: a:ptr u:n :}
   a u s" DEFER-UNSET" CORE-STR=CI  k ACAP-REC-DST 40 + ACAP-W32@ 0= and
   if true exit then
   a u s" CHECKER-STORAGE-CELLS" CORE-STR=CI
   k ACAP-REC-DST 40 + ACAP-W32@ 0= and if true exit then
   a u s" IMPORT-ROWS" CORE-STR=CI
   a u s" ATTACH-SEGMENTS" CORE-STR=CI or
   k s" NSTR" ACAP-KEEP-PKG? and if true exit then
   a u s" FILE-REF-RESET" CORE-STR=CI
   a u s" FILE-REF-USE" CORE-STR=CI or
   a u s" FILE-IDENTITY$" CORE-STR=CI or
   a u s" FILE-FIND" CORE-STR=CI or
   k s" TFAM" ACAP-KEEP-PKG? and if true exit then
   a u s" FILE-CON-RESET" CORE-STR=CI
   a u s" FILE-CON-USE" CORE-STR=CI or
   a u s" PUBLICATION-USE" CORE-STR=CI or
   a u s" PUBLICATION-CLEAR" CORE-STR=CI or
   k s" CHECKER-REG" ACAP-KEEP-PKG? and if true exit then
   a u s" FILE-MARK" CORE-STR=CI
   a u s" FILE-ROWS" CORE-STR=CI or
   a u s" FILE-IMPORT" CORE-STR=CI or
   a u s" FILE-RESTORE" CORE-STR=CI or
   a u s" FILE-HOOK?" CORE-STR=CI or
   a u s" FILE-PENDING?" CORE-STR=CI or
   a u s" INSTALL" CORE-STR=CI or
   k s" ENGINE-ORIGIN-EARLY" ACAP-KEEP-PKG? and if true exit then
   a u s" FILE-ROWS" CORE-STR=CI
   a u s" FILE-IMPORT" CORE-STR=CI or
   a u s" FILE-STATE" CORE-STR=CI or
   a u s" FILE-BOUNDARY" CORE-STR=CI or
   a u s" FILE-DRAIN" CORE-STR=CI or
   a u s" FILE-RESTORE" CORE-STR=CI or
   a u s" FILE-ACCEPT-USE" CORE-STR=CI or
   a u s" FILE-ACCEPT-CLEAR" CORE-STR=CI or
   a u s" INSTALL" CORE-STR=CI or
   k s" ENGINE-ORIGIN" ACAP-KEEP-PKG? and ;

variable ACAP-ALL-NAMES

: ACAP-INTERNAL? ( ptr u8 -- bool )
   20 + ACAP-W32@ $80000000 and 0<> ;

: ACAP-NAMED? ( n -- bool ) {: k:n :}
   ACAP-ALL-NAMES @ if true exit then
   k ACAP-REC-DST {: v:ptr :}
   v CELL-VIEW AOT-RWID -1 = if true exit then
   v 40 + ACAP-W32@ ACAP-PRIVATE? 0= v ACAP-INTERNAL? 0= and if true exit then
   v ACAP-W32@ ACAP-XTOFF-ENTRY? if true exit then
   k ACAP-KEEP? if true exit then
   k ACAP-REC-NAME$ {: a:ptr u:n :}
   a u ACAP-XTSITE-NAMES? if true exit then
   a u ACAP-BOOTRUN-NAMES? ;

\ A retained definer can create words after boot; their patched final B
\ targets its ;does clause, whose record follows the parent inside its code
\ span. The stripped linker needs that exact entry record:
\ ACAP-GRAPH-NAME-DOES retains it after reachability, including a private
\ definer reached through a public wrapper or EXPORT alias.
: ACAP-DOES-COMPANION? ( n -- bool ) {: k:n :}
   k 0= if false exit then
   k 1- {: parent-k:n :}
   parent-k ACAP-REC-DST {: parent:ptr :}
   k ACAP-REC-DST {: clause:ptr :}
   parent CELL-VIEW AOT-RWID -1 = if false exit then
   parent 40 + ACAP-W32@ clause 40 + ACAP-W32@ <> if false exit then
   parent ACAP-W32@ {: first:n :}
   clause ACAP-W32@ {: entry:n :}
   entry first <= if false exit then
   entry clause 8 + ACAP-W32@ CODE-SPAN:BYTES +
      first parent 8 + ACAP-W32@ CODE-SPAN:BYTES + > if false exit then
   parent-k ACAP-REC-NAME$ {: name:ptr len:n :}
   k ACAP-REC-NAME$ {: derived:ptr derived-len:n :}
   DOES-CLAUSE:SUFFIX$ {: suffix:ptr sufu:n :}
   derived-len len sufu + <> if false exit then
   derived len name len CORE-STR=CI 0= if false exit then
   derived len + sufu suffix sufu CORE-STR=CI ;

\ An unnamed reachable body keeps an 8-byte AOT-SPAN row (blob offset, raw code
\ span) for the application linker; an unreachable body carries neither. The
\ sidecar marks a removed body's start -1, never an old coordinate that could
\ name other code after compaction.
\ Reserved at first use: AOT-REC-MAX cells is 262,144 bytes that a static allot
\ would spend in the build host's DATA prefix below the window; the bound is
\ still AOT-REC-MAX.
DYNAMIC-BUFFER ACAP-NAMED-STORAGE n
: ACAP-NAMED-BIT ( n -- ptr n )                 \ record k's cell: 1 retains its name
   AOT-REC-MAX ACAP-NAMED-STORAGE-RESERVE
   ACAP-NAMED-STORAGE ;
variable ACAP-REC-ALL

\ Numbered once, after the name and reachability passes. ACAP-DICT>CAP excludes
\ retired records; this table excludes stripped private records.
variable SH-SHIP-N
DYNAMIC-BUFFER SH-ROWS n
: SH-NUMBER ( -- )
   ACAP-REC-ALL @ {: n:n :}
   n SH-ROWS-RESERVE
   0 SH-SHIP-N !
   n 0 ?do
      i ACAP-NAMED-BIT @ 0<> if
         SH-SHIP-N @ i SH-ROWS !  1 SH-SHIP-N +!
      else
         -1 i SH-ROWS !
      then
   loop ;

: SH-SHIPPED ( n -- n )
   ACAP-DICT>CAP {: k:n :}
   k 0 < if -1 exit then
   k SH-ROWS @ ;

\ A retained row does not decide which anonymous spans travel: a PC-relative
\ reference, a code literal in DATA or fall-through can keep an unnamed body
\ alive. Host-only scratch, separate from AOT-SPAN and the payload so a proof
\ failure cannot silently change the artifact.
DYNAMIC-BUFFER ACAP-GMARK n
DYNAMIC-BUFFER ACAP-GOWNER n
DYNAMIC-BUFFER ACAP-GMAP n
DYNAMIC-BUFFER ACAP-GWORK n
variable ACAP-GWORK-N
variable ACAP-GOLDLEN
variable ACAP-GNEWLEN
variable ACAP-GACC
variable ACAP-GRAPH-READY
variable ACAP-NATIVE-MODE

: ACAP-GRAPH-START ( n -- n ) ACAP-REC-DST ACAP-W32@ ;
: ACAP-GRAPH-END ( n -- n ) {: k:n :}
   k ACAP-GRAPH-START
   k ACAP-REC-DST 8 + ACAP-W32@ {: raw:n :}
   raw CODE-SPAN:BYTES + ;

: ACAP-GRAPH-MARK-REC ( n -- ) {: k:n :}
   k ACAP-GMARK @ 0<> if exit then
   1 k ACAP-GMARK !
   k ACAP-GRAPH-END 4 / k ACAP-GRAPH-START 4 / ?do
      1 i ACAP-GMAP !
   loop
   k ACAP-GWORK-N @ ACAP-GWORK !
   ACAP-GWORK-N @ 1+ ACAP-GWORK-N ! ;

: ACAP-GRAPH-CODE? ( n -- bool )
   ACAP-REC-DST CELL-VIEW AOT-RWID -1 = 0= ;

: ACAP-GRAPH-OWNER! ( n n -- ) {: k:n slot:n :}
   slot ACAP-GOWNER @ {: old:n :}
   old 0= if k 1+ slot ACAP-GOWNER ! exit then
   \ Aliases need one body. Choose the covering record reaching furthest, so
   \ a code-cell target at a shared entry preserves every alias's full extent.
   k ACAP-GRAPH-END old 1- ACAP-GRAPH-END > if
      k 1+ slot ACAP-GOWNER !
   then ;

: ACAP-GRAPH-RAW-SPAN ( n n -- ) {: off:n size:n :}
   off 0< size 0< or off size + AOT-BLOB-LEN @ > or if
      s" aot-capture: raw metadata outside copied code" 74 die
   then
   off size + 3 + 4 / off 4 / ?do
      3 i ACAP-GSITE !
   loop ;

: ACAP-GRAPH-RAW-NAME ( ptr n -- ) {: rec:ptr :}
   rec AOT-REXT? 0= if exit then
   rec AOT-RNPTR AOT-CODE-B0 @ AOT-N>U8 - {: off:n :}
   off 0< off AOT-BLOB-LEN @ >= or if exit then
   off rec AOT-RNLEN ACAP-GRAPH-RAW-SPAN ;

: ACAP-GRAPH-RAW-RECORD ( ptr n -- ) {: rec:ptr :}
   rec ACAP-GRAPH-RAW-NAME
   rec AOT-RWID DICT-WL:NAMESPACE = if exit then
   rec AOT-RXT rec AOT-RBODY + AOT-CODE-B0 @ - {: off:n :}
   off 0< off 16 + AOT-BLOB-LEN @ > or if exit then
   AOT-BLOB-BUF@ off + CELL-VIEW AOT-CELL@ DEFER-MAGIC = if
      off 16 ACAP-GRAPH-RAW-SPAN
   then ;

: ACAP-GRAPH-RAW-CELLS ( -- )
   AOT-DSITE-N @ 0 ?do
      i 4 * AOT-DSITE-BUF@ + ACAP-W32@ {: site:n :}
      site AOT-DSITE-CELL and 0<> if
         site AOT-DSITE-OFF-MASK and {: off:n :}
         off 8 ACAP-GRAPH-RAW-SPAN
         off 8 >= if
            AOT-BLOB-BUF@ off 8 - + CELL-VIEW AOT-CELL@ DEFER-MAGIC = if
               off 8 - 8 ACAP-GRAPH-RAW-SPAN
            then
         then
      then
   loop ;

: ACAP-GRAPH-INDEX-RECS ( -- )
   ACAP-REC-ALL @ ACAP-GMARK-RESERVE
   ACAP-REC-ALL @ ACAP-GWORK-RESERVE
   AOT-BLOB-LEN @ 4 / ACAP-GOWNER-RESERVE
   AOT-BLOB-LEN @ 4 / ACAP-GMAP-RESERVE
   ACAP-REC-ALL @ 0 ?do 0 i ACAP-GMARK ! loop
   AOT-BLOB-LEN @ 4 / 0 ?do
      0 i ACAP-GOWNER ! 0 i ACAP-GMAP !
   loop
   ACAP-REC-ALL @ 0 ?do
      i {: k:n :}
      k ACAP-GRAPH-CODE? if
         k ACAP-GRAPH-START {: start:n :}
         k ACAP-GRAPH-END {: end:n :}
         start 0< start 3 and 0<> or end AOT-BLOB-LEN @ > or if
            s" aot-capture: dictionary span outside copied code" 74 die
         then
         end 4 / start 4 / ?do k i ACAP-GRAPH-OWNER! loop
      then
   loop ;

: ACAP-GRAPH-MARK-OFF ( n -- ) {: off:n :}
   off 0< off AOT-BLOB-LEN @ >= or if exit then
   off 4 / ACAP-GOWNER @ {: owner:n :}
   owner 0<> if owner 1- ACAP-GRAPH-MARK-REC then ;

$FFFFFC1F constant ACAP-GRET-MASK
$D65F0000 constant ACAP-GRET
$D61F0000 constant ACAP-GBR-REG
$D69F0000 constant ACAP-GERET
$D6BF0000 constant ACAP-GDRPS
$FFE0001F constant ACAP-GBRK-MASK
$D4200000 constant ACAP-GBRK
$FC000000 constant ACAP-GBR-TERM-MASK
$14000000 constant ACAP-GBR-TERM

: ACAP-GRAPH-TERMINAL? ( n -- bool ) {: w:n :}
   w ACAP-GRET-MASK and ACAP-GRET =
   w ACAP-GRET-MASK and ACAP-GBR-REG = or
   w ACAP-GRET-MASK and ACAP-GERET = or
   w ACAP-GRET-MASK and ACAP-GDRPS = or
   w ACAP-GBRK-MASK and ACAP-GBRK = or
   w ACAP-GBR-TERM-MASK and ACAP-GBR-TERM = or ;

: ACAP-GRAPH-W32@ ( n -- n ) AOT-BLOB-BUF@ swap + ACAP-W32@ ;
: ACAP-GRAPH-BRANCH? ( n -- bool ) {: w:n :}
   w $FC000000 and $14000000 =
   w $FC000000 and $94000000 = or ;
: ACAP-GRAPH-TARGET ( n n -- n ) {: off:n w:n :}
   w $3FFFFFF and $2000000 xor $2000000 - 2 lshift off + ;

\ The native emitter uses direct branches and ADR, never page-relative or
\ literal-pool addressing: refuse the latter rather than copy a stale delta.
\ The kind is the signed displacement width; ADR alone has a byte delta.
: ACAP-GRAPH-PC-KIND ( n -- n ) {: w:n :}
   w ACAP-GRAPH-BRANCH? if 26 exit then
   w $FF000010 and $54000000 =
   w $7E000000 and $34000000 = or if 19 exit then
   w $7E000000 and $36000000 = if 14 exit then
   w $9F000000 and $10000000 = if 21 exit then
   w $9F000000 and $90000000 =
   w $3B000000 and $18000000 = or if
      s" aot-capture: page-relative or literal-pool instruction unsupported" 74 die
   then
   0 ;

: ACAP-GRAPH-PC-TARGET ( n n n -- n ) {: off:n w:n kind:n :}
   kind 26 = if off w ACAP-GRAPH-TARGET exit then
   kind 21 = if
      w 5 rshift $7FFFF and 2 lshift w 29 rshift 3 and or
      $100000 xor $100000 - off + exit
   then
   1 kind 1- lshift {: sign:n :}
   w 5 rshift sign 2 * 1- and sign xor sign - 4 * off + ;

: ACAP-GRAPH-PC-WORD ( n n n -- n ) {: w:n kind:n delta:n :}
   kind 26 = if w $FC000000 and delta 4 / $3FFFFFF and or exit then
   kind 19 = if w $FF00001F and delta 4 / $7FFFF and 5 lshift or exit then
   kind 14 = if w $FFF8001F and delta 4 / $3FFF and 5 lshift or exit then
   w $9F00001F and delta 3 and 29 lshift or
   delta 2 rshift $7FFFF and 5 lshift or ;

\ C-DIE-BAD-TAG skips padded message bytes, then writes them with MOVZ x0,2;
\ ADR x1,first-byte; MOVZ x2,length. The byte length must account for the
\ entire skipped interval before those bytes are classified as data.
: ACAP-GRAPH-DIAG? ( n n -- bool ) {: at:n end:n :}
   end 12 + AOT-BLOB-LEN @ > if false exit then
   end ACAP-GRAPH-W32@ $D2800040 <> if false exit then
   end 4 + ACAP-GRAPH-W32@ {: adr:n :}
   adr $9F00001F and $10000001 <> if false exit then
   end 4 + adr 21 ACAP-GRAPH-PC-TARGET at 4 + <> if false exit then
   end 8 + ACAP-GRAPH-W32@ {: len:n :}
   len $FFE0001F and $D2800002 <> if false exit then
   len 5 rshift $FFFF and 3 + -4 and end at 4 + - = ;

\ The bootstrap compiler's C-SDQ/C-CQ and escaped forms emit exactly
\ B after-bytes; padded bytes; ADR x9,first-byte; STR x9,[x19],#8. The target
\ and the address must agree before the skipped interval is treated as data.
\ Tier 1 interns its literals in DATA and has no such in-code interval.
: ACAP-GRAPH-RAW-STRING ( n -- ) {: at:n :}
   at ACAP-GSITE@ 3 = if exit then
   at ACAP-GRAPH-W32@ {: w:n :}
   w $FC000000 and $14000000 <> if exit then
   at w ACAP-GRAPH-TARGET {: end:n :}
   end at 4 + <= end 8 + AOT-BLOB-LEN @ > or if exit then
   at end ACAP-GRAPH-DIAG? if
      at 4 + end at 4 + - ACAP-GRAPH-RAW-SPAN exit
   then
   end ACAP-GRAPH-W32@ {: adr:n :}
   adr $9F00001F and $10000009 <> if exit then
   end 4 + ACAP-GRAPH-W32@ $F8008669 <> if exit then
   end adr 21 ACAP-GRAPH-PC-TARGET at 4 + <> if exit then
   at 4 + end at 4 + - ACAP-GRAPH-RAW-SPAN ;

\ Capture and elaboration agree on the engine calls with no continuation;
\ resolved once per site-table rebuild.
: ACAP-TERMINAL-PRIMITIVE? ( ptr u8 n -- bool )
   2dup 0 search-wl TERMINAL-CALL:BOUND? ;

: ACAP-TERMINAL-SITE? ( ptr u8 bool bool -- bool )
   {: row:ptr fatal:bool unwinds:bool :}
   row 8 + ACAP-W32@ 0<> if false exit then
   row 4 + ACAP-W32@ AOT-NAMES-BUF@ + {: name:ptr :}
   name 1+ name c@ s" die" CORE-STR=CI if fatal exit then
   name 1+ name c@ s" throw" CORE-STR=CI unwinds and ;

: ACAP-GRAPH-SITES ( -- )
   s" die" ACAP-TERMINAL-PRIMITIVE? {: fatal:bool :}
   s" throw" ACAP-TERMINAL-PRIMITIVE? {: unwinds:bool :}
   AOT-BLOB-LEN @ 4 / ACAP-GSITE-RESERVE
   AOT-BLOB-LEN @ 4 / 0 ?do 0 i ACAP-GSITE ! loop
   AOT-SITE-N @ 0 ?do
      i ACAP-SITE-ROW {: row:ptr :}
      row fatal unwinds ACAP-TERMINAL-SITE? if 4 else 1 then
      row ACAP-W32@ ACAP-GSITE!
   loop
   AOT-CSITE-N @ 0 ?do
      2 AOT-DSITE-N @ i + 4 * AOT-DSITE-BUF@ + ACAP-W32@ ACAP-GSITE!
   loop
   ACAP-GRAPH-RAW-CELLS
   \ Include retired records: undefine removes a name, not its old CP bytes.
   ACAP-W-R1 @ ACAP-W-R0 @ ?do i AOT-REC ACAP-GRAPH-RAW-RECORD loop
   AOT-BLOB-LEN @ 4 / 0 ?do i 4 * ACAP-GRAPH-RAW-STRING loop ;

: ACAP-GRAPH-INDEX ( -- )
   ACAP-GRAPH-SITES ACAP-GRAPH-INDEX-RECS ;

: ACAP-GRAPH-LIVE? ( n -- bool ) {: k:n :}
   ACAP-GRAPH-READY @ 0= if true exit then
   k ACAP-GMARK @ 0<> ;

\ A code literal's target beneath a PC-relative one, -1 for each it lacks. Raw
\ metadata sends neither, and a named call site is relocated by name.
: ACAP-GRAPH-EDGES ( n -- n n ) {: at:n :}
   at ACAP-GSITE@ {: site:n :}
   site 3 = if -1 -1 exit then
   at ACAP-GRAPH-W32@ {: w:n :}
   w ACAP-GRAPH-PC-KIND {: kind:n :}
   site 2 = if AOT-BLOB-BUF@ at + ADDRESS-CARRIER:CHAINV else -1 then
   kind 0<> site 1 <> and site 4 <> and if
      at w kind ACAP-GRAPH-PC-TARGET
   else -1 then ;

: ACAP-GRAPH-SCAN-AT ( n -- )
   ACAP-GRAPH-EDGES ACAP-GRAPH-MARK-OFF ACAP-GRAPH-MARK-OFF ;

: ACAP-GRAPH-ENDS? ( n -- bool ) {: at:n :}
   at ACAP-GSITE@ 4 = if true exit then
   at ACAP-GRAPH-W32@ ACAP-GRAPH-TERMINAL? ;

: ACAP-GRAPH-SWEEP-ONE ( n -- ) {: k:n :}
   k ACAP-GRAPH-START {: from:n :}
   k ACAP-GRAPH-END {: to:n :}
   from begin dup to < while
      dup ACAP-GRAPH-SCAN-AT 4 +
   repeat drop
   to 4 - ACAP-GRAPH-ENDS? 0= if
      to ACAP-GRAPH-MARK-OFF
   then ;

: ACAP-GRAPH-SWEEP ( -- )
   begin ACAP-GWORK-N @ 0 > while
      ACAP-GWORK-N @ 1- ACAP-GWORK-N !
      ACAP-GWORK-N @ ACAP-GWORK @ ACAP-GRAPH-SWEEP-ONE
   repeat ;

\ Bytes between dictionary bodies stay in the payload and can hold a branch
\ from a body whose row was not recorded (padding, a declaration trailer, an
\ assembler seam), so their edges and continuations seed the same graph.
: ACAP-GRAPH-SWEEP-GAP ( n -- ) {: at:n :}
   at ACAP-GSITE@ 3 = if exit then
   at 4 / ACAP-GOWNER @ 0= if
      at ACAP-GRAPH-SCAN-AT
      at 4 + AOT-BLOB-LEN @ < if
         at 4 / 1+ ACAP-GOWNER @ 0<> if
            at ACAP-GRAPH-ENDS? 0= if
               at 4 + ACAP-GRAPH-MARK-OFF
            then
         then
      then
   then ;

: ACAP-GRAPH-SWEEP-GAPS ( -- )
   AOT-BLOB-LEN @ 4 / 0 ?do i 4 * ACAP-GRAPH-SWEEP-GAP loop ;

: ACAP-GRAPH-MAP@ ( n -- n ) {: off:n :}
   off 0< off ACAP-GOLDLEN @ >= or if -1 exit then
   off 4 / ACAP-GMAP @ dup 0 >= if off 3 and + then ;

: ACAP-GRAPH-BUILD-MAP ( -- )
   AOT-BLOB-LEN @ ACAP-GOLDLEN !
   0 ACAP-GNEWLEN !
   ACAP-GOLDLEN @ 4 / 0 ?do
      i 4 * {: off:n :}
      i ACAP-GOWNER @ 0= i ACAP-GMAP @ 0<> or if
         ACAP-GNEWLEN @ i ACAP-GMAP !
         ACAP-GNEWLEN @ 4 + ACAP-GNEWLEN !
      else
         -1 i ACAP-GMAP !
      then
   loop ;

: ACAP-GRAPH-COPY-BLOB ( -- )
   ACAP-GOLDLEN @ 4 / 0 ?do
      i 4 * {: old:n :}
      old ACAP-GRAPH-MAP@ dup 0 >= if {: new:n :}
         4 0 ?do
            AOT-BLOB-BUF@ old + i + c@
            AOT-BLOB-BUF@ new + i + c!
         loop
      else drop then
   loop
   ACAP-GNEWLEN @ AOT-BLOB-LEN ! ;

: ACAP-GRAPH-PATCH-BRANCH ( n -- ) {: old:n :}
   old ACAP-GSITE@ 0<> if exit then
   old ACAP-GRAPH-MAP@ dup 0 < if drop exit then {: new:n :}
   AOT-BLOB-BUF@ new + ACAP-W32@ {: w:n :}
   w ACAP-GRAPH-PC-KIND {: kind:n :}
   kind 0= if exit then
   old w kind ACAP-GRAPH-PC-TARGET ACAP-GRAPH-MAP@ {: target:n :}
   target 0 < if
      s" aot-capture: PC-relative target removed or outside window" 74 die
   then
   \ Removing intervals preserves order and can only shorten a displacement;
   \ the instruction's original range therefore still fits, including ADR.
   w kind target new - ACAP-GRAPH-PC-WORD AOT-BLOB-BUF@ new + AOT-P32! ;

: ACAP-GRAPH-PATCH-CODE-SITE ( n -- ) {: old:n :}
   old ACAP-GSITE@ 2 <> if exit then
   old ACAP-GRAPH-MAP@ dup 0 < if drop exit then {: new:n :}
   AOT-BLOB-BUF@ new + ADDRESS-CARRIER:CHAINV ACAP-GRAPH-MAP@ {: target:n :}
   target 0 < if
      s" aot-capture: live code literal targets a removed span" 74 die
   then
   AOT-BLOB-BUF@ new + target ADDRESS-CARRIER:SET-CHAIN ;

: ACAP-GRAPH-PATCH ( -- )
   ACAP-GOLDLEN @ 4 / 0 ?do
      i 4 * dup ACAP-GRAPH-PATCH-BRANCH
      dup ACAP-GRAPH-PATCH-CODE-SITE drop
   loop ;

: ACAP-GRAPH-REMAP-RECS ( -- )
   ACAP-REC-ALL @ 0 ?do
      i ACAP-REC-DST {: v:ptr :}
      v CELL-VIEW AOT-RWID -1 <> if
         v ACAP-W32@ ACAP-GRAPH-MAP@ dup 0 >= if v AOT-P32! else drop then
      then
   loop ;

: ACAP-GRAPH-REMAP-SITES ( -- )
   0 ACAP-GACC !
   AOT-SITE-N @ 0 ?do
      i ACAP-SITE-ROW {: src:ptr :}
      src ACAP-W32@ ACAP-GRAPH-MAP@ {: new:n :}
      new 0 >= if
         ACAP-GACC @ ACAP-SITE-ROW {: dst:ptr :}
         new dst AOT-P32!
         src 4 + ACAP-W32@ dst 4 + AOT-P32!
         src 8 + ACAP-W32@ dst 8 + AOT-P32!
         ACAP-GACC @ 1+ ACAP-GACC !
      then
   loop
   ACAP-GACC @ AOT-SITE-N ! ;

: ACAP-GRAPH-REMAP-DSITES ( -- )
   AOT-DSITE-N @ {: olddn:n :}
   AOT-CSITE-N @ {: oldcn:n :}
   0 ACAP-GACC !
   olddn 0 ?do
      i 4 * AOT-DSITE-BUF@ + ACAP-W32@ {: row:n :}
      row AOT-DSITE-OFF-MASK and ACAP-GRAPH-MAP@ {: new:n :}
      new 0 >= if
         row AOT-DSITE-CELL and new or ACAP-GACC @ 4 * AOT-DSITE-BUF@ + AOT-P32!
         ACAP-GACC @ 1+ ACAP-GACC !
      then
   loop
   ACAP-GACC @ {: newdn:n :}
   0 ACAP-GACC !
   oldcn 0 ?do
      olddn i + 4 * AOT-DSITE-BUF@ + ACAP-W32@ {: row:n :}
      row ACAP-GRAPH-MAP@ {: new:n :}
      new 0 >= if
         new ACAP-GACC @ newdn + 4 * AOT-DSITE-BUF@ + AOT-P32!
         ACAP-GACC @ 1+ ACAP-GACC !
      then
   loop
   newdn AOT-DSITE-N !
   ACAP-GACC @ AOT-CSITE-N ! ;

: ACAP-GRAPH-REMAP-XTSITES ( -- )
   0 ACAP-GACC !
   AOT-XTSITE:N @ 0 ?do
      AOT-XTSITE:BUF@ i 8 * + {: src:ptr :}
      src ACAP-W32@ ACAP-GRAPH-MAP@ {: new:n :}
      new 0 >= if
         ACAP-GACC @ 8 * AOT-XTSITE:BUF@ + {: dst:ptr :}
         new dst AOT-P32!
         src 4 + ACAP-W32@ dst 4 + AOT-P32!
         ACAP-GACC @ 1+ ACAP-GACC !
      then
   loop
   ACAP-GACC @ AOT-XTSITE:N ! ;

: ACAP-GRAPH-REMAP ( -- )
   ACAP-GRAPH-REMAP-RECS
   ACAP-GRAPH-REMAP-SITES
   ACAP-GRAPH-REMAP-DSITES
   ACAP-GRAPH-REMAP-XTSITES ;

: ACAP-SPAN-ROW ( n -- ptr u8 ) AOT-SPAN:ROW * AOT-SPAN:BUF@ + ;

: ACAP-ADD-SPAN ( n n -- ) {: start:n raw:n :}
   AOT-SPAN:N @ AOT-SPAN:MAX >= if s" aot-capture: too many code spans" 74 die then
   AOT-SPAN:N @ ACAP-SPAN-ROW {: r:ptr :}
   start r AOT-P32!  raw r 4 + AOT-P32!
   AOT-SPAN:N @ 1+ AOT-SPAN:N ! ;

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
   v 20 + ACAP-W32@ 18 rshift 3 and
   v 20 + ACAP-W32@ 16 rshift 1 and 2 lshift or
   v 20 + ACAP-W32@ 15 rshift 1 and 3 lshift or
   v 20 + ACAP-W32@ 14 rshift 1 and 4 lshift or {: dkind:n :} \ DKIND, CELL, ALLOC, REP
   \ [16] bits 32-45 hold the length's high bits, and bit 49 is DNAME-OWNED.
   \ The compact record carries neither; bits 47-48 are allocation geometry and
   \ bit 46 is DNAME-REP.
   v 20 + ACAP-W32@ $00023FFF and 0= 0= if s" aot-capture: rec [16] stray high bits" 74 die then
   v ACAP-REC-EXT? {: ext:bool :}                          \ name out of line (DNAME-EXT)
   v 16 + ACAP-W32@ {: len:n :}                            \ name length ([16] low word)
   ext 0= len 16 > and if s" aot-capture: rec name too long for inline" 74 die then
   pkg if $FFFFFFFF else v 40 + ACAP-W32@ ACAP-REL-WID then {: wid:n :} \ package marker or window-relative u32 WID
   v ACAP-W32@ pkg ACAP-WID-FIELD {: start:n :}            \ a package row's [0]/[8] are WIDs, not a code span
   v 8 + ACAP-W32@ pkg ACAP-WID-FIELD {: clen:n :}
   ACAP-GRAPH-READY @ if k ACAP-NAMED-BIT @ 0<> else k ACAP-NAMED? then
   {: named:bool :}
   named 0= k ACAP-GRAPH-LIVE? 0= and if
      0 k ACAP-NAMED-BIT !
      exit
   then
   named if 1 else 0 then  k ACAP-NAMED-BIT !
   named 0= if
      pkg if s" aot-capture: a package row carries no code span" 74 die then
      start clen ACAP-ADD-SPAN exit                        \ 8B: start u32 + raw code span u32
   then
   v ext ACAP-REC-NAME len ACAP-POOL-ADD {: noff:n :}
   AOT-REC-N @ ACAP-CREC-DST {: c:ptr :}                   \ 20B: start u32 + len u32 + name-off u32 + flags u8 + min-in u8 + dkind u8 + wid u32
   start c AOT-P32!  clen c 4 + AOT-P32!  noff c 8 + AOT-P32!
   flags  minin 8 lshift or  dkind 16 lshift or  c 12 + AOT-P32!   \ one store so the spare byte is written zero
   wid c 16 + AOT-P32!
   AOT-REC-N @ 1+ AOT-REC-N ! ;

\ The verbatim count moves aside first: AOT-REC-N becomes the shipped-row
\ cursor and ACAP-REC-ALL is what every walk over the capture's records reads.
: ACAP-COMPACT-RECS ( -- )
   ACAP-GRAPH-READY @ ACAP-ALL-NAMES @ or 0= if ACAP-ENTRY-BUILD then
   AOT-REC-N @ ACAP-REC-ALL !
   0 AOT-REC-N !  0 AOT-SPAN:N !
   ACAP-REC-ALL @ 0 ?do
      i ACAP-COMPACT-ONE
   loop
   ACAP-ENTRY-OFF-RELEASE 0 ACAP-ENTRY-N ! ;

variable ACAP-SIG-KNOWN                            \ window records the checker knows an effect for
variable ACAP-SIG-EXEMPT                           \ package, retired, and unrecorded records

: ACAP-REFUSE-SIG-WID ( n n -- ) {: k:n w:n :}
   s" aot-capture: window record " type k ACAP-NAME.
   s"  is in wordlist " type w .INT
   s" , which no package record claims" type cr
   s" aot-capture: a window record's wordlist names no package" 74 die ;

: ACAP-REFUSE-SIG ( n -- ) {: k:n :}
   s" aot-capture: the checker knows an effect for window word " type k ACAP-NAME.
   s"  and the captured signature pool carries none" type cr
   s" aot-capture: a checked window word's signature was not captured" 74 die ;

\ The same walk emits the rows.
\ The copy is the audit's walk, not a pass over the store, so the artifact
\ cannot name a word the target lacks: copying the store added 723 rows (the
\ capture tool's post-window words), and a signature for an absent word
\ certifies a definition that then calls nothing.
\ The row (four u32: name, signature and package offsets into the pool's string
\ arena, then visibility) is copied with the arena verbatim, so nothing is
\ re-interned. The checker owns the format and is its only reader.
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

\ Written by the registry that owns those records; this file carries the bytes
\ and never reads them.
: ACAP-SIG-REGISTRY ( -- )
   AOT-REG-BUF@ AOT-REG-CAP AOT-ARM:PAYLOAD-REG-SAVE AOT-REG-LEN ! ;

\ Runs last of the audits: the call and address scans ask whether what travels
\ is sound, this whether it is complete, and running it earlier masked them
\ (measured: an address refusal came back as an uncarried signature).
\ The arming has no separate test: a window compiled disarmed has an empty pool
\ and the walk names its first checked word; an armed-now guard would be wrong,
\ since capture tools close the signature window (AOT-ARM:SIG-CLOSE) first.
\ Rows are chosen per record of [R0,R1), but the registry's delta is two
\ high-waters subtracted, so it means what the registry holds when read here,
\ unless the window was closed early: tools/aot-chain-capture.f loads its
\ assembler and artifact writer after its window and closes at the window
\ (AOT-ARM:SIG-CLOSE); read here, the delta counted their families too and the
\ seeded engine refused its own registry. A closed window keeps its end.
: ACAP-AUDIT-SIGS ( -- )
   0 ACAP-SIG-KNOWN !  0 ACAP-SIG-EXEMPT !  0 AOT-SIG-N !
   0 ACAP-PKG-MEMO-W !  0 ACAP-PKG-MEMO-ROW !  0 ACAP-PKG-MEMO-PUB !
   ACAP-W-R1 @ ACAP-W-R0 @ ?do i ACAP-?SIG loop
   ACAP-SIG-STRINGS
   ACAP-SIG-REGISTRY ;

\ Audit (d): the row resolves the way the seed will ask.
\ EM-AOT-PATCH-SITES's boot question, asked here of the row just written
\ (pooled name, stored scope, the engine's own find), so a wrong scope (dot
\ 9d7d8e72), a wrong pool offset or a shadowed name fails at the build, not at
\ a boot that says only that a name was not found. `search-wl` refuses
\ OWNER-API-PRI-WID, which the seed does search, so a site there must name a
\ pre-window record, an engine helper whose wid is a layout constant.
: ACAP-XREF-XT ( ptr n -- n )
   dup XREF-FOUND? if XREF-START exit then drop 0 ;

\ The engine's one-wordlist hash search (habu1.f WLFIND), as
\ EM-AOT-PATCH-NAMED-SITES calls for a scoped site. xref.f XREF-FIND-WL walks
\ every record and, once per site, was 99% of CAPTURE's samples on a
\ 1024-filler window and 92% on the compiler chain.
: ACAP-WL-XT ( ptr u8 n n -- n ) AOT-WL-RECORD ACAP-XREF-XT ;

\ Split where xref.f XREF-FIND splits: at the one colon (a second resolves
\ nothing, an edge colon leaves the name bare); the qualifier probes the
\ namespace wordlist, the tail the package row's public wordlist.
: ACAP-QUAL-XT ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q -2 = if 0 exit then
   q 0 < if a u 0 ACAP-WL-XT exit then
   a q DICT-WL:NAMESPACE AOT-WL-RECORD {: ns:ptr :}
   ns XREF-FOUND? 0= if 0 exit then
   a q 1+ +  u q - 1-  ns XREF-PKG-PUBLIC  ACAP-WL-XT ;

: ACAP-SITE-XT ( ptr u8 n n -- n ) {: a:ptr u:n w:n :}
   w WID-QUAL = if a u ACAP-QUAL-XT exit then
   a u w ACAP-WL-XT ;

\ The probe misses a live callee only for a does>-clause record, which J-DOES
\ names after its definer (habu2.f DOES-REC) without the duplicate wall: a
\ public definer whose tail ends in a colon gives a two-colon spelling the
\ qualifier path refuses, and a clause's name can already be held by a word
\ defined as X;does before X. A definer `undefine` replaced is not one: its
\ clause retires with it (xref.f XREF-RETIRE-INDEX).
: ACAP-REFUSE-SITE ( n n ptr u8 n n -- ) {: s:n k:n a:ptr u:n w:n :}
   s" aot-capture: call site " type s .INT
   s"  bakes the name " type a u type
   s"  in scope " type w .INT
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

\ Both BL and an out-of-window B travel by name. LDOESPATCH plants `b D` at a
\ created word's RET, D the defining word's does> clause; outside the window its
\ displacement measures against code the target lacks (three chain words
\ branched into PATHZ that way). Such a
\ B must name a record entry (habu2.f J-DOES gives the clause one) and is
\ refused by name otherwise, as an unresolved BL is. The compactor retargets
\ in-window branches.
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
   s"  at blob offset " type ACAP-P @ .INT
   s"  branches out of the window to " type t .INT
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
      ACAP-P @ ACAP-GSITE@ 3 <> if
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-CALL? if ACAP-SITE-HERE then
         AOT-BLOB-BUF@ ACAP-P @ + ACAP-BRANCH? if ACAP-BRANCH-HERE then
      then
      ACAP-P @ 4 + ACAP-P !
   repeat ;

\ Address sites come from the emitter's relocation map, never from instruction
\ bytes or value guesses; SITES:EACH-IN-SPAN is the one reader and yields chain
\ starts in ascending order. A direct call creates no address literal in the
\ caller. In-window addresses move with their span; pre-window code travels by
\ the callee's name; pre-window DATA has no target mapping and is refused (an
\ explicit handler such as `is` on a pre-window defer still makes one).

\ The per-site word runs inside SITES:EACH-IN-SPAN's quotation, which cannot
\ read the sweep's locals, so the span travels in these cells.
variable ACAP-SWEEP-B0
variable ACAP-SWEEP-B1

\ Sites as a region offset from the dictionary base and a kind.
: ACAP-SWEEP ( n n [ n n -- ] -- ) {: bstart:n bend:n q :}
   bstart ACAP-SWEEP-B0 !  bend ACAP-SWEEP-B1 !
   bstart AOT-DBASE-N -  AOT-BLOB-LEN @  q SITES:EACH-IN-SPAN ;

: ACAP-SITE-BOFF ( n -- n )                  \ region offset -> blob offset
   AOT-DBASE-N +  ACAP-SWEEP-B0 @ - ;

: ACAP-CHAIN@ ( n -- n n ) {: boff:n :}
   AOT-BLOB-BUF@ boff + {: p:ptr :}
   p AOT-BLOB-BUF@ AOT-BLOB-LEN @ + ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
   size 0= if s" aot-capture: malformed recorded address chain" 74 die then
   size  p size ADDRESS-CARRIER:CHAIN-VALUE ;

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

\ A named code site: the seed resolves the word called `a u` in the booting
\ engine instead of rebasing the chain, which is zeroed (a host address is
\ builder-dependent and wrong in the seed) so the boot patch alone fills it. The
\ producer is ACAP-OUT-CHAIN: a code literal a window word creates for a
\ pre-window word (`['] X` on a prefix word; C-BTICK calls C-CODE-ADDR). An
\ in-window code literal is rebased by the code delta instead.
: ACAP-ADD-XTSITE ( n ptr u8 n -- ) {: boff:n a:ptr u:n :}
   AOT-XTSITE:N @ AOT-XTSITE:MAX >= if s" aot-capture: too many named code sites" 74 die then
   a u ACAP-POOL-ADD {: noff:n :}
   AOT-XTSITE:N @ 8 * AOT-XTSITE:BUF@ + {: r:ptr :}
   boff r AOT-P32!  noff r 4 + AOT-P32!
   AOT-BLOB-BUF@ boff +  0 ADDRESS-CARRIER:SET-CHAIN            \ no host address travels in the blob
   AOT-XTSITE:N @ 1+ AOT-XTSITE:N ! ;

\ A capture that cannot classify one of its own recorded chains has nothing
\ correct to bake.
: ACAP-UNCLASSIFIED ( n n -- ) {: boff:n v:n :}
   s" aot-capture: window word " type boff ACAP-REC-AT ACAP-NAME.
   s"  at blob offset " type boff .INT
   s"  carries " type v .INT
   s"  which is " type v ACAP-BAND.
   s"  and so in neither the window's DATA span nor its code span" type cr
   s" aot-capture: recorded address site outside both window spans" 74 die ;

\ A recorded chain the window's DATA span does not hold.
\ In-window code is left to ACAP-SCAN-CSITES,
\ which stores (value - b0). A word's entry becomes a name-keyed row: the
\ metabuild host recompiles the core prefix without rewinding DP, so no delta
\ relates its prefix band to the target, and leaving it would bake a host
\ address. The name must resolve to this exact entry in the eligible prefix,
\ with the stored CODE cells' global/public alias check. Anything else ends the
\ build: a pre-window DATA address never matches, because DATA-VA lies far above
\ every code address on either target.
: ACAP-OUT-CHAIN ( n n n n -- ) {: boff:n v:n bstart:n bend:n :}
   v bstart >= v bend < and if exit then          \ in-window code: the CODE sweep rebases it
   v ACAP-TARGET-NAME? if
      {: name:ptr size:n :}
      boff name size ACAP-ADD-XTSITE exit
   then 2drop
   boff v ACAP-UNCLASSIFIED ;

\ Every recorded site is classified: one in the DATA span is recorded for the
\ boot DATA-reloc pass, every other one goes to ACAP-OUT-CHAIN. A named row's
\ chain holds 0, which fails ACAP-SCAN-CSITES's in-span test, so it is not also
\ rebased; SITES yields only chain starts.
: ACAP-DSITE-YIELD ( n n -- ) {: off:n kind:n :}
   kind SNAP-RELOC:SITE-ADDR <> if exit then
   off ACAP-SITE-BOFF {: boff:n :}
   boff ACAP-CHAIN@ nip {: v:n :}
   AOT-DATA-D0 @ {: d0:n :}
   v d0 >= v d0 AOT-DATA-SIZE @ + < and if boff ACAP-ADD-DSITE exit then
   boff v ACAP-SWEEP-B0 @ ACAP-SWEEP-B1 @ ACAP-OUT-CHAIN ;

: ACAP-SCAN-DSITES ( n n n n -- ) {: bstart:n bend:n d0:n d1:n :}
   d0 AOT-DATA-D0 !  d1 d0 - AOT-DATA-SIZE !
   bstart bend [: ACAP-DSITE-YIELD ;] ACAP-SWEEP ;

\ A deferred word's trailer holds the same DATA address as its dispatch code;
\ its magic identifies the field, and aliases relocate it once.
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

\ The CODE half: quotation entry addresses, stored b0-relative with captureB0 =
\ 0. The boot pass rebases by (seedCP - captureB0), so only (value - b0) must be
\ preserved; the region base is __text-relative (ASLR on macOS, fixed VMBASE on
\ Linux), so a raw absolute would keep the seed from reaching a fixpoint. A
\ second sweep, because the DATA offsets precede every code offset in one buffer
\ (ACAP-ADD-CSITE appends past AOT-DSITE-N).
: ACAP-CSITE-YIELD ( n n -- ) {: off:n kind:n :}
   kind SNAP-RELOC:SITE-ADDR <> if exit then
   off ACAP-SITE-BOFF {: boff:n :}
   boff ACAP-CHAIN@ {: size:n v:n :}
   ACAP-SWEEP-B0 @ {: bstart:n :}
   v bstart < v ACAP-SWEEP-B1 @ >= or if exit then
   \ Live shared DATA is in the fixed DATA mapping, disjoint from this JIT CODE
   \ window. A short carrier here is corrupt, not a CODE literal: all later
   \ CODE-site passes require four words.
   size ADDRESS-CARRIER:DATA-CHAIN-BYTES = if
      s" aot-capture: DATA carrier lies in the CODE band" 74 die
   then
   boff ACAP-ADD-CSITE
   AOT-BLOB-BUF@ boff +  v bstart -  ADDRESS-CARRIER:SET-CHAIN ;

: ACAP-SCAN-CSITES ( n n -- ) {: bstart:n bend:n :}
   0 AOT-CODE-B0 !                                      \ canonical code base 0
   bstart bend [: ACAP-CSITE-YIELD ;] ACAP-SWEEP ;

\ After both sweeps classify live addresses: a window-relative DATA value could
\ otherwise read as a live CODE address. D0 keeps its 8-residue, so the seed's
\ (seed-DP - D0) delta preserves alignment.
: ACAP-NORMALIZE-DSITES ( -- )
   AOT-DATA-D0 @ {: d0:n :}
   d0 7 and {: base:n :}
   AOT-DSITE-N @ 0 ?do
      AOT-DSITE-BUF@ i 4 * + ACAP-W32@ {: site:n :}
      AOT-BLOB-BUF@ site AOT-DSITE-OFF-MASK and + {: p:ptr :}
      site AOT-DSITE-CELL and 0<> if
         p CELL-VIEW @ d0 - base + p AOT-N-C!
      else
         p AOT-BLOB-BUF@ AOT-BLOB-LEN @ + ADDRESS-CARRIER:CHAIN-SIZE {: size:n :}
         p p size ADDRESS-CARRIER:CHAIN-VALUE d0 - base + size ADDRESS-CARRIER:SET-CHAIN-VALUE
      then
   loop
   base AOT-DATA-D0 ! ;

\ The bytes travel: a TRUST row's name and signature are `s"` literals in the DP
\ heap (the metabuild window: 5726 bytes of span, 24 nonzero). The span does
\ not: the compiler chain's 1,531,045-byte span holds 32 bytes of content,
\ so only present cells travel and the seed
\ zeroes the span first.
\ A declared address cell's value is owned by its declaration, never by the
\ window's bytes: the seed recreates the exact null or window-relative target
\ and re-registers the kind, and a non-null target the window does not place is
\ refused. The set comes from the table, written where a cell's kind is
\ decided. ACAP-BAKE-DATA says which cells outside the window may travel.
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

\ Each scan borrows LIVE-SPAN once and neither registers address cells nor calls
\ back into source while it is live.
: ACAP-XTCELL-OFF ( n -- n ) SNAP-RELOC:XTCELL-OFF-MASK and ;

: ACAP-XTCELL-DATA? ( n -- bool )
   SNAP-RELOC:XTCELL-DATA-TAG and 0 <> ;

: ACAP-XTCELL-AT ( n -- ptr n )
   AOT-LIVE-DATA swap ACAP-XTCELL-OFF + ;

: ACAP-XTCELL-CELL-REFUSE ( n -- ) {: off:n :}
   s" aot-capture: declared address cell outside DATA: " type off . cr
   s" aot-capture: declared address cell is outside DATA" 74 die ;

: ACAP-XTCELL-CELL-CHECK ( n -- ) {: off:n :}
   off SNAP-RELOC:XTCELL-OFF-MAX > if off ACAP-XTCELL-CELL-REFUSE then ;

\ A row that overlaps the window without lying inside it would leave half a host
\ address in the baked bytes.
: ACAP-XTCELL-STRADDLES ( n -- ) {: woff:n :}
   s" aot-capture: declared address cell straddles the window edge at offset " type woff . cr
   s" aot-capture: declared address cell straddles the window edge" 74 die ;

\ The capture describes the window as one presence bit and one varint per whole
\ cell, so a declared cell that begins part way into a cell has no bit to carry
\ it.
: ACAP-XTCELL-GRID ( n -- ) {: woff:n :}
   s" aot-capture: declared address cell off the window cell grid at offset " type woff . cr
   s" aot-capture: a declared DATA cell is not on the window cell grid" 74 die ;

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

\ A DATA-pointer cell is answered by the DATA span and an execution-token cell
\ by the code span: the kind is recorded where it is decided, not guessed.
: ACAP-XTCELL-TARGET ( n n n n n -- n n n )
   {: row:n b0:n b1:n d0:n d1:n :}
   row ACAP-XTCELL-AT AOT-CELL@
   row ACAP-XTCELL-DATA? if d0 d1 else b0 b1 then ;

: ACAP-XTCELL-TARGET-IN? ( n n n n n -- bool )
   ACAP-XTCELL-TARGET {: v:n lo:n hi:n :}
   v lo >= v hi < and ;

: ACAP-CELL-NAMED ( n -- n ) {: target:n :}
   target ACAP-TARGET-NAME? if
      ACAP-POOL-ADD 1+ AOT-WINDOW:XTOFF-NAME-TAG or exit
   then 2drop
   target ACAP-TARGET-REFUSE ;

: ACAP-XTCELL-META ( n n n n n n -- n )
   {: k:n row:n b0:n b1:n d0:n d1:n :}
   row b0 b1 d0 d1 ACAP-XTCELL-TARGET {: v:n lo:n hi:n :}
   row ACAP-XTCELL-DATA? 0= v 0<> and if
      v lo < v hi >= or if v ACAP-CELL-NAMED exit then
   then
   v 0<> v lo < v hi >= or and if
      s" aot-capture: address row " type k .INT
      s"  cell DATA+" type row ACAP-XTCELL-OFF .INT
      s"  expected range " type lo .INT space hi .INT cr
   then
   v lo hi ACAP-TARGET-OFFSET
   row ACAP-XTCELL-DATA? if AOT-WINDOW:XTOFF-DATA-TAG or then ;

\ -1 when the row holds no window code.
: ACAP-GRAPH-CELL-OFF ( n -- n )
   {: row:n :}
   row ACAP-XTCELL-DATA? if -1 exit then
   row ACAP-XTCELL-AT AOT-CELL@ AOT-CODE-B0 @ - {: off:n :}
   off 0 >=  off AOT-BLOB-LEN @ <  and if off else -1 then ;

: ACAP-GRAPH-ROOTS ( -- )
   ACAP-ALL-NAMES @ 0= if ACAP-ENTRY-BUILD then
   0 ACAP-GWORK-N !
   ACAP-REC-ALL @ 0 ?do
      i ACAP-NAMED? if
         1 i ACAP-NAMED-BIT !
         i ACAP-GRAPH-CODE? if
            i ACAP-GRAPH-MARK-REC
         then
      else
         0 i ACAP-NAMED-BIT !
      then
   loop
   ACAP-ENTRY-OFF-RELEASE 0 ACAP-ENTRY-N !
   \ Every declared code cell is a root even when its owning definition is
   \ private: the DATA row is part of the captured ABI.
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do rows i cells + @ ACAP-GRAPH-CELL-OFF ACAP-GRAPH-MARK-OFF loop ;

\ A shadowed record the capture ships no row for keeps its routine only while
\ live code reaches it (src/habu/aot-shadow.f SH-STRIP). The reach above cannot
\ say so: a retired record has no row (ACAP-ADD-REC), so its body is a gap the
\ gap sweep scans as a root. This reach starts from ACAP-GRAPH-ROOTS, the
\ declared code cells and every gap but a retired body, follows edges only out
\ of reached bodies and marks only its own nodes, leaving ARM64 retention and
\ the payload alone. A node is a capture record, or a retired window record
\ numbered ACAP-REC-ALL plus its window offset. Reached code reaches its owner
\ (ACAP-GOWNER) and the lowest retired record covering it, and a shared entry
\ names the lowest record (ACAP-TGT>REC), so an EXPORT alias reaches the body's
\ record. A retired id is a window offset plus one, 0 for none.
DYNAMIC-BUFFER ACAP-RWORD n          \ blob word -> retired id over it
DYNAMIC-BUFFER ACAP-SMARK n          \ node -> 1 once reached
DYNAMIC-BUFFER ACAP-SWORK n          \ the nodes reached and not yet scanned
variable ACAP-SWORK-N

\ Empty when the retired record's code is not the window's.
: ACAP-RETIRED-SPAN ( n -- n n ) {: idx:n :}
   idx AOT-REC AOT-RXT AOT-CODE-B0 @ - {: start:n :}
   start idx AOT-REC AOT-RBYTES + {: end:n :}
   start 0 >=  end AOT-BLOB-LEN @ <=  and if start end else 0 0 then ;

: ACAP-SHADOW-INDEX ( -- )
   AOT-BLOB-LEN @ 4 / {: words:n :}
   ACAP-REC-ALL @ ACAP-W-R1 @ + ACAP-W-R0 @ - {: nodes:n :}
   words ACAP-RWORD-RESERVE  nodes ACAP-SMARK-RESERVE  nodes ACAP-SWORK-RESERVE
   words 0 ?do 0 i ACAP-RWORD ! loop
   nodes 0 ?do 0 i ACAP-SMARK ! loop
   0 ACAP-SWORK-N !
   ACAP-W-R1 @ ACAP-W-R0 @ ?do
      i ACAP-DICT>CAP 0 < if
         i ACAP-W-R0 @ - 1+ {: id:n :}
         i ACAP-RETIRED-SPAN {: start:n end:n :}
         end 4 / start 4 / ?do
            i ACAP-RWORD @ 0= if id i ACAP-RWORD ! then
         loop
      then
   loop ;

: ACAP-SHADOW-NODE ( n -- ) {: v:n :}
   v ACAP-SMARK @ 0<> if exit then
   1 v ACAP-SMARK !
   v ACAP-SWORK-N @ ACAP-SWORK !
   ACAP-SWORK-N @ 1+ ACAP-SWORK-N ! ;

: ACAP-SHADOW-OFF ( n -- ) {: off:n :}
   off 0<  off AOT-BLOB-LEN @ >=  or if exit then
   off 4 / ACAP-GOWNER @ {: owner:n :}
   owner 0<> if owner 1- ACAP-SHADOW-NODE then
   off 4 / ACAP-RWORD @ {: id:n :}
   id 0<> if ACAP-REC-ALL @ id + 1- ACAP-SHADOW-NODE then ;

: ACAP-SHADOW-SCAN ( n -- ) {: v:n :}
   v ACAP-REC-ALL @ < if
      v ACAP-GRAPH-START  v ACAP-GRAPH-END
   else
      v ACAP-REC-ALL @ - ACAP-W-R0 @ + ACAP-RETIRED-SPAN
   then {: from:n to:n :}
   from to >= if exit then
   from begin dup to < while
      dup ACAP-SHADOW-OFF
      dup ACAP-GRAPH-EDGES ACAP-SHADOW-OFF ACAP-SHADOW-OFF
      4 +
   repeat drop
   to 4 - ACAP-GRAPH-ENDS? 0= if to ACAP-SHADOW-OFF then ;

\ A gap word no retired record covers is a root, as the gap sweep has it.
: ACAP-SHADOW-GAP ( n -- ) {: at:n :}
   at ACAP-GSITE@ 3 = if exit then
   at 4 / ACAP-GOWNER @ 0<>  at 4 / ACAP-RWORD @ 0<>  or if exit then
   at ACAP-GRAPH-EDGES ACAP-SHADOW-OFF ACAP-SHADOW-OFF
   at ACAP-GRAPH-ENDS? 0= if at 4 + ACAP-SHADOW-OFF then ;

\ Runs after the sweeps and before compaction: it reads their owners and sites.
: ACAP-SHADOW-REACH ( -- )
   ACAP-SHADOW-INDEX
   ACAP-REC-ALL @ 0 ?do
      i ACAP-NAMED-BIT @ 0<>  i ACAP-GRAPH-CODE?  and if i ACAP-SHADOW-NODE then
   loop
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do rows i cells + @ ACAP-GRAPH-CELL-OFF ACAP-SHADOW-OFF loop
   AOT-BLOB-LEN @ 4 / 0 ?do i 4 * ACAP-SHADOW-GAP loop
   begin ACAP-SWORK-N @ 0 > while
      ACAP-SWORK-N @ 1- ACAP-SWORK-N !
      ACAP-SWORK-N @ ACAP-SWORK @ ACAP-SHADOW-SCAN
   repeat ;

: ACAP-SHADOW-LIVE? ( n -- bool ) {: idx:n :}
   idx ACAP-DICT>CAP {: k:n :}
   k 0 < if idx ACAP-W-R0 @ - ACAP-REC-ALL @ + else k then
   ACAP-SMARK @ 0<> ;

\ A named alias or a direct call through a public wrapper can retain a private
\ definer's code while stripping its name. The graph may mark only the covering
\ parent or alias record, so the clause's exact-entry metadata is kept when the
\ parent's entry survived the sweep.
: ACAP-GRAPH-NAME-DOES ( -- )
   ACAP-REC-ALL @ 0 ?do
      i ACAP-DOES-COMPANION? if
         i 1- ACAP-GRAPH-START 4 / ACAP-GMAP @ 0<> if
            1 i ACAP-NAMED-BIT !
         then
      then
   loop ;

\ A native x86 capture keeps the source window's byte offsets; the recorded
\ routine graph decides which records stay live.
: ACAP-NATIVE-ROOTS ( [ n -- bool ] -- ) {: live :}
   ACAP-REC-ALL @ ACAP-GMARK-RESERVE
   ACAP-REC-ALL @ 0 ?do
      i live execute if 1 else 0 then i ACAP-GMARK !
      i ACAP-NAMED? if 1 else 0 then i ACAP-NAMED-BIT !
   loop
   ACAP-REC-ALL @ 0 ?do
      i ACAP-DOES-COMPANION? if
         i 1- ACAP-GMARK @ 0<> if 1 i ACAP-NAMED-BIT ! then
      then
   loop
   -1 ACAP-GRAPH-READY ! ;

: ACAP-GRAPH-REMAP-XTOFF ( -- )
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i ACAP-XTMETA@ {: meta:n :}
      meta AOT-WINDOW:XTOFF-KIND-MASK and 0= meta AOT-WINDOW:XTOFF-VALUE-MASK and 0<> and if
         meta AOT-WINDOW:XTOFF-VALUE-MASK and 1- ACAP-GRAPH-MAP@ dup 0 < if
            s" aot-capture: declared code cell targets a removed span" 74 die
         then
         1+ i AOT-WINDOW:XTOFF-ROW * AOT-WINDOW:XTOFF-BUF@ + 4 + AOT-P32!
      then
   loop ;

\ One bit a cell, and a varint per present cell in cell order, so the decoder
\ walks the values with a running cursor (counts not stored). The bitmap ends at
\ the highest present cell. Both buffers refuse their own overflow by name.
variable ACAP-RQ      \ the segment cursor across the window
variable ACAP-WLEN    \ the window's content length: bytes above it are not read

\ The zeros are written rather than assumed, because a chain capture reaches
\ this buffer more than once.
: ACAP-BM-EXTEND ( n -- ) {: b:n :}
   b 1+ AOT-WINDOW:BM-CAP > if
      s" aot-capture: the window DATA cells exceed the AOT bitmap buffer" 74 die then
   begin AOT-WINDOW:BM-LEN @ b <= while
      0 AOT-WINDOW:BM-BUF@ AOT-WINDOW:BM-LEN @ + c!
      AOT-WINDOW:BM-LEN @ 1+ AOT-WINDOW:BM-LEN !
   repeat ;

: ACAP-ADD-CELL ( n n -- ) {: c:n v:n :}
   c AOT-WINDOW:CELL-BITS / ACAP-BM-EXTEND
   AOT-WINDOW:BM-BUF@ c AOT-WINDOW:CELL-BITS / + c@
   1 c AOT-WINDOW:CELL-BITS mod lshift or
   AOT-WINDOW:BM-BUF@ c AOT-WINDOW:CELL-BITS / + c!
   AOT-WINDOW:VAL-LEN @ AOT-WINDOW:VMAX + AOT-WINDOW:VAL-CAP > if
      s" aot-capture: the window DATA cells exceed the AOT value buffer" 74 die then
   v  AOT-WINDOW:VAL-BUF@ AOT-WINDOW:VAL-LEN @ +  AOT-WINDOW:CELL-V! {: w:n :}
   AOT-WINDOW:VAL-LEN @ w + AOT-WINDOW:VAL-LEN !
   c 1+ AOT-WINDOW:CELL-BYTES * AOT-WINDOW:CONTENT-END !
   AOT-WINDOW:CELL-N @ 1+ AOT-WINDOW:CELL-N ! ;

\ The span is rounded up to a whole cell, so the last cell can reach above the
\ builder's DP; those bytes read as the zeros the seed leaves there.
: ACAP-CELL@ ( n n -- n )
   {: d0:n c:n :}
   c AOT-WINDOW:CELL-BYTES * {: off:n :}
   off AOT-WINDOW:CELL-BYTES + ACAP-WLEN @ <= if
      d0 off + AOT-N>U8 CELL-VIEW @ exit
   then
   0
   AOT-WINDOW:CELL-BYTES 0 ?do
      off i + ACAP-WLEN @ < if
         d0 off + i + AOT-N>U8 c@ i 8 * lshift or
      then
   loop ;

\ One gap between declared cells: a declared cell keeps its bit clear and the
\ seed writes its row's value. Both bounds are cell multiples (cell-aligned
\ base, grid-checked cells, span rounded up).
: ACAP-SCAN-SEG ( n n n -- ) {: d0:n from:n to:n :}
   to AOT-WINDOW:CELL-BYTES /  from AOT-WINDOW:CELL-BYTES / ?do
      d0 i ACAP-CELL@ {: v:n :}
      v 0<> if i v ACAP-ADD-CELL then
   loop ;

\ A window offset under the window tag for a carried cell, a plain DATA offset
\ for a fixed engine cell. Readers take the low 31 bits as the offset and the
\ top bit as the tag; DATA spans more than 2^31 bytes, so a fixed offset past
\ the field is refused.
: ACAP-XTCELL-LOC ( n n n -- n ) {: woff:n celloff:n len:n :}
   woff 0 >= woff len < and if woff AOT-WINDOW:XTOFF-WINDOW-TAG or exit then
   celloff AOT-WINDOW:XTOFF-LOC-MASK > if
      s" aot-capture: fixed address cell lies past the row's 31-bit DATA offset"
      74 die then
   celloff ;


\ Rows go out in location order, fixed cells first, never in registration
\ order: a fixed cell the build host's prefix or seed declared keeps the
\ registry slot it took there, so registration order is the host's history (a
\ native host and a source-only recovery host built one tree into two tables).
\ Each row's location is distinct, so the order is total.
: ACAP-SORT-XTOFF ( -- )
   AOT-WINDOW:XTOFF-N @ {: count:n :}
   count 2 < if exit then
   0 AOT-WINDOW:XTOFF-STORAGE count
   [: $FFFFFFFF and swap $FFFFFFFF and > ;] SORT:SORT! ;


\ Whose cell it is. A cell inside the captured DATA span travels with the
\ window: its row is window-relative, the seed recreates its value and kind, and
\ ACAP-CLASSIFY-XTCELL keeps its bytes out of every sparse run. A cell outside
\ belongs to the engine the window was loaded into, and a row travels only when
\ the window's load stored a window address there, a load-time effect no byte
\ carries: `set-check` storing the recompiled checker's xt into HOOK-CELL is row
\ 0. Any other target is state the window did not create (a booted engine's own
\ hook cells, its own captured window's cells): the seeded engine's state is
\ already right, so the cell is left alone. Refusing it ended every capture in a
\ booted engine on HOOK-CELL; the refusal
\ stays for a cell inside the window whose target the window does not place
\ (test/aot-address-cell-target-out-bad.f).
\ The span is whole cells; the rounding bytes are above the captured DP and
\ zero in the seed. The base is cell-aligned (src/habu/aot-arm.f LATCH-OPEN
\ aligns DP before latching D0; ACAP-START admits only the latched span) so
\ every declared cell falls on one grid cell (release engine: all 33,324 window
\ cells eight-byte aligned).
: ACAP-BAKE-DATA ( n n n n -- )
   {: b0:n b1:n d0:n d1:n :}
   d1 d0 - {: len:n :}
   len AOT-WINDOW:SPAN-CAP > if
      s" aot-capture: DATA window exceeds the AOT window span cap" 74 die then
   len ACAP-WLEN !
   len AOT-WINDOW:CELL-BYTES 1- +
   AOT-WINDOW:CELL-BYTES / AOT-WINDOW:CELL-BYTES * {: rlen:n :}
   rlen AOT-WINDOW:SPAN-CAP > if
      s" aot-capture: DATA window exceeds the AOT window span cap" 74 die then
   rlen AOT-DATA-SIZE !
   d0 AOT-DATA-N - {: d0off:n :}
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row ACAP-XTCELL-OFF {: celloff:n :}
      celloff ACAP-XTCELL-CELL-CHECK
      celloff d0off - {: woff:n :}
      woff len ACAP-CLASSIFY-XTCELL
      woff 0 >= woff len < and  woff AOT-WINDOW:CELL-BYTES mod 0<> and if
         woff ACAP-XTCELL-GRID then
      woff 0 >= woff len < and
      row b0 b1 d0 d1 ACAP-XTCELL-TARGET-IN? or if
         i row b0 b1 d0 d1 ACAP-XTCELL-META {: meta:n :}
         woff celloff len ACAP-XTCELL-LOC
         meta ACAP-ADD-XTOFF
      then
   loop
   ACAP-SORT-XTOFF ;

\ Copied once everything that may still change the cells has run (CAPTURE says
\ what that is). ACAP-SORT-XTOFF left the window's rows as the table's tail in
\ ascending offset order, so the gaps between them are walked off the table.
: ACAP-COPY-DATA ( n -- )
   {: d0:n :}
   AOT-DATA-SIZE @ {: len:n :}
   0 ACAP-RQ !
   AOT-WINDOW:XTOFF-N @ 0 ?do
      i ACAP-XTOFF@ {: loc:n :}
      loc AOT-WINDOW:XTOFF-WINDOW-TAG and 0<> if
         loc AOT-WINDOW:XTOFF-LOC-MASK and {: off:n :}
         d0 ACAP-RQ @ off ACAP-SCAN-SEG
         off AOT-WINDOW:CELL-BYTES + ACAP-RQ !
      then
   loop
   ACAP-RQ @ len < if d0 ACAP-RQ @ len ACAP-SCAN-SEG then ;

\ The boot-run list: a 0-terminated [len][name] list EM-AOT-BOOTRUN walks (LFIND
\ + blr) after the seed installs the REPL; the live trailing 0 is uncounted, so
\ the bake needs no pad. It runs at the end of the engine prefix on every boot
\ (src/habu/habu2.f, AOT-BOOTRUN-CAP), before the first user token of a piped
\ program, a `--load` run and a tty REPL alike, so a capture-window fixture can
\ be a batch fixture; a pty is needed only for an interactive claim (the entry
\ words ask TTY?).
public

\ Native x86 keeps the copied window intact, so its coordinate is the source
\ byte offset.
: CODE-OFFSET ( n -- n ) {: address:n :}
   address ACAP-W-B0 @ - {: off:n :}
   ACAP-NATIVE-MODE @ if
      off 0< off AOT-BLOB-LEN @ >= or if -1 exit then
      off exit
   then
   off ACAP-GRAPH-MAP@ ;

\ The live bounds cover the whole source window, removed bodies included; the
\ byte copy owns a separate output extent after compaction.
: CODE-WINDOW ( -- n n n )
   ACAP-W-B0 @ dup ACAP-GOLDLEN @ + ACAP-GNEWLEN @ ;

\ Every record the capture saw, shipped or not, by capture order, for the map
\ tools/native-build-core.f writes beside a stripped image (<image>.map). The
\ verbatim 48-byte records still hold every name.

: MAP-N ( -- n )
   ACAP-REC-ALL @ ;

: MAP-NAMED ( n -- n )                             \ 1 when the image kept the name, 0 when stripped
   ACAP-NAMED-BIT @ ;

: MAP-NAME$ ( n -- ptr u8 n )
   ACAP-REC-NAME$ ;

\ A package row spends these two cells on wids, which take the image's form as
\ MAP-WID's does.
: MAP-PKG? ( n -- bool )
   ACAP-REC-DST CELL-VIEW AOT-RWID -1 = ;

: MAP-START ( n -- n )                             \ code offset (-1 if removed), or package public wid
   dup MAP-PKG? 0= if
      dup MAP-NAMED 0= over ACAP-GRAPH-LIVE? 0= and if drop -1 exit then
   then
   dup ACAP-REC-DST ACAP-W32@ swap MAP-PKG? ACAP-WID-FIELD ;

: MAP-LEN ( n -- n )                               \ code length in bytes, or a package's private wid
   dup ACAP-REC-DST 8 + ACAP-W32@ swap MAP-PKG? ACAP-WID-FIELD ;

\ The wid the image carries, not the host's: a sidecar of the building engine's
\ ids made two builds of one tree on two hosts differ in 197 map rows (measured,
\ the only difference left once the image matched).
: MAP-WID ( n -- n )                               \ window offset, or -1 for a package row
   ACAP-REC-DST {: v:ptr :}
   v CELL-VIEW AOT-RWID -1 = if -1 exit then
   v 40 + ACAP-W32@ ACAP-REL-WID ;

: BOOTRUN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u 255 > if s" aot-capture: boot-run name too long" 74 die then
   u 0 < u AOT-BOOTRUN-CAP 2 - AOT-BOOTRUN-LEN @ - > or if
      s" aot-capture: boot-run overflow" 74 die
   then
   AOT-BOOTRUN-LEN @ {: off:n :}
   u  AOT-BOOTRUN-BUF@ off + c!                     \ [len]
   u 0 ?do a i + c@  AOT-BOOTRUN-BUF@ off + 1+ i + c!  loop
   off u + 1+ AOT-BOOTRUN-LEN !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;      \ live terminator (uncounted)

\ What the row table must hold, recounted off the live table with
\ ACAP-BAKE-DATA's arithmetic, so a producer checks the table against a reading
\ not taken from it (tools/aot-chain-capture.f ?XTOFF,
\ test/aot-artifact-roundtrip.f): a row never written, written twice, or for a
\ cell in neither population. The two are disjoint: a cell inside the window
\ counts here however its target is placed. DECLARED-IN: cells inside the
\ window's DATA span, whose bytes stay out of every sparse run, so the seed's
\ value comes only from their row.
: DECLARED-IN ( n n -- n )
   {: d0:n d1:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   d1 d0 - {: len:n :}
   0
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ ACAP-XTCELL-OFF d0off - {: woff:n :}
      woff 0 >= woff len < and if 1+ then
   loop ;

\ TARGETED-OUT: cells outside the window whose declared target is inside their
\ kind's span. TRAPPED-BELOW asks a narrower question of the same population
\ (the CODE cells below the window a boot-run installer must refill).
: TARGETED-OUT ( n n n n -- n )
   {: b0:n b1:n d0:n d1:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   d1 d0 - {: len:n :}
   0
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row ACAP-XTCELL-OFF d0off - {: woff:n :}
      woff 0 >= woff len < and 0= if
         row b0 b1 d0 d1 ACAP-XTCELL-TARGET-IN? if 1+ then
      then
   loop ;

\ Declared address cells below the window holding an address inside it: each is
\ a boot-run row owed, because the seeded engine's cell holds what its prefix
\ put there and only a boot-run entry restores the window's routine. A caller
\ declaring fewer rows has an undeclared installer. It counts content, not kind,
\ so an installer of a new kind counts too, and an install made after the window
\ closes points elsewhere and is not counted.
: TRAPPED-BELOW ( n n n -- n )
   {: b0:n b1:n d0:n :}
   d0 AOT-DATA-N - {: d0off:n :}
   0
   ADDRESS-CELLS:LIVE-SPAN {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      row ACAP-XTCELL-OFF {: off:n :}
      row ACAP-XTCELL-DATA? 0= off d0off < and if
         row ACAP-XTCELL-AT AOT-CELL@ {: v:n :}
         v b0 >= v b1 < and if 1+ then
      then
   loop ;

\ The signature audit's counts: records with a known checker effect, and those
\ exempted (package, retired, no effect). They sum to the window's record
\ count, which tools/aot-chain-capture.f prints them beside.
: SIG-KNOWN ( -- n ) ACAP-SIG-KNOWN @ ;
: SIG-EXEMPT ( -- n ) ACAP-SIG-EXEMPT @ ;

private

: ACAP-PWID-IN-RANGE? ( n -- bool ) {: wid:n :}
   wid 0 < 0=  wid PROT-WID-MAX <  and ;
: ACAP-LIVE-PWID? ( n -- bool ) {: wid:n :}
   wid ACAP-PWID-IN-RANGE? 0= if 0 0= 0= exit then
   AOT-LIVE-DATA PROT-BITS-OFF + wid 3 rshift + BYTE-VIEW c@
   wid 7 and rshift 1 and 0= 0= ;
: ACAP-PWIN-ADD ( n -- ) {: rel:n :}
   rel AOT-PWIN-N @ 4 * AOT-PWIN-BUF@ + AOT-P32!
   AOT-PWIN-N @ 1+ AOT-PWIN-N ! ;

\ Only WIDs created in the runtime window: replaying absolute build-host bits
\ would resurrect the discarded namespace.
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

\ [d0,d1) is the REPL DATA span (create/variable).
: ACAP-RESET ( -- )
   ACAP-ENTRY-OFF-RELEASE 0 ACAP-ENTRY-N !
   0 AOT-BLOB-LEN !  0 AOT-REC-N !  0 AOT-SITE-N !  ACAP-POOL-RESET
   0 AOT-DSITE-N !  0 AOT-DATA-D0 !  0 AOT-DATA-SIZE !
   0 AOT-CSITE-N !  0 AOT-CODE-B0 !  0 AOT-WINDOW:XTOFF-N !  0 AOT-SPAN:N !
   AOT-WINDOW:WINDOW-RESET
   0 ACAP-NATIVE-MODE !  0 ACAP-GOLDLEN !  0 ACAP-GNEWLEN !
   0 AOT-XTSITE:N !  0 AOT-PWIN-N !  0 ACAP-GRAPH-READY !
   0 AOT-BOOTRUN-LEN !  0 AOT-BOOTRUN-BUF@ c!
   AOT-SHADOW:RESET ;                           \ src/habu/aot-shadow.f fills it after CAPTURE
public

\ AOT-ARM owns the captured cursors and frozen checker payload; producers latch
\ them before loading this tool, whose compiler dependencies stay outside the
\ captured window.

\ Not reset with the buffers: the marks describe the process, and a widened
\ re-capture of one window (test/aot-wid-build.f) is the same process.
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
private
CAST: ACAP-CLEANUP-XT ( n -- [ -- ] )
CAST: ACAP-RANGE-XT ( n -- [ ptr u8 n -- ] )
public

: ACAP-RELEASE-DYNAMIC ( n n n n -- ) {: b0:n b1:n d0:n d1:n :}
   d1 d0 < if s" aot-capture: reversed dynamic DATA span" 74 die then
   s" DYNAMIC-STORAGE:RELEASE-ALL" ACAP-DBUF-XT {: xt:n :}
   xt b0 >= xt b1 < and if xt ACAP-CLEANUP-XT execute exit then
   s" DYNAMIC-STORAGE:RELEASE-RANGE" ACAP-DBUF-XT {: range:n :}
   range 0= if
      s" aot-capture: dynamic storage has no range release" 74 die then
   d0 AOT-N>U8 d1 d0 - range ACAP-RANGE-XT execute ;

\ The window's checker keeps a symbol only while a record the image ships with
\ its name resolves to it (src/core/checker-surface.f). CHECKER-CAPTURE-PREPARE
\ answered as a snapshot does (every record named); the names this capture keeps
\ are final after ACAP-GRAPH-NAME-DOES, so the checker sweeps again
\ (src/core/checker.f CHECKER-SWEEP STRIP) with ACAP-NAMED-BIT before its DATA
\ is copied. Records below the prelude band always ship named; the band
\ [ACAP-PRE-R, ACAP-W-R0) and records defined after the window never ship.
: ACAP-SHIPS-NAMED? ( n -- bool ) {: idx:n :}
   idx ACAP-PRE-R @ < if true exit then
   idx ACAP-DICT>CAP {: k:n :}
   k 0 < if false exit then
   k ACAP-NAMED-BIT @ 0<> ;

private
CAST: ACAP-STRIP-XT ( n -- [ [ n -- bool ] -- ] )
public

: ACAP-CHECKER-STRIP ( n n -- ) {: b0:n b1:n :}
   AOT-ARM:PAYLOAD-MODE @ 2 <> if exit then
   s" CHECKER-SWEEP:STRIP" ACAP-DBUF-XT {: xt:n :}
   xt b0 < xt b1 >= or if
      s" aot-capture: the window's checker has no symbol strip" 74 die then
   ['] ACAP-SHIPS-NAMED? xt ACAP-STRIP-XT execute ;

\ Full-runtime payload mode is explicit. Its verified checker stores travel in
\ DATA, so an empty sidecar is valid only for that captured owner and closure.
: ACAP-ADDRESS ( ptr u8 -- n ) NULL-PTR BYTE-VIEW - ;

private
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
public

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

private
CAST: ACAP-RECORD-CAPTURE-XT ( n -- [ [ n -- n ] [ -- ] -- ] )
public

: ACAP-COPY-CHECKER-DATA ( -- )
   AOT-ARM:PAYLOAD-OWNER @ CHECKER-OWNER-ABI:RECORD-CAPTURE-OFF CELL +
      CHECKER-OWNER-GUARD:VALIDATE {: owner:ptr :}
   owner CHECKER-OWNER-ABI:RECORD-CAPTURE-OFF + CELL-VIEW @
      dup 0= if
         drop s" aot-capture: persistent checker has no record capture callback" 74 die
      then
      ACAP-RECORD-CAPTURE-XT {: capture :}
   ['] SH-SHIPPED [: ACAP-W-D0 @ ACAP-COPY-DATA ;]
      capture execute ;

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

private
\ The payload membership is exactly the bytes and records CAPTURE copied. The
\ DATA end may move after owner persistence, before WINDOW$ is read.
: ACAP-PAYLOAD-BAND? ( n n n n n n -- bool )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart AOT-ARM:B0 @ = bend AOT-ARM:B1 @ = and
   rstart AOT-ARM:R0 @ = and rend AOT-ARM:R1 @ = and
   d0 AOT-ARM:D0 @ = and d1 AOT-ARM:D1 @ = and ;
public

\ Both entries share the frozen-window check, record copy and wid audit.
: ACAP-START ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart bend rstart rend d0 d1 ACAP-PAYLOAD-BAND? 0= if
      s" aot-capture: capture bounds differ from frozen payload window" 74 die then
   bstart rstart rend d0 ACAP-BAND!
   PAYLOAD-CAPTURE
   ACAP-RESET
   ACAP-TIDX-BUILD                              \ xt -> record index for THIS dictionary
   bstart bend ACAP-COPY-BLOB
   rend rstart - ACAP-REC-OF-RESERVE
   rend rstart ?do i bstart ACAP-ADD-REC  i rstart - ACAP-REC-OF ! loop
   AOT-REC-N @ ACAP-REC-ALL !
   ACAP-AUDIT-WIDS ;

: ACAP-FINISH ( -- )
   0 AOT-CODE-B0 !                              \ the payload and merge contract use canonical zero
   ACAP-NORMALIZE-DSITES
   ACAP-COMPACT-RECS                            \ build 20B compact records + add record names to pool
   ACAP-PWIN-CAPTURE                            \ only the window's own seals travel
   AOT-ARM:PAYLOAD-MODE @ 1 = SITE-ROW AOT-SECTION:BYTES drop ;

: CAPTURE-PREPARE ( n n n n n n -- ) {: bstart:n bend:n rstart:n rend:n d0:n d1:n :}
   bstart bend rstart rend d0 d1 ACAP-START
   ACAP-GRAPH-SITES
   ACAP-SCAN-CALLS
   bstart bend d0 d1 ACAP-SCAN-DSITES
   bstart bend d0 d1 ACAP-SCAN-DEFER-SITES
   bstart bend ACAP-SCAN-CSITES
   bstart bend d0 d1 ACAP-RELEASE-DYNAMIC
   bstart bend d0 d1 ACAP-BAKE-DATA
   \ The code-site pass uses a zero base while canonicalising DATA chains.
   \ Reachability roots still inspect the live declared XT addresses, so restore
   \ the capture base before translating those addresses to blob offsets.
   bstart AOT-CODE-B0 !
   ACAP-GRAPH-INDEX
   ACAP-GRAPH-ROOTS
   ACAP-GRAPH-SWEEP  -1 ACAP-GRAPH-READY !
   ACAP-GRAPH-SWEEP-GAPS
   ACAP-GRAPH-SWEEP
   ACAP-SHADOW-REACH
   ACAP-GRAPH-NAME-DOES ;                       \ the names that ship are final here

: CAPTURE-COMPLETE ( n n n -- ) {: bstart:n bend:n d0:n :}
   bstart bend ACAP-CHECKER-STRIP               \ ... so the checker retires what they leave
   SH-NUMBER                                    \ final named rows are the shipped rows
   AOT-ARM:PAYLOAD-MODE @ 2 = if
      ACAP-COPY-CHECKER-DATA
   else
      d0 ACAP-COPY-DATA
   then
   ACAP-GRAPH-BUILD-MAP
   ACAP-GRAPH-COPY-BLOB
   ACAP-GRAPH-PATCH
   ACAP-GRAPH-REMAP
   ACAP-GRAPH-REMAP-XTOFF
   ACAP-FINISH ;

\ Reach comes from the native compiler's published x86 routine map. The
\ callback runs after declared address cells are classified, so shipped records
\ and code cells both root that graph; its query takes a capture record index.
\ Main code bytes stay at their copied offsets for the x86 shadow linker.
: CAPTURE-NATIVE ( n n n n n n [ -- ] [ n -- bool ] -- )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n prepare live :}
   bstart bend rstart rend d0 d1 ACAP-START
   -1 ACAP-NATIVE-MODE !
   AOT-BLOB-LEN @ dup ACAP-GOLDLEN ! ACAP-GNEWLEN !
   d0 AOT-DATA-D0 !
   d1 d0 - AOT-DATA-SIZE !
   bstart bend d0 d1 ACAP-RELEASE-DYNAMIC
   bstart bend d0 d1 ACAP-BAKE-DATA
   prepare execute
   live ACAP-NATIVE-ROOTS
   bstart bend ACAP-CHECKER-STRIP
   d0 ACAP-COPY-DATA
   ACAP-FINISH ;

\ The whitebox image keeps every name: its build passes true here, as its seal
\ pass stands down (src/core/util.f ENGINE-UNSEAL?). Whitebox suites are
\ the callers the derived keep rule cannot see - a suite reopens the package it
\ audits and calls a private word by name - and an engine that strips them has
\ nothing for the suite to name. Stripping is a property of the image a build
\ writes, so the build answers it here rather than through a widening keep-set.
: CAPTURE-WITH-NAMES ( n n n n n n bool -- )
   {: bstart:n bend:n rstart:n rend:n d0:n d1:n all:bool :}
   all ACAP-ALL-NAMES !
   bstart bend rstart rend d0 d1 CAPTURE-PREPARE
   bstart bend d0 CAPTURE-COMPLETE ;

: CAPTURE ( n n n n n n -- )
   false CAPTURE-WITH-NAMES ;

;package
