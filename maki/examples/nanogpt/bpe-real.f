\ maki/bpe-real.f - REAL GPT-2 vocab loader + id-translation over the landed byte-level
\ BPE engine (dot habu-bpe-real-vocab-c973932a; follow-up to habu-bpe-tokenizer-gpt-37d7f243,
\ landed 815f6437). EXTENDS maki/bpe.f - it does not rewrite it.
\
\ The landed engine represents tokens by INTERNAL ids (byte b -> id b; merge of rank r ->
\ id 256+r) and encodes with those. Real GPT-2 ids come from encoder.json and bear no
\ relation to that scheme (e.g. "To be" -> [2514,307], not byte/rank ids). This module
\ loads the real merges through the engine's own BPE-BEGIN/BPE-MERGE+/BPE-SEAL (so the
\ merge STRUCTURE and priority order are the engine's, unchanged) and adds one array,
\ BPR-GID, mapping each internal id to its real encoder.json id. BPR-ENCODE runs the
\ engine then translates internal -> real; BPR-DECODE inverts real -> internal (the id
\ map is a proven bijection, so the inverse is unique) and expands through the engine.
\
\ Loader discipline mirrors maki/tokenizer.f + maki/bpe.f: FULL validation BEFORE any
\ mutation (byte-id table in range + pairwise distinct, merged ids in range + distinct +
\ disjoint from the byte ids => the id map is injective, i.e. a bijection onto its image),
\ named E-codes per rejected domain, ready-state gating on every public word, and no
\ partial caller write on rejection (encode reuses the engine's staged single write;
\ decode validates every id into scratch before the engine's single write). The committed
\ real subset + the fetched-artifact provenance live in maki/bpe-real-data.f. maki -> habu
\ only. bpe-real owns -5303..-5306.

require maki/examples/nanogpt/bpe.f

-5303 constant E-BPR-EMPTY   \ public op before BPR-INSTALL: no real vocab loaded yet
-5304 constant E-BPR-RANGE   \ id/byte outside its domain: a decode cell not a finite exact id in [0,50257), or a real id absent from the loaded map (incl. the special/EOT, which carries no bytes)
-5305 constant E-BPR-CAP     \ invalid length/capacity: negative id count, or a decode id stream past the scratch bound
-5306 constant E-BPR-VOCAB   \ malformed real vocab: merge count out of [0,cap], a byte/merged id out of [0,50257), or the id map not injective (a duplicate byte id, merged id, or byte==merged collision)

package MAKI
private

50257 constant BPR-VMAX        \ real GPT-2 vocab size: ids 0..50256 (50256 = <|endoftext|>)
256   constant BPR-BYTE-N       \ base byte tokens (internal ids 0..255)
512   constant BPR-MAX          \ merge-subset capacity (internal merged id = 256+rank)
768   constant BPR-GID-CAP      \ BPR-BYTE-N + BPR-MAX: internal-id -> real-id table size
4096  constant BPR-INT-CAP      \ max ids one BPR-DECODE stages into internal-id scratch

create BPR-GID BPR-GID-CAP cells allot   \ internal id -> real GPT-2 id (bytes 0..255, then merges)
create BPR-INT BPR-INT-CAP cells allot    \ decode: real ids translated to internal, then expanded
variable BPR-GN                           \ live BPR-GID length = 256 + loaded merges
variable BPR-DONE                         \ 1 once BPR-INSTALL has loaded a real vocab (ready)

\ ---- readiness gate --------------------------------------------------------------
: BPR-READY ( -- )  BPR-DONE @ 0= if E-BPR-EMPTY throw then ;

\ ---- pre-mutation vocab validation (all on the raw arrays; no state touched) ------
\ 256 byte ids: each in [0,VMAX), pairwise distinct
: BPR-BID-OK? ( ptr a -- )  {: bid:ptr :}
   BPR-BYTE-N 0 ?do
      bid i cells + @  dup 0 <  swap BPR-VMAX >=  or if E-BPR-VOCAB throw then
   loop
   BPR-BYTE-N 0 ?do
      BPR-BYTE-N i 1+ ?do
         bid i cells + @  bid j cells + @  = if E-BPR-VOCAB throw then
      loop
   loop ;
\ nm merged ids: each in [0,VMAX), pairwise distinct, and disjoint from the byte ids
: BPR-MID-OK? ( ptr a ptr a n -- )  {: mid:ptr bid:ptr nm:n :}
   nm 0 ?do
      mid i cells + @  dup 0 <  swap BPR-VMAX >=  or if E-BPR-VOCAB throw then
   loop
   nm 0 ?do
      nm i 1+ ?do
         mid i cells + @  mid j cells + @  = if E-BPR-VOCAB throw then
      loop
   loop
   nm 0 ?do
      BPR-BYTE-N 0 ?do
         mid j cells + @  bid i cells + @  = if E-BPR-VOCAB throw then
      loop
   loop ;

\ ---- id translation --------------------------------------------------------------
\ internal id (engine output) -> real GPT-2 id; internal id must be in [0,BPR-GN)
: BPR-GID-AT ( n -- n )  {: k:n :}
   k 0 <  k BPR-GN @ >=  or if E-BPR-RANGE throw then  BPR-GID k cells + @ ;
\ validate a stored float cell as an exact real id in [0,VMAX): NaN/inf/fractional/
\ negative/>=VMAX are rejected (mirrors BPE-CELL>ID)
: BPR-CELL>REAL ( r -- n )  {: v:r :}
   v f0< if E-BPR-RANGE throw then
   v BPR-VMAX s>f f< 0= if E-BPR-RANGE throw then
   v f>s {: k:n :}
   k s>f v f= 0= if E-BPR-RANGE throw then
   k ;
\ real GPT-2 id -> internal id (the map is injective, so the match is unique); a real id
\ absent from the loaded map (e.g. the special, or any token outside the subset) is rejected
: BPR-REAL>INT ( n -- n )  {: r:n :}
   BPR-GN @ 0 ?do  BPR-GID i cells + @ r = if i unloop exit then  loop  E-BPR-RANGE throw ;

public

\ Install a real vocab: byte-id table (256), merges as engine-internal child pairs
\ (ma,b) with their real merged ids (mid), count nm. Validates fully, then loads the
\ merges through the engine and builds the internal->real id map. A rejected call throws
\ before mutating (bad vocab data) so a previously loaded vocab stays intact and ready.
: BPR-INSTALL ( ptr a ptr a ptr a ptr a n -- )  {: bid:ptr ma:ptr mb:ptr mid:ptr nm:n :}
   nm 0 <  nm BPR-MAX >  or if E-BPR-VOCAB throw then
   bid BPR-BID-OK?
   mid bid nm BPR-MID-OK?
   0 BPR-DONE !
   BPE-BEGIN
   nm 0 ?do  ma i cells + @  mb i cells + @  BPE-MERGE+  loop
   BPE-SEAL
   BPR-BYTE-N 0 ?do  bid i cells + @  BPR-GID i cells + !  loop
   nm 0 ?do  mid i cells + @  BPR-GID BPR-BYTE-N i + cells + !  loop
   BPR-BYTE-N nm + BPR-GN !
   1 BPR-DONE ! ;

\ ---- ready-gated queries ---------------------------------------------------------
: BPR-VOCAB  ( -- n )  BPR-READY BPR-VMAX ;          \ 50257
: BPR-EOT    ( -- n )  BPR-READY BPR-VMAX 1- ;        \ 50256 <|endoftext|>
: BPR-MERGES ( -- n )  BPR-READY BPR-GN @ BPR-BYTE-N - ;

\ ---- encode: engine (byte-level) then internal -> real id translation -------------
\ Reuses BPE-ENCODE's staged single write (so a cap/length rejection leaves d untouched);
\ then rewrites each staged internal id in place to its real id.
: BPR-ENCODE ( ptr u8 n ptr a n -- n )  {: a:ptr u:n d:ptr cap:n :}
   BPR-READY
   a u d cap BPE-ENCODE {: m:n :}
   m 0 ?do  d i T-GET f>s BPR-GID-AT s>f  d i T-SET  loop
   m ;

\ ---- decode: real -> internal into scratch (validated), then engine expansion ------
\ Every real id is validated and mapped to an internal id in scratch BEFORE any caller
\ write; the engine's BPE-DECODE then re-validates and does its own single caller write,
\ so a rejected id (out of range, non-integral, or absent from the map) writes no byte.
: BPR-DECODE ( ptr a n ptr u8 n -- n )  {: ids:ptr u:n d:ptr cap:n :}
   BPR-READY
   u 0 < if E-BPR-CAP throw then
   u BPR-INT-CAP > if E-BPR-CAP throw then
   u 0 ?do  ids i T-GET BPR-CELL>REAL BPR-REAL>INT s>f  BPR-INT i T-SET  loop
   BPR-INT u d cap BPE-DECODE ;

;package
