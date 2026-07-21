\ maki/bpe.f - GPT-2 byte-level BPE tokenizer (dot habu-bpe-tokenizer-gpt-37d7f243).
\
\ GPT-2's scheme: a 256 base-byte vocab + learned merges + one special token
\ (<|endoftext|>). Every input is a UTF-8 byte stream, so every byte encodes and
\ the round-trip decode(encode(x))==x holds for ARBITRARY bytes, including invalid
\ UTF-8 - decode only ever expands a token back to its exact constituent bytes.
\ A merge is a rank-ordered pair (a,b); the merged token gets id 256+rank. Ranks
\ are priority order: rank 0 is applied first (lowest rank = highest priority).
\
\ Pipeline: pre-split the text into word-ish chunks, then greedily merge WITHIN each
\ chunk (GPT-2's bpe(): repeatedly take the lowest-rank adjacent pair present, merge
\ every non-overlapping occurrence left-to-right, recompute, until none remain).
\
\ PRE-SPLIT DECISION (honest byte-level simplification, documented - no silent
\ approximation). GPT-2's pattern is
\   's|'t|'re|'ve|'m|'ll|'d| ?\p{L}+| ?\p{N}+| ?[^\s\p{L}\p{N}]+|\s+(?!\S)|\s+
\ No regex engine exists in-tree, so BPE-CHUNK-LEN is a hand-rolled matcher for
\ exactly that pattern with \p{L}->ASCII [A-Za-z], \p{N}->[0-9], \s->[ \t\n\v\f\r].
\ It reproduces the regex's leftmost-alternative + greedy + whitespace-backtracking
\ semantics EXACTLY on ASCII (verified in maki/bpe-test.f against Python `re` with
\ the same pattern - see maki/bpe-data.f provenance). DIVERGENCE CLASS: bytes >=0x80
\ (UTF-8 lead/continuation) are Unicode letters/digits under \p{L}/\p{N} but fall in
\ this matcher's "other" class, so non-ASCII text chunks differently; pure-ASCII
\ input matches real GPT-2 exactly (measured zero divergence on the ASCII fixtures).
\ The round-trip property is independent of the split: the matcher is a total
\ partition (every byte lands in exactly one chunk), so decode reconstructs all bytes.
\
\ TRAINING (from-scratch merges). BPE-TRAIN learns ranks from a corpus, DETERMINISTIC:
\ each round counts adjacent pairs within chunks and picks the most frequent, TIE-BROKEN
\ by smallest a then smallest b (fixed rule); it stops at k or when no pair repeats
\ (count<2). maki/bpe-data.f commits a bounded corpus and the locked table it yields.
\
\ STORAGE. The synthetic tables here are tiny (a few hundred bytes) so they are
\ COMMITTED fixtures (maki/bpe-data.f), measured well within the DATA budget. The full
\ GPT-2 50257-vocab is ~MB-scale and would be a runtime-loaded checked artifact (as
\ maki/data-loader.f reads the corpus) - deferred to the tiktoken-parity follow-up.
\
\ Safety discipline (maki/tokenizer.f fe907212): every public word is readiness-gated
\ and domain-validated before it touches a table/loop, named E-codes reject each domain,
\ and encode/decode stage into module scratch so a rejected call writes no caller byte.
\ Id buffers are float cells (one id per cell, s>f) - the maki/array.f T-SET/T-GET
\ representation the embedding + batch buffers use. maki -> habu only. bpe owns -5322..-5327.

require maki/array.f
require lib/hashmap.f     \ HM:PROBE/HM:CLEAR pair->rank open-addressing lookup (O(1) vs the old linear scan)

-5322 constant E-BPE-EMPTY    \ public op before a table is ready (no BPE-SEAL / BPE-TRAIN yet)
-5323 constant E-BPE-RANGE    \ id/byte outside its domain: decode cell not a finite exact id in [0,256+merges), or the special/out-of-range id in a byte decode
-5324 constant E-BPE-CAP      \ invalid length/capacity: negative len/cap, caller dst too small, or a chunk/stream/output past a scratch bound
-5325 constant E-BPE-MERGE    \ malformed merge: a child id not an existing token, or a duplicate pair
-5326 constant E-BPE-STATE    \ build word out of order (BPE-MERGE+/BPE-SEAL without a live BPE-BEGIN)
-5327 constant E-BPE-CORPUS   \ training input invalid: empty corpus, corpus past the bound, or negative merge target

package MAKI
private

256  constant BPE-BYTE-N       \ base byte tokens: ids 0..255
50000 constant BPE-MAX-MERGE   \ merge-table capacity: the full GPT-2 table (merged token id = 256+rank)
1024 constant BPE-CHUNK-CAP     \ max bytes in one pre-split chunk (encode work buffer)
4096 constant BPE-OUT-CAP       \ max token ids one BPE-ENCODE stages (>= input bytes)
8192 constant BPE-DEC-CAP       \ max bytes one BPE-DECODE stages
4096 constant BPE-CORPUS-CAP    \ max training-corpus bytes
4096 constant BPE-PC-CAP        \ pair-count scratch rows during a training round

create BPE-A BPE-MAX-MERGE cells allot   \ rank -> child a id
create BPE-B BPE-MAX-MERGE cells allot   \ rank -> child b id

\ pair -> rank lookup: an open-addressing hashmap (lib/hashmap.f) keyed by the pair
\ (a,b). The child ids a,b are internal ids in [0,BPE-BYTE-N+BPE-N), so the pair packs
\ into one integer key a*BPE-KSTRIDE+b, then multiplied by an odd constant (a bijection
\ mod 2^64, so distinct pairs keep distinct keys) to spread the low bits the probe masks.
\ This replaces the old O(BPE-N) linear scan, making encode O(1)/pair even at 50000 merges.
BPE-BYTE-N BPE-MAX-MERGE + constant BPE-KSTRIDE   \ id bound: every internal id < KSTRIDE
131072 constant BPE-HCAP                          \ probe slots: power of two > 1.4*BPE-MAX-MERGE
$9E3779B97F4A7C15 constant BPE-HMIX               \ odd multiplier (golden ratio); key bijection
create BPE-HK BPE-HCAP cells allot   \ slot -> packed pair key
create BPE-HU BPE-HCAP cells allot   \ slot -> used flag (0 = empty)
create BPE-HV BPE-HCAP cells allot   \ slot -> rank
variable BPE-N                            \ merges in the table (ranks 0..N-1)
variable BPE-BUILDING                     \ 1 between BPE-BEGIN and BPE-SEAL
variable BPE-DONE                         \ 1 once a table is sealed/trained (ready)

create BPE-WORK   BPE-CHUNK-CAP cells allot   \ encode: per-chunk symbol sequence
create BPE-OUTBUF BPE-OUT-CAP   cells allot   \ encode: staged output ids (int)
create BPE-DECBUF BPE-DEC-CAP        allot    \ decode: staged output bytes
variable BPE-WLEN     \ live length of BPE-WORK
variable BPE-BEST     \ min rank found across adjacent pairs (-1 = none)
variable BPE-WI       \ merge rebuild: write index
variable BPE-RI       \ merge rebuild: read index
variable BPE-POS      \ pre-split scan position
variable BPE-OUT      \ encode: staged token count
variable BPE-DEC#     \ decode: staged byte count

create BPE-TR-S    BPE-CORPUS-CAP cells allot \ training symbol stream
create BPE-TR-SAME BPE-CORPUS-CAP allot       \ 1 iff S[i],S[i+1] share a chunk
variable BPE-TR-M                              \ live stream length
create BPE-PC-A BPE-PC-CAP cells allot         \ pair-count round: a
create BPE-PC-B BPE-PC-CAP cells allot         \ pair-count round: b
create BPE-PC-C BPE-PC-CAP cells allot         \ pair-count round: count
variable BPE-PC-N                              \ live pair-count rows
variable BPE-BEST-C variable BPE-BEST-A variable BPE-BEST-B  \ selected pair
variable BPE-KI variable BPE-STOP              \ training loop control

\ ---- readiness / build-state gates -----------------------------------------------
: BPE-READY  ( -- )  BPE-DONE @ 0= if E-BPE-EMPTY throw then ;
: BPE-BUILD? ( -- )  BPE-BUILDING @ 0= if E-BPE-STATE throw then ;

\ engine-layer bool false (lib/prelude.f `false`, unavailable to this layer)
: BPE-FALSE ( -- bool )  0 0= 0= ;

\ ---- character classes (ASCII simplification of \p{L} \p{N} \s) -------------------
: BPE-WS?  ( n -- bool )  {: b:n :}
   b 32 =  b 9 =  or  b 10 =  or  b 11 =  or  b 12 =  or  b 13 =  or ;
: BPE-LET? ( n -- bool )  {: b:n :}
   b 65 >= b 90 <= and  b 97 >= b 122 <= and  or ;
: BPE-DIG? ( n -- bool )  {: b:n :}  b 48 >= b 57 <= and ;
: BPE-OTHER? ( n -- bool )  {: b:n :}
   b BPE-WS?  b BPE-LET?  or  b BPE-DIG?  or  0= ;

\ ---- maximal class runs from p (len>=0; caller guarantees the class at p) ---------
: BPE-LET-RUN ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   0 begin dup p + n < if a p + over + c@ BPE-LET? else BPE-FALSE then while 1+ repeat ;
: BPE-DIG-RUN ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   0 begin dup p + n < if a p + over + c@ BPE-DIG? else BPE-FALSE then while 1+ repeat ;
: BPE-OTHER-RUN ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   0 begin dup p + n < if a p + over + c@ BPE-OTHER? else BPE-FALSE then while 1+ repeat ;
: BPE-WS-RUN ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   0 begin dup p + n < if a p + over + c@ BPE-WS? else BPE-FALSE then while 1+ repeat ;

\ contraction at p (a[p] is '): length (2 or 3) for 's 't 're 've 'm 'll 'd, else 0.
: BPE-CONTRACT ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   p 1+ n >= if 0 exit then
   a p 1+ + c@ {: c1:n :}
   p 2 + n < if
      c1 [char] r = a p 2 + + c@ [char] e = and if 3 exit then
      c1 [char] v = a p 2 + + c@ [char] e = and if 3 exit then
      c1 [char] l = a p 2 + + c@ [char] l = and if 3 exit then
   then
   c1 [char] s = if 2 exit then
   c1 [char] t = if 2 exit then
   c1 [char] m = if 2 exit then
   c1 [char] d = if 2 exit then
   0 ;

public

\ Length of the pre-split chunk starting at p (caller guarantees 0<=p<n; len>=1).
\ Rule order mirrors the regex alternation, with the ` ?` (literal space) leading only
\ the letter/digit/other alternatives and the \s runs handled by the whitespace tail.
: BPE-CHUNK-LEN ( ptr u8 n n -- n )  {: a:ptr n:n p:n :}
   a p + c@ {: b:n :}
   b 39 = if  a n p BPE-CONTRACT dup 0 > if exit then drop  then
   b BPE-LET? if a n p BPE-LET-RUN exit then
   b BPE-DIG? if a n p BPE-DIG-RUN exit then
   b 32 = if
      p 1+ n < if
         a p 1+ + c@ BPE-LET?   if 1 a n p 1+ BPE-LET-RUN   + exit then
         a p 1+ + c@ BPE-DIG?   if 1 a n p 1+ BPE-DIG-RUN   + exit then
         a p 1+ + c@ BPE-OTHER? if 1 a n p 1+ BPE-OTHER-RUN + exit then
      then
   then
   b BPE-OTHER? if a n p BPE-OTHER-RUN exit then
   a n p BPE-WS-RUN {: wl:n :}                \ b is whitespace here; wl>=1
   p wl + n >= if wl exit then                \ run reaches EOS -> whole run
   wl 2 >= if wl 1- exit then                 \ leave the final ws char for the next chunk
   1 ;

private

\ packed, spread key for the pair (a,b): a*KSTRIDE+b is a unique integer id for the pair
\ (b<KSTRIDE), and *BPE-HMIX is a bijection mod 2^64 so the key stays unique while its low
\ bits (the ones HM:PROBE masks) mix well
: BPE-HKEY ( n n -- n )  {: a:n b:n :}  a BPE-KSTRIDE * b +  BPE-HMIX * ;
: BPE-HCLEAR ( -- )  BPE-HU BPE-HCAP HM:CLEAR ;
\ record rank rk for pair (a,b) in the hashmap (caller guarantees the pair is new)
: BPE-HPUT ( n n n -- )  {: a:n b:n rk:n :}
   BPE-HK BPE-HU BPE-HCAP  a b BPE-HKEY  HM:PROBE {: s:n :}
   a b BPE-HKEY  BPE-HK s cells + !
   1            BPE-HU s cells + !
   rk           BPE-HV s cells + ! ;

\ rank of adjacent pair (a,b) in the table, or -1 (O(1) hashmap probe; ranks are priority order)
: BPE-RANK ( n n -- n )  {: a:n b:n :}
   BPE-HK BPE-HU BPE-HCAP  a b BPE-HKEY  HM:PROBE {: s:n :}
   BPE-HU s cells + @ 0= if -1 exit then
   BPE-HV s cells + @ ;

\ load one chunk's bytes into BPE-WORK as byte-ids
: BPE-CHUNK>WORK ( ptr u8 n n -- )  {: a:ptr s:n L:n :}
   L BPE-CHUNK-CAP > if E-BPE-CAP throw then
   L 0 ?do  a s + i + c@  BPE-WORK i cells + !  loop
   L BPE-WLEN ! ;

\ lowest rank among adjacent pairs of BPE-WORK, or -1
: BPE-MIN-RANK ( -- n )
   -1 BPE-BEST !
   BPE-WLEN @ 1- 0 ?do
      BPE-WORK i cells + @  BPE-WORK i 1+ cells + @  BPE-RANK {: r:n :}
      r 0 >= if
         BPE-BEST @ 0 <  r BPE-BEST @ <  or if r BPE-BEST ! then
      then
   loop  BPE-BEST @ ;

\ merge every non-overlapping (A[rk],B[rk]) pair in BPE-WORK, left to right
: BPE-APPLY ( n -- )  {: rk:n :}
   BPE-A rk cells + @ {: fa:n :}
   BPE-B rk cells + @ {: fb:n :}
   BPE-BYTE-N rk + {: nid:n :}
   0 BPE-WI !  0 BPE-RI !
   begin BPE-RI @ BPE-WLEN @ < while
      BPE-RI @ BPE-WLEN @ 1- <
      BPE-WORK BPE-RI @ cells + @ fa =  and
      BPE-WORK BPE-RI @ 1+ cells + @ fb =  and
      if  nid BPE-WORK BPE-WI @ cells + !  BPE-RI @ 2 + BPE-RI !
      else  BPE-WORK BPE-RI @ cells + @  BPE-WORK BPE-WI @ cells + !  BPE-RI @ 1+ BPE-RI !
      then
      BPE-WI @ 1+ BPE-WI !
   repeat
   BPE-WI @ BPE-WLEN ! ;

\ GPT-2 bpe(): merge to a fixpoint over BPE-WORK
: BPE-MERGE-WORK ( -- )
   begin
      BPE-WLEN @ 2 < if exit then
      BPE-MIN-RANK dup 0 < if drop exit then
      BPE-APPLY
   again ;

\ append token id's byte expansion into d (cap) at idx; return new idx (recurses on children)
: BPE-EMIT ( n ptr u8 n n -- n )  {: id:n d:ptr cap:n ix:n :}
   id BPE-BYTE-N < if
      ix cap >= if E-BPE-CAP throw then
      id d ix + c!  ix 1+ exit
   then
   id BPE-BYTE-N - {: rk:n :}
   rk 0 <  rk BPE-N @ >=  or if E-BPE-RANGE throw then
   BPE-A rk cells + @  d cap ix  recurse {: ix1:n :}
   BPE-B rk cells + @  d cap ix1 recurse ;

\ validate a stored float cell as an exact byte-bearing id in [0,256+N): NaN/inf/
\ fractional/negative/>=256+N (incl. the special <|endoftext|>) are rejected
: BPE-CELL>ID ( r -- n )  {: v:r :}
   v f0< if E-BPE-RANGE throw then
   v  BPE-BYTE-N BPE-N @ +  s>f  f< 0= if E-BPE-RANGE throw then
   v f>s {: k:n :}
   k s>f v f= 0= if E-BPE-RANGE throw then
   k ;

public

\ ---- table build (fixture path): BPE-BEGIN, BPE-MERGE+*, BPE-SEAL ----------------
: BPE-BEGIN ( -- )  0 BPE-N !  1 BPE-BUILDING !  0 BPE-DONE !  BPE-HCLEAR ;

\ append merge (a,b): both must already be tokens (0<=id<256+rank), pair unique
: BPE-MERGE+ ( n n -- )  {: a:n b:n :}
   BPE-BUILD?
   BPE-BYTE-N BPE-N @ + {: nx:n :}
   a 0 <  a nx >=  or  b 0 <  b nx >=  or  or if E-BPE-MERGE throw then
   a b BPE-RANK 0 >= if E-BPE-MERGE throw then
   BPE-N @ BPE-MAX-MERGE >= if E-BPE-CAP throw then
   a BPE-A BPE-N @ cells + !
   b BPE-B BPE-N @ cells + !
   a b BPE-N @ BPE-HPUT
   BPE-N @ 1+ BPE-N ! ;

: BPE-SEAL ( -- )  BPE-BUILD?  0 BPE-BUILDING !  1 BPE-DONE ! ;

\ ---- table queries (ready-gated) -------------------------------------------------
: BPE-MERGES ( -- n )  BPE-READY BPE-N @ ;
: BPE-VOCAB  ( -- n )  BPE-READY BPE-BYTE-N BPE-N @ + 1+ ;   \ 256 bytes + N merges + 1 special
: BPE-EOT    ( -- n )  BPE-READY BPE-BYTE-N BPE-N @ + ;      \ <|endoftext|>: top id, carries no bytes
: BPE-MERGE-A ( n -- n )  {: r:n :}
   BPE-READY  r 0 <  r BPE-N @ >=  or if E-BPE-RANGE throw then  BPE-A r cells + @ ;
: BPE-MERGE-B ( n -- n )  {: r:n :}
   BPE-READY  r 0 <  r BPE-N @ >=  or if E-BPE-RANGE throw then  BPE-B r cells + @ ;

\ ---- encode: pre-split, merge per chunk, stage ids, then one caller write ---------
: BPE-ENCODE ( ptr u8 n ptr a n -- n )  {: a:ptr u:n d:ptr cap:n :}
   BPE-READY
   u 0 < if E-BPE-CAP throw then
   cap 0 < if E-BPE-CAP throw then
   u BPE-OUT-CAP > if E-BPE-CAP throw then
   0 BPE-OUT !  0 BPE-POS !
   begin BPE-POS @ u < while
      a u BPE-POS @ BPE-CHUNK-LEN {: cl:n :}
      a BPE-POS @ cl BPE-CHUNK>WORK
      BPE-MERGE-WORK
      BPE-WLEN @ 0 ?do
         BPE-WORK i cells + @  BPE-OUTBUF BPE-OUT @ i + cells + !
      loop
      BPE-OUT @ BPE-WLEN @ + BPE-OUT !
      BPE-POS @ cl + BPE-POS !
   repeat
   BPE-OUT @ cap > if E-BPE-CAP throw then           \ caller dst still untouched
   BPE-OUT @ 0 ?do  BPE-OUTBUF i cells + @ s>f  d i T-SET  loop
   BPE-OUT @ ;

\ ---- decode: validate + expand into scratch, then one caller write ----------------
: BPE-DECODE ( ptr a n ptr u8 n -- n )  {: ids:ptr u:n d:ptr cap:n :}
   BPE-READY
   u 0 < if E-BPE-CAP throw then
   cap 0 < if E-BPE-CAP throw then
   0 BPE-DEC# !
   u 0 ?do
      ids i T-GET BPE-CELL>ID  BPE-DECBUF BPE-DEC-CAP BPE-DEC# @ BPE-EMIT  BPE-DEC# !
   loop
   BPE-DEC# @ cap > if E-BPE-CAP throw then          \ caller dst still untouched
   BPE-DEC# @ 0 ?do  BPE-DECBUF i + c@  d i + c!  loop
   BPE-DEC# @ ;

private

\ ---- training over a corpus ------------------------------------------------------
\ build the initial symbol stream (bytes) and the same-chunk flags from the pre-split
: BPE-TR-INIT ( ptr u8 n -- )  {: a:ptr u:n :}
   u 0 ?do  a i + c@ BPE-TR-S i cells + !  1 BPE-TR-SAME i + c!  loop
   u BPE-TR-M !
   0 BPE-POS !
   begin BPE-POS @ u < while
      a u BPE-POS @ BPE-CHUNK-LEN {: cl:n :}
      0 BPE-TR-SAME BPE-POS @ cl + 1- + c!          \ chunk's last index: no cross-chunk pair
      BPE-POS @ cl + BPE-POS !
   repeat ;

: BPE-PC-FIND ( n n -- n )  {: a:n b:n :}
   BPE-PC-N @ 0 ?do
      BPE-PC-A i cells + @ a =  BPE-PC-B i cells + @ b =  and if i unloop exit then
   loop -1 ;

: BPE-PC-BUMP ( n n -- )  {: a:n b:n :}
   a b BPE-PC-FIND {: ix:n :}
   ix 0 >= if  BPE-PC-C ix cells + @ 1+ BPE-PC-C ix cells + !  exit  then
   BPE-PC-N @ BPE-PC-CAP >= if E-BPE-CAP throw then
   a BPE-PC-A BPE-PC-N @ cells + !
   b BPE-PC-B BPE-PC-N @ cells + !
   1 BPE-PC-C BPE-PC-N @ cells + !
   BPE-PC-N @ 1+ BPE-PC-N ! ;

\ count adjacent within-chunk pairs across the whole stream
: BPE-TR-COUNT ( -- )
   0 BPE-PC-N !
   BPE-TR-M @ 1- 0 ?do
      BPE-TR-SAME i + c@ 1 = if
         BPE-TR-S i cells + @  BPE-TR-S i 1+ cells + @  BPE-PC-BUMP
      then
   loop ;

\ select the pair with max count>=2; tie-break smallest a then smallest b. 1 if found.
: BPE-TR-BEST ( -- n )
   -1 BPE-BEST-C !  0 BPE-BEST-A !  0 BPE-BEST-B !
   BPE-PC-N @ 0 ?do
      BPE-PC-C i cells + @ {: c:n :}
      c 2 >= if
         BPE-PC-A i cells + @ {: pa:n :}
         BPE-PC-B i cells + @ {: pb:n :}
         c BPE-BEST-C @ >
         c BPE-BEST-C @ =  pa BPE-BEST-A @ <  and  or
         c BPE-BEST-C @ =  pa BPE-BEST-A @ =  and  pb BPE-BEST-B @ <  and  or
         if  c BPE-BEST-C !  pa BPE-BEST-A !  pb BPE-BEST-B !  then
      then
   loop
   BPE-BEST-C @ 0 < if 0 exit then  1 ;

\ apply the selected pair over the stream (in place) and record it as the next rank
: BPE-TR-APPLY ( -- )
   BPE-BEST-A @ {: fa:n :}
   BPE-BEST-B @ {: fb:n :}
   BPE-BYTE-N BPE-N @ + {: nid:n :}
   0 BPE-WI !  0 BPE-RI !
   begin BPE-RI @ BPE-TR-M @ < while
      BPE-RI @ BPE-TR-M @ 1- <
      BPE-TR-SAME BPE-RI @ + c@ 1 =  and
      BPE-TR-S BPE-RI @ cells + @ fa =  and
      BPE-TR-S BPE-RI @ 1+ cells + @ fb =  and
      if
         nid BPE-TR-S BPE-WI @ cells + !
         BPE-TR-SAME BPE-RI @ 1+ + c@  BPE-TR-SAME BPE-WI @ + c!
         BPE-RI @ 2 + BPE-RI !
      else
         BPE-TR-S BPE-RI @ cells + @  BPE-TR-S BPE-WI @ cells + !
         BPE-TR-SAME BPE-RI @ + c@  BPE-TR-SAME BPE-WI @ + c!
         BPE-RI @ 1+ BPE-RI !
      then
      BPE-WI @ 1+ BPE-WI !
   repeat
   BPE-WI @ BPE-TR-M !
   fa BPE-A BPE-N @ cells + !
   fb BPE-B BPE-N @ cells + !
   fa fb BPE-N @ BPE-HPUT
   BPE-N @ 1+ BPE-N ! ;

public

\ Learn up to k merges from the corpus, deterministically, and seal the table.
\ Returns the merge count (< k when the corpus runs out of repeating pairs).
: BPE-TRAIN ( ptr u8 n n -- n )  {: a:ptr u:n k:n :}
   u 0 <= if E-BPE-CORPUS throw then
   u BPE-CORPUS-CAP > if E-BPE-CORPUS throw then
   k 0 < if E-BPE-CORPUS throw then
   k BPE-MAX-MERGE > if E-BPE-CAP throw then
   a u BPE-TR-INIT
   0 BPE-N !  0 BPE-KI !  0 BPE-STOP !  0 BPE-BUILDING !  BPE-HCLEAR
   begin BPE-KI @ k <  BPE-STOP @ 0=  and while
      BPE-TR-COUNT
      BPE-TR-BEST 0= if  1 BPE-STOP !
      else  BPE-TR-APPLY  BPE-KI @ 1+ BPE-KI !  then
   repeat
   1 BPE-DONE !
   BPE-N @ ;

;package
