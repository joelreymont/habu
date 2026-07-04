\ maki/sched-key.f - schedule cache/replay keys + the cad-5 replay-table seam (cad-4).
\
\ CAD-PLAN section 7.4. The cache and replay key over a fusion region is
\ (region signature, shape class, dtype key, layout key, alignment class, target,
\ engine hash, ptxas version). This file renders that whole key as one string and
\ owns the in-memory key->selection replay table. The families/defaults are
\ maki/schedule.f; the TILE/TUNE wiring is maki/cad.f. One concern: keys + replay.
\
\ Region signature: an FNV-1a 64-bit content hash over the region's node facts
\ (op kind, rows, cols, dtype, layout) in node order, rendered as 16 hex digits.
\ lib/content-key.f's SHA256 keys are a file-content cache (fs paths, mtime, mmap)
\ that is not loadable as an in-memory region hash here, and lib/map.f's hash is a
\ tag-newtype-wrapped map internal; a small documented FNV-1a over the region's node
\ facts is the self-contained content hash. (The separate engine field carries the
\ real SHA-256 content key over bin/hb via lib/engine-id.f; the region signature
\ itself stays an in-memory FNV-1a.)
\
\ Shape class (section 7.4): each extent <= 64 is rendered exactly; a larger extent
\ becomes a power-of-two bucket plus a tail flag ("p128+t" when it is not itself a
\ power of two, "p128" when it is); an unbound extent (0) renders "?".
\
\ Alignment class: the most conservative model-input alignment the region reads
\ (AL-16 when it reads no model input - compiler-allocated buffers are aligned by
\ construction). Target is the "sm_87" v1 constant; the engine hash is the real
\ SHA-256 content key over bin/hb (lib/engine-id.f, resolved engine-side, lazy +
\ cached); ptxas version is the honest "unprobed" placeholder (no ptxas is probed
\ on a host without a device).
\
\ Replay: a bounded in-memory key->selection table with GET/PUT. This is the cad-5
\ store SEAM - a query that misses returns (-1 false) so the caller falls back to the
\ closed-form defaults ("unmeasured shape class -> using defaults"), since cad-4 has no
\ measurements (those land in cad-5/cad-6).
\
\ Fail closed: an out-of-range region id or alignment class and a table/arena overflow
\ are named throws. maki -> habu only; sched-key owns -5084..-5086.

require lib/prelude.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/engine-id.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/schedule.f

-5084 constant E-SK-REGION     \ region id out of range / empty
-5085 constant E-SK-ALIGN      \ alignment class out of range (AL-* domain)
-5086 constant E-SK-FULL       \ replay table / key arena capacity exceeded

package MAKI
private

\ ---- FNV-1a 64-bit content hash over the region's node facts ----------------
$cbf29ce484222325 constant FNV-BASIS
$100000001b3       constant FNV-PRIME
variable SK-FOLD               \ scratch for little-endian byte decomposition

: FNV-BYTE ( n n -- n )  xor FNV-PRIME * ;      \ h byte -> h' (64-bit wrap)

: FNV-CELL ( n n -- n ) {: h:n v:n :}           \ fold one integer fact (8 LE bytes)
   v SK-FOLD !
   h
   8 0 ?do  SK-FOLD @ $FF and FNV-BYTE  SK-FOLD @ 8 rshift SK-FOLD !  loop ;

: RSIG-NODE ( n n -- n ) {: node:n :}           \ ( h node -- h' ) fold a node's facts
   node MIR-OP@   FNV-CELL
   node MIR-ROWS@ FNV-CELL
   node MIR-COLS@ FNV-CELL
   node MIR-DT@   FNV-CELL
   node MIR-LAY@  FNV-CELL ;

: RSIG ( n -- n ) {: r:n :}                     \ region -> content hash (nodes in order)
   FNV-BASIS
   MIR-N@ 0 ?do  i FP-RID@ r = if i RSIG-NODE then  loop ;

\ ---- hex render (16 digits, MSB first) into the shared builder --------------
: HEX-NIB ( n -- n )  $F and dup 10 < if $30 + else $37 + then ;
: SK-HEX+ ( n -- ) {: v:n :}
   16 0 ?do  v  15 i - 4 * rshift HEX-NIB SB-APPEND-C  loop ;

\ ---- shape class (exact <= 64, else pow2 bucket + tail flag, ? for unbound) --
: DIM-CLASS+ ( n -- ) {: e:n :}
   e 0= if s" ?" SB-APPEND exit then
   e 64 <= if e SB-INT exit then
   $70 SB-APPEND-C  e NEXT-POW2 SB-INT
   e POW2? 0= if s" +t" SB-APPEND then ;

: SHAPE-CLASS+ ( n n -- ) {: rows:n cols:n :}
   rows DIM-CLASS+  $78 SB-APPEND-C  cols DIM-CLASS+ ;

\ ---- alignment class over the region's model-input reads --------------------
: NODE-ALIGN ( n n -- n ) {: node:n :}          \ ( al node -- al' ) min input-slot alignment
   node MIR-IN-COUNT@ 0 ?do
      node i MIR-IN@ dup MIR-REF-INPUT?
      if MIR-REF-SLOT MIR-SLOT-AL@ min else drop then
   loop ;

: REGION-ALIGN ( n -- n ) {: r:n :}
   AL-16
   MIR-N@ 0 ?do  i FP-RID@ r = if i NODE-ALIGN then  loop ;

: AL-KEY ( n -- ptr u8 n )
   case
      AL-UNKNOWN of s" al?"  endof
      AL-BYTE    of s" al1"  endof
      AL-4       of s" al4"  endof
      AL-8       of s" al8"  endof
      AL-16      of s" al16" endof
      E-SK-ALIGN throw
   endcase ;

\ ---- region validation + representative (output) node -----------------------
: SK-REGION-CK ( n -- n )
   dup 0 < over FP-REGION-COUNT >= or if E-SK-REGION throw then ;

: REGION-REP ( n -- n ) {: r:n :}               \ last (output) node in the region
   -1  MIR-N@ 0 ?do  i FP-RID@ r = if drop i then  loop
   dup 0 < if E-SK-REGION throw then ;

public

\ ---- key field placeholders -------------------------------------------------
: SK-TARGET$ ( -- ptr u8 n )  s" sm_87" ;         \ single supported target (v1)
\ Real engine content key: the SHA-256 of bin/hb, resolved engine-side from the
\ kernel-provided self-path and hashed once on first request, then cached
\ (lib/engine-id.f). It distinguishes schedules produced by different engine builds
\ so a schedules.rows written by one engine is never replayed under another; the
\ lazy+cached hash keeps it off the interactive key-render hot path.
: SK-ENGINE$ ( -- ptr u8 n )  ENGINE-KEY$ ;
: SK-PTXAS$  ( -- ptr u8 n )  s" unprobed" ;       \ no ptxas probed off-device

\ representative (output) node of a region - the default-context source (rowlen/dtype)
: SK-REGION-REP ( n -- n )  SK-REGION-CK REGION-REP ;

\ ---- individual key fields (standalone renders, for inspection + tests) ------
: SK-RSIG$ ( n -- ptr u8 n )  SK-REGION-CK RSIG SB-RESET SK-HEX+ SB$ ;
: SK-SHAPE-CLASS$ ( n n -- ptr u8 n )  SB-RESET SHAPE-CLASS+ SB$ ;   \ rows cols -> class
: SK-ALIGN$ ( n -- ptr u8 n )  SK-REGION-CK REGION-ALIGN AL-KEY ;

\ ---- the full section 7.4 key as one "|"-joined string ----------------------
: SK-KEY+ ( n -- ) {: r:n :}                     \ append the key to SB (already reset)
   r REGION-REP {: rep:n :}
   r RSIG SK-HEX+
   $7C SB-APPEND-C  rep MIR-ROWS@ rep MIR-COLS@ SHAPE-CLASS+
   $7C SB-APPEND-C  rep MIR-DTYPE-KEY  SB-APPEND
   $7C SB-APPEND-C  rep MIR-LAYOUT-KEY SB-APPEND
   $7C SB-APPEND-C  r REGION-ALIGN AL-KEY SB-APPEND
   $7C SB-APPEND-C  SK-TARGET$ SB-APPEND
   $7C SB-APPEND-C  SK-ENGINE$ SB-APPEND
   $7C SB-APPEND-C  SK-PTXAS$  SB-APPEND ;

: SK-KEY$ ( n -- ptr u8 n )  SK-REGION-CK SB-RESET SK-KEY+ SB$ ;

private

\ ---- replay table (cad-5 store seam: in-memory key -> selection) -------------
32   constant SK-TAB-CAP
$1000 constant SK-ARENA-CAP
create SK-ARENA SK-ARENA-CAP allot   variable SK-ARENA-U
create SK-KO  SK-TAB-CAP cells allot     \ per-entry key offset
create SK-KL  SK-TAB-CAP cells allot     \ per-entry key length
create SK-SEL SK-TAB-CAP cells allot     \ per-entry selection (candidate index)
variable SK-TAB-N

: SK-INTERN ( ptr u8 n -- n n ) {: a:ptr u:n :}
   SK-ARENA-U @ u + SK-ARENA-CAP > if E-SK-FULL throw then
   SK-ARENA-U @ {: off:n :}
   a  SK-ARENA off +  u BYTE-COPY
   off u + SK-ARENA-U !
   off u ;

: SK-ENTRY$ ( n -- ptr u8 n ) {: i:n :}
   SK-ARENA i cells SK-KO + @ +  i cells SK-KL + @ ;

: SK-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}      \ key -> entry index or -1
   SK-TAB-N @ 0 ?do  a u i SK-ENTRY$ STR= if i unloop exit then  loop  -1 ;

public

: SK-TAB-RESET ( -- )  0 SK-ARENA-U !  0 SK-TAB-N ! ;
: SK-TAB-COUNT ( -- n )  SK-TAB-N @ ;

: SK-PUT ( ptr u8 n n -- ) {: a:ptr u:n sel:n :}  \ key selection -> store / update
   a u SK-FIND {: e:n :}
   e 0 < 0= if sel e cells SK-SEL + ! exit then   \ update existing key
   SK-TAB-N @ SK-TAB-CAP >= if E-SK-FULL throw then
   a u SK-INTERN {: off:n len:n :}
   off SK-TAB-N @ cells SK-KO  + !
   len SK-TAB-N @ cells SK-KL  + !
   sel SK-TAB-N @ cells SK-SEL + !
   SK-TAB-N @ 1+ SK-TAB-N ! ;

\ cad-5 store seam: a miss returns (-1 false) so the caller uses the defaults.
: SK-GET ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   a u SK-FIND {: e:n :}
   e 0 < if -1 false exit then
   e cells SK-SEL + @  true ;

end-package
