\ maki/tokenizer.f - v0 character tokenizer for the text data pipeline (dot
\ habu-tiny-shakespeare-char-125d9684). Builds a byte-level vocab from a corpus
\ (the DISTINCT bytes, sorted ascending), encodes text->token ids, and decodes
\ token ids->text with an exact round-trip. Char granularity: every byte 0..255 is
\ a candidate token; the vocab is the set that actually occurs, indexed 0..N-1 in
\ ascending byte order, so the mapping is SORTED and STABLE by construction (it is
\ built by scanning the alphabet 0..255 up, independent of the corpus byte order).
\
\ Id buffer contract (feeds maki/embedding.f EMB-GATHER): one token id per CELL,
\ stored as its FLOAT representation (s>f). EMB-GATHER reads ids back with
\ `T-GET f>s`, so ids are integer-VALUED but held in float cells, one cell wide -
\ the same representation the (B,T) window buffers use (maki/batch-loader.f BL-IDS).
\ Pure module: no filesystem dependency; the corpus bytes are supplied by the
\ caller (maki/data-loader.f reads them from a file). maki -> habu only.
\ tokenizer owns -5152..-5154.

require maki/array.f

-5152 constant E-TOK-EMPTY   \ no ready vocabulary: empty corpus on build, or a public word used before a successful TOK-BUILD
-5153 constant E-TOK-RANGE   \ value outside its token/byte domain: byte outside [0,256) or absent from vocab; id outside [0,vocab); or a decoded cell not a finite exact id in [0,vocab)
-5154 constant E-TOK-CAP     \ invalid length or capacity: negative length, negative capacity, or destination smaller than the token/byte count

package MAKI
private

256 constant TOK-BYTE-MAX     \ byte alphabet size (a char token is one byte)
create TOK-VOCAB TOK-BYTE-MAX allot        \ id -> byte (ascending); first TOK-N entries valid
create TOK-SEEN  TOK-BYTE-MAX allot        \ byte -> 0/1 present in the corpus (build scratch)
create TOK-INV   TOK-BYTE-MAX cells allot  \ byte -> id, or -1 when the byte is not in the vocab
variable TOK-N                              \ vocab size (distinct bytes in the corpus)
variable TOK-ID#                            \ scratch: next id to assign during a build

\ Readiness gate: every public lookup/encode/decode requires a built vocab. TOK-N is
\ 0 until a successful TOK-BUILD (which also fills TOK-INV with -1 for absent bytes),
\ so a call before any build reads an unbuilt table - reject it up front.
: TOK-READY ( -- )  TOK-N @ 1 < if E-TOK-EMPTY throw then ;

public

: TOK-SIZE ( -- n )  TOK-N @ ;

\ id -> byte; requires a ready vocab; rejects an id outside [0,vocab)
: TOK-CHAR ( n -- n ) {: k:n :}
   TOK-READY
   k 0 <  k TOK-N @ >=  or if E-TOK-RANGE throw then
   TOK-VOCAB k + c@ ;

\ byte -> id; requires a ready vocab; rejects a byte outside [0,256) before addressing
\ TOK-INV, then a byte that never occurred in the corpus
: TOK-ID ( n -- n ) {: b:n :}
   TOK-READY
   b 0 <  b TOK-BYTE-MAX >=  or if E-TOK-RANGE throw then
   TOK-INV b cells + @  dup 0 < if E-TOK-RANGE throw then ;

private

: TOK-INV-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  -1 TOK-INV i cells + !  loop ;

: TOK-SEEN-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  0 TOK-SEEN i + c!  loop ;

\ Validate a stored float cell as an exact token id, returning it as an int. The two
\ float comparisons bound v into [0,vocab) before f>s: v<0 (or -inf) fails the first;
\ v>=vocab, +inf, and NaN fail the second (NaN compares false, so NOT(v<vocab) holds).
\ f>s is therefore reached only for a finite v in range; the round-trip equality then
\ proves v is exactly integral (a fractional v differs from its truncation). TOK-N>0 is
\ guaranteed by the decode-time readiness gate.
: TOK-CELL>ID ( r -- n ) {: v:r :}
   v f0< if E-TOK-RANGE throw then
   v TOK-N @ s>f f< 0= if E-TOK-RANGE throw then
   v f>s {: k:n :}
   k s>f v f= 0= if E-TOK-RANGE throw then
   k ;

\ Length / capacity gate shared by encode and decode: reject a negative count, a
\ negative capacity, and a destination too small for the whole count - all E-TOK-CAP.
\ Runs before either pass so a rejected call performs no addressing.
: TOK-BOUNDS ( n n -- ) {: u:n cap:n :}
   u   0 < if E-TOK-CAP throw then
   cap 0 < if E-TOK-CAP throw then
   u cap > if E-TOK-CAP throw then ;

\ Encode is two passes so a rejected byte leaves the destination untouched:
\ TOK-ENC-CHECK proves every source byte maps to a vocab id (E-TOK-RANGE) before any
\ write; TOK-ENC-WRITE then stores the proven ids as float cells.
: TOK-ENC-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do  a i + c@ TOK-ID drop  loop ;
: TOK-ENC-WRITE ( ptr u8 n  ptr a -- ) {: a:ptr u:n d:ptr :}
   u 0 ?do  a i + c@ TOK-ID s>f  d i T-SET  loop ;

\ Decode is two passes so a rejected cell leaves the destination untouched:
\ TOK-DEC-CHECK proves every stored cell is a finite, exactly-integral, in-vocab id
\ (E-TOK-RANGE) via TOK-CELL>ID + TOK-CHAR before any write; TOK-DEC-WRITE writes bytes.
: TOK-DEC-CHECK ( ptr a n -- ) {: ids:ptr u:n :}
   u 0 ?do  ids i T-GET TOK-CELL>ID TOK-CHAR drop  loop ;
: TOK-DEC-WRITE ( ptr a n  ptr u8 -- ) {: ids:ptr u:n d:ptr :}
   u 0 ?do  ids i T-GET TOK-CELL>ID TOK-CHAR  d i + c!  loop ;

public

\ Build the vocab from corpus bytes: the DISTINCT bytes, indexed 0..N-1 in
\ ascending byte value (sorted & stable by construction). Rejects an empty corpus.
: TOK-BUILD ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-TOK-EMPTY throw then
   TOK-INV-CLEAR
   TOK-SEEN-CLEAR
   u 0 ?do  1 TOK-SEEN  a i + c@  + c!  loop
   0 TOK-ID# !
   TOK-BYTE-MAX 0 ?do
      TOK-SEEN i + c@ 1 = if
         i  TOK-VOCAB TOK-ID# @ +  c!       \ id -> byte
         TOK-ID# @  TOK-INV i cells + !      \ byte -> id
         TOK-ID# @ 1+ TOK-ID# !
      then
   loop
   TOK-ID# @ TOK-N ! ;

\ Encode src bytes into dst as float-cell token ids (one id per cell). Returns the
\ token count. Requires a ready vocab (E-TOK-EMPTY). Rejects a negative length or
\ capacity and a dst too small (E-TOK-CAP) before the loop, and a byte absent from
\ the vocab (E-TOK-RANGE). Validates every source byte before storing any id, so a
\ rejected call leaves the destination byte-identical to its pre-call contents.
: TOK-ENCODE ( ptr u8 n ptr a n -- n ) {: a:ptr u:n d:ptr cap:n :}
   TOK-READY
   u cap TOK-BOUNDS
   a u TOK-ENC-CHECK
   a u d TOK-ENC-WRITE
   u ;

\ Decode float-cell token ids into dst bytes. Returns the byte count. Requires a ready
\ vocab (E-TOK-EMPTY). Rejects a negative length or capacity and a dst too small
\ (E-TOK-CAP) before the loop. Each stored cell must be a finite, exactly-integral id
\ in [0,vocab) (E-TOK-RANGE) - NaN, infinite, fractional, and out-of-range are rejected.
\ Validates every cell before writing any byte, so a rejected call leaves the
\ destination byte-identical to its pre-call contents.
: TOK-DECODE ( ptr a n ptr u8 n -- n ) {: ids:ptr u:n d:ptr cap:n :}
   TOK-READY
   u cap TOK-BOUNDS
   ids u TOK-DEC-CHECK
   ids u d TOK-DEC-WRITE
   u ;

;package
