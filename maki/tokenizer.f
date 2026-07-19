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

-5152 constant E-TOK-EMPTY   \ build vocab from an empty corpus (no bytes -> no tokens)
-5153 constant E-TOK-RANGE   \ id outside [0,vocab) on decode, or byte absent from vocab on encode
-5154 constant E-TOK-CAP     \ encode/decode destination buffer smaller than the token/byte count

package MAKI
private

256 constant TOK-BYTE-MAX     \ byte alphabet size (a char token is one byte)
create TOK-VOCAB TOK-BYTE-MAX allot        \ id -> byte (ascending); first TOK-N entries valid
create TOK-SEEN  TOK-BYTE-MAX allot        \ byte -> 0/1 present in the corpus (build scratch)
create TOK-INV   TOK-BYTE-MAX cells allot  \ byte -> id, or -1 when the byte is not in the vocab
variable TOK-N                              \ vocab size (distinct bytes in the corpus)
variable TOK-ID#                            \ scratch: next id to assign during a build

public

: TOK-SIZE ( -- n )  TOK-N @ ;

\ id -> byte; rejects an id outside the built vocab
: TOK-CHAR ( n -- n ) {: k:n :}
   k 0 <  k TOK-N @ >=  or if E-TOK-RANGE throw then
   TOK-VOCAB k + c@ ;

\ byte -> id; rejects a byte that never occurred in the corpus
: TOK-ID ( n -- n ) {: b:n :}
   TOK-INV b cells + @  dup 0 < if E-TOK-RANGE throw then ;

private

: TOK-INV-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  -1 TOK-INV i cells + !  loop ;

: TOK-SEEN-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  0 TOK-SEEN i + c!  loop ;

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
\ token count. Rejects a byte absent from the vocab (E-TOK-RANGE) and a dst too
\ small (E-TOK-CAP).
: TOK-ENCODE ( ptr u8 n ptr a n -- n ) {: a:ptr u:n d:ptr cap:n :}
   u cap > if E-TOK-CAP throw then
   u 0 ?do
      a i + c@ TOK-ID  s>f  d i T-SET
   loop
   u ;

\ Decode float-cell token ids into dst bytes. Returns the byte count. Rejects an id
\ outside the vocab (E-TOK-RANGE) and a dst too small (E-TOK-CAP).
: TOK-DECODE ( ptr a n ptr u8 n -- n ) {: ids:ptr u:n d:ptr cap:n :}
   u cap > if E-TOK-CAP throw then
   u 0 ?do
      ids i T-GET f>s TOK-CHAR  d i + c!
   loop
   u ;

;package
