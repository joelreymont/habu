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
\
\ Safety contract: every public word rejects its entire invalid input domain
\ BEFORE any address arithmetic, loop entry, or output write, so a rejected call
\ never reads or writes a caller/module buffer. A lookup or (de)code before a
\ successful TOK-BUILD throws E-TOK-UNBUILT instead of silently mapping every byte
\ to id 0 through the zero-image TOK-INV. Decode proves each stored cell is finite,
\ exactly integral, and inside the current vocabulary before it converts or writes,
\ and validates every id before writing any byte (LESSONS: `f>s` is not validation;
\ validate every index before a scatter can partially mutate output). Throw codes
\ live in the shared registry lib/errors.f, block E-TOK-FIRST..E-TOK-LAST.

require lib/errors.f
require maki/array.f

package MAKI
private

256 constant TOK-BYTE-MAX     \ byte alphabet size (a char token is one byte)
create TOK-VOCAB TOK-BYTE-MAX allot        \ id -> byte (ascending); first TOK-N entries valid
create TOK-SEEN  TOK-BYTE-MAX allot        \ byte -> 0/1 present in the corpus (build scratch)
create TOK-INV   TOK-BYTE-MAX cells allot  \ byte -> id, or -1 when the byte is not in the vocab
variable TOK-N                              \ vocab size (distinct bytes in the corpus)
variable TOK-ID#                            \ scratch: next id to assign during a build

\ Throw unless a vocabulary has been built. A fresh TOK-INV is zero-filled, so
\ without this guard every byte would look up as id 0 and a caller would "succeed"
\ against no vocabulary at all.
: TOK-READY ( -- )  TOK-N @ 0 <= if E-TOK-UNBUILT throw then ;

\ Length / capacity gate shared by encode and decode: reject a negative count, a
\ negative capacity, and a destination too small for the whole count. Runs before
\ any loop so a rejected call performs no addressing.
: TOK-BOUNDS ( n n -- ) {: u:n cap:n :}
   u   0 < if E-TOK-LEN throw then
   cap 0 < if E-TOK-CAP throw then
   u cap > if E-TOK-CAP throw then ;

\ Read one stored id cell as an exact token id: reject NaN, infinity, and any
\ fractional value. f>s truncates toward zero and s>f is exact, so a value survives
\ only when it is finite AND already integral (it round-trips bit-for-bit). The
\ vocab-range check is done by the TOK-CHAR that consumes the returned id.
: TOK-EXACT-ID ( r -- n ) {: v:r :}
   v f>s {: k:n :}
   k s>f v f= 0= if E-TOK-VALUE throw then
   k ;

public

: TOK-SIZE ( -- n )  TOK-N @ ;

\ id -> byte; rejects an unbuilt vocab and an id outside the built vocab
: TOK-CHAR ( n -- n ) {: k:n :}
   TOK-READY
   k 0 <  k TOK-N @ >=  or if E-TOK-ID throw then
   TOK-VOCAB k + c@ ;

\ byte -> id; rejects an out-of-range byte before any addressing, an unbuilt vocab,
\ and a byte that never occurred in the corpus
: TOK-ID ( n -- n ) {: b:n :}
   b 0 <  b TOK-BYTE-MAX >=  or if E-TOK-BYTE throw then
   TOK-READY
   TOK-INV b cells + @  dup 0 < if E-TOK-RANGE throw then ;

private

: TOK-INV-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  -1 TOK-INV i cells + !  loop ;

: TOK-SEEN-CLEAR ( -- )
   TOK-BYTE-MAX 0 ?do  0 TOK-SEEN i + c!  loop ;

\ Encode is two passes so a rejected call writes nothing: pass 1 proves every
\ source byte maps to a vocab id (throwing before any write); pass 2 stores the ids.
: TOK-ENC-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 ?do  a i + c@ TOK-ID drop  loop ;
: TOK-ENC-WRITE ( ptr u8 n  ptr a -- ) {: a:ptr u:n d:ptr :}
   u 0 ?do  a i + c@ TOK-ID s>f  d i T-SET  loop ;

\ Decode is two passes so a rejected call leaves the destination untouched: pass 1
\ proves every id cell is finite, integral, and in-vocab; pass 2 writes the bytes.
: TOK-DEC-CHECK ( ptr a n -- ) {: ids:ptr u:n :}
   u 0 ?do  ids i T-GET TOK-EXACT-ID TOK-CHAR drop  loop ;
: TOK-DEC-WRITE ( ptr a n  ptr u8 -- ) {: ids:ptr u:n d:ptr :}
   u 0 ?do  ids i T-GET TOK-EXACT-ID TOK-CHAR  d i + c!  loop ;

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
\ token count. Requires a built vocab (E-TOK-UNBUILT), rejects a byte absent from
\ the vocab (E-TOK-RANGE), a negative length (E-TOK-LEN), and a dst too small or a
\ negative capacity (E-TOK-CAP). No id is written unless every byte validates.
: TOK-ENCODE ( ptr u8 n ptr a n -- n ) {: a:ptr u:n d:ptr cap:n :}
   TOK-READY
   u cap TOK-BOUNDS
   a u TOK-ENC-CHECK
   a u d TOK-ENC-WRITE
   u ;

\ Decode float-cell token ids into dst bytes. Returns the byte count. Requires a
\ built vocab (E-TOK-UNBUILT), rejects a non-finite / non-integral id cell
\ (E-TOK-VALUE), an id outside the vocab (E-TOK-ID), a negative length (E-TOK-LEN),
\ and a dst too small or a negative capacity (E-TOK-CAP). No byte is written unless
\ every id validates.
: TOK-DECODE ( ptr a n ptr u8 n -- n ) {: ids:ptr u:n d:ptr cap:n :}
   TOK-READY
   u cap TOK-BOUNDS
   ids u TOK-DEC-CHECK
   ids u d TOK-DEC-WRITE
   u ;

;package
