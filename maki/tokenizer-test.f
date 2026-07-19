\ maki/tokenizer-test.f - acceptance for the v0 char tokenizer (maki/tokenizer.f):
\ the vocab is the DISTINCT corpus bytes sorted ascending & stable, encode/decode is
\ an exact round-trip, and the empty/out-of-range/capacity rejects throw named
\ errors. Fixture: a public-domain Hamlet line as a single s" literal (no external
\ file needed). Names are TKT-prefixed (the maki suite shares one dictionary across
\ every -test.f).

require lib/test.f
require lib/prelude.f
require maki/tokenizer.f

package MAKI

: TKT-TEXT ( -- ptr u8 n )  s" To be, or not to be, that is the question:" ;

64 constant TKT-CAP
create TKT-IDS  TKT-CAP cells allot     \ encoded ids (float cells)
create TKT-BACK TKT-CAP allot           \ decoded bytes
variable TKT-CNT                         \ token count from the last encode

\ encode the fixture, decode it back, compare byte-for-byte to the original
: TKT-ROUNDTRIP? ( -- bool )
   TKT-TEXT TOK-BUILD
   TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE  TKT-CNT !
   TKT-IDS TKT-CNT @ TKT-BACK TKT-CAP TOK-DECODE drop
   TKT-BACK TKT-CNT @  TKT-TEXT  T-STR= ;

\ vocab strictly ascending by byte value => unique AND sorted
: TKT-SORTED? ( -- bool )
   TOK-SIZE 1 <= if true exit then
   TOK-SIZE 1- 0 ?do
      i TOK-CHAR  i 1+ TOK-CHAR  >= if false unloop exit then
   loop true ;

\ rebuilding from the same corpus yields the same vocab (size + first char)
: TKT-STABLE? ( -- bool )
   TKT-TEXT TOK-BUILD  TOK-SIZE {: n1:n :}
   0 TOK-CHAR {: c0:n :}
   TKT-TEXT TOK-BUILD
   TOK-SIZE n1 =  0 TOK-CHAR c0 =  and ;

\ ---- named rejects (each throws before producing output) ---------------------
: TKT-BUILD-EMPTY     ( -- )  TKT-IDS 0 TOK-BUILD ;                 \ zero-byte corpus
: TKT-DECODE-BAD      ( -- )  TKT-TEXT TOK-BUILD
   TOK-SIZE s>f TKT-IDS 0 T-SET                                     \ id == vocab size (out of range)
   TKT-IDS 1 TKT-BACK TKT-CAP TOK-DECODE drop ;
: TKT-ENCODE-UNKNOWN  ( -- )  TKT-TEXT TOK-BUILD
   s" Z" TKT-IDS TKT-CAP TOK-ENCODE drop ;                          \ 'Z' absent from the line
: TKT-ENCODE-OVERFLOW ( -- )  TKT-TEXT TOK-BUILD
   TKT-TEXT TKT-IDS 2 TOK-ENCODE drop ;                            \ cap 2 < fixture length

T-RESET

TKT-ROUNDTRIP? TTRUE
TKT-SORTED?    TTRUE
TKT-STABLE?    TTRUE

' TKT-BUILD-EMPTY     E-TOK-EMPTY TTHROWS
' TKT-DECODE-BAD      E-TOK-RANGE TTHROWS
' TKT-ENCODE-UNKNOWN  E-TOK-RANGE TTHROWS
' TKT-ENCODE-OVERFLOW E-TOK-CAP   TTHROWS

T-REPORT

;package
