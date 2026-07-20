\ maki/tokenizer-test.f - acceptance for the v0 char tokenizer (maki/tokenizer.f):
\ the vocab is the DISTINCT corpus bytes sorted ascending & stable, encode/decode is
\ an exact round-trip, and every invalid input domain is rejected with a named error
\ BEFORE any address arithmetic, loop entry, or output write. The reject cases cover
\ pre-build lookups, out-of-domain bytes, negative/overflow lengths and capacities,
\ and NaN/infinite/fractional/huge float id cells. Canary buffers prove a rejected
\ call touches no caller memory; property words prove both tables stay in bounds and
\ round-trip exactly. Fixture: a public-domain Hamlet line as a single s" literal (no
\ external file needed). Names are TKT-prefixed (the maki suite shares one dictionary
\ across every -test.f).

require lib/test.f
require lib/prelude.f
require maki/tokenizer.f

package MAKI

: TKT-TEXT ( -- ptr u8 n )  s" To be, or not to be, that is the question:" ;

64 constant TKT-CAP
create TKT-IDS  TKT-CAP cells allot     \ encoded ids (float cells)
create TKT-BACK TKT-CAP allot           \ decoded bytes
create TKT-BAD  8 cells allot           \ hand-planted id cells for the value rejects
variable TKT-CNT                         \ token count from the last encode
variable TKT-LEN                         \ fixture length (exact-capacity cases)

\ ---- canary buffers: a rejected call must not touch caller memory -------------
create TKT-ENC-CAN TKT-CAP cells allot   \ encode destination sentinel (-7.0 per cell)
create TKT-DEC-CAN TKT-CAP allot         \ decode destination sentinel (254 per byte)
create TKT-GUARD   TKT-CAP cells allot   \ payload + one trailing guard cell (overrun proof)

: TKT-ENC-FILL ( -- )  TKT-CAP 0 ?do  7.0 fnegate TKT-ENC-CAN i T-SET  loop ;
: TKT-ENC-INTACT? ( -- bool )
   TKT-CAP 0 ?do  TKT-ENC-CAN i T-GET  7.0 fnegate f= 0= if false unloop exit then  loop true ;
: TKT-DEC-FILL ( -- )  TKT-CAP 0 ?do  254 TKT-DEC-CAN i + c!  loop ;
: TKT-DEC-INTACT? ( -- bool )
   TKT-CAP 0 ?do  TKT-DEC-CAN i + c@ 254 <> if false unloop exit then  loop true ;

\ ---- round-trip / order / stability ------------------------------------------
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

\ property: for every id, TOK-CHAR yields a byte in 0..255 whose TOK-ID maps back
\ to the same id (both tables stay in bounds and are mutual inverses)
: TKT-DUAL? ( -- bool )
   TOK-SIZE 0 ?do
      i TOK-CHAR                              \ id -> byte
      dup 0 < over 256 >= or if drop false unloop exit then
      TOK-ID i <> if false unloop exit then
   loop true ;

\ property: every id produced by the last encode is inside [0, vocab)
: TKT-IDS-INRANGE? ( -- bool )
   TKT-CNT @ 0 ?do
      TKT-IDS i T-GET f>s
      dup 0 < swap TOK-SIZE >= or if false unloop exit then
   loop true ;

\ exact-capacity: cap == count is the tight (non-overflow) boundary, both ways
: TKT-EXACT-CAP? ( -- bool )
   TKT-TEXT nip TKT-LEN !
   TKT-TEXT TKT-IDS TKT-LEN @ TOK-ENCODE  TKT-LEN @ =
   TKT-IDS TKT-LEN @ TKT-BACK TKT-LEN @ TOK-DECODE  TKT-LEN @ =
   and ;

\ a successful exact-fit encode must not overrun into the trailing guard cell
: TKT-NO-OVERRUN? ( -- bool )
   TKT-TEXT nip TKT-LEN !
   7.0 fnegate TKT-GUARD TKT-LEN @ T-SET
   TKT-TEXT TKT-GUARD TKT-LEN @ TOK-ENCODE drop
   TKT-GUARD TKT-LEN @ T-GET  7.0 fnegate f= ;

\ ---- named rejects (each throws before producing output) ---------------------
\ pre-build lookups: no vocabulary yet (must run before any TOK-BUILD)
: TKT-PB-ID   ( -- )  65 TOK-ID drop ;                               \ valid byte 'A'
: TKT-PB-CHAR ( -- )  0 TOK-CHAR drop ;
: TKT-PB-ENC  ( -- )  TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE drop ;
: TKT-PB-DEC  ( -- )  TKT-IDS 1 TKT-BACK TKT-CAP TOK-DECODE drop ;

\ byte-domain rejects: out of 0..255, caught before any addressing (state-agnostic)
: TKT-BYTE-NEG  ( -- )  -1 TOK-ID drop ;
: TKT-BYTE-256  ( -- )  256 TOK-ID drop ;
: TKT-BYTE-HUGE ( -- )  $7FFFFFFFFFFFFFFF TOK-ID drop ;

\ post-build byte rejects: valid bytes that never occurred in the corpus
: TKT-BYTE-ABSENT ( -- )  0 TOK-ID drop ;                            \ NUL absent
: TKT-BYTE-255    ( -- )  255 TOK-ID drop ;                          \ 0xFF absent

\ build rejects
: TKT-BUILD-EMPTY ( -- )  TKT-IDS 0 TOK-BUILD ;                      \ zero-length corpus
: TKT-BUILD-NEG   ( -- )  TKT-IDS -1 TOK-BUILD ;                     \ negative-length corpus

\ encode rejects
: TKT-ENC-UNKNOWN  ( -- )  s" Z" TKT-IDS TKT-CAP TOK-ENCODE drop ;   \ 'Z' absent from the line
: TKT-ENC-OVERFLOW ( -- )  TKT-TEXT TKT-IDS 2 TOK-ENCODE drop ;      \ cap 2 < fixture length
: TKT-ENC-NEGLEN   ( -- )  TKT-TEXT drop -1 TKT-IDS TKT-CAP TOK-ENCODE drop ;
: TKT-ENC-NEGCAP   ( -- )  TKT-TEXT TKT-IDS -1 TOK-ENCODE drop ;

\ decode rejects
: TKT-DEC-OOR      ( -- )  TOK-SIZE s>f TKT-BAD 0 T-SET
   TKT-BAD 1 TKT-BACK TKT-CAP TOK-DECODE drop ;                      \ id == vocab size
: TKT-DEC-FRAC     ( -- )  1.5 TKT-BAD 0 T-SET
   TKT-BAD 1 TKT-BACK TKT-CAP TOK-DECODE drop ;                      \ fractional
: TKT-DEC-NAN      ( -- )  0.0 0.0 f/ TKT-BAD 0 T-SET
   TKT-BAD 1 TKT-BACK TKT-CAP TOK-DECODE drop ;                      \ NaN
: TKT-DEC-INF      ( -- )  1.0 0.0 f/ TKT-BAD 0 T-SET
   TKT-BAD 1 TKT-BACK TKT-CAP TOK-DECODE drop ;                      \ +infinity
: TKT-DEC-HUGE     ( -- )  100000.0 dup f* dup f* TKT-BAD 0 T-SET
   TKT-BAD 1 TKT-BACK TKT-CAP TOK-DECODE drop ;                      \ 1e20 (> int64, no round-trip)
: TKT-DEC-NEGLEN   ( -- )  TKT-IDS -1 TKT-BACK TKT-CAP TOK-DECODE drop ;
: TKT-DEC-NEGCAP   ( -- )  TKT-IDS 1 TKT-BACK -1 TOK-DECODE drop ;
: TKT-DEC-OVERFLOW ( -- )  TKT-IDS TKT-CNT @ TKT-BACK 2 TOK-DECODE drop ;

\ canary rejects: throw mid-input, prove nothing was written to the destination
: TKT-ENC-CAN-REJECT ( -- )  s" Z" TKT-ENC-CAN TKT-CAP TOK-ENCODE drop ;
: TKT-DEC-CAN-REJECT ( -- )
   0 s>f TKT-BAD 0 T-SET                                             \ valid id 0
   1.5   TKT-BAD 1 T-SET                                             \ fractional -> throws in pass 1
   TKT-BAD 2 TKT-DEC-CAN TKT-CAP TOK-DECODE drop ;

T-RESET

\ --- unbuilt state: lookups and (de)code reject before any vocabulary exists ---
' TKT-PB-ID     E-TOK-UNBUILT TTHROWS
' TKT-PB-CHAR   E-TOK-UNBUILT TTHROWS
' TKT-PB-ENC    E-TOK-UNBUILT TTHROWS
' TKT-PB-DEC    E-TOK-UNBUILT TTHROWS
' TKT-BYTE-NEG  E-TOK-BYTE    TTHROWS
' TKT-BYTE-256  E-TOK-BYTE    TTHROWS
' TKT-BYTE-HUGE E-TOK-BYTE    TTHROWS

\ --- build & round-trip / properties ---
TKT-ROUNDTRIP?   TTRUE
TKT-SORTED?      TTRUE
TKT-STABLE?      TTRUE
TKT-DUAL?        TTRUE
TKT-IDS-INRANGE? TTRUE
TKT-EXACT-CAP?   TTRUE
TKT-NO-OVERRUN?  TTRUE

\ --- byte boundary after build ---
' TKT-BYTE-ABSENT E-TOK-RANGE TTHROWS
' TKT-BYTE-255    E-TOK-RANGE TTHROWS
' TKT-BYTE-NEG    E-TOK-BYTE  TTHROWS
' TKT-BYTE-256    E-TOK-BYTE  TTHROWS

\ --- build rejects ---
' TKT-BUILD-EMPTY E-TOK-EMPTY TTHROWS
' TKT-BUILD-NEG   E-TOK-EMPTY TTHROWS

\ --- encode rejects ---
' TKT-ENC-UNKNOWN  E-TOK-RANGE TTHROWS
' TKT-ENC-OVERFLOW E-TOK-CAP   TTHROWS
' TKT-ENC-NEGLEN   E-TOK-LEN   TTHROWS
' TKT-ENC-NEGCAP   E-TOK-CAP   TTHROWS

\ --- decode rejects ---
' TKT-DEC-OOR      E-TOK-ID    TTHROWS
' TKT-DEC-FRAC     E-TOK-VALUE TTHROWS
' TKT-DEC-NAN      E-TOK-VALUE TTHROWS
' TKT-DEC-INF      E-TOK-VALUE TTHROWS
' TKT-DEC-HUGE     E-TOK-VALUE TTHROWS
' TKT-DEC-NEGLEN   E-TOK-LEN   TTHROWS
' TKT-DEC-NEGCAP   E-TOK-CAP   TTHROWS
' TKT-DEC-OVERFLOW E-TOK-CAP   TTHROWS

\ --- canaries: a rejected call leaves the caller buffer untouched ---
TKT-ENC-FILL
' TKT-ENC-CAN-REJECT E-TOK-RANGE TTHROWS
TKT-ENC-INTACT?  TTRUE
TKT-DEC-FILL
' TKT-DEC-CAN-REJECT E-TOK-VALUE TTHROWS
TKT-DEC-INTACT?  TTRUE

T-REPORT

;package
