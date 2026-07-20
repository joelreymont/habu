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

\ ==== refined API bounds + value validation (dot habu-bound-tokenizer-api-111a9a88) ==
\ Every guard below is red-first: on the pre-fix engine the invalid input reaches the
\ table read / loop / f>s (no throw, or a coincidental one), so these assertions fail;
\ the guarded engine rejects each domain up front and they pass.

\ Edge corpus: byte extremes 0 and 255 as valid vocab members (ids 0 and 2).
create TKT-EDGE 3 allot   0 TKT-EDGE c!  65 TKT-EDGE 1 + c!  255 TKT-EDGE 2 + c!
100000 constant TKT-FAR-BYTE      \ a byte far outside [0,256): ~800KB past the TOK-INV table

\ Canary buffers: a rejected call must read/write no caller element. Fill with a
\ sentinel, run the rejected call, then require every element still the sentinel.
72  constant TKT-CANN
171 constant TKT-SENT
create TKT-CANB TKT-CANN allot          \ byte destination canary (decode)
create TKT-CANF TKT-CANN cells allot    \ float-cell destination canary (encode)
: TKT-CANB-FILL   ( -- )  TKT-CANN 0 ?do  TKT-SENT TKT-CANB i + c!  loop ;
: TKT-CANB-CLEAN? ( -- bool )
   TKT-CANN 0 ?do  TKT-CANB i + c@ TKT-SENT <> if false unloop exit then  loop  true ;
: TKT-CANF-FILL   ( -- )  TKT-CANN 0 ?do  TKT-SENT s>f TKT-CANF i T-SET  loop ;
: TKT-CANF-CLEAN? ( -- bool )
   TKT-CANN 0 ?do  TKT-CANF i T-GET f>s TKT-SENT <> if false unloop exit then  loop  true ;

\ ---- pre-build readiness: every operational word rejects before any TOK-BUILD -------
: TKT-PRE-ID     ( -- )  65 TOK-ID drop ;
: TKT-PRE-CHAR   ( -- )  0 TOK-CHAR drop ;
: TKT-PRE-ENCODE ( -- )  TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE drop ;
: TKT-PRE-DECODE ( -- )  TKT-IDS 1 TKT-BACK TKT-CAP TOK-DECODE drop ;

\ ---- byte-domain boundaries on TOK-ID ----------------------------------------------
\ 0 and 255 are valid extremes and invert exactly; -1/256/far are out of [0,256).
: TKT-EDGE-BYTES? ( -- bool )
   TKT-EDGE 3 TOK-BUILD
   0   TOK-ID 0 =
   255 TOK-ID 2 =            and
   0   TOK-ID TOK-CHAR 0   = and
   255 TOK-ID TOK-CHAR 255 = and ;
: TKT-ID-NEG1   ( -- )  TKT-TEXT TOK-BUILD  -1 TOK-ID drop ;
: TKT-ID-256    ( -- )  TKT-TEXT TOK-BUILD  256 TOK-ID drop ;
: TKT-ID-FAR    ( -- )  TKT-TEXT TOK-BUILD  TKT-FAR-BYTE TOK-ID drop ;
: TKT-ID-ABSENT ( -- )  TKT-TEXT TOK-BUILD  0 TOK-ID drop ;   \ byte 0 in [0,256) but not in the line

\ ---- length / capacity boundaries --------------------------------------------------
: TKT-BUILD-NEG  ( -- )  TKT-IDS -1 TOK-BUILD ;
: TKT-ENC-NEGLEN ( -- )  TKT-TEXT TOK-BUILD  TKT-TEXT drop -1 TKT-IDS TKT-CAP TOK-ENCODE drop ;
: TKT-ENC-NEGCAP ( -- )  TKT-TEXT TOK-BUILD  TKT-TEXT TKT-IDS -1 TOK-ENCODE drop ;
: TKT-DEC-NEGLEN ( -- )  TKT-TEXT TOK-BUILD  TKT-IDS -1 TKT-BACK TKT-CAP TOK-DECODE drop ;
: TKT-DEC-NEGCAP ( -- )  TKT-TEXT TOK-BUILD  TKT-IDS 1 TKT-BACK -1 TOK-DECODE drop ;
: TKT-DEC-OVERFLOW ( -- )  TKT-TEXT TOK-BUILD
   TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE {: m :}
   TKT-IDS m TKT-BACK m 1- TOK-DECODE drop ;                  \ cap one short of the id count

\ ---- decode float-cell value validation: finite, exactly integral, in [0,vocab) -----
: TKT-NANF  ( -- r )   0.0 0.0 f/ ;
: TKT-INFF  ( -- r )   1.0 0.0 f/ ;
: TKT-NINFF ( -- r )  -1.0 0.0 f/ ;
: TKT-DEC-1BAD ( r -- )  TKT-IDS 0 T-SET  TKT-IDS 1 TKT-BACK TKT-CAP TOK-DECODE drop ;
: TKT-DEC-FRAC   ( -- )  TKT-TEXT TOK-BUILD  2.5 TKT-DEC-1BAD ;
: TKT-DEC-NAN    ( -- )  TKT-TEXT TOK-BUILD  TKT-NANF  TKT-DEC-1BAD ;
: TKT-DEC-POSINF ( -- )  TKT-TEXT TOK-BUILD  TKT-INFF  TKT-DEC-1BAD ;
: TKT-DEC-NEGINF ( -- )  TKT-TEXT TOK-BUILD  TKT-NINFF TKT-DEC-1BAD ;
: TKT-DEC-HUGE   ( -- )  TKT-TEXT TOK-BUILD  1000000.0 TKT-DEC-1BAD ;
: TKT-DEC-NEGID  ( -- )  TKT-TEXT TOK-BUILD  -3.0 TKT-DEC-1BAD ;

\ ---- exact-capacity buffers succeed and write exactly count, guard element untouched -
: TKT-ENC-EXACT? ( -- bool )
   TKT-TEXT TOK-BUILD
   TKT-TEXT nip {: L :}
   TKT-SENT s>f TKT-IDS L T-SET                     \ guard cell just past the output
   TKT-TEXT TKT-IDS L TOK-ENCODE {: m :}            \ cap == token count (exact)
   m L =
   TKT-IDS L T-GET f>s TKT-SENT =  and ;            \ guard cell untouched
: TKT-DEC-EXACT? ( -- bool )
   TKT-TEXT TOK-BUILD
   TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE {: m :}
   TKT-SENT TKT-BACK m + c!                         \ guard byte just past the output
   TKT-IDS m TKT-BACK m TOK-DECODE {: p :}          \ cap == byte count (exact)
   p m =
   TKT-BACK m + c@ TKT-SENT =  and
   TKT-BACK m TKT-TEXT T-STR=  and ;

\ ---- canaries: a rejected call leaves the caller destination fully sentinel ----------
: TKT-CAN-DEC-CAP? ( -- bool )
   TKT-TEXT TOK-BUILD  TKT-CANB-FILL
   TKT-TEXT TKT-IDS TKT-CAP TOK-ENCODE drop
   [: TKT-IDS  TKT-TEXT nip  TKT-CANB  TKT-TEXT nip 1-  TOK-DECODE drop ;] catch
   E-TOK-CAP = TKT-CANB-CLEAN? and ;
: TKT-CAN-DEC-FRAC? ( -- bool )
   TKT-TEXT TOK-BUILD  TKT-CANB-FILL
   2.5 TKT-IDS 0 T-SET
   [: TKT-IDS 1 TKT-CANB TKT-CANN TOK-DECODE drop ;] catch
   E-TOK-RANGE = TKT-CANB-CLEAN? and ;
: TKT-CAN-ENC-CAP? ( -- bool )
   TKT-TEXT TOK-BUILD  TKT-CANF-FILL
   [: TKT-TEXT TKT-CANF 2 TOK-ENCODE drop ;] catch
   E-TOK-CAP = TKT-CANF-CLEAN? and ;
: TKT-CAN-ENC-NEGLEN? ( -- bool )
   TKT-TEXT TOK-BUILD  TKT-CANF-FILL
   [: TKT-TEXT drop -1 TKT-CANF TKT-CANN TOK-ENCODE drop ;] catch
   E-TOK-CAP = TKT-CANF-CLEAN? and ;

\ ---- property: random corpora round-trip exactly and every id stays inside [0,vocab) -
variable TKT-RNG
create TKT-RSRC 64 allot
: TKT-RND   ( -- n )  TKT-RNG @ 1103515245 * 12345 + $7FFFFFFF and dup TKT-RNG ! ;
: TKT-RLEN  ( -- n )  TKT-RND 48 mod 1+ ;             \ corpus length 1..48
: TKT-FILL  ( n -- )  0 ?do  TKT-RND 256 mod TKT-RSRC i + c!  loop ;
: TKT-IDS-INRANGE? ( n -- bool )
   0 ?do
      TKT-IDS i T-GET f>s {: id:n :}
      id 0 <  id TOK-SIZE >=  or if false unloop exit then
   loop true ;
: TKT-TRIAL? ( -- bool )
   TKT-RLEN {: L :}
   L TKT-FILL
   TKT-RSRC L TOK-BUILD
   TKT-RSRC L TKT-IDS TKT-CAP TOK-ENCODE {: m :}
   m L =
   m TKT-IDS-INRANGE?  and
   TKT-IDS m TKT-BACK TKT-CAP TOK-DECODE L =  and
   TKT-RSRC L  TKT-BACK L  T-STR=  and ;
: TKT-PROP? ( -- bool )
   1 TKT-RNG !
   40 0 ?do  TKT-TRIAL? 0= if false unloop exit then  loop  true ;
\ inverse: each id maps to a valid byte and inverts back to itself (stays in both tables)
: TKT-INVERSE? ( -- bool )
   TKT-TEXT TOK-BUILD
   TOK-SIZE 0 ?do
      i TOK-CHAR {: c:n :}
      c 0 <  c 256 >=  or if false unloop exit then
      c TOK-ID i <> if false unloop exit then
   loop true ;

T-RESET

\ pre-build readiness MUST be asserted before any test builds a vocab
' TKT-PRE-ID     E-TOK-EMPTY TTHROWS
' TKT-PRE-CHAR   E-TOK-EMPTY TTHROWS
' TKT-PRE-ENCODE E-TOK-EMPTY TTHROWS
' TKT-PRE-DECODE E-TOK-EMPTY TTHROWS

TKT-ROUNDTRIP? TTRUE
TKT-SORTED?    TTRUE
TKT-STABLE?    TTRUE

' TKT-BUILD-EMPTY     E-TOK-EMPTY TTHROWS
' TKT-DECODE-BAD      E-TOK-RANGE TTHROWS
' TKT-ENCODE-UNKNOWN  E-TOK-RANGE TTHROWS
' TKT-ENCODE-OVERFLOW E-TOK-CAP   TTHROWS

\ byte-domain boundaries
TKT-EDGE-BYTES? TTRUE
' TKT-ID-NEG1   E-TOK-RANGE TTHROWS
' TKT-ID-256    E-TOK-RANGE TTHROWS
' TKT-ID-FAR    E-TOK-RANGE TTHROWS
' TKT-ID-ABSENT E-TOK-RANGE TTHROWS

\ length / capacity boundaries
' TKT-BUILD-NEG    E-TOK-EMPTY TTHROWS
' TKT-ENC-NEGLEN   E-TOK-CAP   TTHROWS
' TKT-ENC-NEGCAP   E-TOK-CAP   TTHROWS
' TKT-DEC-NEGLEN   E-TOK-CAP   TTHROWS
' TKT-DEC-NEGCAP   E-TOK-CAP   TTHROWS
' TKT-DEC-OVERFLOW E-TOK-CAP   TTHROWS

\ decode float-cell value validation
' TKT-DEC-FRAC   E-TOK-RANGE TTHROWS
' TKT-DEC-NAN    E-TOK-RANGE TTHROWS
' TKT-DEC-POSINF E-TOK-RANGE TTHROWS
' TKT-DEC-NEGINF E-TOK-RANGE TTHROWS
' TKT-DEC-HUGE   E-TOK-RANGE TTHROWS
' TKT-DEC-NEGID  E-TOK-RANGE TTHROWS

\ exact-capacity buffers, canaries, and properties
TKT-ENC-EXACT?      TTRUE
TKT-DEC-EXACT?      TTRUE
TKT-CAN-DEC-CAP?    TTRUE
TKT-CAN-DEC-FRAC?   TTRUE
TKT-CAN-ENC-CAP?    TTRUE
TKT-CAN-ENC-NEGLEN? TTRUE
TKT-PROP?           TTRUE
TKT-INVERSE?        TTRUE

T-REPORT

;package
