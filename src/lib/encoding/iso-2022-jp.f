\ iso-2022-jp.f - the Encoding Standard's ISO-2022-JP decoder, a part of
\ package ENCODING (src/lib/encoding.f), in fatal mode, as the ISO-2022-JP
\ table. Its states are the Standard's decoder states, `ended` its finished
\ and `failed` its first error, whose row throws E-DECODE and leaves what was
\ appended before. Each IN row's SENSE takes the next byte, as the Standard's
\ handler is given it, and answers the case the Standard's step switches on;
\ a line holds that case's effect. Fatal mode returns at an error, so the
\ Standard's output state, read only after an escape's error, is not kept.
\ STORAGE CLASS. CALLER-OWNED: the decoder's record lives for one call.

require src/lib/encoding/data.f
require src/lib/encoding/fault.f
require src/lib/fsm.f
require src/lib/utf8-scalar.f
require src/lib/vector.f

package ENCODING
private

\ The input, the cursor, the byte a SENSE took last, the Standard's
\ ISO-2022-JP leading and output, and the destination.
STRUCTURE jp-decoder DERIVE addr
   FIELD text str
   FIELD cursor n
   FIELD byte n
   FIELD lead n
   FIELD output bool
   FIELD out ^VEC:vector<u8>
;STRUCTURE

: JP-BYTE@ ( ^jp-decoder -- n ) JP-DECODER-BYTE @ ;
: JP-LEAD@ ( ^jp-decoder -- n ) JP-DECODER-LEAD @ ;
: JP-OUTPUT@ ( ^jp-decoder -- bool ) JP-DECODER-OUTPUT @ ;

\ Takes the next byte; false at the input's end.
: JP-TAKE? ( ^jp-decoder -- bool )
   {: d :}
   d JP-DECODER-TEXT @ {: s:str :}
   d JP-DECODER-CURSOR @ {: at:n :}
   at s STR:LEN >= if false exit then
   s STR:DATA at + c@ d JP-DECODER-BYTE !
   at 1+ d JP-DECODER-CURSOR !  true ;

: JP-IN? ( ^jp-decoder n n -- bool )
   {: d lo:n hi:n :}
   d JP-BYTE@ lo >= d JP-BYTE@ hi <= and ;

\ The jis0208 code point of the lead and the byte, 0 where it has none.
: JP-JIS0208@ ( ^jp-decoder -- n )
   {: d :}
   d JP-LEAD@ $21 - 94 * d JP-BYTE@ + $21 - INDEX-JIS0208 @ ;

ENUM jp-state ascii roman katakana lead trail escape-start escape
   ended failed ;ENUM
ENUM ascii-byte escape text end other ;ENUM
ENUM roman-byte escape yen overline text end other ;ENUM
ENUM kana-byte escape kana end other ;ENUM
ENUM lead-byte escape lead end other ;ENUM
ENUM trail-byte escape mapped unmapped end other ;ENUM
ENUM escape-byte intermediate end other ;ENUM
ENUM designation ascii roman katakana jis0208 repeated end other ;ENUM

\ The senses, one per IN row.

: JP-ASCII ( ^jp-decoder -- ascii-byte )
   {: d :}
   d JP-TAKE? 0= if construct ascii-byte end exit then
   d JP-BYTE@ {: b:n :}
   b $1B = if construct ascii-byte escape exit then
   b $0E = b $0F = or b $7F > or if construct ascii-byte other exit then
   construct ascii-byte text ;

: JP-ROMAN ( ^jp-decoder -- roman-byte )
   {: d :}
   d JP-TAKE? 0= if construct roman-byte end exit then
   d JP-BYTE@ {: b:n :}
   b $1B = if construct roman-byte escape exit then
   b $5C = if construct roman-byte yen exit then
   b $7E = if construct roman-byte overline exit then
   b $0E = b $0F = or b $7F > or if construct roman-byte other exit then
   construct roman-byte text ;

: JP-KATAKANA ( ^jp-decoder -- kana-byte )
   {: d :}
   d JP-TAKE? 0= if construct kana-byte end exit then
   d JP-BYTE@ $1B = if construct kana-byte escape exit then
   d $21 $5F JP-IN? if construct kana-byte kana exit then
   construct kana-byte other ;

: JP-LEAD ( ^jp-decoder -- lead-byte )
   {: d :}
   d JP-TAKE? 0= if construct lead-byte end exit then
   d JP-BYTE@ $1B = if construct lead-byte escape exit then
   d $21 $7E JP-IN? if construct lead-byte lead exit then
   construct lead-byte other ;

: JP-TRAIL ( ^jp-decoder -- trail-byte )
   {: d :}
   d JP-TAKE? 0= if construct trail-byte end exit then
   d JP-BYTE@ $1B = if construct trail-byte escape exit then
   d $21 $7E JP-IN? 0= if construct trail-byte other exit then
   d JP-JIS0208@ 0= if construct trail-byte unmapped exit then
   construct trail-byte mapped ;

: JP-ESCAPE-START ( ^jp-decoder -- escape-byte )
   {: d :}
   d JP-TAKE? 0= if construct escape-byte end exit then
   d JP-BYTE@ {: b:n :}
   b $24 = b $28 = or if construct escape-byte intermediate exit then
   construct escape-byte other ;

\ An escape right after another, with no output between, is `repeated`.
: JP-SELECTION ( ^jp-decoder designation -- designation )
   {: d e:designation :}
   d JP-OUTPUT@ if construct designation repeated else e then ;

\ The state the escape's lead and byte select.
: JP-ESCAPE ( ^jp-decoder -- designation )
   {: d :}
   d JP-TAKE? 0= if construct designation end exit then
   d JP-LEAD@ {: lead:n :}
   d JP-BYTE@ {: b:n :}
   lead $28 = b $42 = and
   if d construct designation ascii JP-SELECTION exit then
   lead $28 = b $4A = and
   if d construct designation roman JP-SELECTION exit then
   lead $28 = b $49 = and
   if d construct designation katakana JP-SELECTION exit then
   lead $24 = b $40 = b $42 = or and
   if d construct designation jis0208 JP-SELECTION exit then
   construct designation other ;

\ The actions.

: JP-APPEND ( n ^jp-decoder -- )
   {: c:n d :}
   c d JP-DECODER-OUT @ UTF8:APPEND-SCALAR
   false d JP-DECODER-OUTPUT ! ;

: JP-TEXT ( ^jp-decoder -- ) dup JP-BYTE@ swap JP-APPEND ;
: JP-YEN ( ^jp-decoder -- ) $A5 swap JP-APPEND ;
: JP-OVERLINE ( ^jp-decoder -- ) $203E swap JP-APPEND ;
: JP-KANA ( ^jp-decoder -- ) dup JP-BYTE@ $FF61 $21 - + swap JP-APPEND ;

: JP-KEEP-LEAD ( ^jp-decoder -- )
   {: d :}
   d JP-BYTE@ d JP-DECODER-LEAD !
   false d JP-DECODER-OUTPUT ! ;

\ The trail keeps the output flag the lead cleared.
: JP-JIS0208 ( ^jp-decoder -- )
   {: d :}
   d JP-JIS0208@ d JP-DECODER-OUT @ UTF8:APPEND-SCALAR ;

: JP-INTERMEDIATE ( ^jp-decoder -- )
   dup JP-BYTE@ swap JP-DECODER-LEAD ! ;

: JP-SELECT ( ^jp-decoder -- ) true swap JP-DECODER-OUTPUT ! ;

: JP-REJECT ( ^jp-decoder -- ) drop E-DECODE throw ;

FSM:DEFINE ISO-2022-JP ( ^jp-decoder ) jp-state ascii-byte
   IN ascii  JP-ASCII ( ascii-byte )
      escape  -        >escape-start
      text    JP-TEXT  >ascii
      end     -        >ended
      other   -        >failed
   IN roman  JP-ROMAN ( roman-byte )
      escape    -            >escape-start
      yen       JP-YEN       >roman
      overline  JP-OVERLINE  >roman
      text      JP-TEXT      >roman
      end       -            >ended
      other     -            >failed
   IN katakana  JP-KATAKANA ( kana-byte )
      escape  -        >escape-start
      kana    JP-KANA  >katakana
      end     -        >ended
      other   -        >failed
   IN lead  JP-LEAD ( lead-byte )
      escape  -             >escape-start
      lead    JP-KEEP-LEAD  >trail
      end     -             >ended
      other   -             >failed
   IN trail  JP-TRAIL ( trail-byte )
      escape    -           >failed
      mapped    JP-JIS0208  >lead
      unmapped  -           >failed
      end       -           >failed
      other     -           >failed
   IN escape-start  JP-ESCAPE-START ( escape-byte )
      intermediate  JP-INTERMEDIATE  >escape
      end           -                >failed
      other         -                >failed
   IN escape  JP-ESCAPE ( designation )
      ascii     JP-SELECT  >ascii
      roman     JP-SELECT  >roman
      katakana  JP-SELECT  >katakana
      jis0208   JP-SELECT  >lead
      repeated  -          >failed
      end       -          >failed
      other     -          >failed
   FINAL ended   -
   FINAL failed  JP-REJECT
;DEFINE

: JP-RUN ( str ^VEC:vector<u8> ^jp-decoder -- )
   {: s:str v:^ d :}
   s d JP-DECODER-TEXT !  v d JP-DECODER-OUT !
   d construct jp-state ascii ISO-2022-JP ;

: DECODE-ISO-2022-JP ( str ^VEC:vector<u8> -- )
   [type] jp-decoder [: JP-RUN ;] MEM:WITH-RECORD ;

;package
