\ string.f - checked byte-string helpers.

s" lib/errors.f" required
s" lib/adt/option.f" required            \ option<n> for STR-PARSE-POS/NEG (switchover wave A)
require lib/cad-num-arithmetic.f         \ CAD-NUM byte-len/byte-off/item-count roles + B5.2 offset algebra (package STR, tail)

9 constant STR-TAB
10 constant STR-LF
13 constant STR-CR
32 constant STR-SPACE
64 constant STR-BEFORE-A
91 constant STR-AFTER-Z
96 constant STR-BEFORE-LOWER-A
123 constant STR-AFTER-LOWER-Z
43 constant STR-PLUS
45 constant STR-MINUS
48 constant STR-ZERO
10 constant STR-BASE
19 constant STR-I64-DIGITS
255 constant STR-BYTE-MAX
1024 constant SB-CAP
$7FFFFFFFFFFFFFFF constant STR-MAX-I64
STR-MAX-I64 negate 1 - constant STR-MIN-I64

create SB-BUF SB-CAP allot
variable SB-LEN

: BUFFER: ( n -- )
   dup 0 < if E-STR-BOUNDS throw then
   create allot does> ( -- ptr u8 ) ;

: STR-LEN ( n -- len )
   dup 0 < if E-STR-BOUNDS throw then
   >LEN ;

: STR-OFF ( n -- off )
   dup 0 < if E-STR-BOUNDS throw then
   >OFF ;

: STR-COUNT ( n -- count )
   dup 0 < if E-STR-BOUNDS throw then
   >COUNT ;

create STR-MAX-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 55 c,
create STR-MIN-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 56 c,

: STR-TRUE ( -- bool )
   0 0= ;

: STR-FALSE ( -- bool )
   STR-TRUE 0= ;

: ASCII-LOWER ( n -- n )
   dup STR-BEFORE-A > over STR-AFTER-Z < and if STR-SPACE + then ;

: ASCII-UPPER ( n -- n )
   dup STR-BEFORE-LOWER-A > over STR-AFTER-LOWER-Z < and if STR-SPACE - then ;

: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a v b v STR= ;

: ENDS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a u v - + v b v STR= ;

: FIND-SUB ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}   \ SOME first offset of b in a (empty needle -> SOME 0), else NONE
   v 0= if 0 >IDX OPTION:SOME exit then
   u v < if OPTION:NONE exit then
   0 begin dup u v - <= while
      dup a + v b v STR= if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB MATCH option
     none OF STR-FALSE ENDOF
     some OF drop STR-TRUE ENDOF
   ;MATCH ;

: INDEX-OF ( ptr u8 n n -- option<idx> ) {: a:ptr u:n c:n :}   \ SOME first index of byte c, else NONE
   0 begin dup u < while
      dup a + c@ c = if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      0= if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      a over 1- + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      if 1- else a swap exit then
   repeat drop a 0 ;

: TRIM ( ptr u8 n -- ptr u8 n )
   LTRIM RTRIM ;

: SB-CHECK-LEN-ROOM ( len -- ) {: add :}
   add LEN>N SB-CAP SB-LEN @ - > if E-STR-CAPACITY throw then ;

: SB-CHECK-ROOM ( n -- )
   STR-LEN SB-CHECK-LEN-ROOM ;

: SB-RESET ( -- )
   0 SB-LEN ! ;

: SB-APPEND-LEN ( ptr u8 len -- ) {: a:ptr u :}
   u SB-CHECK-LEN-ROOM
   a SB-BUF SB-LEN @ + u BYTE-COPY-LEN
   SB-LEN @ u LEN>N + SB-LEN ! ;

: SB-APPEND ( ptr u8 n -- )
   STR-LEN SB-APPEND-LEN ;

: SB-APPEND-C ( n -- ) {: c :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   1 SB-CHECK-ROOM
   c SB-BUF SB-LEN @ + c!
   SB-LEN @ 1+ SB-LEN ! ;

: SB$ ( -- ptr u8 n )
   SB-BUF SB-LEN @ ;

: BUF-CHECK-LEN ( len len ptr len -- )
   {: add cap lenp:ptr :}
   add LEN>N 0 < if E-STR-BOUNDS throw then
   lenp @ LEN>N 0 < if E-STR-BOUNDS throw then
   lenp @ LEN>N cap LEN>N > if E-STR-CAPACITY throw then
   add LEN>N cap LEN>N lenp @ LEN>N - > if E-STR-CAPACITY throw then ;

: BUF-RESET ( ptr len -- )
   0 >LEN swap ! ;

: BUF-LEN@ ( ptr len -- n )
   @ LEN>N ;

: BUF-APPEND-LEN ( ptr u8 len ptr u8 len ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   u cap lenp BUF-CHECK-LEN
   src dst lenp @ LEN>N + u BYTE-COPY-LEN
   lenp @ LEN>N u LEN>N + >LEN lenp ! ;

: BUF-APPEND ( ptr u8 n ptr u8 n ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   src u STR-LEN dst cap STR-LEN lenp BUF-APPEND-LEN ;

: BUF-APPEND-C ( n ptr u8 n ptr len -- )
   {: c dst:ptr cap lenp:ptr :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   1 >LEN cap STR-LEN lenp BUF-CHECK-LEN
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1+ >LEN lenp ! ;

: SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool ) {: a:ptr u sep start :}
   start 0 < if a 0 start STR-FALSE exit then
   start u > if a 0 start STR-FALSE exit then
   start begin dup u < while
      dup a + c@ sep = if
         a start + over start - rot 1+ STR-TRUE exit
      then
      1+
   repeat drop
   a start + u start - u 1+ STR-TRUE ;

: STR-DIGIT? ( n -- bool )
   dup STR-ZERO 1- > swap STR-ZERO STR-BASE + < and ;

: STR-DIGIT-VALUE ( n -- n )
   STR-ZERO - ;

: STR-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if drop STR-FALSE exit then
      1+
   repeat drop STR-TRUE ;

: STR-DIGITS<= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if STR-TRUE exit then
   u v > if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if
         dup a + c@ swap b + c@ < exit
      then
      1+
   repeat drop STR-TRUE ;

: STR-PARSE-POS ( ptr u8 n -- option<n> ) {: a:ptr u:n :}   \ SOME parsed nonnegative bounded i64, else NONE
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   a u STR-MAX-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE + swap
      1+
   repeat drop OPTION:SOME ;

: STR-PARSE-NEG ( ptr u8 n -- option<n> ) {: a:ptr u:n :}   \ SOME parsed negative bounded i64, else NONE
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   a u STR-MIN-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE - swap
      1+
   repeat drop OPTION:SOME ;

: STR>NUMBER? ( ptr u8 n -- option<n> ) {: a:ptr u:n :}   \ SOME optional-sign bounded i64, else NONE
   u 0= if OPTION:NONE exit then
   a c@ STR-MINUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- STR-PARSE-NEG exit
   then
   a c@ STR-PLUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- STR-PARSE-POS exit
   then
   a u STR-PARSE-POS ;

\ ---- B5.5 typed STR surface over CAD-NUM roles (MODEL-CAD-V2-PLAN.md B5.5) -----
\
\ The raw string words above conflate byte lengths, byte offsets, and logical
\ counts on one interchangeable `n`. Package STR re-states the same helpers over
\ CAD-NUM roles: a string byte length is a `CAD-NUM:byte-len`, a scan offset is a
\ `CAD-NUM:byte-off`, a character count is a `CAD-NUM:item-count`, and a found
\ position is a `CAD-NUM:index` carried by the shared cell-polymorphic option
\ (option<CAD-NUM:index>). A length/offset swap, an index/offset swap, or a raw
\ `n` where a role is required is a checker reject. Positive behavior is
\ byte-identical to the raw words - same scan, same E-STR-BOUNDS/E-STR-CAPACITY
\ throws, same empty-string/empty-needle zeros and not-found NONE - so the raw
\ words stay untouched for their not-yet-migrated fleet callers; this wave moves
\ only owner-internal callers (none survive as private) and lib/string-test.f.
\
\ STR owns exactly two audited representation projections (BYTE-LEN>N, BYTE-OFF>N),
\ mirroring VEC's ITEM-COUNT>N/INDEX>N and MEM's ALLOC-*>N: the raw scan words and
\ their byte pointers still consume a bare `n`, so the typed boundary reads a
\ validated role's cell to drive them. Private, no public export, confined to
\ lib/string.f; retire when TVK-RAW (habu-nominal-storage-raw-a3430ef2) lets the
\ byte-scan primitives take the nominal role directly. The typed layer wraps the
\ raw words as black boxes (RAW-* aliases captured before the package words shadow
\ their global names) and only STR:SPLIT-NEXT adds a check the raw word cannot
\ express: the largest field boundary it can report is one past the string end, so
\ the offset advance (end + 1) is proven through the B5.2 CAD-NUM:ADVANCE-BYTE-OFF
\ BEFORE the scan, turning an offset-space overflow into an E-STR-BOUNDS throw
\ instead of a silent wrap.
\ Retirement owner: habu-epic-model-cad-70b629a9.

package STR
private

\ ---- representation projections (STR's only retypes, and both are checked) -----
CAST: BYTE-LEN>N ( CAD-NUM:byte-len -- n )
CAST: BYTE-OFF>N ( CAD-NUM:byte-off -- n )

\ ---- raw-word aliases: bind the legacy globals before the package words shadow --
\ their names. Defined here (before the public STR:<name> exist) so a bare
\ FIND-SUB / INDEX-OF / SPLIT-NEXT / BUF-* resolves to the global raw word.
: RAW-FIND-SUB ( ptr u8 n ptr u8 n -- option<idx> )    FIND-SUB ;
: RAW-INDEX-OF ( ptr u8 n n -- option<idx> )           INDEX-OF ;
: RAW-SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool )   SPLIT-NEXT ;
: RAW-BUF-RESET ( ptr len -- )                         BUF-RESET ;
: RAW-BUF-LEN@ ( ptr len -- n )                        BUF-LEN@ ;
: RAW-BUF-APPEND ( ptr u8 n ptr u8 n ptr len -- )      BUF-APPEND ;
: RAW-BUF-APPEND-C ( n ptr u8 n ptr len -- )           BUF-APPEND-C ;

\ ---- ok extractors: a validated/derived raw cell that fails a role predicate is -
\ a string bounds violation. The reachable arm is the caller-facing negative for
\ LENGTH/OFFSET/COUNT (byte-identical to the raw STR-LEN/STR-OFF/STR-COUNT
\ E-STR-BOUNDS) and the SPLIT-NEXT offset-advance overflow; the rest are
\ unreachable invariants over cells the scan proves nonnegative.
: OK-BYTE-LEN ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- CAD-NUM:byte-len )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-BYTE-OFF ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- CAD-NUM:byte-off )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-ITEM-COUNT ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- CAD-NUM:item-count )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-INDEX ( CAD-NUM:numeric-result<CAD-NUM:index> -- CAD-NUM:index )
   MATCH CAD-NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;

\ ---- raw cell -> validated role bridges ---------------------------------------
: N>BYTE-LEN ( n -- CAD-NUM:byte-len )       CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: N>BYTE-OFF ( n -- CAD-NUM:byte-off )       CAD-NUM:BYTE-OFF OK-BYTE-OFF ;
: N>ITEM-COUNT ( n -- CAD-NUM:item-count )   CAD-NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX ( n -- CAD-NUM:index )             CAD-NUM:INDEX OK-INDEX ;

\ ---- option<idx> (raw find result) -> option<CAD-NUM:index> --------------------
: IDX-OPT>INDEX-OPT ( option<idx> -- option<CAD-NUM:index> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF IDX>N N>INDEX OPTION:SOME ENDOF
   ;MATCH ;

\ ---- one-past-end offset advance guard (B5.2 offset algebra) -------------------
: ONE-BYTE-LEN ( -- CAD-NUM:byte-len )   1 CAD-NUM:BYTE-LEN OK-BYTE-LEN ;
: SPLIT-SENTINEL-OK ( CAD-NUM:byte-len -- )
   \ the raw scan's largest reported boundary is (length + 1); prove that offset
   \ advance is representable (overflow -> E-STR-BOUNDS) before scanning.
   BYTE-LEN>N N>BYTE-OFF  ONE-BYTE-LEN  CAD-NUM:ADVANCE-BYTE-OFF  OK-BYTE-OFF drop ;

public

\ ---- role validators (raw n -> role; negative throws E-STR-BOUNDS) -------------
: LENGTH ( n -- CAD-NUM:byte-len )      N>BYTE-LEN ;
: OFFSET ( n -- CAD-NUM:byte-off )      N>BYTE-OFF ;
: COUNT ( n -- CAD-NUM:item-count )     N>ITEM-COUNT ;

\ ---- substring / byte search: found position as option<CAD-NUM:index> ----------
: FIND-SUB ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len -- option<CAD-NUM:index> )
   {: a:ptr u:CAD-NUM:byte-len b:ptr v:CAD-NUM:byte-len :}
   a u BYTE-LEN>N b v BYTE-LEN>N RAW-FIND-SUB IDX-OPT>INDEX-OPT ;
: INDEX-OF ( ptr u8 CAD-NUM:byte-len n -- option<CAD-NUM:index> )
   {: a:ptr u:CAD-NUM:byte-len c:n :}
   a u BYTE-LEN>N c RAW-INDEX-OF IDX-OPT>INDEX-OPT ;

\ ---- next separator-delimited field (offset advance proven, then raw scan) ------
\ A split step yields the next separator-delimited field slice plus the advance
\ cursor (the offset one past the separator, or one past the string end on the
\ final field). STR:SPLIT-NEXT returns option<str:split>: SOME(slice,next) while
\ a field remains, NONE once the start cursor is past the string end - the checked
\ replacement for the former (ptr u8 byte-len byte-off bool) value+flag return
\ (switchover wave B), so a caller cannot read the slice or cursor on the
\ no-field path.
STRUCTURE split 0
  FIELD ptr ptr u8
  FIELD len CAD-NUM:byte-len
  FIELD next CAD-NUM:byte-off
;STRUCTURE

: SPLIT-NEXT ( ptr u8 CAD-NUM:byte-len n CAD-NUM:byte-off -- option<str:split> )   \ SOME next field slice + advance cursor, else NONE
   {: a:ptr u:CAD-NUM:byte-len sep:n start:CAD-NUM:byte-off :}
   u SPLIT-SENTINEL-OK
   a u BYTE-LEN>N sep start BYTE-OFF>N RAW-SPLIT-NEXT   \ ( sub-ptr sub-rawlen raw-next more? )
   {: rn:n more?:bool :}                                \ sub-ptr sub-rawlen remain on the stack
   more? if
      N>BYTE-LEN  rn N>BYTE-OFF  STR-SPLIT:MAKE OPTION:SOME
   else
      2drop OPTION:NONE
   then ;

\ ---- caller-owned bounded byte buffer (length cell as byte-len role) ------------
: BUF-RESET ( ptr len -- )                    RAW-BUF-RESET ;
: BUF-LEN@ ( ptr len -- CAD-NUM:byte-len )    RAW-BUF-LEN@ N>BYTE-LEN ;
: BUF-APPEND ( ptr u8 CAD-NUM:byte-len ptr u8 CAD-NUM:byte-len ptr len -- )
   {: src:ptr srclen:CAD-NUM:byte-len dst:ptr cap:CAD-NUM:byte-len lenp:ptr :}
   src srclen BYTE-LEN>N dst cap BYTE-LEN>N lenp RAW-BUF-APPEND ;
: BUF-APPEND-C ( n ptr u8 CAD-NUM:byte-len ptr len -- )
   {: c:n dst:ptr cap:CAD-NUM:byte-len lenp:ptr :}
   c dst cap BYTE-LEN>N lenp RAW-BUF-APPEND-C ;

;package
