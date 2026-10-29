\ string-roles.f - the string words over NUM's numeric roles (package STR). A
\ file of its own because the engine bakes src/lib/string.f for the compiler and
\ the REPL, which use only the raw words, while STR requires
\ src/lib/num-arithmetic.f: keeping STR here keeps NUM out of the image.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/string.f
require src/lib/num-arithmetic.f

\ A string byte length is a NUM:byte-len, a scan offset a NUM:byte-off, a
\ character count a NUM:item-count and a found position a NUM:index in an
\ option<NUM:index>; swapping roles or passing a raw `n` is a checker reject.
\ Behavior is byte-identical to the raw words, which STR wraps as black boxes.
\ Only SPLIT-NEXT adds a check: its largest boundary is one past the string end,
\ so the advance is proven before the scan and an overflow is E-STR-BOUNDS, not
\ a silent wrap.

package STR
private

\ STR's only representation projections, both checked and private: the raw scan
\ words take a bare `n`.
CAST: BYTE-LEN>N ( NUM:byte-len -- n )
CAST: BYTE-OFF>N ( NUM:byte-off -- n )

\ Bound before the public STR words shadow the global names, so a bare
\ FIND-SUB, INDEX-OF, SPLIT-NEXT or BUF-* here is the raw word.
: RAW-FIND-SUB ( ptr u8 n ptr u8 n -- option<idx> )    FIND-SUB ;
: RAW-INDEX-OF ( ptr u8 n n -- option<idx> )           INDEX-OF ;
: RAW-SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool )   SPLIT-NEXT ;
: RAW-BUF-RESET ( ptr len -- )                         BUF-RESET ;
: RAW-BUF-LEN@ ( ptr len -- n )                        BUF-LEN@ ;
: RAW-BUF-APPEND ( ptr u8 n ptr u8 n ptr len -- )      BUF-APPEND ;
: RAW-BUF-APPEND-C ( n ptr u8 n ptr len -- )           BUF-APPEND-C ;

\ A cell that fails a role predicate is E-STR-BOUNDS. The reachable arm is the
\ negative for LENGTH, OFFSET and COUNT and the SPLIT-NEXT advance overflow; the
\ rest are unreachable over cells the scan proves nonnegative.
: OK-BYTE-LEN ( NUM:numeric-result<NUM:byte-len> -- NUM:byte-len )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-BYTE-OFF ( NUM:numeric-result<NUM:byte-off> -- NUM:byte-off )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-ITEM-COUNT ( NUM:numeric-result<NUM:item-count> -- NUM:item-count )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;
: OK-INDEX ( NUM:numeric-result<NUM:index> -- NUM:index )
   MATCH NUM:numeric-result
      ok OF ENDOF                             negative OF E-STR-BOUNDS throw ENDOF
      zero OF E-STR-BOUNDS throw ENDOF          overflow OF E-STR-BOUNDS throw ENDOF
      underflow OF E-STR-BOUNDS throw ENDOF     bad-alignment OF E-STR-BOUNDS throw ENDOF
      misaligned OF E-STR-BOUNDS throw ENDOF
   ;MATCH ;

: N>BYTE-LEN ( n -- NUM:byte-len )       NUM:BYTE-LEN OK-BYTE-LEN ;
: N>BYTE-OFF ( n -- NUM:byte-off )       NUM:BYTE-OFF OK-BYTE-OFF ;
: N>ITEM-COUNT ( n -- NUM:item-count )   NUM:ITEM-COUNT OK-ITEM-COUNT ;
: N>INDEX ( n -- NUM:index )             NUM:INDEX OK-INDEX ;

: IDX-OPT>INDEX-OPT ( option<idx> -- option<NUM:index> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF IDX>N N>INDEX OPTION:SOME ENDOF
   ;MATCH ;

: ONE-BYTE-LEN ( -- NUM:byte-len )   1 NUM:BYTE-LEN OK-BYTE-LEN ;
: SPLIT-SENTINEL-OK ( NUM:byte-len -- )
   \ the raw scan's largest reported boundary is (length + 1); prove that offset
   \ advance is representable (overflow -> E-STR-BOUNDS) before scanning.
   BYTE-LEN>N N>BYTE-OFF  ONE-BYTE-LEN  NUM:ADVANCE-BYTE-OFF  OK-BYTE-OFF drop ;

public

\ Validators: a negative n throws E-STR-BOUNDS.
: LENGTH ( n -- NUM:byte-len )      N>BYTE-LEN ;
: OFFSET ( n -- NUM:byte-off )      N>BYTE-OFF ;
: COUNT ( n -- NUM:item-count )     N>ITEM-COUNT ;

: FIND-SUB ( ptr u8 NUM:byte-len ptr u8 NUM:byte-len -- option<NUM:index> )
   {: a:ptr u:NUM:byte-len b:ptr v:NUM:byte-len :}
   a u BYTE-LEN>N b v BYTE-LEN>N RAW-FIND-SUB IDX-OPT>INDEX-OPT ;
: INDEX-OF ( ptr u8 NUM:byte-len n -- option<NUM:index> )
   {: a:ptr u:NUM:byte-len c:n :}
   a u BYTE-LEN>N c RAW-INDEX-OF IDX-OPT>INDEX-OPT ;

\ SOME(slice, next) while a field remains, NONE once the start cursor is past
\ the string end, so a caller cannot read the slice or cursor on the no-field
\ path. next is the offset one past the separator, or one past the string end
\ on the final field.
STRUCTURE split
  FIELD ptr ptr u8
  FIELD len NUM:byte-len
  FIELD next NUM:byte-off
;STRUCTURE

: SPLIT-NEXT ( ptr u8 NUM:byte-len n NUM:byte-off -- option<str:split> )
   {: a:ptr u:NUM:byte-len sep:n start:NUM:byte-off :}
   u SPLIT-SENTINEL-OK
   a u BYTE-LEN>N sep start BYTE-OFF>N RAW-SPLIT-NEXT   \ ( sub-ptr sub-rawlen raw-next more? )
   {: rn:n more?:bool :}                                \ sub-ptr sub-rawlen remain on the stack
   more? if
      N>BYTE-LEN  rn N>BYTE-OFF  STR-SPLIT:MAKE OPTION:SOME
   else
      2drop OPTION:NONE
   then ;

\ The caller-owned bounded byte buffer, its length cell a NUM:byte-len.
: BUF-RESET ( ptr len -- )                    RAW-BUF-RESET ;
: BUF-LEN@ ( ptr len -- NUM:byte-len )    RAW-BUF-LEN@ N>BYTE-LEN ;
: BUF-APPEND ( ptr u8 NUM:byte-len ptr u8 NUM:byte-len ptr len -- )
   {: src:ptr srclen:NUM:byte-len dst:ptr cap:NUM:byte-len lenp:ptr :}
   src srclen BYTE-LEN>N dst cap BYTE-LEN>N lenp RAW-BUF-APPEND ;
: BUF-APPEND-C ( n ptr u8 NUM:byte-len ptr len -- )
   {: c:n dst:ptr cap:NUM:byte-len lenp:ptr :}
   c dst cap BYTE-LEN>N lenp RAW-BUF-APPEND-C ;

;package
