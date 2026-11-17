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
\ Each word answers what the raw word it wraps answers, or throws E-STR-BOUNDS.

package STR
private

\ STR's only representation projections, both checked and private: the raw scan
\ words take a bare `n`.
CAST: BYTE-LEN>N ( NUM:byte-len -- n )
CAST: BYTE-OFF>N ( NUM:byte-off -- n )

\ Bound before the public STR words shadow the global names, so a bare
\ FIND-SUB, INDEX-OF, SPLIT-NEXT or BUF-* here is the raw word.
: RAW-FIND-SUB ( ^u8 n ^u8 n -- option<idx> )    FIND-SUB ;
: RAW-INDEX-OF ( ^u8 n n -- option<idx> )           INDEX-OF ;
: RAW-SPLIT-NEXT ( ^u8 n n n -- ^u8 n n bool )   SPLIT-NEXT ;
: RAW-BUF-RESET ( ^len -- )                         BUF-RESET ;
: RAW-BUF-LEN@ ( ^len -- n )                        BUF-LEN@ ;
: RAW-BUF-APPEND ( ^u8 n ^u8 n ^len -- )      BUF-APPEND ;
: RAW-BUF-APPEND-C ( n ^u8 n ^len -- )           BUF-APPEND-C ;

: N>BYTE-LEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF E-STR-BOUNDS throw ENDOF
      some OF ENDOF
   ;MATCH ;
: N>BYTE-OFF ( n -- NUM:byte-off )
   NUM:BYTE-OFF MATCH option
      none OF E-STR-BOUNDS throw ENDOF
      some OF ENDOF
   ;MATCH ;
: N>ITEM-COUNT ( n -- NUM:item-count )
   NUM:ITEM-COUNT MATCH option
      none OF E-STR-BOUNDS throw ENDOF
      some OF ENDOF
   ;MATCH ;

\ A split slice ends at or after its start, so its length is never negative.
CAST: RAW>BYTE-LEN ( n -- NUM:byte-len )
\ A found idx is a position in the string, never negative.
CAST: IDX>INDEX ( idx -- NUM:index )

: IDX-OPT>INDEX-OPT ( option<idx> -- option<NUM:index> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF IDX>INDEX OPTION:SOME ENDOF
   ;MATCH ;

public

\ Validators: a negative n throws E-STR-BOUNDS.
: LENGTH ( n -- NUM:byte-len )      N>BYTE-LEN ;
: OFFSET ( n -- NUM:byte-off )      N>BYTE-OFF ;
: COUNT ( n -- NUM:item-count )     N>ITEM-COUNT ;

: FIND-SUB ( ^u8 NUM:byte-len ^u8 NUM:byte-len -- option<NUM:index> )
   {: a:^ u:NUM:byte-len b:^ v:NUM:byte-len :}
   a u BYTE-LEN>N b v BYTE-LEN>N RAW-FIND-SUB IDX-OPT>INDEX-OPT ;
: INDEX-OF ( ^u8 NUM:byte-len n -- option<NUM:index> )
   {: a:^ u:NUM:byte-len c:n :}
   a u BYTE-LEN>N c RAW-INDEX-OF IDX-OPT>INDEX-OPT ;

\ SOME(slice, next) while a field remains, NONE once the start cursor is past
\ the string end, so a caller cannot read the slice or cursor on the no-field
\ path. next is the offset one past the separator, or one past the string end
\ on the final field.
STRUCTURE split
  FIELD ptr ^u8
  FIELD len NUM:byte-len
  FIELD next NUM:byte-off
;STRUCTURE

: SPLIT-NEXT ( ^u8 NUM:byte-len n NUM:byte-off -- option<str:split> )
   {: a:^ u:NUM:byte-len sep:n start:NUM:byte-off :}
   a u BYTE-LEN>N sep start BYTE-OFF>N RAW-SPLIT-NEXT   \ ( sub-ptr sub-rawlen raw-next more? )
   {: rn:n more?:bool :}                                \ sub-ptr sub-rawlen remain on the stack
   more? if
      RAW>BYTE-LEN  rn N>BYTE-OFF  STR-SPLIT:MAKE OPTION:SOME
   else
      2drop OPTION:NONE
   then ;

\ The caller-owned bounded byte buffer, its length cell a NUM:byte-len.
: BUF-RESET ( ^len -- )                    RAW-BUF-RESET ;
: BUF-LEN@ ( ^len -- NUM:byte-len )    RAW-BUF-LEN@ N>BYTE-LEN ;
: BUF-APPEND ( ^u8 NUM:byte-len ^u8 NUM:byte-len ^len -- )
   {: src:^ srclen:NUM:byte-len dst:^ cap:NUM:byte-len lenp:^ :}
   src srclen BYTE-LEN>N dst cap BYTE-LEN>N lenp RAW-BUF-APPEND ;
: BUF-APPEND-C ( n ^u8 NUM:byte-len ^len -- )
   {: c:n dst:^ cap:NUM:byte-len lenp:^ :}
   c dst cap BYTE-LEN>N lenp RAW-BUF-APPEND-C ;

;package
