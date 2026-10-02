\ A table initializes many committed records under one C2 scope.
require lib/test.f
require lib/memory.f
require lib/c2-memory.f
require lib/c2-owner.f
require test/c2-records-types.f
require src/habu/xref.f

package C2-RECORDS-PROGRAM
private

41 constant MANY
MANY 2 * CELL * constant MANY-BYTES

: ENTRY ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if
      drop s" c2-records-program: missing native entry" 71 die
   then XREF-START ;

: EDIT-LAST ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> -- mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> )
   101 C2--RECORDS--TYPES-PAIR:LEFT! ;

: READ-LEFT ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> -- n mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> )
   C2--RECORDS--TYPES-PAIR:LEFT@ swap ;

: ASSERT-SEED ( mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> -- mut-view<b,j,a,init<i,C2-RECORDS-TYPES:pair>> )
   C2--RECORDS--TYPES-PAIR:LEFT@ 11 T= ;

: TABLE-BODY ( records<b,i,a,C2-RECORDS-TYPES:pair> -- bool records<b,i,a,C2-RECORDS-TYPES:pair> )
   0 [: ASSERT-SEED ;] C2-MEM:WITH-RECORD
   MANY 2 / [: ASSERT-SEED ;] C2-MEM:WITH-RECORD
   MANY 1- [: ASSERT-SEED ;] C2-MEM:WITH-RECORD
   MANY 1- [: EDIT-LAST ;] C2-MEM:WITH-RECORD
   MANY 1- [: READ-LEFT ;] C2-MEM:WITH-RECORD
   swap 101 = swap ;

: MANY-BODY ( mut-view<b,l,a,u8> -- bool mut-view<b,l,a,u8> )
   MANY 11 22 C2--RECORDS--TYPES-PAIR:MAKE
   [: TABLE-BODY ;] C2-MEM:WITH-RECORDS
   swap >r
   0 C2-MEM:MUT-BYTE@ swap 0= >r
   MANY-BYTES 1- C2-MEM:MUT-BYTE@ swap 0=
   r> and r> and swap ;

: MANY-RESULT ( -- bool )
   MANY-BYTES MEM:BYTES-ALLOC-LEN [: MANY-BODY ;] C2-MEM:WITH-MUT ;

: ZERO-BODY ( mut-view<b,l,a,u8> -- bool mut-view<b,l,a,u8> )
   0 99 C2-MEM:MUT-BYTE!
   0 1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: ;] C2-MEM:WITH-RECORDS
   0 C2-MEM:MUT-BYTE@ swap 99 = swap ;

: ZERO-RESULT ( -- bool )
   16 MEM:BYTES-ALLOC-LEN [: ZERO-BODY ;] C2-MEM:WITH-MUT ;

: OUT-OF-BOUNDS ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> )
   1 [: ;] C2-MEM:WITH-RECORD ;

: BAD-INDEX ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   1 1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: OUT-OF-BOUNDS ;] C2-MEM:WITH-RECORDS ;

: BAD-INDEX-OWNER ( -- )
   16 MEM:BYTES-ALLOC-LEN [: BAD-INDEX ;] C2-MEM:WITH-MUT ;

: NEGATIVE-INDEX ( records<b,i,a,C2-RECORDS-TYPES:pair> -- records<b,i,a,C2-RECORDS-TYPES:pair> )
   -1 [: ;] C2-MEM:WITH-RECORD ;

: BAD-NEGATIVE ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   1 1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: NEGATIVE-INDEX ;] C2-MEM:WITH-RECORDS ;

: BAD-NEGATIVE-OWNER ( -- )
   16 MEM:BYTES-ALLOC-LEN [: BAD-NEGATIVE ;] C2-MEM:WITH-MUT ;

: TOO-SHORT ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   2 1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: ;] C2-MEM:WITH-RECORDS ;

: TOO-SHORT-OWNER ( -- )
   16 MEM:BYTES-ALLOC-LEN [: TOO-SHORT ;] C2-MEM:WITH-MUT ;

: OVERFLOW ( mut-view<b,l,a,u8> -- mut-view<b,l,a,u8> )
   $7FFFFFFFFFFFFFFF 1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: ;] C2-MEM:WITH-RECORDS ;

: OVERFLOW-OWNER ( -- )
   16 MEM:BYTES-ALLOC-LEN [: OVERFLOW ;] C2-MEM:WITH-MUT ;

\ A node view crosses two other live views before it reaches the records
\ runtime. Each view is two cells; 2swap and rot must move complete values.
: SHUFFLE-HEADER ( C2-MEM:owner<p,i,a> mut-view<q,j,b,init<j,C2-RECORDS-TYPES:pair>> -- C2-MEM:owner<p,i,a> mut-view<q,j,b,init<j,C2-RECORDS-TYPES:pair>> )
   swap 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   rot swap
   rot 16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   2swap rot
   1 11 22 C2--RECORDS--TYPES-PAIR:MAKE
   [: ;] C2-MEM:WITH-RECORDS
   C2-MEM:PUBLISH drop C2-MEM:PUBLISH drop ;

: SHUFFLE-OWNER ( mut-view<p,i,a,init<i,C2-MEM:owner-state>> -- mut-view<p,i,a,init<i,C2-MEM:owner-state>> )
   C2-MEM:BIND
   16 MEM:BYTES-ALLOC-LEN C2-MEM:ALLOC
   1 2 C2--RECORDS--TYPES-PAIR:MAKE
   [: SHUFFLE-HEADER ;] C2-MEM:WITH-INIT
   C2-MEM:PUBLISH drop C2-MEM:UNBIND ;

: SHUFFLE-ROOT ( mut-view<p,p,a,u8> -- mut-view<p,p,a,u8> )
   C2-MEM:SEED-OWNER [: SHUFFLE-OWNER ;] C2-MEM:WITH-INIT ;

: SHUFFLE-RESULT ( -- )
   C2-MEM:OWNER-SIZE [: SHUFFLE-ROOT ;] C2-MEM:WITH-MUT ;

public

: RUN ( -- )
   T-RESET
   s" real table entries have their own authenticated kinds" T-LABEL
   s" C2-MEM:WITH-RECORDS" ENTRY scope-kind? 6 T=
   s" C2-MEM:WITH-RECORD" ENTRY scope-kind? 7 T=
   s" forty-one records share one table scope and edits survive later loans" T-LABEL
   MANY-RESULT TTRUE
   s" an empty table initializes no bytes and restores its parent" T-LABEL
   ZERO-RESULT TTRUE
   s" an index equal to the count is refused" T-LABEL
   [: BAD-INDEX-OWNER ;] E-SPAN-RANGE TTHROWSQ
   s" a negative index is refused" T-LABEL
   [: BAD-NEGATIVE-OWNER ;] E-SPAN-RANGE TTHROWSQ
   s" the complete table must fit the original byte bound" T-LABEL
   [: TOO-SHORT-OWNER ;] E-SPAN-CAPACITY TTHROWSQ
   s" count times committed stride cannot overflow" T-LABEL
   [: OVERFLOW-OWNER ;] E-SPAN-CAPACITY TTHROWSQ
   s" nested owner views reach the records runtime intact after a shuffle" T-LABEL
   SHUFFLE-RESULT
   T-REPORT
   s" c2-records-program: ok" type cr ;

;package

C2-RECORDS-PROGRAM:RUN
