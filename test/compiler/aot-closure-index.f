\ WHAT THE CLOSURE'S TWO INDEXES ANSWER (src/habu/aot-closure.f): the record
\ index behind FINDADDR-PTR, and the member set behind IN-CLO?. An image that
\ runs cannot say either, because both rules are about which of several true
\ answers the linker takes.
\ TWO RECORDS CAN SHARE ONE CODE ENTRY - `EXPORT` publishes a second name for a
\ word that already exists - so "which record starts here" has two answers and
\ the linker takes the FIRST, the one the ascending scan this index replaced
\ gave. A later record winning would hand the walk a different record for the
\ same span, and the image would carry a different diagnostic and a different
\ member row for it.
\ THE INDEX IS BUILT FOR A RECORD COUNT, so a record defined after one question
\ is found by the next. A link never needs that - the dictionary is closed
\ before CLOSURE runs - but every caller outside one defines words between its
\ questions, which is why the count is the index's validity test.
\ THE ENTRY IS THE MEMBER'S IDENTITY: both records name one span, and one span
\ is one member however many names reach it.
require lib/test.f
require src/habu/aot-closure.f

package CIX-FIXTURE
: CIX-SHARED ( -- n ) 7 ;      \ the record the definition makes ...
public
EXPORT CIX-SHARED              \ ... and a second one for the same entry
;package

package AOT-LINK
variable CIX-I  variable CIX-N
\ The nth record carrying this name, or -1. The fixture's two are the only ones
\ the engine holds, and they are in definition order, so `0` is the record
\ FINDADDR-PTR has to answer with and `1` is the one it must not.
: CIX-NTH-NAMED ( ptr u8 n n -- n ) {: a:ptr u:n nth:n :}
   0 CIX-N !  0 CIX-I !
   BEGIN CIX-I @ ndict@ < WHILE
      CIX-I @ REC a u REC-NAME= IF
         CIX-N @ nth = IF CIX-I @ exit THEN
         CIX-N @ 1+ CIX-N ! THEN
      CIX-I @ 1+ CIX-I ! REPEAT  -1 ;

: CIX-RUN ( -- )
   T-RESET
   s" EXPORT gives one code entry two records" T-LABEL
   s" CIX-SHARED" 0 CIX-NTH-NAMED {: r1:n :}
   s" CIX-SHARED" 1 CIX-NTH-NAMED {: r2:n :}
   r1 -1 T<>
   r2 -1 T<>
   r1 r2 < TTRUE
   r1 REC REC-CODE-PTR@ {: e:ptr :}
   r2 REC REC-CODE-PTR@ e = TTRUE
   s" the record index answers the first record with the entry" T-LABEL
   e FINDADDR-PTR  r1 REC  = TTRUE
   s" an address inside a record is not its entry" T-LABEL
   e 4 + FINDADDR-PTR XREF-FOUND? TFALSE
   s" one entry is one member however many records name it" T-LABEL
   4 CLO-TABLES  0 NCLO !
   r1 REC  e  16  ADD-CLO
   r2 REC  e  16  ADD-CLO
   NCLO @ 1 T=
   0 CLO-AT e = TTRUE
   0 CLO-REC@ r1 REC = TTRUE
   s" a second entry is a second member" T-LABEL
   r2 REC  e 16 +  16  ADD-CLO
   NCLO @ 2 T= ;
CIX-RUN
;package

\ The record the walk could not have known about: CIX-LATE-RUN's own, made after
\ CIX-RUN above built the index. `ndict@ 1-` is that record because the word
\ running is the last one this file defined.
package AOT-LINK
: CIX-LATE-RUN ( -- )
   s" a record defined after the index was built is found" T-LABEL
   ndict@ 1- REC {: r:ptr :}
   r REC-CODE-PTR@ FINDADDR-PTR  r  = TTRUE
   T-REPORT ;
CIX-LATE-RUN
;package
