\ RFC 3986 section 5.2 resolution. Components remain byte spans; the
\ algorithm does not normalize case, percent escapes, or default ports.

require src/lib/sparql/lex.f

package SPARQL
private

STRUCTURE iri DERIVE addr
   FIELD scheme ptr u8 FIELD scheme-size n
   FIELD authority ptr u8 FIELD authority-size n
   FIELD path ptr u8 FIELD path-size n
   FIELD query ptr u8 FIELD query-size n
   FIELD fragment ptr u8 FIELD fragment-size n
   FIELD flags n
;STRUCTURE

1 constant I-AUTHORITY
2 constant I-QUERY
4 constant I-FRAGMENT

: CHAR-OFF ( ptr u8 n n -- n )
   {: text size:n char:n :}
   text size char INDEX-OF MATCH option
      none OF size ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: URI-SCHEME? ( ptr u8 n -- bool )
   {: text size:n :}
   size 0= if false exit then
   text c@ LETTER? 0= if false exit then
   size 1 ?do
      text i + c@ {: char:n :}
      char LETTER? char DIGIT? or char 43 = or
      char 45 = or char 46 = or 0= if
         false unloop exit
      then
   loop true ;

: IRI-SPLIT ( ptr syntax ptr u8 n -- ptr iri )
   {: state text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR
      [type] iri ALLOC:NEW {: parts :}
   text size 35 CHAR-OFF {: fragment:n :}
   fragment size < if
      text fragment + 1+ parts IRI-FRAGMENT !
      size fragment - 1- parts IRI-FRAGMENT-SIZE !
      I-FRAGMENT parts IRI-FLAGS +!
   then
   text fragment 63 CHAR-OFF {: query:n :}
   query fragment < if
      text query + 1+ parts IRI-QUERY !
      fragment query - 1- parts IRI-QUERY-SIZE !
      I-QUERY parts IRI-FLAGS +!
   then
   text query 58 CHAR-OFF {: colon:n :}
   colon query < text colon URI-SCHEME? and if
      text parts IRI-SCHEME ! colon parts IRI-SCHEME-SIZE !
      colon 1+
   else 0 then {: start:n :}
   text start + query start - s" //" STARTS-WITH? if
      start 2 + {: auth:n :}
      text auth + query auth - 47 CHAR-OFF {: length:n :}
      text auth + parts IRI-AUTHORITY !
      length parts IRI-AUTHORITY-SIZE !
      I-AUTHORITY parts IRI-FLAGS +!
      auth length +
   else start then {: path:n :}
   text path + parts IRI-PATH !
   query path - parts IRI-PATH-SIZE ! parts ;

: JOIN$ ( ptr syntax ptr u8 n ptr u8 n -- ptr u8 n )
   {: state left ln:n right rn:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR ln rn +
      ALLOC:BYTES {: out size:n :}
   left out ln BYTE-COPY right out ln + rn BYTE-COPY
   out size ;

STRUCTURE path-work DERIVE addr
   FIELD text ptr u8 FIELD size n
   FIELD cursor n FIELD out ptr u8 FIELD used n
;STRUCTURE

: PATH-REST ( ptr path-work -- ptr u8 n )
   {: work :}
   work PATH-WORK-TEXT @ work PATH-WORK-CURSOR @ +
   work PATH-WORK-SIZE @ work PATH-WORK-CURSOR @ - ;

: PATH-DROP ( ptr path-work n -- )
   swap PATH-WORK-CURSOR +! ;

: PATH-SLASH ( ptr path-work -- )
   {: work :}
   47 work PATH-WORK-OUT @ work PATH-WORK-USED @ + c!
   1 work PATH-WORK-USED +! ;

: PATH-POP ( ptr path-work -- )
   {: work :}
   work PATH-WORK-USED @
   begin dup 0 > while
      1- dup work PATH-WORK-OUT @ + c@ 47 = if
         work PATH-WORK-USED ! exit
      then
   repeat work PATH-WORK-USED ! ;

: PATH-MOVE ( ptr path-work -- )
   {: work :}
   work PATH-REST {: text size:n :}
   text c@ 47 = if 1 else 0 then {: start:n :}
   text start + size start - 47 CHAR-OFF start +
      {: length:n :}
   text work PATH-WORK-OUT @ work PATH-WORK-USED @ +
      length BYTE-COPY
   length work PATH-WORK-USED +!
   work length PATH-DROP ;

: PATH-STEP ( ptr path-work -- )
   {: work :}
   work PATH-REST {: text size:n :}
   text size s" ../" STARTS-WITH? if work 3 PATH-DROP exit then
   text size s" ./" STARTS-WITH? if work 2 PATH-DROP exit then
   text size s" /./" STARTS-WITH? if work 2 PATH-DROP exit then
   text size s" /." STR= if
      work 2 PATH-DROP work PATH-SLASH exit
   then
   text size s" /../" STARTS-WITH? if
      work 3 PATH-DROP work PATH-POP exit
   then
   text size s" /.." STR= if
      work 3 PATH-DROP work PATH-POP work PATH-SLASH exit
   then
   text size s" ." STR= text size s" .." STR= or if
      work size PATH-DROP exit
   then work PATH-MOVE ;

: REMOVE-DOTS ( ptr syntax ptr u8 n -- ptr u8 n )
   {: state text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR
      [type] path-work ALLOC:NEW {: work :}
   text work PATH-WORK-TEXT ! size work PATH-WORK-SIZE !
   state SYNTAX-ARENA @ ARENA:ALLOCATOR size 1+
      ALLOC:BYTES drop work PATH-WORK-OUT !
   begin work PATH-WORK-CURSOR @ size < while
      state CHECK-BUDGET
      work PATH-STEP
   repeat work PATH-WORK-OUT @ work PATH-WORK-USED @ ;

: PATH-DIRECTORY ( ptr u8 n -- ptr u8 n )
   {: text size:n :}
   size begin dup 0 > while
      dup 1- text + c@ 47 = if text swap exit then
      1-
   repeat text swap ;

: MERGE-PATH ( ptr syntax ptr iri ptr iri -- ptr u8 n )
   {: state base ref :}
   base IRI-PATH-SIZE @ 0=
   base IRI-FLAGS @ I-AUTHORITY and 0<> and if
      state s" /" ref IRI-PATH @ ref IRI-PATH-SIZE @ JOIN$
   else
      state base IRI-PATH @ base IRI-PATH-SIZE @
      PATH-DIRECTORY ref IRI-PATH @ ref IRI-PATH-SIZE @ JOIN$
   then ;

: INHERIT-AUTHORITY ( ptr iri ptr iri -- )
   {: base ref :}
   base IRI-AUTHORITY @ ref IRI-AUTHORITY !
   base IRI-AUTHORITY-SIZE @ ref IRI-AUTHORITY-SIZE !
   base IRI-FLAGS @ I-AUTHORITY and ref IRI-FLAGS +! ;

: INHERIT-QUERY ( ptr iri ptr iri -- )
   {: base ref :}
   base IRI-QUERY @ ref IRI-QUERY !
   base IRI-QUERY-SIZE @ ref IRI-QUERY-SIZE !
   base IRI-FLAGS @ I-QUERY and ref IRI-FLAGS +! ;

: RESOLVE-PATH ( ptr syntax ptr iri ptr iri -- )
   {: state base ref :}
   ref IRI-FLAGS @ I-AUTHORITY and 0= if
      base ref INHERIT-AUTHORITY
      ref IRI-PATH-SIZE @ 0= if
         base IRI-PATH @ ref IRI-PATH !
         base IRI-PATH-SIZE @ ref IRI-PATH-SIZE !
         ref IRI-FLAGS @ I-QUERY and 0= if
            base ref INHERIT-QUERY
         then exit
      then
      ref IRI-PATH @ c@ 47 <> if
         state base ref MERGE-PATH
         ref IRI-PATH-SIZE ! ref IRI-PATH !
      then
   then
   state ref IRI-PATH @ ref IRI-PATH-SIZE @ REMOVE-DOTS
   ref IRI-PATH-SIZE ! ref IRI-PATH ! ;

: PART+ ( ptr syntax ptr u8 n ptr u8 n -- ptr u8 n ) JOIN$ ;

: IRI-JOIN ( ptr syntax ptr iri -- ptr u8 n )
   {: state parts :}
   state parts IRI-SCHEME @ parts IRI-SCHEME-SIZE @
   s" :" JOIN$ {: first fn:n :}
   parts IRI-FLAGS @ I-AUTHORITY and 0<> if
      state first fn s" //" JOIN$
      state -rot parts IRI-AUTHORITY @
         parts IRI-AUTHORITY-SIZE @ JOIN$
   else first fn then {: auth an:n :}
   state auth an parts IRI-PATH @ parts IRI-PATH-SIZE @
      JOIN$ {: path pn:n :}
   parts IRI-FLAGS @ I-QUERY and 0<> if
      state path pn s" ?" JOIN$
      state -rot parts IRI-QUERY @ parts IRI-QUERY-SIZE @ JOIN$
   else path pn then {: query qn:n :}
   parts IRI-FLAGS @ I-FRAGMENT and 0<> if
      state query qn s" #" JOIN$
      state -rot parts IRI-FRAGMENT @
         parts IRI-FRAGMENT-SIZE @ JOIN$
   else query qn then ;

: RESOLVE-IRI ( ptr syntax ptr u8 n -- ptr u8 n )
   {: state text size:n :}
   state text size IRI-SPLIT {: ref :}
   ref IRI-SCHEME-SIZE @ 0<> if
      state ref IRI-PATH @ ref IRI-PATH-SIZE @ REMOVE-DOTS
      ref IRI-PATH-SIZE ! ref IRI-PATH !
      state ref IRI-JOIN exit
   then
   state state SYNTAX-BASE @ state SYNTAX-BASE-SIZE @
      IRI-SPLIT {: base :}
   base IRI-SCHEME-SIZE @ 0= if E-SYNTAX throw then
   base IRI-SCHEME @ ref IRI-SCHEME !
   base IRI-SCHEME-SIZE @ ref IRI-SCHEME-SIZE !
   state base ref RESOLVE-PATH state ref IRI-JOIN ;

;package
