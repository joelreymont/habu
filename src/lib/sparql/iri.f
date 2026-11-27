\ RFC 3986 section 5.2 resolution of a reference without a scheme; one with a
\ scheme is kept as written. Components remain byte spans: no case, percent
\ escape, or default port normalization. The base's directory, its dot segments
\ removed, is kept, so a reference adds only its own segments after it: a path
\ being written is empty or ends at a slash, and `..` cuts back to the slash
\ before.

require src/lib/sparql/lex.f
require src/lib/sparql/syntax.f
require src/lib/vector.f

package SPARQL
private

1 constant I-AUTHORITY
2 constant I-QUERY
4 constant I-FRAGMENT

: CHAR-OFF ( str n -- n )
   {: text:str char:n :}
   text char STR:INDEX-OF MATCH option
      none OF text str> nip ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: URI-SCHEME? ( str -- bool )
   str> {: text size:n :}
   size 0= if false exit then
   text c@ LETTER? 0= if false exit then
   size 1 ?do
      text i + c@ {: char:n :}
      char LETTER? char DIGIT? or char 43 = or
      char 45 = or char 46 = or 0= if
         false unloop exit
      then
   loop true ;

: IRI-SPLIT ( ^syntax str -- ^iri )
   {: state source:str :}
   source str> {: text size:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR
      [type] iri ALLOC:NEW {: parts :}
   source 35 CHAR-OFF {: fragment:n :}
   fragment size < if
      text fragment + 1+ size fragment - 1- >str
         parts IRI-FRAGMENT !
      I-FRAGMENT parts IRI-FLAGS +!
   then
   text fragment >str 63 CHAR-OFF {: query:n :}
   query fragment < if
      text query + 1+ fragment query - 1- >str
         parts IRI-QUERY !
      I-QUERY parts IRI-FLAGS +!
   then
   text query >str 58 CHAR-OFF {: colon:n :}
   colon query < text colon >str URI-SCHEME? and if
      text colon >str parts IRI-SCHEME !
      colon 1+
   else 0 then {: start:n :}
   text start + query start - >str s" //" STR:STARTS-WITH? if
      start 2 + {: auth:n :}
      text auth + query auth - >str 47 CHAR-OFF {: length:n :}
      text auth + length >str parts IRI-AUTHORITY !
      I-AUTHORITY parts IRI-FLAGS +!
      auth length +
   else start then {: path:n :}
   text path + query path - >str parts IRI-PATH ! parts ;

: JOIN$ ( ^syntax str str -- str )
   {: state left:str right:str :}
   left str> {: from ln:n :}
   right str> {: tail rn:n :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR ln rn +
      ALLOC:BYTES {: out size:n :}
   from out ln BYTE-COPY tail out ln + rn BYTE-COPY
   out size >str ;

\ An IRI node's text follows the first d bytes of IRI node c, 0 for none.
\ A head is whole: it is made whole before it becomes one.
: HEAD-BYTES ( ^syntax n n -- str )
   {: state head:n keep:n :}
   head 0= if s" " exit then
   state head TEXT$ str> drop keep >str ;

\ The head's bytes of node n, empty for none.
: HEAD$ ( ^syntax n -- str )
   {: state index:n :}
   state state index NODE-C@ state index NODE-D@ HEAD-BYTES ;

\ IRI node n's whole text; a headed node is made whole, once.
: IRI$ ( ^syntax n -- str )
   {: state index:n :}
   state index NODE-C@ 0= if state index TEXT$ exit then
   state state index HEAD$ state index TEXT$ JOIN$ {: whole:str :}
   state index whole TEXT!
   state index 0 NODE-C! state index 0 NODE-D! whole ;

\ Cuts the last segment of the path that starts at n.
: POP-SEGMENT ( ^VEC:vector<u8> n -- )
   {: out from:n :}
   out VEC:COUNT from = if exit then
   out VEC:COUNT 1- begin
      dup from > if out over 1- VEC:AT c@ 47 <> else false then
   while 1- repeat
   dup out swap VEC:RESIZE
   from = if s" /" out VEC:APPEND then ;

: SEGMENT+ ( ^VEC:vector<u8> n str -- )
   {: out from:n segment:str :}
   segment s" ." STR:= segment s" ./" STR:= or if exit then
   segment s" .." STR:= segment s" ../" STR:= or if
      out from POP-SEGMENT exit
   then
   segment out VEC:APPEND ;

\ The length of the path's first segment, its slash included.
: SEGMENT-LEN ( str -- n )
   {: path:str :}
   path 47 CHAR-OFF 1+ path str> nip min ;

\ Appends the path, dot segments applied, to the path that starts at n.
: PATH+ ( ^VEC:vector<u8> n str -- )
   {: out from:n path:str :}
   path str> {: text size:n :}
   0 begin dup size < while
      {: start:n :}
      text start + size start - >str SEGMENT-LEN {: length:n :}
      out from text start + length >str SEGMENT+
      start length +
   repeat drop ;

: DOT-SEGMENT? ( str -- bool )
   {: segment:str :}
   segment s" ." STR:= segment s" ./" STR:= or
   segment s" .." STR:= or segment s" ../" STR:= or ;

\ Whether PATH+ appends the path as it is.
: DOT-FREE? ( str -- bool )
   {: path:str :}
   path str> {: text size:n :}
   0 begin dup size < while
      {: start:n :}
      text start + size start - >str SEGMENT-LEN {: length:n :}
      text start + length >str DOT-SEGMENT? if false exit then
      start length +
   repeat drop true ;

: PATH-DIRECTORY ( str -- str )
   str> {: text size:n :}
   size begin dup 0 > while
      dup 1- text + c@ 47 = if text swap >str exit then
      1-
   repeat text swap >str ;

: IRI-HAS? ( ^iri n -- bool )
   swap IRI-FLAGS @ and 0<> ;

\ What section 5.2.3 merges before a reference's path, from a base without
\ dot segments.
: RESOLVED-DIRECTORY ( ^iri -- str )
   {: base :}
   base IRI-PATH @ str> nip 0= base I-AUTHORITY IRI-HAS? and if
      s" /" exit
   then
   base IRI-PATH @ PATH-DIRECTORY ;

\ The same from any base.
: DIRECTORY ( ^syntax ^iri -- str )
   {: state base :}
   base RESOLVED-DIRECTORY {: raw:str :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   out raw str> nip VEC:RESERVE
   out 0 raw PATH+ out VEC:BYTES ;

\ The node is the resolved base, the text the base as written: without a
\ scheme, its dot segments are gone or its path is the base's before.
: DECLARE-BASE ( ^syntax n str -- )
   {: state node:n written:str :}
   node state SYNTAX-DECLARED-BASE !
   state state node IRI$ IRI-SPLIT {: base :}
   state written IRI-SPLIT {: ref :}
   base state SYNTAX-BASE !
   ref IRI-SCHEME @ str> nip 0<> if
      state base DIRECTORY state SYNTAX-DIRECTORY ! exit
   then
   ref I-AUTHORITY IRI-HAS? ref IRI-PATH @ str> nip 0<> or if
      base RESOLVED-DIRECTORY state SYNTAX-DIRECTORY !
   then ;

: AUTHORITY+ ( ^VEC:vector<u8> ^iri -- )
   {: out parts :}
   parts I-AUTHORITY IRI-HAS? if
      s" //" out VEC:APPEND parts IRI-AUTHORITY @ out VEC:APPEND
   then ;

: QUERY+ ( ^VEC:vector<u8> ^iri -- )
   {: out parts :}
   parts I-QUERY IRI-HAS? if
      s" ?" out VEC:APPEND parts IRI-QUERY @ out VEC:APPEND
   then ;

: FRAGMENT+ ( ^VEC:vector<u8> ^iri -- )
   {: out parts :}
   parts I-FRAGMENT IRI-HAS? if
      s" #" out VEC:APPEND parts IRI-FRAGMENT @ out VEC:APPEND
   then ;

: MERGED-PATH+ ( ^syntax ^VEC:vector<u8> ^iri -- )
   {: state out ref :}
   out VEC:COUNT {: from:n :}
   ref IRI-PATH @ s" /" STR:STARTS-WITH? 0= if
      state SYNTAX-DIRECTORY @ out VEC:APPEND
   then
   out from ref IRI-PATH @ PATH+ ;

: RESOLVED+ ( ^syntax ^VEC:vector<u8> ^iri -- )
   {: state out ref :}
   state SYNTAX-BASE @ {: base :}
   base IRI-SCHEME @ out VEC:APPEND s" :" out VEC:APPEND
   ref I-AUTHORITY IRI-HAS? if
      out ref AUTHORITY+
      out out VEC:COUNT ref IRI-PATH @ PATH+
      out ref QUERY+ exit
   then
   out base AUTHORITY+
   ref IRI-PATH @ str> nip 0<> if
      state out ref MERGED-PATH+ out ref QUERY+ exit
   then
   base IRI-PATH @ out VEC:APPEND
   out ref I-QUERY IRI-HAS? if ref else base then QUERY+ ;

\ One byte is the slash of a base with an authority and no path.
: RESOLVED-MOST ( ^syntax str -- n )
   {: state text:str :}
   state state SYNTAX-DECLARED-BASE @ IRI$ str> nip
      text str> nip + 1+ ;

: RESOLVED$ ( ^syntax str ^iri -- str )
   {: state text:str ref :}
   state SYNTAX-ARENA @ ARENA:ALLOCATOR 0 [type] u8 VEC:NEW {: out :}
   out state text RESOLVED-MOST VEC:RESERVE
   state out ref RESOLVED+ out ref FRAGMENT+
   out 0 VEC:TRIM out VEC:BYTES ;

\ Where the part of the base's text starts in it.
: BASE-AT ( ^syntax str -- n )
   {: state part:str :}
   state state SYNTAX-DECLARED-BASE @ IRI$ str> drop {: base :}
   part str> drop base - ;

\ The base's bytes up to the directory after the path start n, or -1 when
\ they are not the directory.
: DIRECTORY-KEPT ( ^syntax n -- n )
   {: state start:n :}
   state state SYNTAX-DECLARED-BASE @ IRI$ str> {: text size:n :}
   state SYNTAX-DIRECTORY @ {: directory:str :}
   start directory str> nip + {: end:n :}
   end size > if -1 exit then
   text start + end start - >str directory STR:= if end else -1 then ;

\ The base's bytes an empty path keeps: through its path, or its query
\ when the reference has none.
: QUERY-KEPT ( ^syntax ^iri -- n )
   {: state ref :}
   state SYNTAX-BASE @ {: base :}
   ref I-QUERY IRI-HAS? base I-QUERY IRI-HAS? 0= or if
      state base IRI-PATH @ BASE-AT base IRI-PATH @ str> nip + exit
   then
   state base IRI-QUERY @ BASE-AT base IRI-QUERY @ str> nip + ;

\ How many of the base's bytes the reference's resolution starts with, the
\ reference as written following them; -1 when RESOLVED+ changes its path.
: BASE-KEPT ( ^syntax ^iri -- n )
   {: state ref :}
   state SYNTAX-BASE @ {: base :}
   ref IRI-PATH @ {: path:str :}
   path DOT-FREE? 0= if -1 exit then
   ref I-AUTHORITY IRI-HAS? if base IRI-SCHEME @ str> nip 1+ exit then
   path str> nip 0= if state ref QUERY-KEPT exit then
   state base IRI-PATH @ BASE-AT {: start:n :}
   path s" /" STR:STARTS-WITH? if start exit then
   state start DIRECTORY-KEPT ;

\ The resolved IRI as a head IRI node, the bytes of it kept and the rest:
\ no head for one with a scheme or a path RESOLVED+ changes.
: RESOLVE-IRI ( ^syntax str -- n n str )
   {: state text:str :}
   state text IRI-SPLIT {: ref :}
   ref IRI-SCHEME @ str> nip 0<> if
      0 0 text exit
   then
   state SYNTAX-DECLARED-BASE @ 0= if
      state s" an absolute IRI"
         s" write the full IRI or declare BASE <...>" REJECT
   then
   state ref BASE-KEPT {: keep:n :}
   keep 0< if 0 0 state text ref RESOLVED$ exit then
   state SYNTAX-DECLARED-BASE @ keep text ;

;package
