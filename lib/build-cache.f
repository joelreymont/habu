\ build-cache.f - canonical checked build-cache root selection.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/fs-root.f, and
\ lib/fs-mutate.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f

package BUILD-CACHE
public

ENUM source none explicit xdg home tmp ;ENUM

private

$2F constant SLASH

create ROOT-BUF FS-PATH-CAP allot
1 LAYOUT-BUFFER SOURCE-BUF source

variable SELECT-A
variable SELECT-CAP
variable SELECT-U
variable ROOT-U
variable OVERRIDE?
variable READY?
variable SELECTED-FLAG
variable CAUSE-CODE

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: ROOT-BYTES ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SELECT-A-FIELD ( -- ptr ptr u8 )
   SELECT-A 0 ptr-field ;

: SELECT-A@ ( -- ptr u8 )
   SELECT-A-FIELD @ ;

: SELECT-A! ( ptr u8 -- )
   SELECT-A-FIELD ! ;

: SELECT-ALLOC ( n -- ptr u8 )
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop ;

: SELECT-BUF ( n -- ptr u8 ) {: need:n :}
   SELECT-CAP @ need < if
      need SELECT-ALLOC SELECT-A!
      need SELECT-CAP !
   then
   SELECT-A@ ;

: SELECT-BYTES ( -- ptr u8 n )
   SELECT-A@ SELECT-U @ ;

: FAIL ( n -- )
   CAUSE-CODE !
   FALSE READY? !
   E-BUILD-PATH throw ;

: ROOT-CAUSE? ( n -- bool )
   dup E-FS-PATH =
   over E-FS-STAT = or
   over E-FS-DIR = or
   over E-FS-IO = or
   over E-FS-PATH-UNSAFE = or
   swap E-FS-CAPACITY = or ;

: FAIL-ROOT ( n -- )
   dup ROOT-CAUSE? 0= if throw then
   FAIL ;

: NUL? ( ptr u8 n -- bool )
   0 COUNT-CHAR 0 > ;

: ROOT-PATH-CHECK ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 <= if E-FS-PATH FAIL then
   u FS-PATH-CAP > if E-FS-CAPACITY FAIL then
   a u NUL? if E-FS-PATH FAIL then ;

: DOT-COMPONENT? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" ." STR= if TRUE exit then
   a u s" .." STR= ;

: ROOT-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 ROOT-U !
   a u ROOT-PATH-CHECK
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: SELECT-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SELECT-U !
   u 0 < if E-FS-PATH FAIL then
   u 0= if exit then
   a u SELECT-BUF u BYTE-COPY
   u SELECT-U ! ;

: SELECT-JOIN-NEED ( ptr u8 n n -- n ) {: a:ptr u:n suffixu:n :}
   suffixu MEM-MAX-N u - > if E-FS-CAPACITY FAIL then
   u suffixu +
   a u 1 - + c@ SLASH <> if
      dup MEM-MAX-N = if E-FS-CAPACITY FAIL then
      1+
   then ;

: SELECT-JOIN! ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n suffix:ptr suffixu:n :}
   0 SELECT-U !
   u 0 <= if E-FS-PATH FAIL then
   suffixu 0 < if E-FS-PATH FAIL then
   a u suffixu SELECT-JOIN-NEED {: need:n :}
   need SELECT-BUF {: dst:ptr :}
   a dst u BYTE-COPY
   a u 1 - + c@ SLASH = if
      suffix dst u + suffixu BYTE-COPY
   else
      SLASH dst u + c!
      suffix dst u 1 + + suffixu BYTE-COPY
   then
   need SELECT-U ! ;

: SELECTED>ROOT ( -- )
   SELECT-BYTES ROOT-COPY! ;

: SOURCE-PTR ( -- ptr source )
   0 SOURCE-BUF ;

: SOURCE! ( source -- )
   SOURCE-PTR ! ;

: SOURCE-VALUE ( -- source )
   SOURCE-PTR @ ;

: SELECT-BEGIN ( source -- )
   SOURCE!
   TRUE SELECTED-FLAG !
   0 CAUSE-CODE !
   FALSE READY? ! ;

: MAKE-ROOT ( -- )
   ROOT-BYTES MAKE-DIRS ;

: MAKE-ROOT-CHECKED ( -- )
   [: MAKE-ROOT ;] catch
   dup 0 <> if FAIL-ROOT then
   drop ;

: PREPARE-ROOT ( -- )
   ROOT-BYTES SYMLINK? if E-FS-PATH-UNSAFE FAIL then
   ROOT-BYTES 2dup EXISTS? if
      2dup DIR? 0= if 2drop E-FS-DIR FAIL then
      2drop
   else
      2drop
      MAKE-ROOT-CHECKED
   then
   ROOT-BYTES FS:WRITABLE-ROOT? 0= if E-FS-IO FAIL then
   0 CAUSE-CODE !
   TRUE READY? ! ;

: SELECT-EXPLICIT ( ptr u8 n -- )
   construct source explicit SELECT-BEGIN
   SELECT-COPY!
   SELECTED>ROOT
   PREPARE-ROOT ;

: SELECT-SUFFIX ( ptr u8 n ptr u8 n source -- )
   {: a:ptr u:n suffix:ptr suffixu:n source:source :}
   source SELECT-BEGIN
   a u suffix suffixu SELECT-JOIN!
   SELECTED>ROOT
   PREPARE-ROOT ;

: NONEMPTY? ( ptr u8 n -- bool )
   nip 0 > ;

: SELECT-ENV ( -- )
   s" HABU_BUILD_CACHE" GETENV 2dup NONEMPTY? if SELECT-EXPLICIT exit then 2drop
   s" XDG_CACHE_HOME" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source xdg SELECT-SUFFIX exit
   then 2drop
   s" HOME" GETENV 2dup NONEMPTY? if
      s" .cache/habu-build" construct source home SELECT-SUFFIX exit
   then 2drop
   s" TMPDIR" GETENV 2dup NONEMPTY? if
      s" habu-build" construct source tmp SELECT-SUFFIX exit
   then 2drop
   0 ROOT-U !
   0 SELECT-U !
   construct source none SOURCE!
   FALSE SELECTED-FLAG !
   E-FS-PATH FAIL ;

: ENSURE ( -- )
   READY? @ 0 <> if exit then
   OVERRIDE? @ 0 <> if
      PREPARE-ROOT
      exit
   then
   SELECT-ENV ;

public

: RESET ( -- )
   0 ROOT-U !
   0 SELECT-U !
   construct source none SOURCE!
   FALSE OVERRIDE? !
   FALSE READY? !
   FALSE SELECTED-FLAG !
   0 CAUSE-CODE ! ;

: ROOT! ( ptr u8 n -- )
   FALSE OVERRIDE? !
   construct source explicit SELECT-BEGIN
   SELECT-COPY!
   SELECTED>ROOT
   TRUE OVERRIDE? !
   FALSE READY? ! ;

: CHILD-SUFFIX-INTO ( ptr u8 n ptr u8 n ptr u8 -- n )
   {: child:ptr childu:n suffix:ptr suffixu:n dst:ptr :}
   childu 0 <= if E-FS-PATH FAIL then
   suffixu 0 < if E-FS-PATH FAIL then
   child childu NUL? if E-FS-PATH FAIL then
   suffix suffixu NUL? if E-FS-PATH FAIL then
   child childu SLASH COUNT-CHAR 0 > if E-FS-PATH FAIL then
   suffix suffixu SLASH COUNT-CHAR 0 > if E-FS-PATH FAIL then
   child childu DOT-COMPONENT? if E-FS-PATH FAIL then
   suffixu MEM-MAX-N childu - > if E-FS-CAPACITY FAIL then
   childu suffixu + {: nameu:n :}
   ENSURE
   ROOT-BYTES {: root:ptr rootu:n :}
   root rootu nameu SELECT-JOIN-NEED {: need:n :}
   need FS-PATH-CAP > if E-FS-CAPACITY FAIL then
   root dst rootu BYTE-COPY
   root rootu 1 - + c@ SLASH = if
      child dst rootu + childu BYTE-COPY
      suffix dst rootu childu + + suffixu BYTE-COPY
   else
      SLASH dst rootu + c!
      child dst rootu 1 + + childu BYTE-COPY
      suffix dst rootu 1 + childu + + suffixu BYTE-COPY
   then
   need ;

: CHILD-INTO ( ptr u8 n ptr u8 -- n ) {: child:ptr childu:n dst:ptr :}
   child childu s" " dst CHILD-SUFFIX-INTO ;

: ROOT$ ( -- ptr u8 n )
   ENSURE
   ROOT-BYTES ;

: SOURCE ( -- source )
   ENSURE
   SOURCE-VALUE ;

: RESOLVE ( -- ptr u8 n source )
   ENSURE
   ROOT-BYTES SOURCE-VALUE ;

: SELECTED? ( -- bool )
   SELECTED-FLAG @ 0 <> ;

: SELECTED-ROOT$ ( -- ptr u8 n )
   SELECT-BYTES ;

: SELECTED-SOURCE ( -- source )
   SOURCE-VALUE ;

: CAUSE ( -- n )
   CAUSE-CODE @ ;

: SOURCE$ ( source -- ptr u8 n )
   MATCH source
      none OF s" none" ENDOF
      explicit OF s" explicit" ENDOF
      xdg OF s" xdg" ENDOF
      home OF s" home" ENDOF
      tmp OF s" tmp" ENDOF
   ;MATCH ;

: CAUSE$ ( -- ptr u8 n )
   CAUSE-CODE @
   dup 0 = if drop s" none" exit then
   dup E-FS-PATH = if drop s" E-FS-PATH" exit then
   dup E-FS-STAT = if drop s" E-FS-STAT" exit then
   dup E-FS-DIR = if drop s" E-FS-DIR" exit then
   dup E-FS-IO = if drop s" E-FS-IO" exit then
   dup E-FS-PATH-UNSAFE = if drop s" E-FS-PATH-UNSAFE" exit then
   dup E-FS-CAPACITY = if drop s" E-FS-CAPACITY" exit then
   throw ;

;package
