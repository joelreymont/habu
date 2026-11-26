\ arena.f — the ARENA definer.
\
\ `ARENA NAME t` declares two words that make zeroed records of `t` in an arena
\ the caller holds (src/lib/arena.f): NAME-NEW ( ^ARENA:arena -- ^t ) and
\ NAME-NEW-DISPOSE ( ^ARENA:arena [ ^t -- ] -- ^t ), whose action runs
\ once when the arena is reset or released. Each is a typed child whose body
\ calls its routine, NEW or NEW-DISPOSE, with the child's one data cell: the
\ declared type's width in bytes.

package ARENA-DECL
EXPORT BOOT:data-base
EXPORT CHECK:REJECT-DUPLICATE

private

: NEW ( ^ARENA:arena ^n -- ^u8 ) @ ARENA:ALLOC drop ;

: NEW-DISPOSE ( ^ARENA:arena [ ^u8 -- ] ^n -- ^u8 )
   @ swap ARENA:ALLOC-DISPOSE drop ;

CAST: AS-CHILD-RUN ( n -- [ str n n str -- n ] )
CAST: AS-ENTRY ( [ R -- S ] -- n )

: CHILD ( str n n str -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

\ Makes NAME+SFX of SHAPE over the routine's ENTRY, and lays its width cell.
: MAKE ( str str n n str n -- )
   {: name:str sfx:str shape:n entry:n type:str width:n :}
   name sfx LBUF:LBUF-SUFFIXED shape entry type CHILD drop
   width , ;

\ Whether neither of NAME's words is taken in the current section; a taken one
\ is rejected at NAME.
: FREE? ( str -- bool )
   {: name:str :}
   name s" -NEW" LBUF:LBUF-SUFFIX-OK? dup 0= if exit then drop
   name s" -NEW-DISPOSE" LBUF:LBUF-SUFFIX-OK? ;

\ The open package's wordlist for the section other than MODE's.
: OTHER-WID ( n -- n ) {: mode:n :}
   mode CHECKER-PACKAGE-PUBLIC = if PKG-PRI-CELL else PKG-PUB-CELL then
   data-base + @ ;

\ NAME+SFX taken in the wordlist WID is rejected at NAME.
: OTHER-FREE ( str str n -- )
   {: name:str sfx:str wid:n :}
   name sfx LBUF:LBUF-SUFFIXED {: full:str :}
   full wid search-wl 0 <> if full name REJECT-DUPLICATE then ;

EXPORT CHECK:CHECKER-ARENA-INFO
EXPORT CHECK:CHECKER-AUTH-PACKAGE-MODE@
EXPORT CHECK:CHECKER-LBUF-NAME-OK?
EXPORT CHECK:CHECKER-QPKG$
EXPORT CHECK:CHECKER-QTAIL$
EXPORT CHECK:CHECKER-QUALIFIED?
EXPORT CHECK:CHECKER-STORAGE-REJECT
EXPORT CHECK:CHECKER-DERIVED-TYPE-REJECT
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:STG-NO-NAME
\ TAIL+SFX taken in Q's private section, for NAME Q:TAIL, is rejected at
\ NAME.
: PRIVATE-FREE ( str str -- )
   {: name:str sfx:str :}
   name CHECKER-QUALIFIED? drop
   CHECKER-QPKG$ XREF-PRIVATE-WL {: wid:n :}
   wid 0= if exit then
   CHECKER-QTAIL$ sfx LBUF:LBUF-SUFFIXED wid search-wl 0= if exit then
   name sfx LBUF:LBUF-SUFFIXED name REJECT-DUPLICATE ;

\ NAME's words taken in the section the declaration does not write are
\ rejected at NAME: a qualified Q:TAIL writes Q's public one.
: OTHERS-FREE ( str -- ) {: name:str :}
   name CHECKER-QUALIFIED? if
      name s" -NEW" PRIVATE-FREE
      name s" -NEW-DISPOSE" PRIVATE-FREE exit
   then
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   mode CHECKER-PACKAGE-NONE =  mode CHECKER-PACKAGE-GLOBAL =  or if exit then
   mode OTHER-WID {: wid:n :}
   name s" -NEW" wid OTHER-FREE
   name s" -NEW-DISPOSE" wid OTHER-FREE ;

\ Both words must be free in both sections of the package they land in: its
\ bare calls resolve private-first.
: RUN ( -- )
   SRCLOC:TOKEN$ >str {: run:str :}
   LBUF:STORAGE-LINE-TOKEN {: name:str :}
   name STR:LEN 0= if
      s" ARENA" run STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run name REJECTION:NOT-A-NAME
   LBUF:STORAGE-PARSE-TYPE {: type:str :}
   name CHECKER-LBUF-NAME-OK? 0= if exit then
   name LBUF:SEAL-NAME
   name FREE? 0= if exit then
   name OTHERS-FREE
   type CHECKER-ARENA-INFO 0= if
      drop
      name s" -NEW" s" -NEW-DISPOSE" type
      CHECKER-DERIVED-TYPE-REJECT exit
   then {: width:n :}
   name s" -NEW" CHILD-ABI:ARENA-NEW ['] NEW AS-ENTRY
   type width MAKE
   name s" -NEW-DISPOSE" CHILD-ABI:ARENA-DISPOSE ['] NEW-DISPOSE AS-ENTRY
   type width MAKE ;

global

: ARENA ( -- ) RUN ;
INTRINSIC-DECLARATION INTRINSIC

;package
