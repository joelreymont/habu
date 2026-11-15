\ arena.f — the ARENA definer.
\
\ `ARENA NAME t` declares two words that make zeroed records of `t` in an arena
\ the caller holds (src/lib/arena.f): NAME-NEW ( ptr ARENA:arena -- ptr t ) and
\ NAME-NEW-DISPOSE ( ptr ARENA:arena [ ptr t -- ] -- ptr t ), whose action runs
\ once when the arena is reset or released. Each is a typed child whose body
\ calls its routine, NEW or NEW-DISPOSE, with the child's one data cell: the
\ declared type's width in bytes.

package ARENA-DECL

private

: NEW ( ptr ARENA:arena ptr n -- ptr u8 ) @ ARENA:ALLOC drop ;

: NEW-DISPOSE ( ptr ARENA:arena [ ptr u8 -- ] ptr n -- ptr u8 )
   @ swap ARENA:ALLOC-DISPOSE drop ;

CAST: AS-CHILD-RUN ( n -- [ ptr u8 n n n ptr u8 n -- n ] )
CAST: AS-ENTRY ( [ R -- S ] -- n )

: CHILD ( ptr u8 n n n ptr u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

\ Makes NAME+SFX of SHAPE over the routine's ENTRY, and lays its width cell.
: MAKE ( ptr u8 n ptr u8 n n n ptr u8 n n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n shape:n entry:n type:ptr typeu:n width:n :}
   name nameu sfx sfxu LBUF-SUFFIXED shape entry type typeu CHILD drop
   width , ;

\ Whether neither of NAME's words is taken in the current section; a taken one
\ is rejected at NAME.
: FREE? ( ptr u8 n -- bool )
   {: name:ptr nameu:n :}
   name nameu s" -NEW" LBUF-SUFFIX-OK? dup 0= if exit then drop
   name nameu s" -NEW-DISPOSE" LBUF-SUFFIX-OK? ;

\ The open package's wordlist for the section other than MODE's.
: OTHER-WID ( n -- n ) {: mode:n :}
   mode CHECKER-PACKAGE-PUBLIC = if PKG-PRI-CELL else PKG-PUB-CELL then
   data-base + @ ;

\ NAME+SFX taken in the wordlist WID is rejected at NAME.
: OTHER-FREE ( ptr u8 n ptr u8 n n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n wid:n :}
   name nameu sfx sfxu LBUF-SUFFIXED {: a:ptr u:n :}
   a u wid search-wl 0 <> if a u name nameu CHECK:REJECT-DUPLICATE then ;

\ TAIL+SFX taken in Q's private section, for NAME Q:TAIL, is rejected at
\ NAME.
: PRIVATE-FREE ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   name nameu CHECKER-QUALIFIED? drop
   CHECKER-QPKG$ XREF-PRIVATE-WL {: wid:n :}
   wid 0= if exit then
   CHECKER-QTAIL$ sfx sfxu LBUF-SUFFIXED wid search-wl 0= if exit then
   name nameu sfx sfxu LBUF-SUFFIXED name nameu CHECK:REJECT-DUPLICATE ;

\ NAME's words taken in the section the declaration does not write are
\ rejected at NAME: a qualified Q:TAIL writes Q's public one.
: OTHERS-FREE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CHECKER-QUALIFIED? if
      name nameu s" -NEW" PRIVATE-FREE
      name nameu s" -NEW-DISPOSE" PRIVATE-FREE exit
   then
   CHECKER-AUTH-PACKAGE-MODE@ {: mode:n :}
   mode CHECKER-PACKAGE-NONE = if exit then
   mode OTHER-WID {: wid:n :}
   name nameu s" -NEW" wid OTHER-FREE
   name nameu s" -NEW-DISPOSE" wid OTHER-FREE ;

public

\ Both words must be free in both sections of the package they land in: its
\ bare calls resolve private-first.
: RUN ( -- )
   SRCLOC:TOKEN$ {: run:ptr runu:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if
      s" ARENA" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if exit then
   name nameu FREE? 0= if exit then
   name nameu OTHERS-FREE
   type typeu CHECKER-ARENA-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REJECT exit
   then {: width:n :}
   name nameu s" -NEW" CHILD-ABI:ARENA-NEW ['] NEW AS-ENTRY
   type typeu width MAKE
   name nameu s" -NEW-DISPOSE" CHILD-ABI:ARENA-DISPOSE ['] NEW-DISPOSE AS-ENTRY
   type typeu width MAKE ;

;package

: ARENA ( -- ) ARENA-DECL:RUN ;

PRIM: ARENA PRIM;
