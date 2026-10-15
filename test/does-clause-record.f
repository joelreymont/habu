\ does-clause-record.f - runs test/does-clause-record-child.f, the record a
\ does> clause gets and the branch that aims at it, in the native build window.
\
\ The child marks the relocation maps through NPUB:CALLMAP-MARK and
\ NPUB:ADDRMAP-MARK, checked words test/reloc-window-prepare.f defines in the
\ reopened owner before the window's seal. The product seals NPUB, so the child
\ runs under test/native-window-owner-child.f on the engine
\ test/whitebox-child.f names.
require src/lib/test.f
require src/lib/fs-mutate.f
require test/whitebox-child.f
require test/window-child-args.f

package DOES-CLAUSE-RECORD-SUITE

: PREPARE ( -- )
   CLEANUP-RESET
   s" does-clause-record" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   s" test/does-clause-record-child.f" WINDOW-CHILD-ARGS:CHILD
   WINDOW-CHILD-ARGS:CORE WINDOW-CHILD-ARGS:TARGET WINDOW-CHILD-ARGS:HABU
   s" test/reloc-window-prepare.f" WINDOW-CHILD-ARGS:PREPARE ;

: PASS$ ( -- ptr u8 n ) S\" test: ok\ndoes-clause-record: ok\nwindow: 0\n" ;

: RESULT ( -- ) PASS$ WINDOW-CHILD-ARGS:EXPECT ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
