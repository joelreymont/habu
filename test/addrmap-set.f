\ addrmap-set.f - runs test/addrmap-set-child.f, the address-literal map
\ primitive's contract, in the native build window.
\
\ The child calls addrmap-set through NPUB:ADDRMAP-MARK, a checked word
\ test/reloc-window-prepare.f defines in the reopened owner before the window's
\ seal. The product seals NPUB, so the child runs under
\ test/native-window-owner-child.f on the engine test/whitebox-child.f names.
require src/lib/test.f
require src/lib/fs-mutate.f
require test/whitebox-child.f
require test/window-child-args.f

package ADDRMAP-SET-SUITE

: PREPARE ( -- )
   CLEANUP-RESET
   s" addrmap-set" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   s" test/addrmap-set-child.f" WINDOW-CHILD-ARGS:CHILD
   WINDOW-CHILD-ARGS:CORE WINDOW-CHILD-ARGS:TARGET WINDOW-CHILD-ARGS:HABU
   s" test/reloc-window-prepare.f" WINDOW-CHILD-ARGS:PREPARE ;

: PASS$ ( -- ptr u8 n ) S\" test: ok\naddrmap-set: ok\nwindow: 0\n" ;

: RESULT ( -- ) PASS$ WINDOW-CHILD-ARGS:EXPECT ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
