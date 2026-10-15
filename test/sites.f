\ sites.f - runs test/sites-child.f, SITES over both band representations and
\ over the engine's own maps, in the native build window.
\
\ The child marks and clears the engine's maps through NPUB:CALLMAP-MARK,
\ NPUB:ADDRMAP-MARK and CODE-RECLAIM:MAPS-CLEAR, checked words
\ test/reloc-window-prepare.f defines in the reopened owners before the window's
\ seal. The product seals both owners, so the child runs under
\ test/native-window-owner-child.f on the engine test/whitebox-child.f names.
require src/lib/test.f
require src/lib/fs-mutate.f
require test/whitebox-child.f
require test/window-child-args.f

package SITES-SUITE

: PREPARE ( -- )
   CLEANUP-RESET
   s" sites" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( -- )
   s" test/sites-child.f" WINDOW-CHILD-ARGS:CHILD
   WINDOW-CHILD-ARGS:CORE WINDOW-CHILD-ARGS:TARGET WINDOW-CHILD-ARGS:HABU
   s" test/reloc-window-prepare.f" WINDOW-CHILD-ARGS:PREPARE ;

: PASS$ ( -- ptr u8 n ) S\" test: ok\nsites: ok\nwindow: 0\n" ;

: RESULT ( -- ) PASS$ WINDOW-CHILD-ARGS:EXPECT ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
