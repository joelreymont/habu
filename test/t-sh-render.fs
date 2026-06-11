\ t-sh-render.fs — the standalone renders inferred stack types back to readable form
\ (the 'render' half of the native sigparse/checker): type vars -> canonical letters
\ a,b,c (bottom-to-top), int -> n. Run: gforth test/t-sh-render.fs -e bye
require sh-driver.fs
: RENDER-OUT ( -- a u )
   0 CL !
   s" selfhost/checker.fs"     slurp-file +B   s"  " +B
   s" selfhost/render.fs"      slurp-file +B   s"  " +B
   s" selfhost/render-demo.fs" slurp-file +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
T{ RENDER-OUT s" a a |a b |a b a |a b c |n |" compare 0= -> true }T
