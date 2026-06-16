\ t-sh-guards.fs — habu's runtime limits fail LOUDLY, never corrupt: dictionary
\ cap (exit 77 past 2304 words), 17th local (exit 75), and a 1500-deep data stack
\ that the old 2KB region would have overrun. Run: gforth test/t-sh-guards.fs -e bye
require sh-driver.fs
: RC-OF ( a u -- code )  s" /tmp/hb-g-bin" FORTH-EXE
   s" /tmp/hb-g-bin >/dev/null 2>/dev/null; echo $? > /tmp/hb-g-rc" system
   s" /tmp/hb-g-rc" slurp-file s>number? 2drop ;
\ 2500 tiny definitions -> dictionary-full death (cap 2304), not corruption
: MANY ( -- a u )  0 CL !
   2500 0 do  s" : W" +B  i 0 <# #s #> +B  s"  1 ; " +B  loop
   s" 42 ." +B  CBUF CL @ ;
T{ MANY RC-OF -> 77 }T
T{ s" : T {: a b c d e f g h i2 j k l m n o p q :} q . ; 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 T"  RC-OF -> 0 }T   \ 17 locals now FIT (per-group frames)
T{ s" : T {: v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v48 v49 v50 v51 v52 v53 v54 v55 v56 v57 v58 v59 v60 v61 v62 v63 v64 :} 1 . ;"  RC-OF -> 75 }T   \ 65th name overflows LOCNAMES
: DEEP ( -- a u )  0 CL !
   1500 0 do s" 7 " +B loop  1500 0 do s" drop " +B loop  s" 8 ." +B  CBUF CL @ ;
T{ DEEP RC-OF -> 0 }T
