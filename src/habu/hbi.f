\ hbi.f — driver: emit the stdin-program engine to /tmp/hbi-got. The output
\ binary reads its program from stdin at startup:  echo '1 2 + .' | bin/hbi
\ Swapped in for stage2.f by `srclist.sh hbi` (see tools/build.sh).
: GO
   1 STDIN? !
   0 0 EMIT-FORTH
   BUILD-IMAGE
   s" hbi" SET-SIGID  CODESIG2
   s" /tmp/hbi-got" PATH0  1537 493 open  dup MBUF MLEN @ write drop  close ;
GO
