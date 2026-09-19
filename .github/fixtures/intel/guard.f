package INTELGUARD
s" GUARD-ARMED" type cr
\ The sealed friend latch begins at DATA + $20 in the pinned recovery ABI.
0 1 data-base $20 + atomic-cas drop
s" GUARD-LEAKED" type cr
;package
