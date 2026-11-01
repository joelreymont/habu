\ clobber-host-epi1-def.f - fixture for a routine ending in a host epilogue
\ just before another entry, called from clobber-host-epi1-call.f, read as
\ text by clobber-lint-test.f (never loaded).

variable LEPI1
variable LEPI1-NEXT

: EPILOGUE ( -- )
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ LEPI1 returns in EPILOGUE, so LEPI1-NEXT's x12 write is not its own.
: EMIT-EPI1 ( -- )
   LEPI1 LABEL@ LBL,
   SP SP 16 SUBI,  30 SP 0 STR,
   EPILOGUE
   LEPI1-NEXT LABEL@ LBL,
   12 0 MOVZ,  RET, ;
