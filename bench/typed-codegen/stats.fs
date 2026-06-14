\ stats.fs — checked codegen microbenchmarks; prints STAT lines for run.sh.
require ../../bootstrap/src/habu.fs
CHECKING-ON? off
require ../../bootstrap/cg/install.fs
CHECKING-ON? on
CODEGEN-ON? on
decimal

create OBUF 65536 allot
variable RPFA  variable RIC  variable RBYTES  variable RRC

: RUN-PFA ( input pfa -- rc )
   swap BUILD-PROGRAM  s" /tmp/habu-typed-codegen" RUN-EXE ;

: REPORT {: input na nu :}
   na nu WORD-PFA dup 0= abort" missing codegen record" RPFA !
   RPFA @ input BUILD-PROGRAM
   #IC @ RIC !
   OBUF ASSEMBLE RBYTES !
   input RPFA @ RUN-PFA RRC !
   s" STAT " type  na nu type  space
   RPFA @ PFA>ARITY .  RPFA @ PFA>EFLAGS .
   RIC @ .  RBYTES @ .  RRC @ .  cr ;

: BOOLCTL ( i64 -- i64 ) dup 0= if drop 7 else 1 + then ;
: ARITHLOOP ( i64 -- i64 ) 0 swap 1+ 1 ?do i 5 + + loop ;
: QUOTCALL ( i64 -- i64 ) [: 3 + ;] execute ;
: PKEEP ( a -- a ) dup drop ;
: POLYHELP ( i64 -- i64 ) PKEEP 9 + ;

0  s" BOOLCTL" REPORT
10 s" ARITHLOOP" REPORT
4  s" QUOTCALL" REPORT
5  s" PKEEP" REPORT
5  s" POLYHELP" REPORT
