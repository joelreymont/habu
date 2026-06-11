\ sh-driver.fs — shared driver for the t-sh-* standalone tests: one program buffer
\ (CBUF/+B) for concatenating selfhost sources, +F to append a file, RUN-RC to run an
\ emitted binary and return its exit code. Each test keeps its own GEN; this kills the
\ per-file CBUF/+B copy-paste (and the redefinition warnings in the combined gate).
require nf.fs
require tester.fs
create CBUF 524288 allot   variable CL
: +B {: a u -- }  a  CBUF CL @ +  u move  u CL +! ;
: +F ( a u -- )  slurp-file +B  s"  " +B ;          \ append a source file + separator
\ run `path`, capture $? via a shell echo, parse it back
create RCMD 256 allot  variable RCL
: +C {: a u -- }  a RCMD RCL @ + u move  u RCL +! ;
: RUN-RC ( a u -- n )  0 RCL !  +C  s" ; echo $? > /tmp/sh-rc" +C
   RCMD RCL @ system  s" /tmp/sh-rc" slurp-file s>number? 2drop ;
