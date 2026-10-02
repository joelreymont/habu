\ The candidate is already an AOT-restored engine. Its first native definition
\ reinstalls the compiler's tape callbacks, exercising the saved `is` code.
\ Public image capture then carries an application's installer across two boots.
\ Each restored process also compiles fresh JIT and native words: live compiler
\ literals can be allocated before the first source-prefix dictionary record.
\ The image also holds the interpret loop written in Habu, loaded at tier 1, and
\ a defer that loop compiled: each boot calls it through its relocated cell and
\ reassigns it with `is`, which finds the cell through the trailer the capture
\ relocates with the defer's record (src/habu/aot-capture.f ACAP-DEFER-SITE).
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f

package DEFER-IMAGE-TEST

$10000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot variable ROOT-U
create IMAGE-BUF FS-PATH-CAP allot variable IMAGE-U
create SECOND-BUF FS-PATH-CAP allot variable SECOND-U
64 BUFFER: VOCAB-HEX

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: SECOND$ ( -- ptr u8 n ) SECOND-BUF SECOND-U @ ;

: PREPARE ( -- )
   s" native-defer-image" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" first" IMAGE-BUF JOIN-PATH IMAGE-U !
   ROOT$ s" second" SECOND-BUF JOIN-PATH SECOND-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         erru LEN>N 0<> if
            OUT outu LEN>N type ERR erru LEN>N type
         then
         erru LEN>N 0 T=
         outu LEN>N
      ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         OUT outu LEN>N type ERR erru LEN>N type
         rc RC>N 0 T=
         rc RC>N throw
      ENDOF
   ;MATCH ;

: ENVIRONMENT ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: BUILD ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/compiler/native/compiler.f\n1 set-tier\n: DEFER-IMAGE-NATIVE ( n -- n ) 1+ ;\n17 DEFER-IMAGE-NATIVE . cr\n0 set-tier\nrequire src/habu/app-image.f\nrequire test/native-defer-image-subject.f\nrequire src/habu/interpret.f\npackage DEFER-IMAGE-LOOP\ns\q defer ACTION ( n -- n ) : TRIPLE ( n -- n ) 3 * ;\q OUTER:INTERPRET\npublic\ns\q : INSTALL ( [ n -- n ] -- ) is ACTION ; : CALL ( n -- n ) ACTION ;\q OUTER:INTERPRET\nprivate\ns\q : FIRST ( -- ) [: TRIPLE ;] INSTALL ; FIRST\q OUTER:INTERPRET\n;package\nrequire test/compiler/native-opcode-image.f\nNATIVE-OPCODE-IMAGE:PRINT\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT {: outu:n :}
   S\" 18\n\ndefer-source: ok\n" {: prefix:ptr pu:n :}
   OUT pu prefix pu T$=
   outu pu - 65 T=
   OUT pu + VOCAB-HEX 64 BYTE-COPY
   IMAGE$ EXECUTABLE? TTRUE ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- n )
   {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT ;

: CHECK-IMAGE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n want:ptr wantu:n :}
   ENVIRONMENT
   path pathu
   S\" require src/compiler/native/compiler.f\n1 set-tier\n17 DEFER-IMAGE-SUBJECT:CALL . cr\nDEFER-IMAGE-SUBJECT:CHECK-REASSIGNMENT\n0 set-tier\n: DEFER-IMAGE-JIT ( n -- n ) 5 + ;\n17 DEFER-IMAGE-JIT . cr\n1 set-tier\n: DEFER-IMAGE-FRESH ( n -- n ) 7 + ;\n: DEFER-IMAGE-INSTALL ( -- ) ['] DEFER-IMAGE-FRESH DEFER-IMAGE-SUBJECT:INSTALL ;\nDEFER-IMAGE-INSTALL\n17 DEFER-IMAGE-SUBJECT:CALL . cr\n17 DEFER-IMAGE-LOOP:CALL . cr\npackage DEFER-IMAGE-CHECK\n: FRESH ( n -- n ) 100 + ;\n: REINSTALL ( -- ) ['] FRESH DEFER-IMAGE-LOOP:INSTALL ;\nREINSTALL\n;package\n17 DEFER-IMAGE-LOOP:CALL . cr\nNATIVE-OPCODE-IMAGE:PRINT\n" RUN-INPUT
   {: outu:n :}
   OUT wantu want wantu T$=
   outu wantu - 65 T=
   s" every restored opcode name, rule, renderer and schema matches its source" T-LABEL
   OUT wantu + 64 VOCAB-HEX 64 T$= ;

: RECAPTURE ( -- )
   ENVIRONMENT
   s" --" >LEN PROC-ARGV+
   SECOND$ >LEN PROC-ARGV+
   IMAGE$
   S\" require src/compiler/native/compiler.f\n1 set-tier\nDEFER-IMAGE-SUBJECT:CHECK-REASSIGNMENT\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" RUN-INPUT
   0 T=
   SECOND$ EXECUTABLE? TTRUE ;

: BODY ( -- )
   PREPARE
   BUILD
   IMAGE$ S\" 18\n\n22\n\n24\n\n51\n\n117\n\n" CHECK-IMAGE
   RECAPTURE
   SECOND$ S\" 34\n\n22\n\n24\n\n51\n\n117\n\n" CHECK-IMAGE
   s" restored opcode vocabulary: " type VOCAB-HEX 64 type cr ;

: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0<> if code throw then
   T-REPORT ;

RUN
;package
