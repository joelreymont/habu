\ process-cwd-test.f - focused tests for lib/process-cwd.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/process-cwd.f lib/process-cwd-test.f

4096 constant PCT-CAP
6 constant PCT-OK-U
PROC-PATHZ-CAP 1 + constant PCT-LONG-U

create PCT-ROOT FS-PATH-CAP allot
create PCT-REL FS-PATH-CAP allot
create PCT-OUT PCT-CAP allot
create PCT-ERR PCT-CAP allot
create PCT-LONG PCT-LONG-U allot
create PCT-OK 99 c, 119 c, 100 c, 45 c, 111 c, 107 c,

variable PCT-ROOT-U
variable PCT-REL-U
variable PCT-I

: PCT-LONG! ( -- )
   0 PCT-I !
   begin PCT-I @ PCT-LONG-U < while
      97 PCT-LONG PCT-I @ + c!
      PCT-I @ 1+ PCT-I !
   repeat ;

: PCT-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PCT-SETUP-ROOT ( -- )
   CLEANUP-RESET
   s" hb-process-cwd" TMPDIR-MKDIR {: root:ptr rootu :}
   root PCT-ROOT rootu BYTE-COPY
   rootu PCT-ROOT-U !
   PCT-ROOT PCT-ROOT-U @ CLEANUP-TREE+
   PCT-ROOT PCT-ROOT-U @ s" rel.txt" PCT-REL JOIN-PATH PCT-REL-U !
   PCT-REL PCT-REL-U @ PCT-OK PCT-OK-U WRITE-ALL ;

: PCT-RUN-CAT-CWD ( -- )
   PCT-SETUP-ROOT
   PCT-RESET
   s" rel.txt" PROC-ARGV+
   s" /bin/cat" PCT-ROOT PCT-ROOT-U @ PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000
   RUN-ARGV-ENV-CWD-CAPTURE
   0 T= 0 T= PCT-OK-U T=
   PCT-OUT PCT-OK-U PCT-OK PCT-OK-U T$=
   s" FILEMAP.md" FILE? TTRUE
   s" rel.txt" FILE? TFALSE
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T=
   CLEANUP-RUN ;

: PCT-RUN-STDIN-CAPTURE ( -- )
   PCT-SETUP-ROOT
   PCT-RESET
   s" /bin/cat" PCT-ROOT PCT-ROOT-U @ PCT-OK PCT-OK-U PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000
   RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   0 T= 0 T= PCT-OK-U T=
   PCT-OUT PCT-OK-U PCT-OK PCT-OK-U T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T=
   CLEANUP-RUN ;

: PCT-MISSING-CWD ( -- )
   PCT-RESET
   s" rel.txt" PROC-ARGV+
   s" /bin/cat" s" /no/such/habu-process-cwd-test" -1 -1 -1
   SPAWN-ARGV-ENV-CWD-IO drop ;

: PCT-RUN-MISSING-CWD ( -- )
   ['] PCT-MISSING-CWD E-PROC-SPAWN TTHROWS
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PCT-CWD-TOO-LONG ( -- )
   PCT-LONG!
   PCT-LONG PCT-LONG-U PROC-CWDZ drop ;

: PROCESS-CWD-TEST-MAIN ( -- )
   T-RESET
   PCT-RUN-CAT-CWD
   PCT-RUN-STDIN-CAPTURE
   PCT-RUN-MISSING-CWD
   ['] PCT-CWD-TOO-LONG E-PROC-OUTPUT TTHROWS
   T-REPORT
   s" process-cwd-test: ok" type cr ;

PROCESS-CWD-TEST-MAIN
