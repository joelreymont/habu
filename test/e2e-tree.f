split-str
\ e2e-tree.f - a scratch source tree for an end-to-end row: the checkout's src/
\ and tools/ linked in, a test/ directory for the sources the row copies, and
\ children run in it with their stdout, stderr and exit code kept for the
\ row's asserts and logs. Rows share it; a row passes the engine each child runs,
\ the engine under test (ENGINE-CANDIDATE:PATH$) or an image it saved in the tree.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/process.f
require test/pcap-result.f

package E2E-TREE
private

$8000 constant IO-CAP

create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create TARGET PATH-CAP allot variable TARGET-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC-CELL

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC-CELL ! ERR-U ! OUT-U ! ;

public

: ROOT$ ( -- str ) ROOT ROOT-U @ ;
: OUT$ ( -- str ) OUT OUT-U @ ;
: ERR$ ( -- str ) ERR ERR-U @ ;
: RC@ ( -- n ) RC-CELL @ ;

: AT-A ( str -- str ) {: rel:^ relu:n :}
   ROOT$ rel relu PATH FS:JOIN-PATH PATH swap ;

: LINK ( ^u8 n -- ) {: rel:^ relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET FS:JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A FS:MAKE-SYMLINK ;

: COPY-TEST ( str -- ) {: rel:^ relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET FS:JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A FS:COPY-FILE-STREAM ;

: TREE ( str -- )
   FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" tools" LINK
   s" test" AT-A FS:MAKE-DIRS ;

: ARGS ( str -- ) {: source:^ size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" PROC-ARGV+
   source size PROC-ARGV+ ;

: RUN-ON ( str str -- )
   {: engine:^ engineu:n cwd:^ cwdu:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu cwd cwdu
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: RUN-INPUT ( str str -- )
   {: engine:^ engineu:n input:^ inputu:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu ROOT$ input inputu
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( str str -- )
   {: outpath:^ outu:n errpath:^ erru:n :}
   outpath outu AT-A OUT$ FS:WRITE-ALL
   errpath erru AT-A ERR$ FS:WRITE-ALL ;

: NEED-OK ( -- )
   RC@ 0<> if OUT$ type ERR$ type then
   RC@ 0 T= ;

;package
