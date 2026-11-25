\ End-to-end ZIP:OPEN rejection under an unusable TMPDIR. A child engine runs
\ test/zip-temp-source.f with TMPDIR naming a regular file, so the source
\ snapshot cannot be created and OPEN throws FS:MKTEMP's E-FS-DIR with no memory
\ fallback.
\ The child reads its archive path from argv, so it runs spawned and stays
\ unregistered. The process removes the temporary tree at exit.
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/pcap-result.f
require src/lib/process.f

package ZIP-TEMP-E2E

$4000 constant CAP
create ROOT PATH-CAP allot variable ROOT-U
create BAD PATH-CAP allot variable BAD-U
create ARCHIVE PATH-CAP allot variable ARCHIVE-U
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
variable RC

: ROOT$ ( -- str ) ROOT ROOT-U @ >str ;
: BAD$ ( -- str ) BAD BAD-U @ >str ;
: ARCHIVE$ ( -- str ) ARCHIVE ARCHIVE-U @ >str ;

\ BAD is a regular file, so FS:MKTEMP rejects a TMPDIR naming it (E-FS-DIR)
\ and OPEN passes that code on.
: SETUP ( -- )
   s" zip-temp" FS:MKTEMP-DIR {: s:str :}
   s FS:CLEANUP-TREE+
   s STR:DATA ROOT s STR:LEN BYTE-COPY s STR:LEN ROOT-U !
   ROOT$ s" not-a-dir" BAD FS:JOIN-PATH BAD-U !
   ROOT$ s" temp.zip" ARCHIVE FS:JOIN-PATH ARCHIVE-U !
   BAD$ s" x" FS:WRITE-ALL ;

\ TMPDIR is staged before the parent's rows are inherited, and inheriting skips
\ a name already staged, so the child sees exactly one TMPDIR: BAD.
: RUN-CHILD ( -- )
   PROC-ARGV-ENV-RESET
   s" TMPDIR" BAD$ PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" PROC-ARGV+
   s" test/zip-temp-source.f" PROC-ARGV+
   s" --" PROC-ARGV+
   ARCHIVE$ PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-CAPTURE PCAP-RESULT:>N RC ! ERR-U ! OUT-U !
   RC @ 0<> if OUT OUT-U @ >str type ERR ERR-U @ >str type then
   RC @ 0 T=
   OUT OUT-U @ >str s" test: ok" STR:CONTAINS? TTRUE ;


public

: RUN ( -- )
   T-RESET
   SETUP
   RUN-CHILD
   T-REPORT ;

;package

ZIP-TEMP-E2E:RUN
