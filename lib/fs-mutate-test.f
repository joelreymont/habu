\ fs-mutate-test.f - focused tests for lib/fs-mutate.f.
\ Run: lib/fs-mutate-test.sh

0 set-check

: FMT-REMOVE-PATH ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: FMT-RENAME-SRC ( -- ptr u8 n )
   1 SCRIPT-ARGV$ ;

: FMT-RENAME-DST ( -- ptr u8 n )
   2 SCRIPT-ARGV$ ;

: FMT-CHMOD-PATH ( -- ptr u8 n )
   3 SCRIPT-ARGV$ ;

: FMT-REMOVE-MISSING ( -- )
   s" no-such-habu-fs-remove-file" REMOVE-FILE ;

: FMT-RENAME-MISSING ( -- )
   s" no-such-habu-fs-rename-file" FMT-RENAME-DST RENAME-FILE ;

: FMT-CHMOD-MISSING ( -- )
   s" no-such-habu-fs-chmod-file" CHMOD-X ;

: FS-MUTATE-TEST-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 4 < if s" fs-mutate-test: missing fixture args" T-EX-FAIL die then
   FMT-REMOVE-PATH FILE? TTRUE
   FMT-REMOVE-PATH REMOVE-FILE
   FMT-REMOVE-PATH EXISTS? TFALSE
   FMT-RENAME-SRC FILE? TTRUE
   FMT-RENAME-DST EXISTS? TFALSE
   FMT-RENAME-SRC FMT-RENAME-DST RENAME-FILE
   FMT-RENAME-SRC EXISTS? TFALSE
   FMT-RENAME-DST FILE? TTRUE
   FMT-CHMOD-PATH CHMOD-X
   FMT-CHMOD-PATH STAT-MODE FS-MUT-MODE-EXEC and FS-MUT-MODE-EXEC = TTRUE
   ['] FMT-REMOVE-MISSING E-FS-IO TTHROWS
   ['] FMT-RENAME-MISSING E-FS-IO TTHROWS
   ['] FMT-CHMOD-MISSING E-FS-STAT TTHROWS
   T-REPORT
   s" fs-mutate-test: ok" type cr ;

FS-MUTATE-TEST-MAIN
