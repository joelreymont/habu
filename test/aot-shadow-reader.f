\ aot-shadow-reader.f - test/aot-shadow-capture.f's reader child: READ an
\ artifact, or READ a host and MERGE an artifact into it. A refusal ends the
\ process that reads, so the suite runs this apart and asks for its exit code
\ and sentence. It loads the reader alone: no shadow, window or capture is
\ needed to read an artifact.
\
\ Run from the repository root:
\   bin/hb --load test/aot-shadow-reader.f -- read <artifact>
\   bin/hb --load test/aot-shadow-reader.f -- merge <host> <artifact>

require src/lib/string.f
require src/habu/aot-file.f
require test/aot-shadow-key.f

package AOTSH-READER

64 constant USAGE-RC                \ sysexits EX_USAGE

: MODE? ( ptr u8 n n -- bool ) {: a:ptr u:n argc:n :}
   SCRIPT-ARGC argc = if 0 SCRIPT-ARGV$ a u STR= exit then
   false ;

public

: RUN ( -- )
   AOTSH-KEY:IDENT!
   s" read" 2 MODE? if
      AOTSH-KEY:KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
      s" aot-shadow-reader: read=ok" type cr exit
   then
   s" merge" 3 MODE? if
      AOTSH-KEY:KEY 1 SCRIPT-ARGV$ AOT-FILE:READ
      AOTSH-KEY:KEY 2 SCRIPT-ARGV$ AOT-FILE:MERGE
      s" aot-shadow-reader: merge=ok" type cr exit
   then
   s" aot-shadow-reader: expected read <artifact> or merge <host> <artifact>"
   USAGE-RC die ;

;package

AOTSH-READER:RUN
