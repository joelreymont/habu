\ vmsize.f - the process's total mapped size, for leak assertions.
\
\ A growth path that installs a new mapping and forgets the old one leaks it, and
\ nothing in the answer it returns shows that. The signal is the process's own
\ mapped extent: field 1 of /proc/self/statm is the total program size in PAGES,
\ counted the moment an mmap is made and whether or not a byte is touched, so a
\ forgotten mapping is visible immediately. Pages, not bytes, because the page
\ size is not ours to assume; assert on RATIOS of one measured growth.

require lib/errors.f
require lib/string.f
require lib/adt/option.f
require lib/fs.f

package VMSIZE
private

64 constant READ-CAP
create READ-BUF READ-CAP allot

32 constant SPACE-C

: FIRST-FIELD ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ SPACE-C = if a swap exit then
      1+
   repeat drop a u ;

public

: PAGES ( -- n )
   s" /proc/self/statm" READ-BUF READ-CAP READ-ALL {: u:n :}
   READ-BUF u FIRST-FIELD STR>NUMBER? MATCH option
     none OF E-FS-IO throw ENDOF
     some OF ENDOF
   ;MATCH ;

;package
