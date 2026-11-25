\ carried-good.f - carried-bad.f with the carried file required.
require src/lib/byte-buffer.f
package RQC-CARRIED-GOOD
public
: TOUCH ( -- bool ) 5 BUF:N>BLEN drop true ;
;package
