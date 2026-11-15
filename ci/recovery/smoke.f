0 set-tier
package CI-T0-SMOKE
private
: RUN ( -- )
   1 2 + 3 <> if -1 throw then
   s" native T0 host started" type cr ;
RUN
;package
