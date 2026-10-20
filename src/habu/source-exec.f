\ Observation of original interpreter actions during one owned source range.
require src/lib/errors.f

package SOURCE-EXEC
public
1 constant NUMBER
2 constant STRING
3 constant CHARACTER
4 constant TICK
5 constant WORD
9 constant SOURCE-END

: RUN ( n ptr u8 n [ n n n n ptr u8 n -- n bool ] [ n n n -- ] [ -- ] -- n )
   source-exec-run ;
: LIVE? ( -- bool ) source-exec-live? ;
: BOUNDARY? ( -- bool ) source-exec-boundary? ;
: LOADER-CUT? ( -- bool ) source-exec-loader-cut? ;
TRUSTED: POSITION ( -- n bool ) source-exec-position ;
: HANDLER ( -- n ) source-exec-handler ;
;package
