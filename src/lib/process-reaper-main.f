\ process-reaper-main.f - a pool's reaper: reads fd 3 until a read answers no
\ bytes or fails, kills its group's other processes and exits 0, writing nothing.

require src/lib/process-tree.f

package PROCESS-REAPER

private

3 constant WATCH-FD
64 constant CHUNK
CHUNK BUFFER: SINK

: DRAIN ( -- )
   begin WATCH-FD SINK CHUNK SPAN:MAKE read 0 > while repeat ;

public

: MAIN ( -- )
   DRAIN
   PROC-TREE:KILL-PEERS ;

;package

PROCESS-REAPER:MAIN
