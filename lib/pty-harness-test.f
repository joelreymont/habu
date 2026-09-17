\ pty-harness-test.f - the harness buffer's claims, without a child.
\
\ Most cases here feed bytes through ROOM$ / TOOK, the same append the fd readers
\ use, so the compaction, the span search and the never-seen facts are exercised
\ where they can be driven exactly. The last one is the spawn's own abort path,
\ which needs a real pair. The rest of the pty half - the waits and both barrier
\ shapes against a live child - is test/proc-pty.f's.
\
\ Run: bin/hb --load lib/pty-harness-test.f
require lib/test.f
require lib/test/outcome.f
require lib/string.f
require lib/pty-harness.f

package PTY-HARNESS-TEST

using PTY-HARNESS

$400 constant FEED-LIMIT           \ chunks one fill loop may feed before giving up
$1F4 constant WEDGE-MS             \ the wedge case's own budget, short enough to watch
$BB8 constant WEDGE-SLACK-MS       \ what a loaded box may add to it before the claim fails

create FILLER 64 allot
variable HIT                       \ did the loop reach the case it was feeding for?

: FILLER$ ( -- ptr u8 n )   FILLER 64 ;
: MISSING$ ( -- ptr u8 n )  s" /nonexistent/hb-is-not-here" ;
: WEDGE$ ( -- ptr u8 n )    s" /usr/bin/yes" ;   \ writes forever, exits never
: MARK$ ( -- ptr u8 n )     s" edge-marker" ;
: GONE$ ( -- ptr u8 n )     s" dropped-marker" ;
: ABSENT$ ( -- ptr u8 n )   s" never-fed-marker" ;


: FILLER! ( -- )                   \ 64 bytes no marker below can match
   64 0 ?do [char] . FILLER i + c! loop ;


\ The append the fd readers take: compacted room, the bytes, then the commit.
: FEED ( ptr u8 n -- ) {: a:ptr u:n :}
   ROOM$ {: room:ptr cap:n :}
   u cap > if E-PTY-CAPACITY throw then
   a room u BYTE-COPY
   u TOOK ;


: BUF-LEN ( -- n )
   BUF$ {: a:ptr u:n :} u ;


\ One filler chunk, and whether appending it compacted the buffer.
: FEED-FILLER ( -- bool )
   BUF-LEN {: before:n :}
   FILLER$ FEED
   BUF-LEN before < ;


: FILL-TO-COMPACTION ( -- )        \ feed filler until the buffer keeps only its tail
   false HIT !
   FEED-LIMIT 0 ?do
      FEED-FILLER if true HIT ! leave then
   loop ;


: CASE-APPEND ( -- )
   BUF-CLEAR
   s" habu> " FEED
   s" the buffer holds what was fed" T-LABEL
   BUF$ s" habu> " STR= TTRUE
   s" and a second feed appends behind it" T-LABEL
   s" 42" FEED
   s" habu> 42" IN-BUF? TTRUE
   s" text nobody fed is not in it" T-LABEL
   ABSENT$ IN-BUF? 0= TTRUE ;


: CASE-FIND-FROM ( -- )
   BUF-CLEAR
   s" abcabc" FEED
   s" the first occurrence is at or after `from`" T-LABEL
   0 s" bc" FIND-FROM 1 T=
   2 s" bc" FIND-FROM 4 T=
   s" a `from` past the last one finds nothing" T-LABEL
   5 s" bc" FIND-FROM -1 T=
   s" and a `from` past the buffer is refused, not read" T-LABEL
   $100 s" bc" FIND-FROM -1 T=
   s" text that is not there is -1" T-LABEL
   0 s" zz" FIND-FROM -1 T= ;


: CASE-ORDER ( -- )
   BUF-CLEAR
   s" 42" FEED
   s" habu> " FEED
   s" the tail after the head's end is found" T-LABEL
   s" 42" s" habu> " AFTER? TTRUE
   s" the same two in the wrong order are not" T-LABEL
   s" habu> " s" 42" AFTER? 0= TTRUE
   s" a head that is not there refuses the pair" T-LABEL
   ABSENT$ s" habu> " AFTER? 0= TTRUE
   s" and one marker twice is an ordered pair of its own" T-LABEL
   s" 42" FEED
   s" 42" s" 42" AFTER? TTRUE ;


\ The defect the watch exists for: the compaction drops the bytes a buffer scan
\ would need, and the scan then answers "absent" about text the child did print.
: CASE-WATCH-SURVIVES ( -- )
   WATCH-RESET
   BUF-CLEAR
   GONE$ WATCH+ {: gone:watch :}
   ABSENT$ WATCH+ {: absent:watch :}
   GONE$ FEED
   s" the fed text is in the buffer and the watch has it" T-LABEL
   GONE$ IN-BUF? TTRUE
   gone NEVER-SEEN? 0= TTRUE
   FILL-TO-COMPACTION
   s" the fill reached a compaction" T-LABEL
   HIT @ TTRUE
   s" which dropped the bytes a buffer scan needed" T-LABEL
   GONE$ IN-BUF? 0= TTRUE
   s" but not the fact that they arrived" T-LABEL
   gone NEVER-SEEN? 0= TTRUE
   s" while text nobody fed is still never seen" T-LABEL
   absent NEVER-SEEN? TTRUE
   s" and clearing the buffer does not clear the fact" T-LABEL
   BUF-CLEAR
   gone NEVER-SEEN? 0= TTRUE ;


\ A marker split across two appends is one the watch still sees, because each
\ append is scanned from far enough back to cover a straddling match.
: CASE-WATCH-SPLIT ( -- )
   WATCH-RESET
   BUF-CLEAR
   s" split-here" WATCH+ {: split:watch :}
   s" split" FEED
   s" half of it is not the marker" T-LABEL
   split NEVER-SEEN? TTRUE
   s" -here" FEED
   s" both halves are" T-LABEL
   split NEVER-SEEN? 0= TTRUE ;


\ What KEEP-TAIL is for: a full buffer keeps its most recent bytes and goes on
\ reading. Without it the next read asks for zero bytes, is handed zero back,
\ and the harness is deaf from there on.
: CASE-KEEP-TAIL ( -- )
   WATCH-RESET
   BUF-CLEAR
   GONE$ FEED
   FILL-TO-COMPACTION
   s" the fill reached a compaction" T-LABEL
   HIT @ TTRUE
   s" which kept the tail and dropped the head" T-LABEL
   GONE$ IN-BUF? 0= TTRUE
   BUF-LEN FEED-LIMIT 64 * < TTRUE
   s" a full buffer still has room to read into" T-LABEL
   ROOM$ {: room:ptr cap:n :} cap 0 > TTRUE
   s" and what arrives after it is found, not swallowed" T-LABEL
   MARK$ FEED
   MARK$ IN-BUF? TTRUE ;


: CASE-NO-WINDOW ( -- )
   BUF-CLEAR
   s" no barrier, no absence claim" T-LABEL
   ABSENT$ WINDOW-ABSENT? 0= TTRUE ;


: FILL-WATCHES ( -- )              \ more registrations than the table holds
   FEED-LIMIT 0 ?do MARK$ WATCH+ drop loop ;


: CASE-WATCH-REFUSALS ( -- )
   WATCH-RESET
   s" an empty needle is refused" T-LABEL
   [: s" " WATCH+ drop ;] catch 0 <> TTRUE
   s" and so is a needle longer than the pool holds" T-LABEL
   [: FILLER 65 WATCH+ drop ;] catch 0 <> TTRUE
   s" and the table itself is bounded" T-LABEL
   [: FILL-WATCHES ;] catch 0 <> TTRUE ;


: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" then ;


\ Only the editor's own prompt proves the child holds the terminal raw, and only
\ then is ^D a key rather than a canonical end of file (test/proc-pty.f
\ PTY-EDITOR-READY has the whole reasoning).
: STOP-CHILD ( -- )
   BUF-CLEAR
   127 SEND-BYTE
   s" habu> " WAIT-FOR TTRUE
   4 SEND-BYTE
   REAP 0 T-OUTCOME-EXITED=
   CLOSE-MASTER ;


\ A spawn that throws must leave neither end of its pair open: the master cell is
\ what says "a child is running", so a leaked one refuses every later spawn.
: CASE-SPAWN-ABORT ( -- )
   BUF-CLEAR
   s" a spawn of a path that does not exist throws the spawn's own error" T-LABEL
   [: MISSING$ SPAWN-ON-PTY ;] catch E-PROC-SPAWN T=
   s" and the next one is refused by nothing it left behind" T-LABEL
   [: MISSING$ SPAWN-ON-PTY ;] catch E-PROC-SPAWN T=
   s" so a real child still reaches its prompt and exits" T-LABEL
   HB$ SPAWN-ON-PTY
   s" habu> " WAIT-FOR TTRUE
   STOP-CHILD ;


\ A child that writes into the terminal faster than anyone empties it and never
\ exits. Reaped by waiting alone, the parent sat in do_wait for 5 m 34 s with the
\ child blocked in write() (dot habu-bound-the-pty-7771d0fb); the bounded reap
\ kills it on the clock and answers the timeout outcome, which reds a case that
\ wanted a clean exit. The elapsed claim is the one that says "instead of
\ hanging": without the bound this case does not return at all.
: CASE-WEDGE-REAP ( -- )
   BUF-CLEAR
   WEDGE$ SPAWN-ON-PTY
   s" y" WAIT-FOR TTRUE                    \ the child is writing at the terminal
   mono-ns {: t0:n :}
   s" a child that never stops writing is reaped as a timeout" T-LABEL
   WEDGE-MS REAP-WITHIN T-OUTCOME-TIMEOUT
   s" and the reap returned on the clock, not on the child" T-LABEL
   mono-ns t0 - PROC-NS-PER-MS / WEDGE-MS WEDGE-SLACK-MS + < TTRUE
   CLOSE-MASTER ;


: BODY ( -- )
   FILLER!
   CASE-APPEND
   CASE-FIND-FROM
   CASE-ORDER
   CASE-WATCH-SURVIVES
   CASE-WATCH-SPLIT
   CASE-KEEP-TAIL
   CASE-NO-WINDOW
   CASE-WATCH-REFUSALS
   CASE-SPAWN-ABORT
   CASE-WEDGE-REAP ;


: RUN ( -- )
   T-RESET
   BODY
   WATCH-RESET
   BUF-CLEAR
   T-REPORT
   s" pty-harness-test: ok" type cr ;

RUN

;using

;package
