split-str
\ test-tls-peer.f - the TLS peer of the TLS and HTTPS tests, never a
\ production one: `openssl s_server`, the test's own child on a loopback port,
\ with certificates CERTS makes in the scratch directory SCRATCH makes under
\ TMPDIR. CERTS makes two CAs, ca.pem and other.pem, and server.pem, a
\ certificate for localhost that ca.pem signs. WWW-SERVE starts a server
\ answering every request with its status page; SCRIPT-SERVE one sending in
\ one record each write SCRIPTED makes to its stdin. PORT answers the
\ server's port, HANDSHAKES the handshakes it has finished, and UNSERVE stops
\ it; CLEAN stops it and removes the scratch directory. LISTEN binds a
\ loopback listener on a port of its own.
\ STORAGE CLASS. PROCESS-WIDE: the scratch directory's path, and the one
\ running server's pid, pipes, output and port; a test UNSERVEs before it
\ serves again. CALLER-OWNED: the cells LISTEN writes.
require src/lib/error.f
require src/lib/test.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/net/tcp4.f

package TEST-TLS-PEER
public

$7F000001 constant LOOPBACK
\ The verify flags of a certificate from the unrelated CA, and of an origin
\ naming another host than the certificate's.
$08 constant NOT-TRUSTED                \ MBEDTLS_X509_BADCERT_NOT_TRUSTED
$04 constant CN-MISMATCH                \ MBEDTLS_X509_BADCERT_CN_MISMATCH

private

8 constant BACKLOG
$200 constant DIR-ROOM
DIR-ROOM BUFFER: DIR                    \ the scratch directory's path
variable DIR-U
$8000 constant OUT-ROOM
OUT-ROOM BUFFER: OUT                    \ the server's output read so far
variable OUT-U
variable SERVER-PID                     \ 0 while no server runs
variable SERVER-OUT                     \ the read end of its output
variable SERVER-IN                      \ its stdin's write end; 0 while none
variable SERVER-PORT


: DIR$ ( -- str ) DIR DIR-U @ ;

public

\ A file of the scratch directory, in SB's buffer.
: IN-DIR ( str -- str )
   {: a:^ u:n :}
   SB-RESET DIR$ SB-APPEND s" /" SB-APPEND a u SB-APPEND SB$ ;

\ The scratch directory, its name starting with the prefix.
: SCRATCH ( str -- )
   FS:MKTEMP-DIR {: a:^ u:n :}
   a u DIR DIR-ROOM SPAN:MAKE SPAN:COPY
   u DIR-U ! ;

private

\ openssl's chatter goes to a log the script shows only when a command fails.
: KEY+ ( -- )
   s"  -newkey ec -pkeyopt ec_paramgen_curve:P-256 -noenc" SB-APPEND ;

: CERTS$ ( -- str )
   SB-RESET
   s" { openssl req -x509 -days 1 -subj /CN=habu-tls-ca" SB-APPEND
   s"  -keyout ca.key -out ca.pem" SB-APPEND KEY+
   s"  && openssl req -x509 -days 1 -subj /CN=habu-tls-other" SB-APPEND
   s"  -keyout other.key -out other.pem" SB-APPEND KEY+
   s"  && openssl req -subj /CN=localhost" SB-APPEND
   s"  -addext subjectAltName=DNS:localhost" SB-APPEND
   s"  -keyout server.key -out server.csr" SB-APPEND KEY+
   s"  && openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key" SB-APPEND
   s"  -set_serial 1 -days 1 -copy_extensions copy -out server.pem; }" SB-APPEND
   s"  2>openssl.log || { cat openssl.log >&2; exit 1; }" SB-APPEND
   SB$ ;

public

: CERTS ( -- )
   s" certificates are made in the scratch directory" T-LABEL
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   CERTS$ PROCESS:ARG
   DIR$ PROCESS:CWD
   PROCESS:SPAWN PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T= ;

private

\ One read of the server's output, once there is some.
: READ-OUT ( -- )
   SERVER-OUT @ >FD FD:POLLIN PROCESS:PFD!  1 -1 >MS PROCESS:POLL drop
   SERVER-OUT @ OUT OUT-U @ + OUT-ROOM OUT-U @ - read {: k:n :}
   k 0 <= if E-PROC-OUTPUT throw then
   k OUT-U +! ;

: ACCEPT$ ( -- str ) s" ACCEPT 127.0.0.1:" ;

\ The output after the ACCEPT line's prefix, empty until it is there.
: TAIL$ ( -- str )
   OUT OUT-U @ ACCEPT$ FIND-SUB MATCH option
      none OF OUT 0 ENDOF
      some OF IDX>N ACCEPT$ nip + dup OUT + swap OUT-U @ swap - ENDOF
   ;MATCH ;

\ The port's digits once their line has ended, else empty.
: PORT$ ( -- str )
   TAIL$ {: a:^ u:n :}
   a u 10 INDEX-OF MATCH option
      none OF a 0 ENDOF
      some OF IDX>N a swap ENDOF
   ;MATCH ;

\ TLS 1.3 encrypts a record's type, so only on TLS 1.2 does a relay see
\ which record is the close_notify; after it s_server sends no FIN and waits
\ for the client's.
: S-SERVER ( bool -- PROCESS:request )
   {: old:bool :}
   s" /usr/bin/env" PROCESS:NEW
   s" openssl" PROCESS:ARG
   s" s_server" PROCESS:ARG
   s" -accept" PROCESS:ARG
   s" 127.0.0.1:0" PROCESS:ARG
   s" -cert" PROCESS:ARG
   s" server.pem" PROCESS:ARG
   s" -key" PROCESS:ARG
   s" server.key" PROCESS:ARG
   old if s" -no_tls1_3" PROCESS:ARG then
   DIR$ PROCESS:CWD ;

\ The server's stdout and stderr share one pipe, read here up to the ACCEPT
\ line and later only by HANDSHAKES: what it writes after that line stays far
\ below the pipe's capacity.
: SERVE ( PROCESS:request -- )
   s" s_server names its loopback port" T-LABEL
   FD:PIPE-PAIR {: r:fd w:fd :}
   1 >FD w PROCESS:FD 2 >FD w PROCESS:FD
   PROCESS:SPAWN PID>N SERVER-PID !
   w FD>N close
   r FD>N SERVER-OUT !
   0 OUT-U !
   begin PORT$ nip 0= while READ-OUT repeat
   PORT$ STR-PARSE-POS MATCH option
      none OF s" no port" T-FAIL-AS ENDOF
      some OF SERVER-PORT ! ENDOF
   ;MATCH ;

public

\ The server answering every request with its status page, on TLS 1.2 only
\ when the flag is true.
: WWW-SERVE ( bool -- ) S-SERVER s" -www" PROCESS:ARG SERVE ;

\ The server sending what SCRIPTED gives it, one record per write; the
\ test's stdin holds descriptor 0, so the pipe's end is never 0.
: SCRIPT-SERVE ( -- )
   FD:PIPE-PAIR {: r:fd w:fd :}
   false S-SERVER 0 >FD r PROCESS:FD SERVE
   r FD>N close
   w FD>N SERVER-IN ! ;

\ The bytes to the server's stdin in one write, which a pipe keeps whole.
: SCRIPTED ( str -- )
   {: a:^ u:n :}
   SERVER-IN @ >FD a u FD:WRITE-FULL ;

: PORT ( -- n ) SERVER-PORT @ ;

private

\ Every byte of output the server has written so far.
: DRAIN-OUT ( -- )
   begin SERVER-OUT @ >FD 0 >MS PROCESS:POLL-IN COUNT>N 0 > while
      READ-OUT
   repeat ;

public

\ The `CIPHER is` lines of the server's output. It writes one for each
\ handshake before any byte SCRIPTED gives that session, so a session that
\ has answered is counted.
: HANDSHAKES ( -- n )
   DRAIN-OUT  OUT OUT-U @ s" CIPHER is" COUNT-SUB ;

: UNSERVE ( -- )
   SERVER-PID @ 0= if exit then
   SERVER-PID @ >PID PROCESS:SIGKILL PROCESS:KILL-RAW drop
   SERVER-PID @ >PID PROCESS:WAIT PROCESS:STATUS>RC drop
   0 SERVER-PID !
   SERVER-OUT @ close
   SERVER-IN @ 0<> if SERVER-IN @ close  0 SERVER-IN ! then ;

\ The server stopped and the scratch directory removed.
: CLEAN ( -- ) UNSERVE DIR$ FS:REMOVE-TREE ;

\ A loopback listener on a port of its own, kept in the cells; false when
\ the socket would not bind, listen or name its port.
: LISTEN ( ^n ^n -- bool )
   {: lis-cell:^ port-cell:^ :}
   LOOPBACK TCP4:ADDRESS 0 TCP4:PORT TCP4:BIND
   MATCH TCP4:bind-result
      bound OF TCP4:LISTENER>N lis-cell ! ENDOF
      failed OF drop false exit ENDOF
   ;MATCH
   lis-cell @ TCP4:>LISTENER BACKLOG TCP4:LISTEN
   MATCH TCP4:status ok OF ENDOF failed OF drop false exit ENDOF ;MATCH
   lis-cell @ TCP4:>LISTENER TCP4:LOCAL
   MATCH TCP4:endpoint-result
      endpoint OF TCP4:PORT>N port-cell ! drop true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

;package
