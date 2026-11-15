\ E2E of SECRET:WRITE and SECRET:READ on Linux under the attribute keys and
\ label SECRET:LINUX-ITEM! binds, against a private D-Bus session bus and a
\ gnome-keyring-daemon on it, unlocked with a fixed password: both this
\ process's children, given only a scratch HOME and XDG directories (sockets
\ under HB_SOCK_TMP), and killed whatever the cases do. This process reaches
\ the bus through DBUS_SESSION_BUS_ADDRESS, set before its first SECRET call,
\ so no case reaches the user's keyring. secret-tool is the other client.
\ Fails on: a write secret-tool cannot look up under the bound keys, or that
\ it reads cut at a NUL; a second write that keeps the old value or adds an
\ item; an item secret-tool stored that READ misses or misreads; a label other
\ than the bound one; an empty name or two equal keys bound; a write or read
\ the stopped keyring fails that is not E-ACCESS logged once.
\ dbus-daemon, gnome-keyring-daemon, gdbus and secret-tool on PATH are required.
HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or ?TARGET
require src/lib/secret.f
require src/lib/test.f
require src/lib/codegen.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/spawn.f
require src/lib/process.f
require src/lib/process-env.f
require src/lib/process-command.f
require src/lib/test/stderr.f

package SECRET-LINUX-TEST
private

PROCESS-SYMBOLS
FUNCTION: ENV-SET setenv ( ^u8 ^u8 n -- i32 ) ;FUNCTION

$1C0 constant MODE-0700        \ XDG_RUNTIME_DIR is its owner's alone
10 constant NEWLINE
-1 constant NO-PID

PATH-CAP BUFFER: DBUS-DAEMON   variable DBUS-DAEMON-U
PATH-CAP BUFFER: KEYRING       variable KEYRING-U
PATH-CAP BUFFER: GDBUS         variable GDBUS-U
PATH-CAP BUFFER: SECRET-TOOL   variable SECRET-TOOL-U
PATH-CAP BUFFER: ROOT          variable ROOT-U
PATH-CAP BUFFER: SOCKETS       variable SOCKETS-U
PATH-CAP BUFFER: JOINED
1 BUFFER: BYTE
CODEGEN:BUFFER ADDRESS
CODEGEN:BUFFER CONF
CODEGEN:BUFFER EXPECTED
64 SPAN-BUFFER: OUT
4096 constant LOG-CAP
LOG-CAP BUFFER: LOGGED

variable BUS-PID               \ dbus-daemon's pid until it is reaped
variable KEY-PID               \ gnome-keyring-daemon's, likewise
variable KEY-WATCH             \ readable once gnome-keyring-daemon has exited
NO-PID BUS-PID !
NO-PID KEY-PID !
-1 KEY-WATCH !

: DBUS-DAEMON$ ( -- ^u8 n ) DBUS-DAEMON DBUS-DAEMON-U @ ;
: KEYRING$ ( -- ^u8 n ) KEYRING KEYRING-U @ ;
: GDBUS$ ( -- ^u8 n ) GDBUS GDBUS-U @ ;
: SECRET-TOOL$ ( -- ^u8 n ) SECRET-TOOL SECRET-TOOL-U @ ;
: ROOT$ ( -- ^u8 n ) ROOT ROOT-U @ ;
: SOCKETS$ ( -- ^u8 n ) SOCKETS SOCKETS-U @ ;
: ADDRESS$ ( -- ^u8 n ) ADDRESS CODEGEN:CONTENTS ;

: SERVICE-KEY$ ( -- ^u8 n ) s" app" ;
: ACCOUNT-KEY$ ( -- ^u8 n ) s" user" ;
: LABEL$ ( -- ^u8 n ) s" Habu secret-linux test item" ;
: SERVICE$ ( -- ^u8 n ) s" org.habu.secret-linux-test" ;
: PASSWORD$ ( -- ^u8 n ) s" habu-test" ;


\ A path under ROOT or SOCKETS, borrowed until the next one.
: ROOT/ ( ^u8 n -- ^u8 n ) ROOT$ 2swap JOINED FS:JOIN-PATH JOINED swap ;
: SOCKETS/ ( ^u8 n -- ^u8 n ) SOCKETS$ 2swap JOINED FS:JOIN-PATH JOINED swap ;


: TOOL ( ^u8 n ^u8 -- n )
   {: name:^ u:n dst:^ :}
   name u >LEN dst FIND-EXECUTABLE MATCH option
      none OF
         s" secret-linux-test: required executable missing on PATH: " type
         name u type cr
         s" " E-PROC-PATH die
      ENDOF
      some OF LEN>N ENDOF
   ;MATCH ;

: TOOLS ( -- )
   s" dbus-daemon" DBUS-DAEMON TOOL DBUS-DAEMON-U !
   s" gnome-keyring-daemon" KEYRING TOOL KEYRING-U !
   s" gdbus" GDBUS TOOL GDBUS-U !
   s" secret-tool" SECRET-TOOL TOOL SECRET-TOOL-U ! ;


\ The bus and keyring sockets go under HB_SOCK_TMP, the short directory a gate
\ pool gives each row: sun_path holds 108 bytes and TMPDIR there about 100.
: SOCKETS-MAKE ( -- )
   s" HB_SOCK_TMP" GETENV dup 0 > if
      s" kr" FS:MKTEMP-DIR-IN
   else
      2drop s" habu-kr" FS:MKTEMP-DIR
   then {: a:^ u:n :}
   a SOCKETS u BYTE-COPY u SOCKETS-U !
   SOCKETS$ FS:CLEANUP-TREE+ ;

: CONF+ ( ^u8 n -- ) CONF CODEGEN:APPEND-STRING ;

\ The policy is the stock session bus's; no service directory, so the bus
\ activates nothing.
: CONF-WRITE ( -- )
   CONF CODEGEN:RESET
   s" <busconfig><type>session</type><listen>" CONF+ ADDRESS$ CONF+
   s\" </listen><policy context=\"default\">" CONF+
   s\" <allow send_destination=\"*\" eavesdrop=\"true\"/>" CONF+
   s\" <allow eavesdrop=\"true\"/><allow own=\"*\"/>" CONF+
   s" </policy></busconfig>" CONF+
   s" bus.conf" ROOT/ CONF CODEGEN:CONTENTS FS:WRITE-ALL ;

: DIRS ( -- )
   s" habu-secret-linux" FS:MKTEMP-DIR {: a:^ u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   SOCKETS-MAKE
   s" home" ROOT/ FS:MAKE-DIR
   s" data" ROOT/ FS:MAKE-DIR
   s" config" ROOT/ FS:MAKE-DIR
   s" cache" ROOT/ FS:MAKE-DIR
   s" run" SOCKETS/ MODE-0700 FS:MKDIR-MODE
   ADDRESS CODEGEN:RESET
   s" unix:path=" ADDRESS CODEGEN:APPEND-STRING
   s" bus" SOCKETS/ ADDRESS CODEGEN:APPEND-STRING
   CONF-WRITE ;


\ Every child gets these rows and no other.
: ROWS ( PROCESS:request -- PROCESS:request )
   s" PATH" s" PATH" GETENV PROCESS:ENV
   s" HOME" s" home" ROOT/ PROCESS:ENV
   s" XDG_DATA_HOME" s" data" ROOT/ PROCESS:ENV
   s" XDG_CONFIG_HOME" s" config" ROOT/ PROCESS:ENV
   s" XDG_CACHE_HOME" s" cache" ROOT/ PROCESS:ENV
   s" XDG_RUNTIME_DIR" s" run" SOCKETS/ PROCESS:ENV
   s" DBUS_SESSION_BUS_ADDRESS" ADDRESS$ PROCESS:ENV ;

: CMD-ROWS ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" PATH" s" PATH" GETENV CMD:ENV+
   s" HOME" s" home" ROOT/ CMD:ENV+
   s" XDG_DATA_HOME" s" data" ROOT/ CMD:ENV+
   s" XDG_CONFIG_HOME" s" config" ROOT/ CMD:ENV+
   s" XDG_CACHE_HOME" s" cache" ROOT/ CMD:ENV+
   s" XDG_RUNTIME_DIR" s" run" SOCKETS/ CMD:ENV+
   s" DBUS_SESSION_BUS_ADDRESS" ADDRESS$ CMD:ENV+ ;


\ libsecret's GDBus reads the variable at its first connection.
: BUS-ENV! ( -- )
   s\" DBUS_SESSION_BUS_ADDRESS\z" drop ADDRESS$ FS:>PATHZ 1 ENV-SET
   0 <> if E-PROC-ENV throw then ;


: SAY ( ^u8 n -- ) s" secret-linux-test: " type type cr ;

\ True once a line has come; false at the end of the output.
: LINE? ( n -- bool )
   {: rd:n :}
   begin rd BYTE 1 read 1 = while
      BYTE c@ NEWLINE = if true exit then
   repeat
   false ;

\ dbus-daemon prints its address once it listens.
: BUS-START ( -- )
   FD:PIPE {: rd:fd wr:fd :}
   DBUS-DAEMON$ PROCESS:NEW PROCESS:HERMETIC
   s" --nofork" PROCESS:ARG
   s" --config-file" PROCESS:ARG s" bus.conf" ROOT/ PROCESS:ARG
   s" --print-address" PROCESS:ARG
   ROWS
   1 >FD wr PROCESS:FD
   PROCESS:SPAWN PID>N BUS-PID !
   wr FD>N close
   rd FD>N LINE? {: up:bool :}
   rd FD>N close
   up 0= if
      s" dbus-daemon ended before it listened" SAY E-PROC-OUTPUT throw
   then ;

\ --unlock reads the login keyring's password from stdin, to its end.
: KEYRING-SPAWN ( -- )
   FD:PIPE {: rd:fd wr:fd :}
   wr PASSWORD$ FD:WRITE-FULL
   wr FD>N close
   KEYRING$ PROCESS:NEW PROCESS:HERMETIC
   s" --foreground" PROCESS:ARG
   s" --unlock" PROCESS:ARG
   s" --components=secrets" PROCESS:ARG
   ROWS
   0 >FD rd PROCESS:FD
   PROCESS:SPAWN PID>N KEY-PID !
   rd FD>N close
   KEY-PID @ proc-watch-open KEY-WATCH ! ;

: RESULT-CODE ( result<n,n> -- n )   \ 0 on clean exit, else the completion code
   MATCH result ok OF ENDOF err OF ENDOF ;MATCH ;

\ The wait ends when the keyring owns its name or, by the watch, exits.
: KEYRING-WAIT ( CMD:command -- CMD:command )
   CMD-ROWS
   s" wait" CMD:ARG+ s" --session" CMD:ARG+
   s" org.freedesktop.secrets" CMD:ARG+
   KEY-WATCH @ >FD CMD:CANCEL-FD!
   GDBUS$ PROCESS:WAIT-FOREVER CMD:RUN-RC RESULT-CODE
   0 <> if
      CMD:ERR$ type s" gdbus wait failed" SAY E-PROC-OUTPUT throw
   then ;

: KEYRING-START ( -- )
   KEYRING-SPAWN
   KEY-WATCH @ 0 < if
      s" gnome-keyring-daemon cannot be watched" SAY E-PROC-OUTPUT throw
   then
   [: [: KEYRING-WAIT ;] CMD:WITH-COMMAND ;] catch {: code:n :}
   code E-PROC-CANCELLED = if
      s" gnome-keyring-daemon ended before it owned its bus name" SAY
   then
   code 0 <> if code throw then ;

: START ( -- ) BUS-START KEYRING-START ;


: END ( ^n -- )
   {: pid:^ :}
   pid @ NO-PID = if exit then
   pid @ >PID PROCESS:KILL-TREE
   pid @ >PID PROCESS:WAIT drop
   NO-PID pid ! ;

: KEYRING-STOP ( -- )
   KEY-PID END
   KEY-WATCH @ 0 >= if KEY-WATCH @ close then
   -1 KEY-WATCH ! ;

\ The keyring goes first: its bus outlives it.
: STOP ( -- ) KEYRING-STOP BUS-PID END ;


: ATTRS ( CMD:command ^u8 n -- CMD:command )
   {: account:^ au:n :}
   SERVICE-KEY$ CMD:ARG+ SERVICE$ CMD:ARG+
   ACCOUNT-KEY$ CMD:ARG+ account au CMD:ARG+ ;

: TOOL-RUN ( CMD:command -- CMD:command )
   SECRET-TOOL$ PROCESS:WAIT-FOREVER CMD:RUN-RC
   RESULT-CODE {: rc:n :} rc 0 <> if CMD:ERR$ type then rc 0 T= ;

: LOOKUP ( CMD:command ^u8 n ^u8 n -- CMD:command )
   {: account:^ au:n want:^ wu:n :}
   CMD:RESET CMD-ROWS
   s" lookup" CMD:ARG+ account au ATTRS TOOL-RUN
   CMD:OUT$ want wu T$= ;

\ secret-tool prints the attributes on stderr and the rest on stdout.
: SEARCH ( CMD:command ^u8 n -- CMD:command ^u8 n )
   {: account:^ au:n :}
   CMD:RESET CMD-ROWS
   s" search" CMD:ARG+ s" --all" CMD:ARG+ account au ATTRS TOOL-RUN
   CMD:OUT$ ;

: LABEL-LINE$ ( -- ^u8 n )
   EXPECTED CODEGEN:RESET
   s" label = " EXPECTED CODEGEN:APPEND-STRING
   LABEL$ EXPECTED CODEGEN:APPEND-STRING
   NEWLINE EXPECTED CODEGEN:APPEND-BYTE
   EXPECTED CODEGEN:CONTENTS ;

: LABEL-ARG$ ( -- ^u8 n )
   EXPECTED CODEGEN:RESET
   s" --label=" EXPECTED CODEGEN:APPEND-STRING
   LABEL$ EXPECTED CODEGEN:APPEND-STRING
   EXPECTED CODEGEN:CONTENTS ;


: WRITES ( CMD:command -- CMD:command )
   s" secret-tool looks up a write under the bound keys, NULs kept" T-LABEL
   SERVICE$ s" written" s\" first\x00value" SECRET:WRITE
   s" written" s\" first\x00value" LOOKUP
   s" a second write replaces the item's value without a second item" T-LABEL
   SERVICE$ s" written" s" second" SECRET:WRITE
   s" written" s" second" LOOKUP
   s" written" SEARCH {: out:^ outu:n :}
   out outu s" label = " COUNT-SUB 1 T=
   s" the written item's label is the bound label" T-LABEL
   out outu LABEL-LINE$ CONTAINS? TTRUE ;

: STORED ( CMD:command -- CMD:command )
   s" a read finds the item secret-tool stored under the bound keys" T-LABEL
   CMD:RESET CMD-ROWS
   s" store" CMD:ARG+ LABEL-ARG$ CMD:ARG+ s" stored" ATTRS
   s\" stored\x00by secret-tool" CMD:IN!
   TOOL-RUN
   SERVICE$ s" stored" OUT SECRET:READ TTRUE {: got:n :}
   OUT SPAN:$ drop got s\" stored\x00by secret-tool" T$= ;

\ The call's throw code, with fd 2 sent to a file for the call.
: CAPTURED ( [ -- ] -- n )
   s" stderr.txt" ROOT/ STDERR:CAPTURE
   catch STDERR:;CAPTURE ;

\ The call logged one line, a secret line holding the text.
: LOGGED-ONCE ( ^u8 n -- )
   {: text:^ tu:n :}
   s" stderr.txt" ROOT/ LOGGED LOG-CAP FS:READ-ALL {: u:n :}
   LOGGED u NEWLINE COUNT-CHAR 1 T=
   EXPECTED CODEGEN:RESET
   s" secret: " EXPECTED CODEGEN:APPEND-STRING
   text tu EXPECTED CODEGEN:APPEND-STRING
   LOGGED u EXPECTED CODEGEN:CONTENTS CONTAINS? TTRUE ;

: STOPPED ( -- )
   s" with the keyring stopped a write and a read are E-ACCESS, logged" T-LABEL
   KEYRING-STOP
   [: SERVICE$ s" written" s" third" SECRET:WRITE ;] CAPTURED
   SECRET:E-ACCESS T=
   s" cannot store the item: " LOGGED-ONCE
   [: SERVICE$ s" written" OUT SECRET:READ 2drop ;] CAPTURED
   SECRET:E-ACCESS T=
   s" cannot search for the item: " LOGGED-ONCE ;

: CASES ( -- )
   START
   [: WRITES STORED ;] CMD:WITH-COMMAND
   STOPPED ;


: REFUSALS ( -- )
   s" a binding with an empty name or two equal keys is E-CONFIG" T-LABEL
   [: s" " ACCOUNT-KEY$ LABEL$ SECRET:LINUX-ITEM! ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE-KEY$ ACCOUNT-KEY$ s" " SECRET:LINUX-ITEM! ;] SECRET:E-CONFIG TTHROWSQ
   [: SERVICE-KEY$ SERVICE-KEY$ LABEL$ SECRET:LINUX-ITEM! ;] SECRET:E-CONFIG TTHROWSQ ;

: MAIN ( -- )
   T-RESET
   TOOLS DIRS BUS-ENV!
   REFUSALS
   SERVICE-KEY$ ACCOUNT-KEY$ LABEL$ SECRET:LINUX-ITEM!
   [: CASES ;] [: STOP ;] finally
   T-REPORT ;
MAIN
;package
