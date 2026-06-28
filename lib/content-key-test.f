\ content-key-test.f - fixtures for manifest-hashed cache keys.
\
\ Native bin/hb already carries SHA256. Load after lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, and lib/content-key.f.

create CKT-ROOT-BUF FS-PATH-CAP allot
create CKT-FILE-BUF FS-PATH-CAP allot
create CKT-HEX-A 80 allot
create CKT-HEX-B 80 allot
create CKT-HEX-C 80 allot

variable CKT-ROOT-U
variable CKT-FILE-U

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT-BUF CKT-ROOT-U @ ;

: CKT-FILE$ ( -- ptr u8 n )
   CKT-FILE-BUF CKT-FILE-U @ ;

: CKT-PREPARE ( -- )
   CLEANUP-RESET
   s" hb-content-key-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a CKT-ROOT-BUF u BYTE-COPY
   u CKT-ROOT-U !
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" key.txt" CKT-FILE-BUF JOIN-PATH CKT-FILE-U ! ;

: CKT-WRITE-A ( -- )
   CKT-FILE$ s" alpha" WRITE-ALL ;

: CKT-WRITE-B ( -- )
   CKT-FILE$ s" beta" WRITE-ALL ;

: CKT-KEY! ( ptr u8 -- ) {: hex:ptr :}
   CK-RESET
   s" content-key-test-v1" CK-TEXT+
   CKT-FILE$ CK-FILE+
   hex CK-FINAL-HEX ;

: CKT-SAME-CONTENT-STABLE ( -- )
   CKT-WRITE-A
   CKT-HEX-A CKT-KEY!
   CKT-HEX-B CKT-KEY!
   CKT-HEX-A 64 CKT-HEX-B 64 T$= ;

: CKT-CONTENT-CHANGES-KEY ( -- )
   CKT-WRITE-A
   CKT-HEX-A CKT-KEY!
   CKT-WRITE-B
   CKT-HEX-B CKT-KEY!
   CKT-HEX-A 64 CKT-HEX-B 64 T$<> ;

: CKT-SALT-CHANGES-KEY ( -- )
   CK-RESET
   s" salt-a" CK-TEXT+
   CKT-FILE$ CK-FILE+
   CKT-HEX-A CK-FINAL-HEX
   CK-RESET
   s" salt-b" CK-TEXT+
   CKT-FILE$ CK-FILE+
   CKT-HEX-B CK-FINAL-HEX
   CKT-HEX-A 64 CKT-HEX-B 64 T$<> ;

: CKT-MISSING-FAILS ( -- )
   CK-RESET
   s" missing-key" CK-TEXT+
   s" no-such-content-key-file" CK-FILE+ ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-PREPARE
   CKT-SAME-CONTENT-STABLE
   CKT-CONTENT-CHANGES-KEY
   CKT-SALT-CHANGES-KEY
   [: CKT-MISSING-FAILS ;] SHA-E-OPEN TTHROWSQ
   CLEANUP-RUN
   T-REPORT ;

CKT-MAIN
