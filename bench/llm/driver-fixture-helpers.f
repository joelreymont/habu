\ driver-fixture-helpers.f - checked fixture/source builder DSL.
\
\ Load after bench/llm/drive-stdlib-lib.f.

10 constant DFH-LF
32 constant DFH-SP

: DFH-C ( n -- )
   DS-TEST-BUF DS-TEST-CAP DS-TEST-U DS-BUF-C ;

: DFH-SPC ( -- )
   DFH-SP DFH-C ;

: DFH-LINE ( -- )
   DFH-LF DFH-C ;

: DFH-U+ ( n -- ) {: n :}
   n 0 < if DS-DASH DFH-C n negate recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod DS-ZERO + DFH-C ;

: DFH-CONSTANT ( n ptr u8 n -- ) {: n name:ptr nameu :}
   n DFH-U+
   s"  constant " DS-TEST+
   name nameu DS-TEST+
   DFH-LINE ;

: DFH-BYTES ( ptr u8 n ptr u8 n -- )
   {: data:ptr datau name:ptr nameu :}
   s" create " DS-TEST+
   name nameu DS-TEST+
   DFH-SPC
   0 begin dup datau < while
      dup data + c@ DFH-U+
      s"  c, " DS-TEST+
      1+
   repeat drop
   DFH-LINE ;

: DFH-$WORD ( ptr u8 n ptr u8 n n -- )
   {: word:ptr wordu buf:ptr bufu len :}
   s" : " DS-TEST+
   word wordu DS-TEST+
   s"  ( -- ptr u8 n ) " DS-TEST+
   buf bufu DS-TEST+
   DFH-SPC
   len DFH-U+
   s"  ;" DS-TEST-LN ;

: DFH-STRING ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: data:ptr datau buf:ptr bufu word:ptr wordu :}
   data datau buf bufu DFH-BYTES
   word wordu buf bufu datau DFH-$WORD ;

: DFH-SOURCE-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" DS-TEST+
   [char] " DFH-C
   DFH-SPC
   a u DS-TEST+
   [char] " DFH-C ;
