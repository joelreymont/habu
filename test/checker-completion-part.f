\ checker-completion-part.f - the packages test/checker-completion.f loads and
\ its subjects of cases 10 and 12 require, so only the dictionary holds their
\ words: CMQA's part CmqB, whose name the top-level package CMQB shares.

package CMQB
public
: QPT ( -- n ) 40 ;
;package

package CMQA
package CmqB
: QPV ( -- n ) 41 ;
public
: QPW ( -- n ) 42 ;
;package
;package
