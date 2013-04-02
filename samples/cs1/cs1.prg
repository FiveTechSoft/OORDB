/*
 * $Id:$
 */

#include "oordb.ch"

FUNCTION Main()
  LOCAL t1
  LOCAL itm

  t1 := Table1():New()
  
  t1:DbGoTop()
  
  WHILE !t1:Eof()
    FOR EACH itm IN t1:FieldList
      ?? itm:Name + ": " + itm:AsString,""
    NEXT
    ?
    t1:DbSkip(1)
  ENDDO

RETURN NIL
