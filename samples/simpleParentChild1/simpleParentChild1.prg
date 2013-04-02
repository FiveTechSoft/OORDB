/*
 * $Id:$
 */

#include "oordb.ch"

/*
  The Database object
*/
CLASS DbParentChild1 FROM TDatabase
PUBLIC:
  PROPERTY Directory VALUE "data"
END CLASS

CLASS Person FROM TTable
PUBLIC:
  DEFINE FIELDS
  DEFINE INDEX
  PROPERTY TableFileName VALUE "person"
ENDCLASS

BEGIN FIELDS CLASS Person

  ADD INTEGER FIELD "Person"
  ADD STRING FIELD "Name" SIZE 40
  ADD STRING FIELD "FirstName" SIZE 40
  ADD STRING FIELD "Gender" SIZE 1 ;
    VALIDVALUES {"M"=>"Male","F"=>"Female" } ;
    DEFAULT "M"
  ADD DATETIME FIELD "Birth"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS Person
  DEFINE INDEX "Primary" KEYFIELD "Person" AUTOINCREMENT
END PRIMARY INDEX CLASS

PROCEDURE Main()
  LOCAL p

  p := Person():New()

  p:DbGoTop()

  ? p:Eof()

RETURN
