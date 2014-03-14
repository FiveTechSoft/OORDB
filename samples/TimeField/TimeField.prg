/*
 * $Id:$
 */

#include "oordb.ch"

CLASS MyTable FROM TTable
PUBLIC:
  DEFINE FIELDS
  DEFINE PRIMARY INDEX
  PROPERTY TableFileName VALUE "test"
ENDCLASS

BEGIN FIELDS CLASS MyTable

  ADD TIME FIELD "Time" FORMAT "HH:MM" ;
    NEWVALUE "12:00"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS MyTable

  DEFINE INDEX "X01" KEYFIELD "Time" //AUTOINCREMENT

END PRIMARY INDEX CLASS

PROCEDURE Main()
  LOCAL t
  
  SetMode( 40, 120 )

  t := MyTable():New()

  AltD()
  t:Insert()
  t:Field_Time:AsString := "12:30"
  t:Post()

  BROWSE()

RETURN
