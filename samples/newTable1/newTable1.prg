/*
 * $Id:$
 */

#include "oordb.ch"

PROCEDURE Main()
  LOCAL person
  LOCAL employee

  person := TPerson():New()

  AddPerson( person )

  ? person:RecCount()

  employee := TEmployee():New()

  ? employee:Field_Type:Value
  ? employee:RecCount()
  
  WAIT

RETURN

STATIC PROCEDURE AddPerson( person )

  IF person:Insert()
    person:Field_FirstName:Value := "Homer"
    person:Field_LastName:Value := "Simpson"
    person:Field_Birth:Value := Date()-(35*365)
    person:Post()
  ENDIF

RETURN
