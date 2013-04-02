/*
 * $Id:$
 */

#include "oordb.ch"

CLASS TEmployee FROM TPerson

  DEFINE FIELDS

ENDCLASS

BEGIN FIELDS CLASS TEmployee

  ADD STRING FIELD "Type" SIZE 1 ;
    NEWVALUE "E"

  ADD OBJECT FIELD "Company" ;
    CLASS "TCompany"

END FIELDS CLASS
