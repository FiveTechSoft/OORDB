/*
 * $Id: dbf1.prg 13 2012-03-03 20:50:58Z tfonrouge $
 */

#include "oordb.ch"

REQUEST DBFCDX

CLASS TDbf1 FROM TTable
PUBLIC:
  DEFINE FIELDS
  DEFINE PRIMARY INDEX
  DEFINE SECONDARY INDEX
  PROPERTY TableFileName VALUE "test"
ENDCLASS

BEGIN FIELDS CLASS TDbf1

  ADD AUTOINC FIELD "RecordId"
  ADD STRING  FIELD "First" SIZE 20
  ADD STRING  FIELD "Last" SIZE 20
  ADD STRING  FIELD "Street" SIZE 30
  ADD STRING  FIELD "City" SIZE 30
  ADD STRING  FIELD "State" SIZE 2
  ADD STRING  FIELD "Zip" SIZE 10
  ADD DATE    FIELD "HireDate"
  ADD LOGICAL FIELD "Married"
  ADD NUMERIC FIELD "Age" LEN 2 DEC 0
  ADD NUMERIC FIELD "Salary" LEN 6 DEC 0 PICTURE "$ 999,999.99"
  ADD STRING  FIELD "Notes" SIZE 70
  
  ADD CALCULATED STRING FIELD {|Self| ::Field_First:Value + ::Field_Last:Value } NAME "FirstLast" SIZE 40
  ADD CALCULATED STRING FIELD {|Self| ::Field_Last:Value + ::Field_First:Value } NAME "LastFirst" SIZE 40

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS TDbf1
  DEFINE INDEX "Primary" KEYFIELD "RecordId"
END PRIMARY INDEX CLASS

BEGIN SECONDARY INDEX CLASS TDbf1
  DEFINE INDEX "FirstLast" KEYFIELD "FirstLast" UNIQUE
  DEFINE INDEX "LastFirst" KEYFIELD "LastFirst" UNIQUE
END SECONDARY INDEX CLASS

PROCEDURE Main()
  LOCAL t
  
  CLS
  
  /* TODO: Check why this not work with .ntx */
  RddSetDefault("DBFCDX")

  t := TDbf1():New()
  
//  t:IndexName := "LastFirst"
  t:IndexName := "FirstLast"
  
  ? "Custom Index:", t:Index:Custom
  
  WAIT "Press any key to list table"

  t:DbGoTop()

  WHILE !t:Eof()
    ?? t:Field_FirstLast:Value
    ?
    t:DbSkip()
  ENDDO
  
RETURN
