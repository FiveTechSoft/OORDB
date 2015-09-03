/*
 * $Id: calcField1.prg 16 2012-03-06 22:56:00Z tfonrouge $
 */

#include "oordb.ch"

CLASS MyTable FROM TTable
PROTECTED:

  DEFINE FIELDS
  DEFINE PRIMARY INDEX

  PROPERTY TableFileName VALUE "mytable"

EXPORT:

ENDCLASS

BEGIN FIELDS CLASS MyTable

  /* RecordId : primary base key field */
  ADD AUTOINC FIELD "RecordId"

  /* Age */
  ADD CALCULATED FLOAT FIELD {|Self| ( Date() - ::Field_Birth:Value ) / 365  } NAME "Age"

  /* Birth */
  ADD DATE FIELD "Birth"

  /* FullName */
  ADD CALCULATED MEMO FIELD {|Self| RTrim( ::Field_Name:Value ) + " " + RTrim( ::Field_LastName:Value ) } NAME "FullName"

  /* Gender */
  ADD STRING FIELD "Gender" SIZE 1 ;
    VALIDVALUES {" "=>"Undefined","M"=>"Male","F"=>"Female"} ;
    DEFAULTVALUE " "

  /* LastName */
  ADD STRING FIELD "LastName" SIZE 40

  /* Name */
  ADD STRING FIELD "Name" SIZE 40

  /* Seconds */
  ADD CALCULATED FLOAT FIELD {|Self| ::Field_Age:Value * ( 60 * 60 * 24 * 365 ) } NAME "Seconds" PICTURE "9,999,999,999"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS MyTable

    DEFINE INDEX "Primary" KEYFIELD "RecordId"

END PRIMARY INDEX CLASS

PROCEDURE Main()
  LOCAL table
  LOCAL o
  LOCAL GetList := {}

  table := MyTable():New()

  o := table:DisplayFieldList()

  IF table:Eof()
    Poblate( table )
  ENDIF

  CLS

  @ 0, 0 SAY "Editing Record #:" GET table:RecNo

  @ 2, 0 SAY "    Name:" GET table:Field_Name:Value
  @ 3, 0 SAY "LastName:" GET table:Field_LastName:Value
  @ 4, 0 SAY "   Birth:" GET table:Field_Birth:Value

  READ

  ? "Table '" + table:ClassName + "' with " + NTrim( table:Count ) + " records."
  ?

  table:DbGoTop()

  WHILE !table:Eof()
    ? o:FullName, "Age:", o:Age, "Seconds:", table:Field_Seconds:AsString
    table:DbSkip()
  ENDDO

RETURN

STATIC PROCEDURE Poblate( table )
  LOCAL h
  LOCAL itm

  h := { ;
         { "Name" => "Homer",  "LastName" => "Simpson", "Birth" => Date() - ( 365 * 40 ) },;
         { "Name" => "Marge",  "LastName" => "Bouvier", "Birth" => Date() - ( 365 * 33 ), "Gender" => "F" }, ;
         { "Name" => "Bart",   "LastName" => "Simpson", "Birth" => Date() - ( 365 * 10 ) } , ;
         { "Name" => "Lisa",   "LastName" => "Simpson", "Birth" => Date() - ( 365 * 8 ), "Gender" => "F" }, ;
         { "Name" => "Maggie", "LastName" => "Simpson", "Birth" => Date() - ( 365 * 2 ), "Gender" => "F" } ;
       }

  FOR EACH itm IN h
    table:InsertRecord( itm )
  NEXT

RETURN
