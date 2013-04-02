/*
 * $Id: buildTable.prg 14 2012-03-05 02:54:51Z tfonrouge $
 */

#include "oordb.ch"

CLASS MyDatabase FROM TDataBase
  PROPERTY Directory VALUE "data"
ENDCLASS

CLASS MyTable FROM TTable

  METHOD InitDataBase INLINE MyDatabase():New()

  DEFINE FIELDS

  PROPERTY AutoCreate    VALUE .T.
  PROPERTY TableFileName VALUE "mytable"

ENDCLASS

BEGIN FIELDS CLASS MyTable

  ADD STRING FIELD "Name" SIZE 40

  ADD STRING FIELD "LastName" SIZE 40

  ADD DATE FIELD "Birth"

  ADD STRING FIELD "Gender" SIZE 1 ;
    VALIDVALUES {"M"=>"Male","F"=>"Female"} ;
    DEFAULT "M"

END FIELDS CLASS

PROCEDURE Main()
  LOCAL table
  LOCAL o

  table := MyTable():New()

  o := table:DisplayFields()

  IF table:Eof()
    Poblate( table )
  ENDIF

  ? "Table '" + table:ClassName + "' with " + NTrim( table:Count ) + " records."
  ?

  table:DbGoTop()

  WHILE !table:Eof()
    ? o:Name, o:LastName, o:Birth, o:Gender
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
