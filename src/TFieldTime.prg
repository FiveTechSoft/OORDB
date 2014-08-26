/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldTime
*/
CLASS TFieldTime FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "I"
   DATA FFieldType INIT ftTime
   DATA FSize INIT 4
   DATA FTime AS OBJECT
   DATA FType INIT "Time"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Tiempo"} )
   METHOD GetAs( index )
   METHOD GetAsVariant( ... )
   METHOD GetEmptyValue INLINE ::Time:AsSeconds := 0, ::Time
   METHOD GetTime INLINE iif( ::FTime = NIL, ::FTime := TTime():New( "00:00:00", "HH:MM:SS" ), ::FTime )
   METHOD TranslateToFieldValue( value )
   METHOD TranslateToValue( value )

   PUBLIC:

   METHOD GetAsDisplay() INLINE ::GetAsVariant():AsString
   METHOD GetAsDisplayEmptyValue INLINE ::GetEmptyValue:AsString
   METHOD GetAsString INLINE ::GetAsVariant:AsString
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsHours   INDEX 1 READ GetAs
   PROPERTY AsMinutes INDEX 2 READ GetAs
   PROPERTY AsSeconds INDEX 3 READ GetAs
   PROPERTY Hours READ Time:Hours
   PROPERTY KeySize INIT 8
   PROPERTY Minutes READ Time:Minutes
   PROPERTY Seconds READ Time:Seconds
   PROPERTY TimeFormat READ Time:FORMAT WRITE Time:SetFormat
   PROPERTY Time READ GetTime

   PUBLISHED:

ENDCLASS

/*
    GetAs
*/
METHOD FUNCTION GetAs( index ) CLASS TFieldTime

   ::Time:AsString := ::GetAsVariant()
   SWITCH INDEX
   CASE 1
      RETURN ::Time:AsHours
   CASE 2
      RETURN ::Time:AsMinutes
   CASE 3
      RETURN ::Time:AsSeconds
   ENDSWITCH

   RETURN NIL

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TFieldTime

   LOCAL time

   time := ::Super:GetAsVariant( ... )

   SWITCH ValType( time )
   CASE "N"
      ::time:AsSeconds := time
      EXIT
   CASE "C"
      ::time:AsString := time
      EXIT
   CASE "O"
      ::time:AsSeconds := time:AsSeconds
      EXIT
   ENDSWITCH

   RETURN ::time

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldTime

   IF keyVal = NIL
      keyVal := ::GetAsVariant()
   ENDIF

   RETURN hb_NumToHex( ::TranslateToFieldValue( keyVal ), 8 )

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldTime

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_NumToHex(" + fieldName + ",8)"

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldTime
    LOCAL time
    LOCAL vt

    SWITCH ( vt := ValType( variant ) )
    CASE "O"
        time := variant
        EXIT
    CASE "C"
    CASE "N"
        time := TTime():New( , ::Time:Format )
        IF vt = "C"
            time:AsString := variant
        ELSE
            time:AsSeconds := variant
        ENDIF
        EXIT
    ENDSWITCH

    ::Super:SetAsVariant( time )

RETURN

/*
    TranslateToFieldValue
*/
METHOD FUNCTION TranslateToFieldValue( value ) CLASS TFieldTime
    SWITCH ValType( value )
    CASE "N"
        RETURN Int( value )
    CASE "O"
        RETURN value:AsSeconds
    ENDSWITCH
RETURN 0

/*
    TranslateToValue
*/
METHOD FUNCTION TranslateToValue( value ) CLASS TFieldTime

   SWITCH ValType( value )
   CASE "C"
      ::Time:AsString := value
      EXIT
   CASE "N"
      ::Time:AsSeconds := value
      EXIT
   CASE "O"
      ::FTime := value
      EXIT
   ENDSWITCH

   RETURN ::FTime

/*
    ENDCLASS TFieldTime
*/
