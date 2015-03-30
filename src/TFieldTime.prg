/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldTime
*/
CLASS TFieldTime FROM TField

PROTECTED:

    DATA FDBS_LEN INIT 4
    DATA FDBS_DEC INIT 0
    DATA FDBS_TYPE INIT "I"
    DATA FFieldType INIT ftTime
    DATA FTimeFormat    INIT "HH:MM:SS"
    DATA FType INIT "Time"
    DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Tiempo"} )
    METHOD GetAs( index )
    METHOD GetAsVariant( ... )
    METHOD GetEmptyValue INLINE ::GetTime()
    METHOD GetTPart( index )
    METHOD SetTimeFormat( timeFormat ) INLINE ::FTimeFormat := timeFormat
    METHOD TranslateToFieldValue( value )
    METHOD TranslateToValue( value )

PUBLIC:

    METHOD GetAsDisplay() INLINE ::GetAsVariant():AsString
    METHOD GetAsDisplayEmptyValue INLINE ::GetEmptyValue:AsString
    METHOD GetAsString INLINE ::GetAsVariant:AsString
    METHOD GetKeyVal( keyVal )
    METHOD GetTime INLINE TTime():New( "00:00:00", ::FTimeFormat )
    METHOD IndexExpression( fieldName )
    METHOD SetAsVariant( variant )

    PROPERTY AsHours    INDEX 1 READ GetAs
    PROPERTY AsMinutes  INDEX 2 READ GetAs
    PROPERTY AsSeconds  INDEX 3 READ GetAs
    PROPERTY Hours      INDEX 1 READ GetTPart     //Time:Hours
    PROPERTY KeySize    INIT 8
    PROPERTY Minutes    INDEX 2 READ GetTPart     //Time:Minutes
    PROPERTY Seconds    INDEX 3 READ GetTPart     //Time:Seconds
    PROPERTY TimeFormat READ FTimeFormat WRITE SetTimeFormat

ENDCLASS

/*
    GetAs
*/
METHOD FUNCTION GetAs( index ) CLASS TFieldTime
    LOCAL time

    time := ::GetAsVariant()

    SWITCH INDEX
    CASE 1
        RETURN time:AsHours
    CASE 2
        RETURN time:AsMinutes
    CASE 3
        RETURN time:AsSeconds
    ENDSWITCH

RETURN time

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TFieldTime
    LOCAL time
    LOCAL v

    v := ::Super:GetAsVariant( ... )

    SWITCH ValType( v )
    CASE "N"
        time := ::GetTime()
        time:AsSeconds := v
        EXIT
    CASE "C"
        time := ::GetTime()
        time:AsString := v
        EXIT
    CASE "O"
        time := v
        EXIT
    ENDSWITCH

RETURN time

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldTime

   IF keyVal = NIL
      keyVal := ::GetAsVariant()
   ENDIF

   RETURN hb_NumToHex( ::TranslateToFieldValue( keyVal ), 8 )

/*
    GetTPart
*/
METHOD FUNCTION GetTPart( index ) CLASS TFieldTime
    LOCAL n := 0
    LOCAL time

    time := ::GetAsVariant()

    SWITCH index
    CASE 1
        n := time:Hours
        EXIT
    CASE 2
        n := time:Minutes
        EXIT
    CASE 3
        n := time:Seconds
        EXIT
    ENDSWITCH

RETURN n

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

    IF ::FDBS_TYPE = "B"
        RETURN "StrZero(" + fieldName + "," +  HB_NToS( ::FDBS_LEN ) + "," + HB_NToS( ::FDBS_DEC ) + ")"
    ENDIF

RETURN "HB_NumToHex(" + fieldName + ",8)"



/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldTime
    LOCAL time

    SWITCH ValType( variant )
    CASE "O"
        time := variant
        EXIT
    CASE "C"
        time := ::GetTime()
        time:AsString := variant
        EXIT
    CASE "N"
        time := ::GetTime()
        time:AsSeconds := variant
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
        SWITCH ::FDBS_TYPE
        CASE "I"
            RETURN Int( value )
        CASE "B"
            RETURN value
        ENDSWITCH
    CASE "O"
        RETURN value:AsSeconds
    ENDSWITCH
RETURN 0

/*
    TranslateToValue
*/
METHOD FUNCTION TranslateToValue( value ) CLASS TFieldTime
    LOCAL time

    SWITCH ValType( value )
    CASE "C"
        time := ::GetTime()
        time:AsString := value
        EXIT
    CASE "N"
        time := ::GetTime()
        time:AsSeconds := value
        EXIT
    CASE "O"
        time := value
        EXIT
    ENDSWITCH

RETURN time

/*
    ENDCLASS TFieldTime
*/
