/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldHash
*/
CLASS TFieldHash FROM TField

    PROTECTED:

    DATA FDBS_LEN INIT 4
    DATA FDBS_DEC INIT 0
    DATA FDBS_TYPE INIT "M"
    DATA FFieldType INIT ftHash
    DATA FSize INIT 0
    DATA FType INIT "Hash"
    DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Hash"} )

    METHOD GetEmptyValue BLOCK {|| {=>} }

    METHOD TranslateToFieldValue( value )
    METHOD TranslateToValue( value )

    PUBLIC:

ENDCLASS

/*
    TranslateToFieldValue
*/
METHOD FUNCTION TranslateToFieldValue( value ) CLASS TFieldHash
    IF ValType( value ) != "H"
        value := {=>}
    ENDIF
RETURN HB_Serialize( value )

/*
    TranslateToValue
*/
METHOD FUNCTION TranslateToValue( value ) CLASS TFieldHash
    SWITCH ValType( value )
    CASE "H"
        EXIT
    CASE "C"
        value := ::TranslateToValue( HB_DeSerialize( value ) )
        EXIT
    OTHERWISE
        value := {=>}
    ENDSWITCH
RETURN value

/*
    ENDCLASS TFieldHash
*/
