/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TLogicalField
*/
CLASS TLogicalField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 1
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "L"
   DATA FSize INIT 1
   DATA FType INIT "Logical"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Logico"} )
   DATA FValType INIT "L"
   METHOD GetEmptyValue BLOCK {|| .F. }
   PUBLIC:

   METHOD GetAsString()
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )

   PROPERTY AsBoolean READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString() CLASS TLogicalField
   RETURN iif( ::Value, ".T.", ".F." )

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TLogicalField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'L'
      RETURN iif( keyVal, "T", "F" )
   CASE 'U'
      RETURN iif( ::GetAsVariant(), "T", "F" )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TLogicalField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "iif(" + fieldName + ",'T','F')"

/*
    ENDCLASS TLogicalField
*/
