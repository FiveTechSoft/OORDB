/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TDateField
*/
CLASS TDateField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "D"
   DATA FNewValue INIT {|| CToD( "" ) }
   DATA FDefaultValue INIT {|| Date() }
   DATA FSize INIT 8     // Size on index is 8 = len of DToS()
   DATA FType INIT "Date"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Fecha"} )
   DATA FValType INIT "D"
   METHOD GetEmptyValue BLOCK {|| CToD( "" ) }
   PUBLIC:

   METHOD GetAsString() INLINE FDateS( ::GetAsVariant() )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PUBLISHED:

ENDCLASS

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TDateField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'D'
      RETURN DToS( keyVal )
   CASE 'U'
      RETURN DToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TDateField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "DToS(" + fieldName + ")"

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TDateField

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( AsDate( variant ) )
      EXIT
   CASE 'D'
      ::Super:SetAsVariant( variant )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TDateField
*/
