/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TNumericField
*/
CLASS TNumericField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 15   // 000000000000.00
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "N"
   DATA FSize
   DATA FType INIT "Numeric"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Numero"} )
   DATA FValType INIT "N"
   METHOD GetEmptyValue BLOCK {|| 0 }
   METHOD StrFormat( value ) INLINE Str( value )
   PUBLIC:

   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsNumeric READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString( value ) CLASS TNumericField

   LOCAL Result

   IF value == NIL
      value := ::GetAsVariant()
   ENDIF

   IF ::OnGetText != NIL
      Result := Value
      ::OnGetText:Eval( Self, @Result )
   ELSE
      IF ::Picture = NIL
         Result := ::StrFormat( value )
      ELSE
         Result := Transform( value, ::Picture )
      ENDIF
   ENDIF

   RETURN Result

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TNumericField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'N'
      RETURN StrZero( keyVal, ::FDBS_LEN, ::FDBS_DEC )
   CASE 'U'
      RETURN StrZero( ::GetAsVariant(), ::FDBS_LEN, ::FDBS_DEC )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TNumericField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "StrZero(" + fieldName + "," +  HB_NToS( ::FDBS_LEN ) + "," + HB_NToS( ::FDBS_DEC ) + ")"

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TNumericField

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( Val( variant ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( variant )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TNumericField
*/
