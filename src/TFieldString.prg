/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TStringField
*/
CLASS TStringField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 0
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "C"
   DATA FSize
   DATA FType INIT "String"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Texto"} )
   DATA FValType INIT "C"
   METHOD GetEmptyValue INLINE Space( ::Size )
   METHOD GetAsNumeric INLINE Val( ::GetAsVariant() )
   METHOD GetSize()
   METHOD SetAsNumeric( n ) INLINE ::SetAsVariant( LTrim( Str( n ) ) )
   METHOD SetBuffer( buffer )
   METHOD SetDBS_LEN( dbs_Len )
   METHOD SetDefaultValue( DefaultValue )
   METHOD SetSize( size )
   PUBLIC:
   METHOD GetAsString()
   METHOD IndexExpression( fieldName, isMasterFieldComponent )
   PROPERTY AsNumeric READ GetAsNumeric WRITE SetAsNumeric
   PUBLISHED:
   PROPERTY Size READ GetSize WRITE SetSize

ENDCLASS

/*
    GetAsString
*/
METHOD FUNCTION GetAsString() CLASS TStringField

   LOCAL Result := ""
   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         Result += ::FTable:FieldList[ i ]:AsString()
      NEXT
      EXIT
   OTHERWISE
      Result := ::GetAsVariant()
   ENDSWITCH

   RETURN Result

/*
    GetSize
*/
METHOD FUNCTION GetSize() CLASS TStringField

   LOCAL i

   IF ::FSize = NIL
      IF ::FFieldMethodType = "A"
         ::FSize := 0
         FOR EACH i IN ::FFieldArrayIndex
            ::FSize += ::FTable:FieldList[ i ]:Size
         NEXT
      ENDIF
   ENDIF

   RETURN ::FSize

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName, isMasterFieldComponent ) CLASS TStringField

   LOCAL exp
   LOCAL i
   LOCAL keyFlags

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF

   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   IF ::FFieldMethodType = "A"
      exp := ""
      FOR EACH i IN ::FFieldArrayIndex
         exp += iif( Len( exp ) = 0, "", "+" ) + ::FTable:FieldList[ i ]:IndexExpression( NIL, isMasterFieldComponent == .T. .OR. ( ::IsKeyIndex .AND. !::KeyIndex:CaseSensitive ) )
      NEXT
   ELSE
      IF isMasterFieldComponent == .T. .OR. ::IsMasterFieldComponent .OR. ( ::IsKeyIndex .AND. ::KeyIndex:CaseSensitive )
         IF ::KeyIndex != NIL
            keyFlags := ::KeyIndex:KeyFlags
            IF keyFlags != NIL .AND. hb_HHasKey( keyFlags, ::Name )
               SWITCH keyFlags[ ::Name ]
               CASE "U"
                  exp := "Upper(" + fieldName + ")"
                  EXIT
               ENDSWITCH
            ENDIF
         ENDIF
         IF exp = NIL
            exp := fieldName
         ENDIF
      ELSE
         IF ::FFieldExpression = NIL
            exp := "<error: IndexExpression on '" + ::Name + "'>"
         ELSE
            exp := "Upper(" + fieldName + ")"
         ENDIF
      ENDIF
   ENDIF

   RETURN exp

/*
    SetBuffer
*/
METHOD FUNCTION SetBuffer( buffer ) CLASS TStringField

   LOCAL size := ::Size

   IF ::FFieldType = ftMemo .OR. Len( buffer ) = size
      RETURN ::Super:SetBuffer( buffer )
   ELSE
      RETURN ::Super:SetBuffer( PadR( buffer, size ) )
   ENDIF

   RETURN .F.

/*
    SetDBS_LEN
*/
METHOD PROCEDURE SetDBS_LEN( dbs_Len ) CLASS TStringField

   ::FDBS_LEN := dbs_Len
   IF ::FSize = NIL
      ::FSize := dbs_Len
   ENDIF

   RETURN

/*
    SetDefaultValue
*/
METHOD PROCEDURE SetDefaultValue( DefaultValue ) CLASS TStringField

   ::FDefaultValue := DefaultValue

   ::FBuffer := NIL /* to force ::Reset on next read */

   RETURN

/*
    SetSize
*/
METHOD PROCEDURE SetSize( size ) CLASS TStringField

   ::FSize := size
   ::FDBS_LEN := size

   RETURN

/*
    ENDCLASS TStringField
*/
