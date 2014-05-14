/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldDateTime
*/
CLASS TFieldDateTime FROM TField

   PRIVATE:

   PROTECTED:
   DATA FSize INIT 23
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "@"
   DATA FNewValue INIT {|| hb_CToT( "" ) }
   DATA FDefaultValue INIT {|| hb_DateTime() }
   DATA FFormatDate
   DATA FFormatTime
   DATA FType INIT "DateTime"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Fecha Hora"} )
   DATA FValType INIT "C"
   METHOD GetAsDate() INLINE hb_TToD( ::Value )
   METHOD GetAsTime()
   METHOD GetEmptyValue BLOCK {|| hb_CToT( "" ) }
   METHOD GetFormatDate INLINE iif( ::FFormatDate = NIL, ::ClsFmtDate, ::FFormatDate )
   METHOD GetFormatTime INLINE iif( ::FFormatTime = NIL, ::ClsFmtTime, ::FFormatTime )
   METHOD SetAsDate( date ) INLINE ::Value := hb_DToT( date, ::GetAsTime )
   METHOD SetAsTime( cTime )
   METHOD SetFormatDate( formatDate ) INLINE ::FFormatDate := formatDate
   METHOD SetFormatTime( formatTime ) INLINE ::FFormatTime := formatTime
   PUBLIC:

   CLASSDATA ClsFmtDate INIT "YYYY-MM-DD"
   CLASSDATA ClsFmtTime INIT "HH:MM"

   METHOD DiffSeconds( dateTimePrev )
   METHOD GetAsDisplay INLINE ::GetAsString
   METHOD GetAsDisplayEmptyValue INLINE ::GetAsString( ::GetEmptyValue )
   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsDate READ GetAsDate WRITE SetAsDate
   PROPERTY AsTime READ GetAsTime WRITE SetAsTime
   PROPERTY FormatDate READ GetFormatDate WRITE SetFormatDate
   PROPERTY FormatTime READ GetFormatTime WRITE SetFormatTime

   PUBLISHED:

ENDCLASS

/*
    DiffSeconds
*/
METHOD FUNCTION DiffSeconds( dateTimePrev ) CLASS TFieldDateTime

   LOCAL t1, t2
   LOCAL t

   IF dateTimePrev = NIL
      dateTimePrev := hb_DateTime()
   ENDIF

   IF dateTimePrev = ::Value
      RETURN 0.0
   ENDIF

   IF dateTimePrev < ::Value
      t1 := ::Value
      t2 := dateTimePrev
   ELSE
      t2 := ::Value
      t1 := dateTimePrev
   ENDIF

   RETURN ( hb_TToD( hb_NToT( t1 - t2 ), @t ) - CToD( "" ) ) * 86400 + t

/*
    GetAsString
*/
METHOD FUNCTION GetAsString( value ) CLASS TFieldDateTime

   IF value = NIL
      value := ::Value
   ENDIF
   IF ::FormatDate = NIL .AND. ::FormatTime = NIL
      RETURN hb_TSToStr( value )
   ENDIF

   RETURN hb_TToC( value, ::FormatDate, ::FormatTime )

/*
    GetAsTime
*/
METHOD GetAsTime() CLASS TFieldDateTime

   LOCAL cTime := "00:00:00"
   LOCAL time

   time := ::GetAsVariant()

   IF !Empty( time )
      HB_TToD( time, @cTime, ::FormatTime )
   ENDIF

   RETURN cTime

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TFieldDateTime

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'T'
      RETURN hb_TToS( keyVal )
   CASE 'U'
      RETURN hb_TToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::Name ERROR "Don't know how to convert to key value..."

   RETURN hb_TToS( keyVal )

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TFieldDateTime

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_TToS(" + fieldName + ")"

/*
    SetAsTime
*/
METHOD PROCEDURE SetAsTime( cTime ) CLASS TFieldDateTime

   ::SetAsVariant( hb_DToT( ::Value, cTime ) )

   RETURN

/*
    SetAsVariant
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TFieldDateTime

   SWITCH ValType( variant )
   CASE 'T'
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'C'
      variant := RTrim( variant )
      IF NumToken( variant ) > 1
         variant := hb_CToT( variant, ::FormatDate, ::FormatTime )
      ELSE
         variant := hb_SToT( variant )
      ENDIF
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'D'
      ::Super:SetAsVariant( hb_DToT( variant, ::GetAsTime() ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( hb_NToT( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    EndClass TFieldDateTime
*/
