/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFloatField
*/
CLASS TFloatField FROM TNumericField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "B"
   DATA FType INIT "Float"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Numerico Decimal"} )
   METHOD StrFormat( value ) INLINE hb_StrFormat( "%10." + Chr( 48 + ::FDBS_DEC ) + "f", value )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TFloatField
*/
