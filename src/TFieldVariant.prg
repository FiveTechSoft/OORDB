/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldVariant
*/
CLASS TFieldVariant FROM TField

PROTECTED:

   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "M"
   DATA FFieldType INIT ftVariant
   DATA FSize INIT 0
   DATA FType INIT "Variant"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Variante"} )

PUBLIC:

    PROPERTY KeySize READ Size

ENDCLASS

/*
    ENDCLASS TFieldVariant
*/
