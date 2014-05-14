/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TVariantField
*/
CLASS TVariantField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FType INIT "Variant"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Variante"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TVariantField
*/
