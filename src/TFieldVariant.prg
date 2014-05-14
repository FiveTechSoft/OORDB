/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldVariant
*/
CLASS TFieldVariant FROM TField

   PRIVATE:

   PROTECTED:
   DATA FType INIT "Variant"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Variante"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TFieldVariant
*/
