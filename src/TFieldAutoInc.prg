/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TFieldAutoInc
*/
CLASS TFieldAutoInc FROM TFieldInteger

   PRIVATE:

   PROTECTED:
   DATA FDBS_TYPE INIT "+"
   DATA FType INIT "AutoInc"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"AutoInc"} )
   DATA FFieldType INIT ftAutoInc
   PUBLIC:
   PUBLISHED:

ENDCLASS
/*
    ENDCLASS TFieldAutoInc
*/
