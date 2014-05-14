/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TMemoField
*/
CLASS TMemoField FROM TStringField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "M"
   DATA FFieldType INIT ftMemo
   DATA FSize INIT 0
   DATA FType INIT "Memo"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Memo"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TMemoField
*/
