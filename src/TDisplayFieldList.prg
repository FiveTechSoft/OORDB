/*
 * $Id: TDisplayFieldList.prg 104 2013-03-02 21:25:49Z tfonrouge $
 */

#include "hbclass.ch"
#include "oordb.ch"
#include "property.ch"

/*
    TDisplayFieldList : Simple Class to display fields
    Teo. Mexico 2013
*/
CLASS TDisplayFieldList

   PUBLIC:

   DATA __FLastLabel
   DATA __FObj
   DATA __FSyncFromAlias

   METHOD _ INLINE ::__FObj

ENDCLASS
