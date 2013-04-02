/*
 * $Id: TDisplayFields.prg 104 2013-03-02 21:25:49Z tfonrouge $
 */

#include "hbclass.ch"
#include "oordb.ch"
#include "property.ch"

/*
    TDisplayFields : Simple Class to display fields
    Teo. Mexico 2013
*/
CLASS TDisplayFields
PUBLIC:
    DATA __FFields
    DATA __FLastLabel
    DATA __FObj
    DATA __FSyncFromAlias
    METHOD _ INLINE ::__FObj
ENDCLASS
