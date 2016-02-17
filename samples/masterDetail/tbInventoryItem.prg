
#include "oordb.ch"

CLASS TBInventoryItem FROM MyTableBase

EXPORTED:

    PROPERTY tableFileName INIT "invitem"

    DEFINE FIELDS
    DEFINE PRIMARY INDEX
    DEFINE SECONDARY INDEX

    METHOD onAfterOpen()

ENDCLASS

/*
    FIELDS
*/
BEGIN FIELDS CLASS TBInventoryItem

    /* InventoryItemId */
    ADD AUTOINC FIELD "InvItemId" NAME "InventoryItemId"

    /* Name */
    ADD STRING FIELD "Name" SIZE 80

    /* Date */
    ADD DATE FIELD "Date" ;
        NEWVALUE {|| date() }

    /* Price */
    ADD FLOAT FIELD "Price" PICTURE "999,999.99" ;
        NEWVALUE {|| hb_random( 100 ) }

END FIELDS CLASS

/*
    PRIMARY INDEX
*/
BEGIN PRIMARY INDEX CLASS TBInventoryItem

    DEFINE INDEX "Primary" KEYFIELD "InventoryItemId"

END PRIMARY INDEX CLASS

/*
    SECONDARY INDEX
*/
BEGIN SECONDARY INDEX CLASS TBInventoryItem

    DEFINE INDEX "Name" KEYFIELD "Name" UNIQUE  /* validates don't duplicate item's name */

END SECONDARY INDEX CLASS

/*
    onAfterOpen
*/
METHOD PROCEDURE onAfterOpen() CLASS TBInventoryItem
    LOCAL itm

    IF ::count() = 0

        /* add some 10 Inventory Items */
        FOR itm := 1 TO 10
            IF ::insert()
                ::Field_Name:value := "INVENTORY ITEM #" + hb_nToS( itm )
                ::post()
            ENDIF
        NEXT
        
    ENDIF

    ::super:onAfterOpen()

RETURN
