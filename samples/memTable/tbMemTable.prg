
#include "oordb.ch"

CLASS TBMemTable FROM TTable

  DEFINE DATABASE WITH MyDataBase CLASS

EXPORTED:

    PROPERTY tableFileName INIT "mem:memtable"

    DEFINE FIELDS
    DEFINE PRIMARY INDEX
    DEFINE SECONDARY INDEX

    METHOD onAfterOpen()

ENDCLASS

/*
    FIELDS
*/
BEGIN FIELDS CLASS TBMemTable

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
BEGIN PRIMARY INDEX CLASS TBMemTable

    DEFINE INDEX "Primary" KEYFIELD "InventoryItemId"

END PRIMARY INDEX CLASS

/*
    SECONDARY INDEX
*/
BEGIN SECONDARY INDEX CLASS TBMemTable

    DEFINE INDEX "Name" KEYFIELD "Name" UNIQUE  /* validates don't duplicate item's name */

END SECONDARY INDEX CLASS

/*
    onAfterOpen
*/
METHOD PROCEDURE onAfterOpen() CLASS TBMemTable
    LOCAL itm

    IF ::count() = 0

        /* add some 10 Inventory Items */
        FOR itm := 1 TO 100
            IF ::insert()
                ::Field_Name:value := "INVENTORY ITEM #" + hb_nToS( itm )
                ::post()
            ENDIF
        NEXT
        
    ENDIF

    ::super:onAfterOpen()

RETURN
