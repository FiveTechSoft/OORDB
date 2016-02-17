
#include "oordb.ch"

PROCEDURE Main()
    LOCAL tbCustomer
    LOCAL tbInvoice
    LOCAL tbInventoryItem
    LOCAL inventoryItemId

    /* Customer's table */
    tbCustomer := TBCustomer():new()

    /* Invoices table */
    tbInvoice := TBInvoice():new()

    /* Inventory Items table */
    tbInventoryItem := TBInventoryItem():new()

    ? "Add one invoice..."
    IF tbInvoice:insert()

        tbInvoice:Field_Customer:value := hb_randomInt( tbCustomer:count )

        ? "Add SOME invoice items..."
        WHILE .T.
            inventoryItemId := hb_randomInt( tbInventoryItem:count() )
            IF tbInvoice:Field_InvoiceItems:dataObj:Field_InventoryItem:seek( inventoryItemId )
                EXIT /* we found a previously inventory item id, exit because is a UNIQUE key field */
            ELSE
                tbInventoryItem:value := inventoryItemId /* db cursor in inventoryItemId */
                IF tbInvoice:Field_InvoiceItems:dataObj:insert()
                    tbInvoice:Field_InvoiceItems:dataObj:Field_InventoryItem:value := tbInventoryItem:value
                    tbInvoice:Field_InvoiceItems:dataObj:Field_Price:value := tbInventoryItem:Field_Price:value
                    tbInvoice:Field_InvoiceItems:dataObj:post()
                ENDIF
            ENDIF
        ENDDO

        tbInvoice:post()

    ENDIF

    WAIT "Press any key to continue..."

    ?
    ?

    ? "List Invoices so far by DATE field ..."
    IF tbInvoice:index_date:dbGoTop()

        WHILE ! tbInvoice:index_date:eof()
            ? "      Invoice Id:", tbInvoice:Field_InvoiceId:value
            ? "    Invoice Date:", tbInvoice:Field_Date:value
            ? "Invoice Customer:", tbInvoice:Field_Customer:dataObj:Field_Name:value
            ? "   Invoice Total:", tbInvoice:Field_Total:value
            ? " # Invoice items:", tbInvoice:Field_InvoiceItems:dataObj:count
            ? "                :"
            IF tbInvoice:Field_InvoiceItems:dataObj:dbGoTop()

                WHILE ! tbInvoice:Field_InvoiceItems:dataObj:eof()
                    ? e"      Item Name:", tbInvoice:Field_InvoiceItems:dataObj:Field_InventoryItem:dataObj:Field_Name:value
                    ? e"   $ Item Price:", tbInvoice:Field_InvoiceItems:dataObj:Field_Price:value
                    tbInvoice:Field_InvoiceItems:dataObj:dbSkip()
                ENDDO

            ENDIF

            ? replicate( "-", 40 )

            tbInvoice:index_date:dbSkip()

        ENDDO

    ENDIF

RETURN
