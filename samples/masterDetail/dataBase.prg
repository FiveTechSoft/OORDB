
#include "oordb.ch"

/*
    database with settings for whole app tables
*/
CLASS MyDataBase FROM TDatabase

  PROPERTY Directory VALUE "data"

  METHOD defineRelations()

ENDCLASS

/*
    defineRelations : master <-> detail
*/
METHOD PROCEDURE defineRelations() CLASS MyDataBase

    ADD TABLE "TBInvoice"
        DEFINE CHILD
            ADD TABLE "TBInvoiceItem_Invoice"
            /// ADD TABLE "Another detail table of TBInvoice"
        END CHILD

RETURN
