
#include "oordb.ch"

CLASS TBInvoice FROM MyTableBase

EXPORTED:

    PROPERTY tableFileName INIT "invoice"

    DEFINE FIELDS
    DEFINE PRIMARY INDEX
    DEFINE SECONDARY INDEX

    METHOD CALCFIELD Total()

ENDCLASS

/*
    FIELDS
*/
BEGIN FIELDS CLASS TBInvoice

    /* InvoiceId */
    ADD AUTOINC FIELD "InvoiceId"

    /* Customer */
    ADD TABLE FIELD "Customer" ;
        CLASS "TBCustomer"

    /* Date */
    ADD DATE FIELD "Date" ;
        NEWVALUE {|| date() - hb_randomInt( 30 ) }

    /* InvoiceItems */
    ADD CALCULATED TABLE FIELD "InvoiceItems" ;
        CLASS "TBInvoiceItem_Invoice"

    /* Total */
    ADD CALCULATED FLOAT FIELD "Total"

END FIELDS CLASS

/*
    PRIMARY INDEX
*/
BEGIN PRIMARY INDEX CLASS TBInvoice

    DEFINE INDEX "Primary" KEYFIELD "InvoiceId"

END PRIMARY INDEX CLASS

/*
    SECONDARY INDEX
*/
BEGIN SECONDARY INDEX CLASS TBInvoice

    DEFINE INDEX "Date" KEYFIELD "Date"

END SECONDARY INDEX CLASS

/*
    Total
*/
METHOD CALCFIELD Total() CLASS TBInvoice
    LOCAL total := 0.0

    IF ::Field_InvoiceItems:dataObj:dbGoTop()
        WHILE ! ::Field_InvoiceItems:dataObj:eof()
            total += ::Field_InvoiceItems:dataObj:Field_Price:value
            ::Field_InvoiceItems:dataObj:dbSkip()
        ENDDO
    ENDIF

RETURN total
