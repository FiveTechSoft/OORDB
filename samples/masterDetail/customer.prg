
#include "oordb.ch"

CLASS TBCustomer FROM MyTableBase

EXPORTED:

    PROPERTY tableFileName INIT "customer"

    DEFINE FIELDS
    DEFINE PRIMARY INDEX
    DEFINE SECONDARY INDEX

    METHOD onAfterOpen()

ENDCLASS

/*
    FIELDS
*/
BEGIN FIELDS CLASS TBCustomer

    /* CustomerId */
    ADD AUTOINC FIELD "CustomerId"

    /* Name */
    ADD STRING FIELD "Name" SIZE 40

END FIELDS CLASS

/*
    PRIMARY INDEX
*/
BEGIN PRIMARY INDEX CLASS TBCustomer

    DEFINE INDEX "Primary" KEYFIELD "CustomerId"

END PRIMARY INDEX CLASS

/*
    SECONDARY INDEX
*/
BEGIN SECONDARY INDEX CLASS TBCustomer

    DEFINE INDEX "Name" KEYFIELD "Name"

END SECONDARY INDEX CLASS

/*
    onAfterOpen
*/
METHOD PROCEDURE onAfterOpen() CLASS TBCustomer
    LOCAL itm

    IF ::count() = 0

        /* add some 10 customers */
        FOR itm := 1 TO 10
            IF ::insert()
                ::Field_Name:value := "CUSTOMER #" + hb_nToS( itm )
                ::post()
            ENDIF
        NEXT
        
    ENDIF

    ::super:onAfterOpen()

RETURN
