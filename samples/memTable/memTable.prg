
#include "oordb.ch"

REQUEST HB_MEMIO

PROCEDURE Main()
    LOCAL tbMemTable
    LOCAL itm

    /* create mem table */
    tbMemTable := TBMemTable():new()

    /* top record */
    IF tbMemTable:dbGoTop()
        WHILE ! tbMemTable:eof()
            ? tbMemTable:recNo()
            FOR EACH itm IN tbMemTable:valueList
                ?? itm
            NEXT
            tbMemTable:dbSkip()
        ENDDO
    ENDIF

RETURN
