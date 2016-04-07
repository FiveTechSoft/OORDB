
#include "oordb.ch"

PROCEDURE Main()
    LOCAL tbMemTable1
    LOCAL tbMemTable2
    LOCAL tbMemTable3

    CLS

    /* create mem table 1 (temporal) */
    tbMemTable1 := TBMemTable():new()

    printRecords( tbMemTable1:indexByName("First") )

    /* create mem table 2 (temporal) */
    tbMemTable2 := TBMemTable():new()

    printRecords( tbMemTable2:indexByName("Last") )

    /* create mem table 3 (temporal) */
    tbMemTable3 := TBMemTable():new()

    printRecords( tbMemTable3:indexByName("State") )

RETURN

/*
    printRecords
*/
STATIC PROCEDURE printRecords( index )
    LOCAL itm

    ? "List of records indexed by " + index:name

    /* top record */
    IF index:dbGoTop()
        WHILE ! index:eof()
            ?
            FOR EACH itm IN index:table:recordValueList
                ?? itm
            NEXT
            index:dbSkip()
        ENDDO
    ENDIF

    WAIT

RETURN
