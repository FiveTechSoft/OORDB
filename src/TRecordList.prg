/*
 * TRecordList
 */

#include "oordb.ch"

CLASS TRecordList FROM OORDBBASE
PROTECTED:
PUBLIC:

    CONSTRUCTOR New( table )

    METHOD Add( itmParam )

    METHOD Clear

    METHOD Op_Index( index ) OPERATOR "[]"

    METHOD Size INLINE Len( ::FList )

    PROPERTY List
    PROPERTY Table

ENDCLASS

/*
    New
*/
METHOD New( table ) CLASS TRecordList
    ::FTable := table
    ::FList := {}
RETURN Self

/*
    Add
*/
METHOD FUNCTION Add( itmParam ) CLASS TRecordList
    LOCAL vt
    LOCAL itm

    vt := ValType( itmParam )

    IF vt = "O"
        IF itmParam:IsDerivedFrom( ::FTable:BaseKeyIndex:TableBaseClass )
            itm := itmParam:Value
        ELSEIF itmParam:IsDerivedFrom( "TFieldTable" ) .AND. itmParam:LinkedTable:IsDerivedFrom( ::FTable:BaseKeyIndex:TableBaseClass )
            itm := itmParam:DataObj:Value
        ENDIF
    ELSE
        IF vt == ValType( ::FTable:BaseKeyField:Value )
            itm := itmParam
        ENDIF
    ENDIF

    IF itm != NIL
        AAdd( ::FList, itm )
    ELSE
        ::ERROR_ADDING_VALUE()
    ENDIF

RETURN Self

/*
    Clear
*/
METHOD PROCEDURE Clear() CLASS TRecordList
    ASize( ::FList, 0 )
RETURN

/*
    Op_Index
*/
METHOD FUNCTION Op_Index( index ) CLASS TRecordList
    IF index >= 1 .AND. index <= Len( ::FList )
        ::FTable:BaseKeyIndex:Seek( ::FList[ index ] )
    ELSE
        ::FTable:DbGoTo( 0 )
    ENDIF
RETURN ::FTable
