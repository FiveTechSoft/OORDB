/*
 * TRecordList
 */

#include "oordb.ch"

CLASS TRecordList FROM OORDBBASE
PROTECTED:
    DATA FList
    METHOD GetList INLINE ::FList[ ::Findex ]
PUBLIC:

    CONSTRUCTOR New( table )

    METHOD Add( itmParam )

    METHOD Clear

    METHOD Op_Index( index ) OPERATOR "[]"

    METHOD Size INLINE Len( ::FList[ ::Findex ] )

    PROPERTY index
    PROPERTY List READ GetList
    PROPERTY Table

    METHOD ListPull()
    METHOD ListPush()

ENDCLASS

/*
    New
*/
METHOD New( table ) CLASS TRecordList
    ::FTable := table
    ::Findex := 1
    ::FList := { { } }
RETURN Self

/*
    Add
*/
METHOD FUNCTION Add( itmParam ) CLASS TRecordList
    LOCAL vt
    LOCAL itm

    vt := ValType( itmParam )

    SWITCH vt
    CASE "O"
        IF itmParam:IsDerivedFrom( ::FTable:BaseKeyIndex:TableBaseClass )
            itm := itmParam:Value
        ELSEIF itmParam:IsDerivedFrom( "TFieldTable" ) .AND. itmParam:LinkedTable:IsDerivedFrom( ::FTable:BaseKeyIndex:TableBaseClass )
            itm := itmParam:DataObj:Value
        ENDIF
        EXIT
    CASE "U"
        IF !::FTable:Eof()
            itm := ::FTable:BaseKeyField:Value
        ENDIF
        EXIT
    OTHERWISE
        IF vt == ValType( ::FTable:BaseKeyField:Value )
            itm := itmParam
        ENDIF
    ENDSWITCH

    IF itm != NIL
        AAdd( ::FList[ ::Findex ], itm )
    ELSE
        ::ERROR_ADDING_VALUE()
    ENDIF

RETURN Self

/*
    Clear
*/
METHOD PROCEDURE Clear() CLASS TRecordList
    ASize( ::FList[ ::Findex ], 0 )
RETURN

/*
    ListPull
*/
METHOD PROCEDURE ListPull() CLASS TRecordList
    ::Findex--
RETURN

/*
    ListPush
*/
METHOD PROCEDURE ListPush() CLASS TRecordList
    ::Findex++
    AAdd( ::FList, {} )
RETURN

/*
    Op_Index
*/
METHOD FUNCTION Op_Index( index ) CLASS TRecordList
    IF index >= 1 .AND. index <= Len( ::FList[ ::Findex ] )
        ::FTable:BaseKeyIndex:Seek( ::FList[ ::Findex, index ] )
    ELSE
        ::FTable:DbGoTo( 0 )
        RETURN NIL
    ENDIF
RETURN ::FTable
