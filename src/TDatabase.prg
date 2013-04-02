/*
 * $Id: TDatabase.prg 17 2012-03-07 18:25:47Z tfonrouge $
 * TDataBase
 */

#include "hbclass.ch"
#include "property.ch"
#include "oordb.ch"
#include "xerror.ch"

#define OORDB_DEFAULT_TABLEAUTOCREATE   .T.

/*
    TDataBase
    Teo. Mexico 2008
*/
CLASS TDataBase FROM OORDBBASE
PRIVATE:
    DATA cmdLevel     INIT { NIL }
PROTECTED:
    DATA FName
    METHOD DefineRelations VIRTUAL
    METHOD GetName() INLINE iif( Empty( ::FName ), ::ClassName, ::FName )
    METHOD SetDirectory( directory ) INLINE ::FDirectory := directory
    METHOD SetName( name ) INLINE ::FName := name
    METHOD SetNetIO( netIO ) INLINE ::FNetIO := netIO
PUBLIC:

    DATA OpenBlock

    CONSTRUCTOR New( databaseName )

    METHOD AddParentChild( parentTableName, childTableName, indexName, virtual, autoDelete )

    METHOD cmdAddTable( tableName, indexName, virtual, autoDelete )
    METHOD cmdDefineChild()
    METHOD cmdEndChild()

    METHOD GetParentChildList( tableName, Result )
    METHOD TableIsChildOf( table, fromTable )

    PROPERTY Name            READ GetName WRITE SetName
    PROPERTY NetIO           WRITE SetNetIO INIT .F.
    PROPERTY RddDriver       INIT "DBFCDX"
    PROPERTY TableAutoCreate INIT OORDB_DEFAULT_TABLEAUTOCREATE
    PROPERTY ChildParentList INIT HB_HSetCaseMatch( {=>}, .F. )
    PROPERTY Directory       WRITE SetDirectory INIT "" 
    PROPERTY ParentChildList INIT HB_HSetCaseMatch( {=>}, .F. )
    PROPERTY TableList       INIT HB_HSetCaseMatch( {=>}, .F. )
PUBLISHED:
ENDCLASS

/*
    New
    Teo. Mexico 2008
*/
METHOD New( databaseName ) CLASS TDataBase
    IF databaseName == NIL
        ::FName := ::ClassName
    ELSE
        ::FName := databaseName
    ENDIF
    IF !Empty( ::FDirectory ) .AND. !HB_DirExists( ::FDirectory )
        MakeDir( ::FDirectory )
    ENDIF
    ::DefineRelations()
RETURN Self

/*
    AddParentChild
    Teo. Mexico 2008
*/
METHOD AddParentChild( parentTableName, childTableName, indexName, virtual, autoDelete ) CLASS TDataBase

    IF ! HB_HHasKey( ::FParentChildList, parentTableName )
        ::FParentChildList[ parentTableName ] := {}
    ENDIF

    IF HB_HHasKey( ::FTableList, childTableName )
        ::Error_Table_already_defined()
    ENDIF

    ::FTableList[ childTableName ] := HB_HSetCaseMatch( {=>}, .F. )
    ::FTableList[ childTableName, "IndexName" ]  := indexName
    ::FTableList[ childTableName, "Virtual"   ]  := virtual == .T.
    ::FTableList[ childTableName, "AutoDelete" ] := autoDelete == .T.

    AAdd( ::FParentChildList[ parentTableName ], Upper( childTableName ) )

    ::FChildParentList[ childTableName ] := parentTableName

RETURN Self

/*
    cmdAddTable
    Teo. Mexico 2008
*/
METHOD PROCEDURE cmdAddTable( tableName, indexName, virtual, autoDelete ) CLASS TDataBase

    ::cmdLevel[ Len( ::cmdLevel ) ] := { tableName, indexName, virtual }

    IF Len( ::cmdLevel ) > 1
        ::AddParentChild( ::cmdLevel[ Len( ::cmdLevel ) - 1, 1 ], tableName, indexName, virtual, autoDelete )
    ENDIF

RETURN

/*
    cmdDefineChild
    Teo. Mexico 2008
*/
METHOD PROCEDURE cmdDefineChild() CLASS TDataBase
    AAdd( ::cmdLevel, { NIL } )
RETURN

/*
    cmdEndChild
    Teo. Mexico 2008
*/
METHOD PROCEDURE cmdEndChild() CLASS TDataBase
    ASize( ::cmdLevel, Len( ::cmdLevel ) - 1 )
RETURN

/*
    GetParentChildList
    Exclude VIRTUAL Tables
    Teo. Mexico 2008
*/
METHOD FUNCTION GetParentChildList( tableName, Result ) CLASS TDataBase
    LOCAL childTableName

    IF Result == NIL
        Result := {}
        /* If Table is VIRTUAL then search for childs in next child level
             Parent tables without parent aren't in the TableList
        */
        IF HB_HHasKey( ::FTableList, tableName ) .AND. ::FTableList[ tableName, "Virtual" ]
            IF HB_HHasKey( ::FParentChildList, tableName )
                FOR EACH childTableName IN ::FParentChildList[ tableName ]
                    ::GetParentChildList( childTableName, Result )
                NEXT
            ENDIF
            RETURN Result
        ENDIF
    ENDIF

    IF HB_HHasKey( ::FParentChildList, tableName )
        FOR EACH childTableName IN ::FParentChildList[ tableName ]
            IF ::FTableList[ childTableName, "Virtual" ]
                IF HB_HHasKey( ::FParentChildList, childTableName )
                    ::GetParentChildList( childTableName, Result )
                ENDIF
            ELSE
                AAdd( Result, childTableName )
            ENDIF
        NEXT
    ENDIF

RETURN Result

/*
    TableIsChildOf
    Teo. Mexico 2008
*/
METHOD FUNCTION TableIsChildOf( table, fromTable ) CLASS TDataBase
    LOCAL Result

    Result := HB_HHasKey( ::FParentChildList, fromTable ) .AND. AScan( ::FParentChildList[ fromTable ], {|e| e == Upper( table ) } ) > 0

RETURN Result

/*
    EndClass TDataBase
*/
