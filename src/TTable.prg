/*
 * $Id: TTable.prg 154 2013-06-04 20:50:27Z tfonrouge $
 */

/*
    TTable
    Teo. Mexico 2007
*/

#include "oordb.ch"
#include "error.ch"
#include "xerror.ch"

#include "dbinfo.ch"

#define rxMasterSourceTypeNone     0
#define rxMasterSourceTypeTTable   1
#define rxMasterSourceTypeTField   2
#define rxMasterSourceTypeBlock    3

#define OORDB_DEFAULT_AUTOCREATE    .T.

STATIC FErrorBlock
STATIC BaseKeyFieldList := {}

REQUEST TField

FUNCTION OordbErrorNew( Self, description, args )

   LOCAL oErr := ErrorNew()

   oErr:cargo := Self
   oErr:description := description
   oErr:args := args

   IF ::IsDerivedFrom( "TField" )
      oErr:operation := "Table: " + ::Table:ClassName() + E"\nField: " + ::Name
   ENDIF

   RETURN oErr

/*
    __ClsInstFromName (Just UpperCase in __ClsInstName)
    Teo. Mexico 2007
*/
FUNCTION __ClsInstFromName( ClassName )
   RETURN __ClsInstName( Upper( ClassName ) )

/*
    ErrorBlockOORDB
    Teo. Mexico 2012
*/
FUNCTION ErrorBlockOORDB( oErr )

   // By default, division by zero results in zero
   IF oErr:genCode == EG_ZERODIV .AND. oErr:canSubstitute
      RETURN 0
   ENDIF

   // By default, retry on RDD lock error failure */
   IF oErr:genCode == EG_LOCK .AND. oErr:canRetry
      // oErr:tries++
      RETURN .T.
   ENDIF

   // Set NetErr() of there was a database open error
   IF oErr:genCode == EG_OPEN .AND. ;
         oErr:osCode == 32 .AND. ;
         oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oErr:genCode == EG_APPENDLOCK .AND. oErr:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF .T.
      Break( oErr )
   ENDIF

   RETURN NIL

/*
    TTable
    Teo. Mexico 2007
*/
CLASS TTable FROM OORDBBASE

   PRIVATE:

   CLASSDATA FFieldTypes
   CLASSDATA FInstances INIT hb_HSetCaseMatch( { => }, .F. )

   DATA FActive    INIT .F.
   DATA FAddress
   DATA FAlias
   DATA FDisplayFields          // Contains a Object
   DATA FHasDeletedOrder INIT .F.
   DATA FIndex              // Current TIndex in Table
   DATA FMasterSource
   DATA FMasterSourceType  INIT rxMasterSourceTypeNone
   DATA FPort

   /* TODO: Check if we can re-use a client socket */
   DATA FRDOClient

   DATA FRecNoBeforeInsert

   DATA FReadOnly              INIT .F.
   DATA FRemote    INIT .F.
   DATA FState INIT dsInactive
   DATA FSubState INIT dssNone
   DATA FSyncingToContainerField INIT .F.
   DATA FTimer INIT 0
   DATA FUndoList

   METHOD DbGoBottomTop( n )
   METHOD GetAlias
   METHOD GetDbStruct
   METHOD GetFieldTypes
   METHOD GetIndexName() INLINE iif( ::GetIndex() = NIL, "", ::GetIndex():Name )
   METHOD GetInstance
   METHOD GetKeyExpression()
   METHOD GetKeyField()
   METHOD GetKeyString INLINE iif( ::GetKeyField == NIL, "", ::GetKeyField:AsString )
   METHOD GetMasterKeyExpression()
   METHOD GetMasterKeyField()
   METHOD GetMasterKeyString INLINE iif( ::GetMasterKeyField == NIL, "", ::GetMasterKeyField:AsString )
   METHOD GetMasterKeyVal INLINE iif( ::GetMasterKeyField == NIL, "", ::GetMasterKeyField:GetKeyVal )
   METHOD GetMasterSource()
   METHOD SetIndex( index )
   METHOD SetIndexName( IndexName )
   METHOD SetMasterSource( masterSource )
   METHOD SetReadOnly( readOnly )
   METHOD SetState( state )
   METHOD SetSyncingToContainerField( value ) INLINE ::FSyncingToContainerField := value
   METHOD Process_TableName( tableName )
   // METHOD SendToServer

   PROTECTED:

   CLASSDATA hDataBase INIT hb_HSetCaseMatch( { => }, .F. )

   DATA FAutoCreate
   DATA FBaseKeyField
   DATA FBaseKeyIndex
   DATA FBof    INIT .T.
   DATA FCustomIndexList   INIT {}
   DATA FDataBaseClass
   DATA FEof    INIT .T.
   DATA FFieldList         INIT {}
   DATA FFilledFieldList   INIT .F.
   DATA FDbFilter
   DATA FIndexList   INIT hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )  // <className> => <indexName> => <indexObject>
   DATA FInitialized       INIT .F.
   DATA FisMetaTable       INIT .T.
   DATA FIsTempTable        INIT .F.
   DATA FFound    INIT .F.
   DATA FMasterSourceFieldBuffer INIT hb_HSetCaseMatch( { => }, .F. )
   DATA FOnActiveSetKeyVal  INIT .F.
   DATA FPrimaryIndex
   DATA FPrimaryIndexList INIT hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )  // <className> => <indexName>
   DATA FRecNo    INIT 0
   DATA FTableFileName     INIT "" // to be assigned (INIT) on inherited classes
   DATA tableState INIT {}
   DATA tableStateLen INIT 0

   METHOD __CheckIndexes()
   METHOD AddRec()
   METHOD CheckDbStruct()
   METHOD CreateTableInstance()
   METHOD DefineFieldsFromDb()
   METHOD FillFieldList()
   METHOD FillPrimaryIndexes( curClass )
   METHOD FixDbStruct( aNewStruct, message )
   METHOD GetAutoCreate() INLINE iif( ::FAutoCreate = NIL, iif( ::DataBase = NIL, OORDB_DEFAULT_AUTOCREATE, ::DataBase:TableAutoCreate ), ::FAutoCreate )
   METHOD GetBof()
   METHOD GetDataBase()
   METHOD GetEof()
   METHOD GetErrorBlock() INLINE iif( FErrorBlock = NIL, FErrorBlock := {| oErr| ErrorBlockOORDB( oErr ) }, FErrorBlock )
   METHOD GetFound()
   METHOD GetHasFilter()
   METHOD GetIndex()
   METHOD GetRecNo()
   METHOD InitDataBase INLINE TDataBase():New()
   METHOD InitTable()
   METHOD RawGet4Seek( direction, xField, keyVal, index, softSeek )
   METHOD SetDataBase( dataBase )
   METHOD SetErrorBlock( errorBlock ) INLINE FErrorBlock := errorBlock
   METHOD SetisMetaTable( isMetaTable )
   METHOD SetTableFileName( tableFileName ) INLINE ::FTableFileName := tableFileName
   METHOD UpdateCustomIndexes()

   PUBLIC:

   DATA aliasIdx
   DATA aliasTmp
   DATA allowOnDataChange  INIT .F.
   DATA autoEdit           INIT .F.
   DATA autoMasterSource   INIT .F.
   DATA autoOpen           INIT .T.
   DATA dataIsOEM          INIT .T.
    /*!
        array of possible TObjectField's that have this (SELF) object referenced
     */
   DATA DetailSourceList INIT { => }
   DATA ExternalIndexList INIT { => }
   DATA FieldNamePrefix INIT "Field_" // Table Field Name prefix
   DATA FUnderReset INIT .F.
   DATA fullFileName
   DATA LinkedObjField

   DATA OnDataChangeBlock
   DATA OnDataChangeBlock_Param

   DATA validateDbStruct INIT .T.      // On Open, Check for a valid struct dbf (against DEFINE FIELDS )

   CONSTRUCTOR New( MasterSource, tableName )
   DESTRUCTOR OnDestruct()
   // ON ERROR FUNCTION OODB_ErrorHandler( ... )

   METHOD _( syncFromAlias ) INLINE ::GetDisplayFields( syncFromAlias )

   METHOD __DefineFields() VIRTUAL         // DEFINE FIELDS
   METHOD __DefinePrimaryIndex() VIRTUAL   // DEFINE PRIMARY INDEX
   METHOD __DefineSecondaryIndexes() VIRTUAL        // DEFINE SECONDARY INDEX

   METHOD BaseSeek( direction, Value, index, lSoftSeek )
   METHOD BuildFieldBlockFromFieldExpression( fieldExp, returnMode, field )
   METHOD AddCustomIndex( index )
   METHOD AddFieldAlias( nameAlias, fld, private )
   METHOD AddFieldMessage( messageName, AField, isAlias )
   METHOD AssociateTableIndex( table, name, getRecNo, setRecNo )
   METHOD Cancel
   METHOD Childs( ignoreAutoDelete, block, curClass, childs )
   METHOD ChildSource( tableName, destroyChild )
   METHOD CopyRecord( origin )
   METHOD COUNT( bForCondition, bWhileCondition, index, scope )
   METHOD CreateIndex( index )
   METHOD CreateTempIndex( index )
   METHOD CreateTable( fullFileName )
   METHOD DefineRelations       VIRTUAL
   METHOD Destroy()
   METHOD dbEval( bBlock, bForCondition, bWhileCondition, index, scope )
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD dbGoto( RecNo )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs )
   METHOD DELETE( lDeleteChilds )
   METHOD DeleteChilds()
   METHOD Edit()
   METHOD FieldByName( name, index )
   METHOD FieldByObjClass( objClass, derived, index )
   METHOD FilterEval( index )
   METHOD FindIndex( index )
   METHOD FindMasterSourceField( detailField )
   METHOD Get4Seek( xField, keyVal, index, softSeek ) INLINE ::RawGet4Seek( 1, xField, keyVal, index, softSeek )
   METHOD Get4SeekLast( xField, keyVal, index, softSeek ) INLINE ::RawGet4Seek( 0, xField, keyVal, index, softSeek )
   METHOD GetAsString
   METHOD GetCurrentRecord( idxAlias )
   METHOD GetDisplayFieldBlock( xField )
   METHOD GetDisplayFields( syncFromAlias )
   METHOD GetField( fld )
   METHOD GetKeyVal( value )
   METHOD GetMasterSourceClassName()
   METHOD GetPublishedFieldNameList( typeList )
   METHOD GetTableFileName()
   METHOD GetValue
   METHOD ImportField( fromField, fieldDbName, fieldName )
   METHOD IndexByName( IndexName, curClass )
   METHOD Insert()
   METHOD InsertRecord( origin )
   METHOD InsideScope()
   METHOD Open
   METHOD ordCondSet( ... )
   METHOD ordCreate( ... )
   METHOD ordKeyNo() INLINE ::GetIndex():ordKeyNo()
   METHOD Post()
   METHOD RawSeek( Value, index )
   METHOD RecLock()
   METHOD RecUnLock()
   METHOD Refresh
   METHOD Reset() // Set Field Record to their default values, Sync MasterKeyVal Value
   METHOD SEEK( Value, AIndex, SoftSeek ) INLINE ::BaseSeek( 0, Value, AIndex, SoftSeek )
   METHOD SeekLast( Value, AIndex, SoftSeek ) INLINE ::BaseSeek( 1, Value, AIndex, SoftSeek )
   METHOD SetAsString( Value ) INLINE ::GetKeyField():AsString := Value
   METHOD SetBaseKeyIndex( baseKeyIndex )
   METHOD SetDbFilter( filter ) INLINE ::FDbFilter := filter
   METHOD SetKeyVal( keyVal )
   METHOD SetPrimaryIndex( primaryIndex )
   METHOD SetPrimaryIndexList( clsName, name )
   METHOD SetValue( value )
   METHOD SkipBrowse( n )
   METHOD SkipFilter( n, index )
   METHOD StatePop()
   METHOD StatePush()
   METHOD SyncFromMasterSourceFields()
   METHOD SyncRecNo( fromAlias )
   METHOD TableClass INLINE ::ClassName + "@" + ::TableFileName

   METHOD Validate( showAlert )

   METHOD OnClassInitializing() VIRTUAL
   METHOD OnCreate() VIRTUAL
   METHOD OnActiveSetKeyVal( value )
   METHOD OnAfterCancel() VIRTUAL
   METHOD OnAfterChange() VIRTUAL
   METHOD OnAfterDelete() VIRTUAL
   METHOD OnAfterInsert() VIRTUAL
   METHOD OnAfterOpen() VIRTUAL
   METHOD OnAfterPost() VIRTUAL
   METHOD OnBeforeCancel() INLINE .T.
   METHOD OnBeforeDelete() INLINE .T.
   METHOD OnBeforeEdit() INLINE .T.
   METHOD OnBeforeInsert() INLINE .T.
   METHOD OnBeforeLock INLINE .T.
   METHOD OnBeforePost() INLINE .T.
   METHOD OnDataChange()
   METHOD OnStateChange( oldState ) VIRTUAL
   METHOD OnSyncFromMasterSource() VIRTUAL

   PROPERTY Active READ FActive
   PROPERTY ALIAS READ GetAlias
   PROPERTY AsString READ GetAsString WRITE SetAsString
   PROPERTY AutoCreate READ GetAutoCreate
   PROPERTY BaseKeyField READ FBaseKeyField
   PROPERTY BaseKeyIndex READ FBaseKeyIndex
   PROPERTY BaseKeyVal READ BaseKeyField:GetKeyVal WRITE BaseKeyField:SetKeyVal
   PROPERTY causeEditState
   PROPERTY Bof READ GetBof
   PROPERTY DataBase READ GetDataBase WRITE SetDataBase
   PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
   PROPERTY DbStruct READ GetDbStruct
   PROPERTY DELETED READ Alias:Deleted()
   PROPERTY DisplayFields READ GetDisplayFields
   PROPERTY ErrorBlock READ GetErrorBlock WRITE SetErrorBlock
   PROPERTY Eof READ GetEof
   PROPERTY FieldList READ FFieldList
   PROPERTY Found READ GetFound
   PROPERTY FieldTypes READ GetFieldTypes
   PROPERTY HasFilter READ GetHasFilter
   PROPERTY Initialized READ FInitialized
   PROPERTY Instance READ GetInstance
   PROPERTY Instances READ FInstances
   PROPERTY isMetaTable READ FisMetaTable WRITE SetisMetaTable
   PROPERTY IsTempTable READ FIsTempTable
   PROPERTY KeyExpression READ GetKeyExpression
   PROPERTY KeyField READ GetKeyField
   PROPERTY KeyString READ GetKeyString
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY MasterKeyExpression READ GetMasterKeyExpression
   PROPERTY MasterKeyString READ GetMasterKeyString
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY PrimaryIndexList READ FPrimaryIndexList
   PROPERTY RDOClient READ FRDOClient
   PROPERTY RecCount READ GetAlias:RecCount()
   PROPERTY RecNo READ GetRecNo WRITE DbGoTo
   PROPERTY State READ FState
   PROPERTY SubState READ FSubState
   PROPERTY SyncingToContainerField READ FSyncingToContainerField WRITE SetSyncingToContainerField
   PROPERTY TableFileName READ GetTableFileName WRITE SetTableFileName
   PROPERTY UndoList READ FUndoList

   PUBLISHED:

   DATA Cargo

   PROPERTY ChildReferenceList READ FInstances[ ::TableClass, "ChildReferenceList" ]
   PROPERTY INDEX READ GetIndex WRITE SetIndex
   PROPERTY IndexList READ FIndexList
   PROPERTY IndexName READ GetIndexName WRITE SetIndexName
   PROPERTY MasterKeyField READ GetMasterKeyField
   PROPERTY MasterSource READ GetMasterSource WRITE SetMasterSource
   PROPERTY PrimaryIndex READ FPrimaryIndex
   PROPERTY PublishedFieldNameList READ GetPublishedFieldNameList
   PROPERTY READONLY READ FReadOnly WRITE SetReadOnly
   PROPERTY Value READ GetValue( ... ) WRITE SetValue

ENDCLASS

/*
    New
    Teo. Mexico 2006
*/
METHOD New( masterSource, tableName ) CLASS TTable

   LOCAL rdoClient
   LOCAL Result, itm
   LOCAL ms

   ::FInitialized := .T.

   ::Process_TableName( tableName )

   IF ::FRDOClient != NIL

      rdoClient := ::FRDOClient

      Result := ::SendToServer( masterSource, ::TableFileName )

      ? "Result from Server:"
      ? "ClassName:", Result:ClassName, ":", Result
      ? "Alias", Result:Alias:Name

      FOR EACH itm IN Result
         Self[ itm:__enumIndex() ] := itm
      NEXT

      ::FRDOClient := rdoClient

      IF !hb_HHasKey( ::FInstances, ::TableFileName )
         ::FInstances[ ::TableFileName ] := ::GetInstance()
      ENDIF

      RETURN Self

   ENDIF

   IF ::DataBase == NIL
      ::DataBase := ::InitDataBase()
   ENDIF

   IF masterSource = NIL .AND. !Empty( ms := ::GetMasterSourceClassName() ) .AND. ::autoMasterSource
      masterSource := __ClsInstName( ms )
      masterSource:autoMasterSource := .T.
      masterSource:New()
   ENDIF

    /*!
     * Sets the MasterSource (maybe will be needed in the fields definitions ahead )
     */
   IF masterSource != NIL

        /*
         * As we have not fields defined yet, this will not execute SyncFromMasterSourceFields()
         */
      ::SetMasterSource( masterSource )

   ENDIF

    /*!
     * Load definitions for Fields
     */
   ::FillFieldList()

   /* Load Primary indexes */
   ::__DefinePrimaryIndex()

   /* Load Secondary Indexes */
   ::__DefineSecondaryIndexes()

   IF !::isMetaTable
      ::CreateTableInstance()
   ENDIF

   RETURN Self

/*
    OnDestruct
    Teo. Mexico 2010
*/
METHOD PROCEDURE OnDestruct() CLASS TTable

   LOCAL dbfName, indexName

   IF ::aliasTmp != NIL
      dbfName := ::aliasTmp:dbInfo( DBI_FULLPATH )
      indexName := ::aliasTmp:dbOrderInfo( DBOI_FULLPATH )
      ::aliasTmp:dbCloseArea()
      FErase( dbfName )
      FErase( indexName )
   ENDIF

   // ::Destroy()

   RETURN

/*
    __CheckIndexes
    Teo. Mexico 2013
*/
METHOD PROCEDURE __CheckIndexes() CLASS TTable

   LOCAL curClass
   LOCAL INDEX

   FOR EACH curClass IN ::FIndexList
      FOR EACH INDEX IN curClass
         IF ::Alias:ordNumber( index:TagName ) = 0
            IF index:temporary
               IF ! ::CreateTempIndex( index )
                  RAISE ERROR "Failure to create temporal Index '" + index:Name + "'"
               ENDIF
            ELSE
               IF ! ::CreateIndex( index )
                  RAISE ERROR "Failure to create Index '" + index:Name + "'"
               ENDIF
               IF index:Custom
                  index:FillCustomIndex()
               ENDIF
            ENDIF
         ENDIF
      NEXT
   NEXT

   RETURN

/*
    AddCustomIndex
    Teo. Mexico 2012
*/
METHOD PROCEDURE AddCustomIndex( index ) CLASS TTable

   IF AScan( ::FCustomIndexList, {| e| e == index } ) = 0
      AAdd( ::FCustomIndexList, index )
   ENDIF

   RETURN

/*
    AddFieldAlias
    Teo. Mexico 2010
*/
METHOD PROCEDURE AddFieldAlias( nameAlias, fld, private ) CLASS TTable

   LOCAL AField

   SWITCH ValType( fld )
   CASE 'C'
      AField := ::FieldByName( fld )
      EXIT
   CASE 'O'
      IF fld:IsDerivedFrom( "TField" )
         AField := fld
         EXIT
      ENDIF
   ENDSWITCH

   IF AField != NIL
      IF AField:nameAlias != NIL
         RAISE ERROR "Alias Field Name '" + nameAlias + "' attempt to re-declare alias name on Field"
      ENDIF
      ::AddFieldMessage( nameAlias, AField, .T. )
      AField:nameAlias := nameAlias
      IF private != NIL
         AField:nameAliasPublished := !private
      ENDIF
   ELSE
      RAISE ERROR "Alias Field Name '" + nameAlias + "' not valid Field from"
   ENDIF

   RETURN

/*
    AddFieldMessage
    Teo. Mexico 2006
*/
METHOD PROCEDURE AddFieldMessage( messageName, AField, isAlias ) CLASS TTable

   LOCAL INDEX
   LOCAL fld

   fld := ::FieldByName( messageName, @index )

   IF index = 0
      IF isAlias == .T.
         ::FieldByName( AField:Name, @index )
      ELSE
         AAdd( ::FFieldList, AField )
         index := Len( ::FFieldList )
      ENDIF
   ELSE
      IF fld:IsKeyIndex
         RAISE ERROR "Attempt to overwrite key index Field '" + messageName + "' on Class <" + ::ClassName + ">"
      ENDIF
      IF AField:TableBaseClass == fld:TableBaseClass
         RAISE ERROR "Attempt to Re-Declare Field '" + messageName + "' on Class <" + ::ClassName + ">"
      ENDIF
      ::FFieldList[ index ] := AField
   ENDIF

   /* Check if Name is already created in class */
   IF !__objHasMsg( Self, ::FieldNamePrefix + messageName )
      IF index < 1 .OR. index > Len( ::FieldList )
         RAISE ERROR "Illegal index field for '" + messageName + "' on Class <" + ::ClassName + ">"
      ENDIF
      EXTEND OBJECT Self WITH MESSAGE ::FieldNamePrefix + messageName INLINE ::FieldList[ index ]
   ENDIF

   RETURN

/*
    AddRec
    Teo. Mexico 2006
*/
METHOD FUNCTION AddRec() CLASS TTable

   LOCAL Result
   LOCAL AField
   LOCAL errObj
   LOCAL INDEX
   LOCAL newValue
   LOCAL aKeyFields := {}
   LOCAL itm

   IF ::FReadOnly
      SHOW WARN "Table '" + ::ClassName() + "' is marked as READONLY..."
      RETURN .F.
   ENDIF

   IF ::FHasDeletedOrder
      index := "__AVAIL"
   ELSEIF ::FPrimaryIndex != NIL
      index := ::FPrimaryIndex:Name
   ENDIF

   ::FRecNoBeforeInsert := ::RecNo()

   IF !( Result := ::Alias:AddRec( index ) )
      RETURN Result
   ENDIF

   ::FEof := .F.
   ::FBof := .F.

   ::FRecNo := ::Alias:RecNo

   ::SetState( dsInsert )
   ::FcauseEditState := dsInsert
   ::FSubState := dssAdding

   // ::Reset() // Reset record data to default values
   FOR EACH AField IN ::FFieldList
      IF !AField:Calculated .AND. AField:FieldMethodType = "C"
         AField:Clear()
      ENDIF
   NEXT

    /*
     * Write the MasterKeyField
     * Write the PrimaryKeyField
     * Write the Fields that have a NewValue
     */
   BEGIN SEQUENCE WITH ::ErrorBlock

      ::FillPrimaryIndexes( Self )

      FOR EACH AField IN ::FFieldList
         IF AField:FieldMethodType = 'C' .AND. !AField:PrimaryKeyComponent .AND. AField:WrittenValue == NIL .AND. AField:Enabled
            newValue := AField:NewValue
            IF newValue != NIL .OR. AField:AutoIncrement
               IF !AField:IsKeyIndex
                  AField:SetData( newValue, .T. )
               ELSE
                  AAdd( aKeyFields, { AField, newValue } )
               ENDIF
            ENDIF
         ENDIF
      NEXT

      /* poblate key field's AFTER all other fields are filled */
      FOR EACH itm IN aKeyFields
         itm[ 1 ]:SetData( itm[ 2 ], .T. )
      NEXT

   RECOVER USING errObj

      ::TTable:Delete()
      ::RecUnLock()

      SHOW ERROR errObj

      Result := .F.

   END SEQUENCE

   ::FSubState := dssNone

   RETURN Result

/*
    AssociateTableIndex
    Teo. Mexico 2010
*/
METHOD PROCEDURE AssociateTableIndex( table, name, getRecNo, setRecNo ) CLASS TTable

   LOCAL INDEX

   IF !hb_HHasKey( ::IndexList, ::ClassName() )
      ::IndexList[ ::ClassName() ] := hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )
   ENDIF

   index := table:IndexByName( name )
   index:associatedTable := Self

   index:getRecNoBlock := getRecNo
   index:setRecNoBlock := setRecNo

   ::IndexList[ ::ClassName(), name ] := index

   ::ExternalIndexList[ index:ObjectH ] := index

   RETURN

/*
    BaseSeek
    Teo. Mexico 2007
*/
METHOD FUNCTION BaseSeek( direction, Value, index, lSoftSeek ) CLASS TTable

   LOCAL AIndex

   AIndex := ::FindIndex( index )

   IF direction = 0
      RETURN AIndex:BaseSeek( 0, Value, lSoftSeek )
   ENDIF

   RETURN AIndex:BaseSeek( 1, Value, lSoftSeek )

/*
    BuildFieldBlockFromFieldExpression
    Teo. Mexico 2012
*/
METHOD FUNCTION BuildFieldBlockFromFieldExpression( fieldExp, returnMode, field ) CLASS TTable

   LOCAL nTokens
   LOCAL i
   LOCAL s
   LOCAL INDEX
   LOCAL table
   LOCAL fldName
   LOCAL block

   fieldExp := AllTrim( fieldExp )

   nTokens := NumToken( fieldExp, ":" )

   table := Self

   FOR i := 1 TO nTokens
      fldName := Token( fieldExp, ":", i )
      field := table:FieldByName( fldName, @index )
      IF field != NIL
         IF i = 1
            s := "::FieldList[" + NTrim( index ) + "]"
         ELSE
            s += ":DataObj:FieldList[" + NTrim( index ) + "]"
         ENDIF
         IF field:IsDerivedFrom( "TObjectField" )
            IF field:LinkedTable:isMetaTable
               table := field:LinkedTable
            ELSE
               table := field:DataObj
            ENDIF
         ENDIF
      ELSE
         RETURN NIL
      ENDIF
   NEXT

   BEGIN SEQUENCE WITH {| oErr| Break( oErr ) }
      IF returnMode = NIL // returns the TField object
         block := &( "{|Self|" + s + "}" )
      ELSE
         block := &( "{|Self|" + s + ":" + returnMode + "}" )
      ENDIF
   RECOVER
      block := NIL
   END SEQUENCE

   RETURN block

/*
    Cancel
    Teo. Mexico 2006
*/
METHOD PROCEDURE Cancel CLASS TTable

   LOCAL AField

   IF AScan( { dsInsert, dsEdit }, ::State ) = 0
      // ::Error_Table_Not_In_Edit_or_Insert_mode()
      RETURN
   ENDIF

   IF ::OnBeforeCancel()

      SWITCH ::State
      CASE dsInsert
         // FOR EACH AField IN ::FFieldList
         // IF !AField:Calculated .AND. AField:FieldMethodType = "C" .AND. !Empty( AField:Value ) .AND. AField:Enabled .AND. AField:Validate( .F. ) != NIL
         // AField:Reset()
         // ENDIF
         // NEXT
         ::TTable:Delete( .T. )
         EXIT
      CASE dsEdit
         FOR EACH AField IN ::FieldList
            IF !AField:Calculated .AND. AField:FieldMethodType = "C" .AND. AField:Changed
               AField:RevertValue()
            ENDIF
         NEXT
         EXIT
      OTHERWISE

      ENDSWITCH

      ::RecUnLock()

      ::OnAfterCancel()

      IF ::FRecNoBeforeInsert != NIL
         ::RecNo := ::FRecNoBeforeInsert
         ::FRecNoBeforeInsert := NIL
      ENDIF

   ENDIF

   RETURN

/*
    CheckDbStruct
    Teo. Mexico 2010
*/
METHOD FUNCTION CheckDbStruct() CLASS TTable

   LOCAL AField
   LOCAL n
   LOCAL i
   LOCAL aDb
   LOCAL dbsType
   LOCAL dbsLen
   LOCAL dbsDec
   LOCAL dbsSize
   LOCAL sResult := ""

   IF !hb_HHasKey( ::FInstances[ ::TableClass ], "DbStructValidating" )

      aDb := AClone( ::dbStruct() )

      ::FInstances[ ::TableClass, "DbStructValidating" ] := NIL

      FOR EACH AField IN ::FieldList
         IF AField:FieldMethodType = "C" .AND. !AField:Calculated .AND. AField:UsingField = NIL

            n := AScan( aDb, {| e| Upper( e[ 1 ] ) == Upper( AField:DBS_NAME ) } )

            IF AField:FieldType = ftObject .AND. ( i := FindTableBaseClass( AField ) ) > 0
               dbsType := BaseKeyFieldList[ i, 2 ]
               dbsLen  := BaseKeyFieldList[ i, 3 ]
               dbsDec  := BaseKeyFieldList[ i, 4 ]
               dbsSize := BaseKeyFieldList[ i, 5 ]
            ELSE
               dbsType := AField:DBS_TYPE
               dbsLen  := AField:DBS_LEN
               dbsDec  := AField:DBS_DEC
               IF dbsType = "C"
                  dbsSize := AField:Size
               ENDIF
            ENDIF

            IF n = 0
               AAdd( aDb, { AField:DBS_NAME, dbsType, dbsLen, dbsDec } )
               sResult += "Field not found '" + AField:DBS_NAME + E"'\n"
            ELSEIF !aDb[ n, 2 ] == dbsType .AND. !( aDb[ n, 2 ] $ "+I" .AND. dbsType $ "+I" )
               sResult += "Wrong type ('" + aDb[ n, 2 ] + "') on field '" + AField:DBS_NAME + "', must be '" + dbsType + E"'\n"
               aDb[ n, 2 ] := dbsType
               aDb[ n, 3 ] := dbsLen
               aDb[ n, 4 ] := dbsDec
            ELSEIF aDb[ n, 2 ] = "C" .AND. aDb[ n, 3 ] < dbsSize
               sResult += "Wrong len value (" + NTrim( aDb[ n, 3 ] ) + ") on 'C' field '" + AField:DBS_NAME + E"', must be " + NTrim( dbsLen ) + E"\n"
               aDb[ n, 3 ] := dbsLen
            ELSEIF aDb[ n, 2 ] = "N" .AND. ( !aDb[ n, 3 ] == dbsLen .OR. !aDb[ n, 4 ] == dbsDec )
               sResult += "Wrong len/dec values (" + NTrim( aDb[ n, 3 ] ) + "," + NTrim( aDb[ n, 4 ] ) + ") on 'N' field '" + AField:DBS_NAME + E"', must be " + NTrim( dbsLen ) + "," + NTrim( dbsDec ) + E"\n"
               aDb[ n, 3 ] := dbsLen
               aDb[ n, 4 ] := dbsDec
            ENDIF

         ENDIF
      NEXT

      ::FInstances[ ::TableClass, "DbStructValidated" ] := .T.

      IF ! Empty( sResult )
         sResult := "Error on Db structure." + ;
            E"\nClass: " + ::ClassName() + ", Table: " + ::Alias:Name + ;
            E"\n\n-----\n" + ;
            sResult + ;
            E"-----\n\n"
         ? sResult

         ::FInstances[ ::TableClass, "DbStructValidated" ] := ::FixDbStruct( aDb, sResult )

      ENDIF

      hb_HDel( ::FInstances[ ::TableClass ], "DbStructValidating" )

   ENDIF

   RETURN .T.

/*
    Childs
    Teo. Mexico 2013
*/
METHOD FUNCTION Childs( ignoreAutoDelete, block, curClass, childs ) CLASS TTable
   RETURN F_Childs( Self, ignoreAutoDelete, block, curClass, childs )

/*
    F_Childs
    Teo. Mexico 2013
*/
STATIC FUNCTION F_Childs( Self, ignoreAutoDelete, block, curClass, childs )

   LOCAL childTableName
   LOCAL ChildDB
   LOCAL clsName
   LOCAL destroyChild

   IF curClass = NIL
      curClass := Self
   ENDIF

   IF childs = NIL
      childs := {}
   ENDIF

   clsName := curClass:ClassName

   IF clsName == "TTABLE"
      RETURN childs
   ENDIF

   IF hb_HHasKey( ::DataBase:ParentChildList, clsName )

      FOR EACH childTableName IN ::DataBase:GetParentChildList( clsName )

         IF !ignoreAutoDelete == .T. .OR. !::DataBase:TableList[ childTableName, "AutoDelete" ]

            ChildDB := ::ChildSource( childTableName, @destroyChild )

            IF ChildDB != NIL

               IF ::DataBase:TableList[ childTableName, "IndexName" ] != NIL
                  ChildDB:IndexName := ::DataBase:TableList[ childTableName, "IndexName" ]
               ENDIF

               ChildDB:StatePush()

               ChildDB:PrimaryIndex:Scope := NIL

               IF ChildDB:PrimaryIndex:Descend
                  ChildDB:PrimaryIndex:dbGoTop()
               ELSE
                  ChildDB:PrimaryIndex:dbGoBottom()
               ENDIF

               IF !ChildDB:Eof() .AND. !Empty( ChildDB:BaseKeyField:Value )
                  AAdd( childs, iif( block == NIL, ChildDB:ClassName, block:Eval( ChildDB ) ) )
                  ChildDB:StatePop()
                  IF destroyChild
                     ChildDB:Destroy()
                  ENDIF
                  LOOP
               ENDIF

               ChildDB:StatePop()

               IF destroyChild
                  ChildDB:Destroy()
               ENDIF

            ENDIF

         ENDIF

      NEXT

   ENDIF

   F_Childs( Self, ignoreAutoDelete, block, curClass:Super, childs )

   RETURN childs

/*
    ChildSource
    Teo. Mexico 2008
*/
METHOD FUNCTION ChildSource( tableName, destroyChild ) CLASS TTable

   LOCAL itm
   LOCAL childDb

   tableName := Upper( tableName )

   /* tableName is in the DetailSourceList */
   FOR EACH itm IN ::DetailSourceList
      IF itm:ClassName() == tableName
         itm:Reset()
         destroyChild := .F.
         RETURN itm
      ENDIF
   NEXT

   destroyChild := .T.

   BEGIN SEQUENCE WITH {| oErr| Break( oErr ) }
      childDb := __ClsInstFromName( tableName ):New( Self )
   END SEQUENCE

   RETURN childDb

/*
    CopyRecord
    Teo. Mexico 2007
*/
METHOD FUNCTION CopyRecord( origin ) CLASS TTable

   LOCAL AField
   LOCAL AField1
   LOCAL entry

   SWITCH ValType( origin )
   CASE 'O' // Record from another Table
      IF !origin:IsDerivedFrom( "TTable" )
         RAISE ERROR "Origin is not a TTable class descendant."
         RETURN .F.
      ENDIF
      IF origin:Eof()
         RAISE ERROR "Origin is at EOF."
         RETURN .F.
      ENDIF
      FOR EACH AField IN ::FFieldList
         IF AField:FieldMethodType = 'C' .AND. !AField:PrimaryKeyComponent
            AField1 := origin:FieldByName( AField:Name )
            IF AField1 != NIL
               AField:Value := AField1:Value
            ENDIF
         ENDIF
      NEXT
      EXIT
   CASE 'H' // Hash of Values
      FOR EACH entry IN origin
         AField := ::FieldByName( entry:__enumKey )
         IF AField != NIL .AND. AField:FieldMethodType = 'C' .AND. !AField:PrimaryKeyComponent
            AField:Value := entry:__enumValue
         ENDIF
      NEXT
      EXIT
   OTHERWISE
      RAISE ERROR "Unknown Record from Origin"
      RETURN .F.
   ENDSWITCH

   RETURN .T.

/*
    Count : number of records
    Teo. Mexico 2008
*/
METHOD FUNCTION COUNT( bForCondition, bWhileCondition, index, scope ) CLASS TTable

   LOCAL nCount := 0

   ::dbEval( {|| ++nCount }, bForCondition, bWhileCondition, index, scope )

   RETURN nCount

/*
    CreateIndex
    Teo. Mexico 2010
*/
METHOD FUNCTION CreateIndex( index ) CLASS TTable

   LOCAL indexExp
   LOCAL recNo
   LOCAL forKeyBlock
   LOCAL whileBlock := NIL
   LOCAL evalBlock := NIL
   LOCAL intervalVal := NIL
   LOCAL additive := .T.
   LOCAL useCurrent := .F.
   LOCAL temporary := .F.
   LOCAL bagName := NIL
   LOCAL unique := .F.

   recNo := ::Alias:RecNo

   IF index:Custom
      indexExp := E"\042" + Replicate( "#", Len( index:MasterKeyVal ) + Len( index:KeyVal ) ) + E"\042"
      dbSelectArea( ::Alias:Name )
      ordCondSet( ,,,,,, RecNo(),,,,,,,, .T. )
      ordCreate( , index:TagName, indexExp )
   ELSE

      indexExp := index:IndexExpression()

      IF !Empty( index:ForKey )
         forKeyBlock := &( "{||" + index:ForKey + "}" )
      ENDIF

      dbSelectArea( ::Alias:Name )  // here because index:IndexExpression() may change active WA

      ordCondSet( ;
         index:ForKey, ;
         forKeyBlock, ;
         NIL, ;
         whileBlock, ;
         evalBlock, ;
         intervalVal, ;
         NIL, ;
         NIL, ;
         NIL, ;
         NIL, ;
         index:Descend, ;
         NIL, ;
         additive, ;
         useCurrent, ;
         index:Custom, ;
         NIL, ;
         NIL, ;
         temporary )

      ordCreate( bagName, index:TagName, indexExp, indexExp, unique )

   ENDIF

   ::Alias:RecNo := recNo

   RETURN .T.

/*
    CreateTable
    Teo. Mexico 2010
*/
METHOD FUNCTION CreateTable( fullFileName ) CLASS TTable

   LOCAL aDbs := {}
   LOCAL fld
   LOCAL cNet
   LOCAL n
   LOCAL dbsType
   LOCAL dbsLen
   LOCAL dbsDec

   ::FillFieldList()

   FOR EACH fld IN ::FieldList
      IF fld:IsTableField .AND. !fld:ReUseField
         IF fld:FieldType = ftObject
            n := FindTableBaseClass( fld )
            IF n > 0
               dbsType := BaseKeyFieldList[ n, 2 ]
               dbsLen  := BaseKeyFieldList[ n, 3 ]
               dbsDec  := BaseKeyFieldList[ n, 4 ]
            ELSE
               dbsType := NIL
            ENDIF
         ELSE
            dbsType := fld:DBS_TYPE
            dbsLen  := fld:DBS_LEN
            dbsDec  := fld:DBS_DEC
         ENDIF
         IF dbsType != NIL
            AAdd( aDbs, { fld:DBS_NAME, dbsType, dbsLen, dbsDec } )
         ENDIF
      ENDIF
   NEXT

   IF Empty( aDbs )
      SHOW WARN "CreateTable: Cannot create table class '" + ::ClassName + "' with empty data..."
      RETURN .F.
   ENDIF

   IF fullFileName = NIL
      fullFileName := ::TableFileName
   ENDIF

   IF !::IsTempTable .AND. ::dataBase != NIL .AND. ::dataBase:NetIO == .T.
      cNet := iif( Upper( fullFileName ) = "NET:", "", "net:" )
   ELSE
      cNet := ""
   ENDIF

   dbCreate( cNet + fullFileName, aDbs )

   RETURN .T.

/*
    CreateTableInstance
    Teo. Mexico 2013
*/
METHOD PROCEDURE CreateTableInstance() CLASS TTable

   LOCAL itm
   LOCAL n

   ::FisMetaTable := .F.

   ::InitTable()

   /* Check for a valid db structure (based on definitions on DEFINE FIELDS) */
   IF !Empty( ::TableFileName ) .AND. ::validateDbStruct .AND. !hb_HHasKey( ::FInstances[ ::TableClass ], "DbStructValidated" )
      ::CheckDbStruct()
   ENDIF

   /* sets the DBS field info for each table field */
   FOR EACH itm IN ::FFieldList
      IF itm:IsTableField()
         n := Upper( itm:DBS_NAME )
         n := AScan( ::DbStruct, {| e| e[ 1 ] == n } )
         IF n > 0
            itm:SetDbStruct( ::DbStruct[ n ] )
         ENDIF
      ENDIF
   NEXT

   ::FState := dsBrowse

   ::__CheckIndexes()

   ::FState := dsInactive

   IF ::GetIndex() = NIL
      IF ::FPrimaryIndex != NIL
         ::SetIndex( ::FPrimaryIndex )
      ELSEIF !Empty( ::FIndexList )
         ::SetIndex( hb_HValueAt( hb_HValueAt( ::FIndexList, 1 ), 1 ) )
      ENDIF
   ENDIF

   ::OnCreate()

   IF ::autoOpen
      ::Open()
   ENDIF

   RETURN

/*
    CreateTempIndex
    Teo. Mexico 2010
*/
METHOD FUNCTION CreateTempIndex( index ) CLASS TTable

   LOCAL fileName
   LOCAL pathName
   LOCAL aliasName
   LOCAL dbsIdx
   LOCAL size
   LOCAL fldName
   LOCAL lNew := .F.

   fldName := index:Name

   IF !index:temporary

      hb_FNameSplit( ::Alias:dbOrderInfo( DBOI_FULLPATH ), @pathName )

      pathName += Lower( ::ClassName )

      fileName := pathName + ".dbf"

      aliasName := "IDX_" + ::ClassName()

      IF File( fileName ) .AND. index:IdxAlias = NIL

         index:IdxAlias := TAlias()
         index:IdxAlias:lShared := .F.
         index:IdxAlias:New( fileName, aliasName )

      ENDIF

   ENDIF

   IF index:IdxAlias = NIL

      IF index:temporary

         FClose( hb_FTempCreateEx( @fileName, NIL, "t", ".dbf" ) )

         aliasName := "TMP_" + ::ClassName()

      ENDIF

      size := 0

      IF index:MasterKeyField != NIL
         size += index:MasterKeyField:Size
      ENDIF

      IF index:KeyField != NIL
         size += index:KeyField:Size
      ENDIF

      dbsIdx := ;
         { ;
         { "RECNO", "I", 4, 0 }, ;
         { fldName, "C", size, 0 } ;
         }

      dbCreate( fileName, dbsIdx )

      index:IdxAlias := TAlias()
      index:IdxAlias:lShared := .F.
      index:IdxAlias:New( fileName, aliasName )

      CREATE INDEX ON "RecNo" TAG "IDX_RECNO" BAG pathName ADDITIVE

      CREATE INDEX ON fldName TAG index:Name BAG pathName ADDITIVE

      lNew := .T.

   ENDIF

   IF index:temporary

      index:IdxAlias:__dbZap()

   ENDIF

   IF index:temporary .OR. lNew
      ::dbEval( ;
         {| Self|
      index:IdxAlias:AddRec()
      index:IdxAlias:SetFieldValue( "RECNO", ::RecNo() )
      index:IdxAlias:SetFieldValue( fldName, index:MasterKeyVal + index:KeyVal )
      RETURN NIL
      }, NIL, NIL, index:useIndex )
   ENDIF

   index:FIdxAlias := .T.

   RETURN .T.

/*
    DbEval
    Teo. Mexico 2008
*/
METHOD PROCEDURE dbEval( bBlock, bForCondition, bWhileCondition, index, scope ) CLASS TTable

   LOCAL oldIndex
   LOCAL oldScope

   ::StatePush()

   IF index != NIL
      oldIndex := ::IndexName
      index := ::FindIndex( index )
      IF index != NIL
         ::IndexName := index:Name
      ENDIF
   ENDIF

   IF scope != NIL
      oldScope := ::GetIndex():Scope
      ::GetIndex():Scope := scope
   ENDIF

   ::dbGoTop()

   WHILE !::Eof() .AND. ( bWhileCondition == NIL .OR. bWhileCondition:Eval( Self ) )

      IF bForCondition == NIL .OR. bForCondition:Eval( Self )
         bBlock:Eval( Self )
      ENDIF

      ::dbSkip()

   ENDDO

   IF oldScope != NIL
      ::GetIndex():Scope := oldScope
   ENDIF

   IF oldIndex != NIL
      ::IndexName := oldIndex
   ENDIF

   ::StatePop()

   RETURN

/*
    DbGoBottomTop
    Teo. Mexico 2007
*/
METHOD FUNCTION DbGoBottomTop( n ) CLASS TTable

   IF AScan( { dsEdit, dsInsert }, ::FState ) > 0
      ::Post()
   ENDIF

   IF ::GetIndex() != NIL
      IF n = 1
         RETURN ::GetIndex():dbGoTop()
      ELSE
         RETURN ::GetIndex():dbGoBottom()
      ENDIF
   ELSE
      IF n = 1
         ::Alias:dbGoTop()
      ELSE
         ::Alias:dbGoBottom()
      ENDIF
      ::GetCurrentRecord()
      IF ::HasFilter .AND. !::FilterEval()
         ::SkipFilter( n )
      ENDIF
   ENDIF

   RETURN .F.

/*
    DbGoTo
    Teo. Mexico 2007
*/
METHOD FUNCTION dbGoto( RecNo ) CLASS TTable

   ::Alias:dbGoto( RecNo )

   RETURN ::GetCurrentRecord()

/*
    DbSkip
    Teo. Mexico 2011
*/
METHOD FUNCTION dbSkip( numRecs ) CLASS TTable

   IF AScan( { dsEdit, dsInsert }, ::FState ) > 0
      ::Post()
   ENDIF

   IF ::GetIndex() != NIL
      RETURN ::GetIndex():dbSkip( numRecs )
   ELSE
      IF !::HasFilter
         ::Alias:dbSkip( numRecs )
         RETURN ::GetCurrentRecord()
      ENDIF
   ENDIF

   RETURN ::SkipFilter( numRecs )

/*
    FIELDS END
    Teo. Mexico 2010
*/
METHOD PROCEDURE DefineFieldsFromDb() CLASS TTable

   LOCAL dbStruct
   LOCAL fld
   LOCAL AField

   IF ::Alias != NIL .AND. Empty( ::FFieldList ) .AND. !Empty( dbStruct := ::GetDbStruct() )
      FOR EACH fld IN dbStruct

         AField := __ClsInstFromName( ::FieldTypes[ fld[ 2 ] ] ):New( Self )

         AField:FieldMethod := fld[ 1 ]
         IF AField:IsDerivedFrom( "TStringField" )
            AField:Size := fld[ 3 ]
         ENDIF
         IF AField:IsDerivedFrom( "TNumericField" )
            AField:DBS_LEN := fld[ 3 ]
            AField:DBS_DEC := fld[ 4 ]
         ENDIF
         AField:AddFieldMessage()

      NEXT

   ENDIF

   RETURN

/*
    Delete
    Teo. Mexico 2006
*/
METHOD FUNCTION DELETE( lDeleteChilds ) CLASS TTable

   LOCAL AField
   LOCAL aChilds
   LOCAL child
   LOCAL lDel

   IF AScan( { dsBrowse, dsEdit, dsInsert }, ::State ) = 0
      ::Error_Table_Not_In_Browse_or_Insert_State()
      RETURN .F.
   ENDIF

   IF ::State = dsBrowse .AND. !::RecLock()
      RETURN .F.
   ENDIF

   IF ::OnBeforeDelete()

      aChilds := ::Childs()

      lDel := .T.

      IF !Empty( aChilds ) .AND. !Empty( ::BaseKeyField:Value )
         FOR EACH child IN aChilds
            IF ! ::DataBase:TableList[ child, "AutoDelete" ]
               lDel := .F.
            ENDIF
         NEXT
         IF !lDel .AND. !lDeleteChilds == .T.
            SHOW WARN "Error_Table_Has_Childs"
            ::RecUnLock()
            RETURN .F.
         ENDIF
         IF !::DeleteChilds()
            SHOW WARN "Error_Deleting_Childs"
            ::RecUnLock()
            RETURN .F.
         ENDIF
      ENDIF

      FOR EACH AField IN ::FieldList
         AField:Delete()
      NEXT

      IF ::FHasDeletedOrder()
         ::Alias:dbDelete()
      ENDIF

      ::RecUnLock()

      ::GetCurrentRecord()

      ::OnAfterDelete()

   ELSE

      ::RecUnLock()

      RETURN .F.

   ENDIF

   RETURN .T.

/*
    DeleteChilds
    Teo. Mexico 2013
*/
METHOD FUNCTION DeleteChilds() CLASS TTable
   RETURN F_DeleteChilds( Self )

/*
    F_DeleteChilds
    Teo. Mexico 2013
*/
STATIC FUNCTION F_DeleteChilds( Self, curClass )

   LOCAL childTableName
   LOCAL ChildDB
   LOCAL clsName
   LOCAL destroyChild
   LOCAL nrec

   IF curClass = NIL
      curClass := Self
   ENDIF

   clsName := curClass:ClassName

   IF clsName == "TTABLE"
      RETURN .T.
   ENDIF

   IF hb_HHasKey( ::DataBase:ParentChildList, clsName )

      nrec := ::Alias:RecNo()

      FOR EACH childTableName IN ::DataBase:GetParentChildList( clsName )

         ChildDB := ::ChildSource( childTableName, @destroyChild )

         IF ChildDB != NIL

            IF ::DataBase:TableList[ childTableName, "IndexName" ] != NIL
               ChildDB:IndexName := ::DataBase:TableList[ childTableName, "IndexName" ]
            ENDIF

            ChildDB:StatePush()

            ChildDB:PrimaryIndex:Scope := NIL

            WHILE .T.
               IF ChildDB:PrimaryIndex:Descend
                  ChildDB:PrimaryIndex:dbGoTop()
               ELSE
                  ChildDB:PrimaryIndex:dbGoBottom()
               ENDIF
               IF ChildDB:Eof() .OR. Empty( ChildDB:BaseKeyField:Value )
                  EXIT
               ENDIF
               IF !ChildDB:TTable:Delete( .T. )
                  ChildDB:StatePop()
                  IF destroyChild
                     ChildDB:Destroy()
                  ENDIF
                  RETURN .F.
               ENDIF
            ENDDO

            ChildDB:StatePop()

            IF destroyChild
               ChildDB:Destroy()
            ENDIF

         ENDIF

      NEXT

      ::Alias:dbGoto( nrec )

   ENDIF

   RETURN F_DeleteChilds( Self, curClass:Super )

/*
    Destroy
    Teo. Mexico 2008
*/
METHOD PROCEDURE Destroy() CLASS TTable

   LOCAL table

   IF HB_ISOBJECT( ::MasterSource )
      IF hb_HHasKey( ::MasterSource:DetailSourceList, ::ObjectH )
         hb_HDel( ::MasterSource:DetailSourceList, ::ObjectH )
      ENDIF
   ENDIF

   IF !HB_ISARRAY( ::FFieldList )
      // WLOG("ERROR!: " + ::ClassName + ":Destroy - :FieldList is not a array...")
      RETURN
   ENDIF

   FOR EACH table IN ::DetailSourceList
      table:Destroy()
   NEXT

   ::FFieldList := NIL
   ::FDisplayFields := NIL
   ::tableState := NIL

   ::FActive := .F.

   IF ::IsTempTable
      ::Alias:dbCloseArea()
      hb_dbDrop( ::TableFileName )
   ENDIF

   RETURN

/*
    Edit
    Teo. Mexico 2006
*/
METHOD FUNCTION Edit() CLASS TTable

   IF !::State = dsBrowse
      ::Error_TableNotInBrowseState()
      RETURN .F.
   ENDIF

   IF ::Eof() .OR. !::OnBeforeEdit() .OR. !::RecLock()
      RETURN .F.
   ENDIF

   RETURN .T.

/*
    FieldByName
    Teo. Mexico 2006
*/
METHOD FUNCTION FieldByName( name, index ) CLASS TTable

   LOCAL AField

   index := 0

   IF Empty( name )
      RETURN NIL
   ENDIF

   name := Upper( name )

   FOR EACH AField IN ::FFieldList
      IF name == Upper( AField:Name ) .OR. ( AField:nameAlias != NIL .AND. name == Upper( AField:nameAlias ) )
         index := AField:__enumIndex
         RETURN AField
      ENDIF
   NEXT

   RETURN NIL

/*
    FieldByObjClass
    Teo. Mexico 2010
*/
METHOD FUNCTION FieldByObjClass( objClass, derived, index ) CLASS TTable

   LOCAL fld

   objClass := Upper( objClass )

   IF derived == .T.
      FOR EACH fld IN ::FFieldList
         IF fld:IsDerivedFrom( "TObjectField" )
            IF fld:LinkedTable:IsDerivedFrom( objClass )
               index := fld:__enumIndex
               RETURN fld
            ENDIF
         ENDIF
      NEXT
   ELSE
      FOR EACH fld IN ::FFieldList
         IF fld:IsDerivedFrom( "TObjectField" )
            IF fld:LinkedTable:ClassName() == objClass
               index := fld:__enumIndex
               RETURN fld
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN NIL

/*
    FillFieldList
    Teo. Mexico 2010
*/
METHOD PROCEDURE FillFieldList() CLASS TTable

   IF ::FFilledFieldList = .F.
      ::FFilledFieldList := .T.
      ::__DefineFields()
      IF Empty( ::FFieldList )
         ::DefineFieldsFromDb()
      ENDIF
   ENDIF

   RETURN

/*
    FillPrimaryIndexes
    Teo. Mexico 2013
*/
METHOD PROCEDURE FillPrimaryIndexes( curClass ) CLASS TTable

   F_FillPrimaryIndexes( Self, curClass )

   RETURN

/*
    F_FillPrimaryIndexes
    Teo. Mexico 2009
*/
STATIC PROCEDURE F_FillPrimaryIndexes( Self, curClass )

   LOCAL className
   LOCAL AIndex
   LOCAL AField
   LOCAL isEmpty
   LOCAL itm

   className := curClass:ClassName()

   IF !className == "TTABLE"

      F_FillPrimaryIndexes( Self, curClass:Super )

      IF hb_HHasKey( ::PrimaryIndexList, className )
         AIndex := ::IndexList[ className, ::PrimaryIndexList[ className ] ]
      ELSE
         AIndex := NIL
      ENDIF

      IF AIndex != NIL
         AField := AIndex:MasterKeyField
         IF AField != NIL
            AField:Reset()
            AField:SetData(, .T. )
         ENDIF
            /*!
             * AutoIncrement fields always need to be written (to set a value)
             */
         AField := AIndex:UniqueKeyField
         IF AField != NIL
            IF AField:FieldType = ftAutoInc
               AField:GetData()
            ELSE
               IF AField:FieldMethodType = "A"
                  isEmpty := .F.
                  FOR EACH itm IN AField:FieldArrayIndex
                     ::FieldList[ itm ]:Reset()
                     IF Empty( ::FieldList[ itm ]:Value )
                        isEmpty := .T.
                        EXIT
                     ENDIF
                  NEXT
               ELSE
                  AField:Reset()
                  isEmpty := Empty( AField:Value )
               ENDIF
               IF AIndex:AutoIncrement .OR. !isEmpty
                  AField:SetData(, .T. )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

   ENDIF

   RETURN

/*
    FilterEval
    Teo. Mexico 2010
*/
METHOD FUNCTION FilterEval( index ) CLASS TTable

   LOCAL table

   IF index != NIL .AND. index:associatedTable != NIL
      table := index:associatedTable
   ELSE
      table := Self
   ENDIF

   IF index != NIL .AND. index:DbFilter != NIL .AND. !index:DbFilter:Eval( table )
      RETURN .F.
   ENDIF

   RETURN table:DbFilter = NIL .OR. table:DbFilter:Eval( table )

/*
    FindIndex
    Teo. Mexico 2010
*/
METHOD FUNCTION FindIndex( index ) CLASS TTable

   LOCAL AIndex

   SWITCH ValType( index )
   CASE 'U'
      AIndex := ::GetIndex()
      EXIT
   CASE 'C'
      IF Empty( index )
         AIndex := ::FPrimaryIndex
      ELSE
         AIndex := ::IndexByName( index )
      ENDIF
      EXIT
   CASE 'O'
      IF index:IsDerivedFrom( "TIndex" )
         AIndex := index
         EXIT
      ENDIF
   OTHERWISE
      RAISE ERROR "Unknown index reference..."
   ENDSWITCH

   RETURN AIndex

/*
    FindMasterSourceField
    Teo. Mexico 2007
*/
METHOD FUNCTION FindMasterSourceField( detailField ) CLASS TTable

   LOCAL itm
   LOCAL name
   LOCAL vt
   LOCAL INDEX
   LOCAL masterSource

   IF ::FMasterSource == NIL
      RETURN NIL
   ENDIF

   masterSource := ::MasterSource

   vt := ValType( detailField )

   IF vt = "C"
      name := detailField
   ELSEIF vt = "O"
      name := detailField:Name
   ELSE
      RETURN NIL
   ENDIF

   IF hb_HHasKey( ::FMasterSourceFieldBuffer, name )
      index := ::FMasterSourceFieldBuffer[ name ]
      IF index > 0
         RETURN masterSource:FieldList[ index ]
      ENDIF
      RETURN NIL
   ENDIF

   IF vt = "C"
      itm := 0
      masterSource:FieldByName( name, @itm )
      ::FMasterSourceFieldBuffer[ name ] := itm
      IF !Empty( itm )
         RETURN masterSource:FieldList[ itm ]
      ENDIF
   ELSEIF vt = "O"
      IF detailField:FieldType = ftObject
         IF detailField:LinkedTable:IsDerivedFrom( masterSource:BaseKeyField:TableBaseClass )
            masterSource:FieldByName( masterSource:BaseKeyField:Name, @index )
            ::FMasterSourceFieldBuffer[ name ] := index
            RETURN masterSource:BaseKeyField
         ENDIF
         FOR EACH itm IN masterSource:FieldList
            IF itm:FieldType = ftObject .AND. detailField:LinkedTable:IsDerivedFrom( itm:BaseKeyField:TableBaseClass )
               ::FMasterSourceFieldBuffer[ name ] := itm:__enumIndex
               RETURN itm
            ENDIF
         NEXT
         ::FMasterSourceFieldBuffer[ name ] := 0
      ELSE
         RETURN ::FindMasterSourceField( detailField:Name )
      ENDIF
   ENDIF

   RETURN NIL

/*
    FixDbStruct
    Teo. Mexico 2010
*/
METHOD FUNCTION FixDbStruct( aNewStruct, message ) CLASS TTable

   LOCAL fileName
   LOCAL tempName
   LOCAL sPath, sName, sExt, sDrv
   LOCAL sPath2, sName2, sExt2, sDrv2
   LOCAL result
   LOCAL recNo

   IF message = NIL
      message := ""
   ENDIF

   IF ::dataBase != NIL .AND. ::dataBase:NetIO
      SHOW WARN "Cannot run FixDbStruct on NetIO tables..."
      RETURN .F.
   ENDIF

   IF ui_AlertYesNo( message + "Proceed to update Db Structure ?" ) = 1

      fileName := ::fullFileName

      sExt := dbInfo( DBI_TABLEEXT )

      hb_FNameSplit( fileName, @sPath, @sName, NIL, @sDrv )

      recNo := ::Alias:RecNo

      ::Alias:dbCloseArea()

      FClose( hb_FTempCreateEx( @tempName, sPath, "tmp", sExt ) )

      dbCreate( tempName, aNewStruct )

      USE ( tempName ) NEW

      BEGIN SEQUENCE WITH ;
            {| oErr|

         IF oErr:GenCode = EG_DATATYPE
            RETURN .F.
         ENDIF

         IF .T.
            Break( oErr )
         ENDIF

         RETURN NIL
         }

         APPEND FROM ( fileName )

         CLOSE ( Alias() )

         FRename( hb_FNameMerge( sPath, sName, sExt, sDrv ), hb_FNameMerge( sPath, "_" + sName, sExt, sDrv ) )

         FRename( hb_FNameMerge( sPath, sName, ".fpt", sDrv ), hb_FNameMerge( sPath, "_" + sName, ".fpt", sDrv ) )

         hb_FNameSplit( tempName, @sPath2, @sName2, @sExt2, @sDrv2 )

         FRename( hb_FNameMerge( sPath2, sName2, sExt2, sDrv2 ), hb_FNameMerge( sPath, sName, sExt, sDrv ) )

         FRename( hb_FNameMerge( sPath2, sName2, ".fpt", sDrv2 ), hb_FNameMerge( sPath, sName, ".fpt", sDrv ) )

         result := ::Alias:DbOpen( Self )

         ::Alias:RecNo := recNo

         hb_HDel( ::FInstances[ ::TableClass ], "DbStruct" )

      RECOVER

         result := .F.

      END SEQUENCE

   ELSE
      ::CancelAtFixDbStruct()
      result := .F.
   ENDIF

   RETURN result

/*
    GetAlias
    Teo. Mexico 2008
*/
METHOD FUNCTION GetAlias CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FAlias

/*
    GetAsString
    Teo. Mexico 2009
*/
METHOD FUNCTION GetAsString() CLASS TTable

   LOCAL pkField := ::GetKeyField()

   IF pkField == NIL
      RETURN ""
   ENDIF

   RETURN pkField:AsString

/*
    GetBof
    Teo. Mexico 2013
*/
METHOD FUNCTION GetBof() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FBof

/*
    GetCurrentRecord
    Teo. Mexico 2010
*/
METHOD FUNCTION GetCurrentRecord( idxAlias ) CLASS TTable

   LOCAL AField
   LOCAL Result
   LOCAL INDEX
   LOCAL READ
   LOCAL table

   IF idxAlias = NIL
      IF ::aliasIdx != NIL
         ::aliasIdx:Seek( ::Alias:RecNo, "IDX_RECNO" )
      ENDIF
      IF ::aliasTmp != NIL
         ::aliasTmp:Seek( ::Alias:RecNo, "IDX_RECNO" )
      ENDIF
      ::FBof   := ::Alias:Bof()
      ::FEof   := ::Alias:Eof()
      ::FFound := ::Alias:Found()
   ELSE
      ::Alias:dbGoto( ( idxAlias:workArea )->RecNo )
      ::FBof   := idxAlias:Bof()
      ::FEof   := idxAlias:Eof()
      ::FFound := idxAlias:Found()
      IF ::aliasIdx != NIL .AND. ::aliasTmp != NIL
         IF idxAlias == ::aliasIdx
            ::aliasTmp:Seek( ::Alias:RecNo, "IDX_RECNO" )
         ELSE
            ::aliasIdx:Seek( ::Alias:RecNo, "IDX_RECNO" )
         ENDIF
      ENDIF
   ENDIF

   ::FRecNo := ::Alias:RecNo

   IF ::GetIndex() != NIL
      ::GetIndex():SetEof( ::FEof )
      ::GetIndex():SetBof( ::FBof )
      ::GetIndex():SetFound( ::FFound )
      ::GetIndex():SetRecNo( ::FRecNo )
      IF hb_HHasKey( ::ExternalIndexList, ::GetIndex():ObjectH )
         index := ::GetIndex() ; ::SetIndex( NIL )
         ::GetCurrentRecord( idxAlias )
         index:setRecNoBlock:Eval( Self, index:Table )
         ::SetIndex( index )
         read := .T.
      ENDIF
   ENDIF

   IF ::FState = dsBrowse

      IF ( Result := ::InsideScope() )

         IF !read == .T.

            FOR EACH AField IN ::FFieldList

               IF AField:Enabled
                  IF AField:FieldMethodType = "C" .AND. !AField:Calculated // .AND. !AField:IsMasterFieldComponent
                     AField:GetData()
                  ENDIF

                  IF AField:FieldType = ftObject .AND. AField:Calculated .AND. AField:LinkedTableAssigned
                     table := AField:LinkedTable
                     IF table:LinkedObjField != NIL .AND. table:LinkedObjField:Calculated .AND. !table:MasterSource == Self .AND. table:MasterSource == table:LinkedObjField:Table:KeyField:LinkedTable
                        table:LinkedObjField:Table:KeyField:DataObj()
                     ENDIF
                  ENDIF
               ENDIF

            NEXT

         ENDIF

      ELSE
         ::FEof := .T.
         ::FBof := .T.
         ::FFound := .F.
         ::Reset()
      ENDIF

   ELSE
      // RAISE ERROR "Table not in dsBrowse mode..."
      Result := .F.
   ENDIF

   IF ::allowOnDataChange
      ::OnDataChange()
   ENDIF

   RETURN Result

/*
    GetDataBase
    Teo. Mexico 2010
*/
METHOD FUNCTION GetDataBase() CLASS TTable

   IF ::FDataBaseClass = NIL
      RETURN NIL
   ENDIF

   RETURN ::hDataBase[ ::FDataBaseClass ]

/*
    GetDbStruct
    Teo. Mexico 2007
*/
METHOD FUNCTION GetDbStruct CLASS TTable

   IF ! hb_HHasKey( ::FInstances[ ::TableClass ], "DbStruct" )
      ::FInstances[ ::TableClass, "DbStruct" ] := ::Alias:DbStruct
   ENDIF

   RETURN ::FInstances[ ::TableClass, "DbStruct" ]

/*
    GetDisplayFieldBlock
    Teo. Mexico 2008
*/
METHOD FUNCTION GetDisplayFieldBlock( xField ) CLASS TTable

   LOCAL AField
   LOCAL msgName

   SWITCH ValType( xField )
   CASE 'C'
      AField := ::FieldByName( xField )
      EXIT
   CASE 'O'
      AField := xField
      EXIT
   CASE 'N'
      AField := ::FieldList[ xField ]
      EXIT
   ENDSWITCH

   IF AField == NIL
      RAISE ERROR "Wrong value"
      RETURN NIL
   ENDIF

   msgName := AField:Name

   IF ! AField:IsDerivedFrom( "TObjectField" )
      RETURN ;
         {| o, ...|
      LOCAL odf
      LOCAL AField
      LOCAL result

      IF hb_HHasKey( o:__FFields, msgName )
         AField := o:__FFields[ msgName ]
      ELSE
         AField := o:__FObj:FieldByName( msgName )
         o:__FFields[ msgName ] := AField
      ENDIF

      IF o:__FSyncFromAlias
         o:__FObj:SyncRecNo( .T. )
      ENDIF

      odf := o

      WHILE odf:__FObj:LinkedObjField != NIL
         odf := odf:__FObj:LinkedObjField:Table:GetDisplayFields()
         odf:__FLastLabel := AField:Label
      ENDDO

      IF o:__FObj:Eof()
         RETURN AField:EmptyValue
      ENDIF

      result := AField:Value( ... )

      o:__FObj:Alias:SyncFromRecNo()

      RETURN result
      }

   ENDIF

   RETURN ;
      {| o|
LOCAL AField

IF hb_HHasKey( o:__FFields, msgName )
AField := o:__FFields[ msgName ]
ELSE
AField := o:__FObj:FieldByName( msgName )
o:__FFields[ msgName ] := AField
ENDIF

IF o:__FSyncFromAlias
o:__FObj:SyncRecNo( .T. )
ENDIF

   RETURN AField:DataObj:GetDisplayFields( NIL )
}

METHOD FUNCTION GetDisplayFields( syncFromAlias ) CLASS TTable

   LOCAL DisplayFieldsClass
   LOCAL msgName
   LOCAL itm

   IF ::FDisplayFields == NIL

      IF ::FisMetaTable
         ::isMetaTable := .F.
      ENDIF

      IF ::FInstances[ ::TableClass, "DisplayFieldsClass" ] == NIL

         DisplayFieldsClass := HBClass():New( ::ClassName + "DisplayFields", { @TDisplayFields() } )

         FOR EACH itm IN ::GetPublishedFieldNameList

            msgName := itm[ 1 ]

            /* TODO: Check for a duplicate message name */
            IF !Empty( msgName ) // .AND. ! __ObjHasMsg( ef, msgName )

               DisplayFieldsClass:AddInline( msgName, ::GetDisplayFieldBlock( msgName ) )

            ENDIF

         NEXT

         // Create the MasterSource field access reference
         IF ::FMasterSource != NIL
            DisplayFieldsClass:AddInline( "MasterSource", {| Self| ::__FObj:MasterSource:GetDisplayFields() } )
         ENDIF

         DisplayFieldsClass:Create()

         ::FInstances[ ::TableClass, "DisplayFieldsClass" ] := DisplayFieldsClass

      ENDIF

      ::FDisplayFields := ::FInstances[ ::TableClass, "DisplayFieldsClass" ]:Instance()
      ::FDisplayFields:__FObj := Self
      ::FDisplayFields:__FFields := { => }
      ::FDisplayFields:__FSyncFromAlias := .F.

   ENDIF

   IF syncFromAlias != NIL
      ::FDisplayFields:__FSyncFromAlias := syncFromAlias
   ENDIF

   RETURN ::FDisplayFields

/*
    GetEof
    Teo. Mexico 2013
*/
METHOD FUNCTION GetEof() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FEof

/*
    GetFieldTypes
    Teo. Mexico 2008
*/
METHOD FUNCTION GetFieldTypes CLASS TTable

   /* obtained from Harbour's src/rdd/workarea.c hb_waCreateFields */

   IF ::FFieldTypes == NIL
      ::FFieldTypes := { => }
      ::FFieldTypes[ 'C' ] := "TStringField"  /* HB_FT_STRING */
      ::FFieldTypes[ 'L' ] := "TLogicalField"  /* HB_FT_LOGICAL */
      ::FFieldTypes[ 'D' ] := "TDateField"   /* HB_FT_DATE */
      ::FFieldTypes[ 'I' ] := "TIntegerField"  /* HB_FT_INTEGER */
      ::FFieldTypes[ 'Y' ] := "TNumericField"  /* HB_FT_CURRENCY */
      ::FFieldTypes[ '2' ] := "TIntegerField"  /* HB_FT_INTEGER */
      ::FFieldTypes[ '4' ] := "TIntegerField"  /* HB_FT_INTEGER */
      ::FFieldTypes[ 'N' ] := "TNumericField"  /* HB_FT_LONG */
      ::FFieldTypes[ 'F' ] := "TNumericField"  /* HB_FT_FLOAT */
      ::FFieldTypes[ '8' ] := "TFloatField"   /* HB_FT_DOUBLE */
      ::FFieldTypes[ 'B' ] := "TFloatField"   /* HB_FT_DOUBLE */

      ::FFieldTypes[ 'T' ] := "TTimeField"   /* HB_FT_TIME(4) */
      ::FFieldTypes[ '@' ] := "TDateTimeField"  /* HB_FT_TIMESTAMP */
      ::FFieldTypes[ '=' ] := "TModTimeField"  /* HB_FT_MODTIME */
      ::FFieldTypes[ '^' ] := "TRowVerField"  /* HB_FT_ROWVER */
      ::FFieldTypes[ '+' ] := "TAutoIncField"  /* HB_FT_AUTOINC */
      ::FFieldTypes[ 'Q' ] := "TVarLengthField"  /* HB_FT_VARLENGTH */
      ::FFieldTypes[ 'V' ] := "TVarLengthField"  /* HB_FT_VARLENGTH */
      ::FFieldTypes[ 'M' ] := "TMemoField"   /* HB_FT_MEMO */
      ::FFieldTypes[ 'P' ] := "TImageField"   /* HB_FT_IMAGE */
      ::FFieldTypes[ 'W' ] := "TBlobField"   /* HB_FT_BLOB */
      ::FFieldTypes[ 'G' ] := "TOleField"   /* HB_FT_OLE */
      ::FFieldTypes[ '0' ] := "TVarLengthField"  /* HB_FT_VARLENGTH (NULLABLE) */
   ENDIF

   RETURN ::FFieldTypes

/*
    GetField
    Teo. Mexico 2009
*/
METHOD FUNCTION GetField( fld ) CLASS TTable

   LOCAL AField

   SWITCH ValType( fld )
   CASE 'C'
      AField := ::FieldByName( fld )
      EXIT
   CASE 'O'
      AField := fld
      EXIT
   OTHERWISE
      RAISE ERROR "Unknown field reference..."
   ENDSWITCH

   RETURN AField

/*
    GetFound
    Teo. Mexico 2013
*/
METHOD FUNCTION GetFound() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FFound

/*
    GetHasFilter
    Teo. Mexico 2010
*/
METHOD GetHasFilter() CLASS TTable
   RETURN ::FDbFilter != NIL

/*
    GetIndex
    Teo. Mexico 2013
*/
METHOD FUNCTION GetIndex() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FIndex

/*
    GetInstance
    Teo. Mexico 2008
*/
METHOD FUNCTION GetInstance CLASS TTable

   // LOCAL instance

   // IF ::FRDOClient != NIL //.AND. !HB_HHasKey( ::FInstances, ::TableClass )
   // instance := ::SendToServer()
   // RETURN instance
   // ENDIF

   IF hb_HHasKey( ::FInstances, ::TableClass )
      RETURN ::FInstances[ ::TableClass ]
   ENDIF

   RETURN NIL

/*
    GetKeyExpression
    Teo. Mexico 2011
*/
METHOD FUNCTION GetKeyExpression() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:KeyExpression
   ENDIF

   RETURN ""

/*
    GetKeyField
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyField() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:KeyField
   ENDIF

   RETURN NIL

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( value ) CLASS TTable

   LOCAL fld

   fld := ::GetKeyField()
   IF fld = NIL
      RETURN NIL
   ENDIF

   RETURN fld:GetKeyVal( value )

/*
    GetMasterKeyExpression
    Teo. Mexico 2011
*/
METHOD FUNCTION GetMasterKeyExpression() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:MasterKeyExpression
   ENDIF

   RETURN ""

/*
    GetMasterKeyField
    Teo. Mexico 2009
*/
METHOD FUNCTION GetMasterKeyField() CLASS TTable

   IF ::FPrimaryIndex != NIL
      RETURN ::FPrimaryIndex:MasterKeyField
   ENDIF

   RETURN NIL

/*
    GetMasterSource
    Teo. Mexico 2009
*/
METHOD FUNCTION GetMasterSource() CLASS TTable

   SWITCH ::FMasterSourceType
   CASE rxMasterSourceTypeTTable
      RETURN ::FMasterSource
   CASE rxMasterSourceTypeTField
      RETURN ::FMasterSource:LinkedTable
   CASE rxMasterSourceTypeBlock
      RETURN ::FMasterSource:Eval()
   ENDSWITCH

   RETURN NIL

/*
    GetMasterSourceClassName
    Teo. Mexico 2008
*/
METHOD FUNCTION GetMasterSourceClassName() CLASS TTable

   LOCAL Result := ""
   LOCAL className := ::ClassName

   IF ::DataBase = NIL
      ::DataBase := ::InitDataBase()
   ENDIF

   IF hb_HHasKey( ::DataBase:ChildParentList, className )
      Result := ::DataBase:ChildParentList[ className ]
      WHILE hb_HHasKey( ::DataBase:TableList, Result ) .AND. ::DataBase:TableList[ Result, "Virtual" ]
         IF !hb_HHasKey ( ::DataBase:ChildParentList, Result )
            EXIT
         ENDIF
         Result := ::DataBase:ChildParentList[ Result ]
      ENDDO
   ENDIF

   RETURN Result

/*
    GetPublishedFieldNameList
    Teo. Mexico 2012
*/
METHOD FUNCTION GetPublishedFieldNameList( typeList ) CLASS TTable

   LOCAL result := {}
   LOCAL AField
   LOCAL itm

   FOR EACH AField IN ::FFieldList
      IF !Empty( AField:Name )
         IF Empty( typeList )
            IF AField:Published
               AAdd( result, { AField:Name, AField } )
            ENDIF
            IF !Empty( AField:nameAlias ) .AND. AField:nameAliasPublished
               AAdd( result, { AField:nameAlias, AField } )
            ENDIF
         ELSE
            FOR EACH itm IN typeList
               IF AField:IsDerivedFrom( itm )
                  IF AField:Published
                     AAdd( result, { AField:Name, AField } )
                  ENDIF
                  IF !Empty( AField:nameAlias ) .AND. AField:nameAliasPublished
                     AAdd( result, { AField:nameAlias, AField } )
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF
   NEXT

   ASort( result,,, {| x, y| x[ 2 ]:ValType < y[ 2 ]:ValType .OR. ( x[ 2 ]:ValType == y[ 2 ]:ValType .AND. x[ 1 ] < y[ 1 ] ) } )

   RETURN result

/*
    GetRecNo
    Teo. Mexico 2013
*/
METHOD FUNCTION GetRecNo() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN ::FRecNo

/*
    GetTableFileName
    Teo. Mexico 2010
*/
METHOD FUNCTION GetTableFileName() CLASS TTable

   IF Empty( ::FTableFileName )
      IF ::AutoCreate
         FClose( hb_FTempCreateEx( @::FTableFileName, NIL, "t", ".dbf" ) )
         ::FIsTempTable := .T.
      ENDIF
   ENDIF

   RETURN ::FTableFileName

/*
    GetValue
    Teo. Mexico 2011
*/
METHOD FUNCTION GetValue CLASS TTable
   RETURN ::FBaseKeyField:Value

/*
    ImportField
    Teo. Mexico 2012
*/
METHOD FUNCTION ImportField( fromField, fieldDbName, fieldName ) CLASS TTable

   LOCAL fld

   IF Empty( fieldDbName )
      fieldDbName := fromField:FieldMethod
   ELSEIF Empty( fieldName )
      fieldName := fieldDbName
   ENDIF

   IF Empty( fieldName )
      fieldName := fromField:Name
   ENDIF

   fld :=  __clsInst( fromField:ClassH() )

   fld:New( Self )

   fld:Name := fieldName
   fld:FieldMethod := fieldDbName

   IF fld:IsDerivedFrom( "TObjectField" )
      fld:ObjClass := fromField:ObjClass
   ENDIF

   AAdd( ::FFieldList, fld )

   RETURN fld

/*
    IndexByName
    Teo. Mexico 2013
*/
METHOD FUNCTION IndexByName( indexName, curClass ) CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   RETURN F_IndexByName( Self, indexName, curClass )

/*
    F_IndexByName
    Teo. Mexico 2013
*/
STATIC FUNCTION F_IndexByName( Self, indexName, curClass )

   LOCAL className

   curClass := iif( curClass = NIL, Self, curClass )
   className := curClass:ClassName()

   IF ! className == "TTABLE"
      IF hb_HHasKey( ::IndexList, className )
         IF hb_HHasKey( ::IndexList[ className ], indexName )
            RETURN ::IndexList[ className, indexName ]
         ENDIF
      ENDIF
      RETURN F_IndexByName( Self, indexName, curClass:Super )
   ENDIF

   RETURN NIL

/*
    InitTable
    Teo. Mexico 2009
*/
METHOD PROCEDURE InitTable() CLASS TTable

   IF !hb_HHasKey( ::FInstances, ::TableClass )

      ::FInstances[ ::TableClass ] := hb_HSetCaseMatch( { "Initializing" => .T. }, .F. )

   ENDIF

    /*!
    * Make sure that database is open here
    */
   IF ::FAlias == NIL
      ::FAlias := TAlias():New( Self )
   ENDIF

   IF ::FInstances[ ::TableClass, "Initializing" ]

      ::OnClassInitializing()

      ::FInstances[ ::TableClass, "ChildReferenceList" ] := {}

      ::DefineRelations()

      ::FInstances[ ::TableClass, "DisplayFieldsClass" ] := NIL

      ::FInstances[ ::TableClass, "Initializing" ] := .F.

   ENDIF

   RETURN

/*
    Insert
    Teo. Mexico 2006
*/
METHOD FUNCTION Insert() CLASS TTable

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   IF !::State = dsBrowse
      ::Error_TableNotInBrowseState()
      RETURN .F.
   ENDIF

   IF ::OnBeforeInsert() .AND. ::AddRec()

      /* To Flush !!! */
      ::Alias:dbSkip( 0 )

      ::OnAfterInsert()

      RETURN .T.

   ENDIF

   RETURN .F.

/*
    InsertRecord
    Teo. Mexico 2007
*/
METHOD FUNCTION InsertRecord( origin ) CLASS TTable

   IF !::Insert()
      RETURN .F.
   ENDIF

   IF ::CopyRecord( origin )

      RETURN ::Post()

   ELSE

      ::Cancel()

   ENDIF

   RETURN .F.

/*
    InsideScope
    Teo. Mexico 2008
*/
METHOD FUNCTION InsideScope() CLASS TTable

   IF ::Eof() .OR. ( ::MasterSource != NIL .AND. ::MasterSource:Eof() )
      RETURN .F.
   ENDIF

   RETURN ::GetIndex() = NIL .OR. ::GetIndex():InsideScope()

/*
    OnActiveSetKeyVal
    Teo. Mexico 2011
*/
METHOD FUNCTION OnActiveSetKeyVal( value ) CLASS TTable

   IF value == NIL
      RETURN ::FOnActiveSetKeyVal
   ENDIF
   ::FOnActiveSetKeyVal := value

   RETURN value

/*
    OnDataChange
    Teo. Mexico 2010
*/
METHOD PROCEDURE OnDataChange() CLASS TTable

   IF ::OnDataChangeBlock != NIL
      ::OnDataChangeBlock:Eval( iif( ::OnDataChangeBlock_Param = NIL, Self, ::OnDataChangeBlock_Param ) )
   ENDIF

   RETURN

/*
    Open
    Teo. Mexico 2008
*/
METHOD FUNCTION Open() CLASS TTable

   LOCAL masterSource := ::GetMasterSourceClassName()

   IF ::MasterSource == NIL .AND. !Empty( masterSource )

      RAISE ERROR "Table '" + ::ClassName() + "' needs a MasterSource of type '" + masterSource  + "'..."

   ENDIF

   ::FActive := .T.

   ::SetState( dsBrowse )

    /*
     * Try to sync with MasterSource (if any)
     */
   ::SyncFromMasterSourceFields()

   ::allowOnDataChange := .T.

   IF ::Alias != NIL
      ::FHasDeletedOrder := ::Alias:ordNumber( "__AVAIL" ) > 0
   ENDIF

   ::OnDataChange()

   ::OnAfterOpen()

   RETURN .T.

/*
    OrdCondSet
    Teo. Mexico 2010
*/
METHOD FUNCTION ordCondSet( ... ) CLASS TTable
   RETURN ::Alias:ordCondSet( ... )

/*
    OrdCreate
    Teo. Mexico 2010
*/
METHOD PROCEDURE ordCreate( ... ) CLASS TTable

   LOCAL scopeTop, scopeBottom
   LOCAL masterKeyVal
   LOCAL syncFromAlias := ::DisplayFields:__FSyncFromAlias

   // LOCAL oDlg

   dbSelectArea( ::Alias:Name )

   IF !Empty( ::IndexName )
      masterKeyVal := ::GetIndex():MasterKeyVal
      ordSetFocus( ::IndexName )
      scopeTop := ordScope( 0, RTrim( masterKeyVal + ::GetIndex():ScopeTop() ) )
      scopeBottom := ordScope( 1, masterKeyVal + ::GetIndex():ScopeBottom() )
   ENDIF

   // DbGoTop()

   ::DisplayFields:__FSyncFromAlias := .T.

/*
    CREATE DIALOG oDlg ;
        TITLE "Un momento..." ;
        PARENT ::Frame

    SHOW WINDOW oDlg CENTRE
*/

   ::Alias:ordCreate( ... )

   IF !Empty( ::IndexName )
      ordSetFocus( ::IndexName )
      ordScope( 0, scopeTop )
      ordScope( 1, scopeBottom )
   ENDIF

/*
    DESTROY oDlg
*/

   ::DisplayFields:__FSyncFromAlias := syncFromAlias

   ordCustom( NIL, NIL, .T. )

   RETURN

/*
    Post
    Teo. Mexico 2010
*/
METHOD FUNCTION Post() CLASS TTable

   LOCAL AField
   LOCAL errObj
   LOCAL postOk := .F.
   LOCAL aChangedFields := {}
   LOCAL changed := .F.
   LOCAL result

   IF AScan( { dsEdit, dsInsert }, ::State ) = 0
      ::Error_Table_Not_In_Edit_or_Insert_mode()
   ENDIF

   BEGIN SEQUENCE WITH ::ErrorBlock

      ::FSubState := dssPosting

      IF ::OnBeforePost()

         FOR EACH AField IN ::FieldList

            IF AField:Enabled .AND. !AField:Calculated
               IF AField:Changed
                  IF AField:OnAfterPostChange != NIL
                     AAdd( aChangedFields, AField )
                  ENDIF
                  changed := .T.
               ENDIF
               result := AField:Validate()
               IF result != NIL
                  RAISE ERROR "Post: Invalid data on Field: " + result
               ENDIF
            ENDIF

         NEXT

         ::RecUnLock()

         postOk := .T.

      ENDIF

   RECOVER USING errObj

      ::Cancel()

      SHOW ERROR errObj

   ALWAYS

      ::FSubState := dssNone

   END SEQUENCE

   IF postOk
      ::OnAfterPost()
      IF Len( aChangedFields ) > 0
         FOR EACH AField IN aChangedFields
            AField:OnAfterPostChange:Eval( Self )
         NEXT
      ENDIF
      IF changed
         IF __objHasMsgAssigned( Self, "OnAfterChange" )
            __objSendMsg( Self, "OnAfterChange" )
         ENDIF
      ENDIF
   ENDIF

   ::FcauseEditState := NIL

   RETURN postOk

/*
    Process_TableName
    Teo. Mexico 2008
*/
METHOD PROCEDURE Process_TableName( tableName ) CLASS TTable

   LOCAL s, sHostPort

   IF tableName == NIL
      tableName := ::TableFileName
   ELSE
      ::FTableFileName := tableName
   ENDIF

    /*
        Process tableName to check if we need a RDOClient to an RDOServer
    */
   IF Upper( tableName ) = "RDO://"

      s := hb_tokenGet( ::TableFileName, 2, "://" )
      sHostPort := hb_tokenGet( s, 1, "/" )
      ::FTableFileName := SubStr( s, At( "/", s ) + 1 )

      ::FAddress := hb_tokenGet( sHostPort, 1, ":" )
      ::FPort := hb_tokenGet( sHostPort, 2, ":" )

        /*
            Checks if RDO Client is required
        */
      // ::FRDOClient := TRDOClient():New( ::FAddress, ::FPort )
      IF !::FRDOClient:Connect()
         ::Error_ConnectToServer_Failed()
         RETURN
      ENDIF

   ENDIF

   RETURN

/*
    RawGet4Seek
    Teo. Mexico 2009
*/
METHOD FUNCTION RawGet4Seek( direction, xField, keyVal, index, softSeek ) CLASS TTable

   LOCAL AIndex := ::FindIndex( index )

   RETURN AIndex:RawGet4Seek( direction, ::GetField( xField ):FieldReadBlock, keyVal, softSeek )

/*
    RawSeek
    Teo. Mexico 2008
*/
METHOD FUNCTION RawSeek( Value, index ) CLASS TTable
   RETURN ::FindIndex( index ):RawSeek( Value )

/*
    RecLock
    Teo. Mexico 2006
*/
METHOD FUNCTION RecLock() CLASS TTable

   LOCAL allowOnDataChange
   LOCAL result

   IF ::FState != dsBrowse
      ::Error_Table_Not_In_Browse_Mode()
      RETURN .F.
   ENDIF

   IF !::OnBeforeLock()
      RETURN .F.
   ENDIF

   IF ::FReadOnly
      SHOW WARN "Table '" + ::ClassName() + "' is marked as READONLY..."
      RETURN .F.
   ENDIF

   IF ::Eof()
      RAISE ERROR "Attempt to lock record at EOF..."
   ENDIF

   IF !::InsideScope .OR. !::Alias:RecLock()
      RETURN .F.
   ENDIF

   allowOnDataChange := ::allowOnDataChange
   ::allowOnDataChange := .F.

   result := ::GetCurrentRecord()

   IF result
      ::SetState( dsEdit )
      ::FcauseEditState := dsEdit
   ELSE
      ::Alias:RecUnLock()
   ENDIF

   ::allowOnDataChange := allowOnDataChange

   RETURN result

/*
    RecUnLock
    Teo. Mexico 2006
*/
METHOD FUNCTION RecUnLock() CLASS TTable

   LOCAL Result

   IF ( Result := ::Alias:RecUnLock() )
      ::SetState( dsBrowse )
      ::OnDataChange()
   ENDIF
   ::UpdateCustomIndexes()

   RETURN Result

/*
    Refresh
    Teo. Mexico 2007
*/
METHOD PROCEDURE Refresh CLASS TTable

   IF ::FRecNo = ::Alias:RecNo
      RETURN
   ENDIF
   ::GetCurrentRecord()

   RETURN

/*
    Reset
    Teo. Mexico 2006
*/
METHOD PROCEDURE Reset() CLASS TTable

   LOCAL AField

   ::FUnderReset := .T.

   FOR EACH AField IN ::FFieldList

      IF AField:FieldMethodType = "C" .AND. AField:Enabled
         IF AField:RawDefaultValue != NIL .OR. AField:RawNewValue != NIL .OR. !AField:IsMasterFieldComponent
            AField:Reset()
         ENDIF
      ENDIF

   NEXT

   ::FUnderReset := .F.

   RETURN

/*
    SetBaseKeyIndex
    Teo. Mexico 2012
*/
METHOD PROCEDURE SetBaseKeyIndex( baseKeyIndex ) CLASS TTable

   LOCAL className
   LOCAL tableBaseClass
   LOCAL baseKeyField

   baseKeyField := baseKeyIndex:KeyField

   ::FBaseKeyIndex := baseKeyIndex
   ::FBaseKeyField := baseKeyField

   className := ::ClassName
   tableBaseClass := baseKeyField:TableBaseClass

   IF AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == Upper( className ) } ) = 0
      AAdd( BaseKeyFieldList, { className, baseKeyField:DBS_TYPE, baseKeyField:DBS_LEN, baseKeyField:DBS_DEC, baseKeyField:Size } )
   ENDIF

   IF !Upper( className ) == Upper( tableBaseClass ) .AND. AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == Upper( tableBaseClass ) } ) = 0
      AAdd( BaseKeyFieldList, { tableBaseClass, baseKeyField:DBS_TYPE, baseKeyField:DBS_LEN, baseKeyField:DBS_DEC, baseKeyField:Size } )
   ENDIF

   RETURN

/*
    SetDataBase
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetDataBase( dataBase ) CLASS TTable

   IF dataBase = NIL
      ::FDataBaseClass := NIL
   ELSE
      ::FDataBaseClass := dataBase:ClassName
      IF !hb_HHasKey( ::hDataBase, ::FDataBaseClass )
         ::hDataBase[ dataBase:ClassName ] := dataBase
      ENDIF
   ENDIF

   RETURN

/*
    SetIndex
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetIndex( index ) CLASS TTable

   IF !Empty( index ) .AND. !::GetIndex() == index
      ::FIndex := index
   ENDIF

   RETURN

/*
    SetIndexName
    Teo. Mexico 2007
*/
METHOD PROCEDURE SetIndexName( indexName ) CLASS TTable

   LOCAL INDEX

   IF ! Upper( indexName ) == Upper( ::IndexName )

      index := ::IndexByName( indexName )

      IF index != NIL
         ::SetIndex( index )
         RETURN
      ENDIF

      RAISE ERROR  "<" + ::ClassName + ">: Index name '" + indexName + "' doesn't exist..."

   ENDIF

   RETURN

/*
    SetisMetaTable
    Teo. Mexico 2013
*/
METHOD PROCEDURE SetisMetaTable( isMetaTable ) CLASS TTable

   IF ::FisMetaTable .AND. !isMetaTable
      ::CreateTableInstance()
   ENDIF

   RETURN

/*
    SetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION SetKeyVal( keyVal ) CLASS TTable

   ::GetKeyField():SetKeyVal( keyVal )

   RETURN Self

/*
    SetMasterSource
    Teo. Mexico 2007
*/
METHOD PROCEDURE SetMasterSource( masterSource ) CLASS TTable

   IF ::FMasterSource == masterSource
      RETURN
   ENDIF

   ::FMasterSource := masterSource

   SWITCH ValType( masterSource )
   CASE 'O'
      IF masterSource:IsDerivedFrom( "TTable" )
         ::FMasterSourceType := rxMasterSourceTypeTTable
      ELSEIF masterSource:IsDerivedFrom( "TObjectField" )
         ::FMasterSourceType := rxMasterSourceTypeTField
      ELSEIF masterSource:IsDerivedFrom( "TField" )
         RAISE ERROR "need to specify TField generic syncing..."
      ELSE
         RAISE ERROR "Invalid object in assigning MasterSource..."
      ENDIF
      EXIT
   CASE 'B'
      ::FMasterSourceType := rxMasterSourceTypeBlock
      EXIT
   CASE 'U'
      ::FMasterSourceType := rxMasterSourceTypeNone
      RETURN
   OTHERWISE
      RAISE ERROR "Invalid type in assigning MasterSource..."
   ENDSWITCH

    /*!
     * Check for a valid GetMasterSourceClassName (if any)
     */
   IF !Empty( ::GetMasterSourceClassName() )
      // IF !::MasterSource:IsDerivedFrom( ::GetMasterSourceClassName ) .AND. !::DataBase:TableIsChildOf( ::GetMasterSourceClassName, ::MasterSource:ClassName )
      // IF ! Upper( ::GetMasterSourceClassName ) == ::MasterSource:ClassName
      IF ! ::MasterSource:IsDerivedFrom( ::GetMasterSourceClassName() )
         RAISE ERROR "Table <" + ::TableClass + "> Invalid MasterSource Class Name: " + ::MasterSource:ClassName + ";Expected class type: <" + ::GetMasterSourceClassName() + ">"
      ENDIF
   ELSE
      RAISE ERROR "Table '" + ::ClassName() + "' has not declared the MasterSource '" + ::MasterSource:ClassName() + "' in the DataBase structure..."
   ENDIF

    /*
     * Check if another Self is already in the MasterSource DetailSourceList
     * and RAISE ERROR if another Self is trying to break the previous link
     */
   IF hb_HHasKey( ::MasterSource:DetailSourceList, Self:ObjectH )
      RAISE ERROR "Cannot re-assign DetailSourceList:<" + ::ClassName + ">"
   ENDIF

   ::MasterSource:DetailSourceList[ Self:ObjectH ] := Self

   ::SyncFromMasterSourceFields()

   RETURN

/*
    SetPrimaryIndex
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetPrimaryIndex( primaryIndex ) CLASS TTable

   ::FPrimaryIndex := primaryIndex

   RETURN

/*
    SetPrimaryIndexList
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetPrimaryIndexList( clsName, name ) CLASS TTable

   ::FPrimaryIndexList[ clsName ] := name

   RETURN

/*
    SetReadOnly
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetReadOnly( readOnly ) CLASS TTable

   IF ! HB_ISLOGICAL( readOnly )
      RAISE ERROR "Invalid value on SetReadOnly..."
   ENDIF
   IF ::FState = dsBrowse
      ::FReadOnly := readOnly
   ENDIF

   RETURN

/*
    SetState
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetState( state ) CLASS TTable

   LOCAL oldState

   IF !::FState == state
      oldState := ::FState
      ::FState := state
      IF state = dsEdit .OR. state = dsInsert
         ::FUndoList := hb_HSetCaseMatch( { => }, .F. )
      ENDIF
      ::OnStateChange( oldState )
   ENDIF

   RETURN

/*
    SetValue
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetValue( value ) CLASS TTable

   ::FBaseKeyField:Value := value

   RETURN

/*
    SkipBrowse : BROWSE skipblock
    Teo. Mexico 2008
*/
METHOD FUNCTION SkipBrowse( n ) CLASS TTable

   LOCAL num_skipped := 0
   LOCAL recNo

   IF n = 0
      ::dbSkip( 0 )
      RETURN 0
   ENDIF

   IF n > 0
      WHILE !::Eof() .AND. num_skipped < n
         recNo := ::RecNo
         IF !::dbSkip( 1 ) .OR. ::Eof()
            ::dbGoto( recNo )
            EXIT
         ENDIF
         num_skipped++
      ENDDO
   ELSE
      WHILE !::Bof() .AND. num_skipped > n
         recNo := ::RecNo
         IF !::dbSkip( -1 ) .OR. ::Bof()
            ::dbGoto( recNo )
            EXIT
         ENDIF
         num_skipped--
      ENDDO
   ENDIF

   RETURN num_skipped

/*
    SkipFilter
    Teo. Mexico 2010
*/
METHOD FUNCTION SkipFilter( n, index ) CLASS TTable

   LOCAL i
   LOCAL tagName
   LOCAL o
   LOCAL ALIAS

   IF n = NIL
      n := 1
   ENDIF

   IF n > 0
      i := 1
   ELSEIF n < 0
      i := -1
   ELSE
      i := 0
   ENDIF

   n := Abs( n )

   IF index = NIL
      tagName := NIL
      o := Self
      alias := ::Alias
   ELSE
      tagName := index:TagName
      o := index
      alias := index:GetAlias()
   ENDIF

   WHILE .T.
      IF !alias:dbSkip( i, tagName ) .OR. ! o:GetCurrentRecord()
         RETURN .F.
      ENDIF
      IF ::FilterEval( index )
         --n
      ENDIF
      IF n <= 0
         EXIT
      ENDIF
   ENDDO

   RETURN .T.

/*
    StatePop
    Teo. Mexico 2010
*/
METHOD PROCEDURE StatePop() CLASS TTable

   LOCAL cloneData
   LOCAL tbl

   IF AScan( { dsInsert, dsEdit }, ::State ) > 0
      ::Cancel()
   ENDIF

   FOR EACH cloneData IN ::tableState[ ::tableStateLen ][ "CloneData" ]
      ::FFieldList[ cloneData:__enumIndex ]:CloneData := cloneData
   NEXT

   ::FRecNo           := ::tableState[ ::tableStateLen ][ "RecNo" ]
   ::FBof             := ::tableState[ ::tableStateLen ][ "Bof" ]
   ::FEof             := ::tableState[ ::tableStateLen ][ "Eof" ]
   ::FFound           := ::tableState[ ::tableStateLen ][ "Found" ]
   ::FState           := ::tableState[ ::tableStateLen ][ "State" ]
   ::FcauseEditState  := ::tableState[ ::tableStateLen ][ "causeEditState" ]
   ::IndexName        := ::tableState[ ::tableStateLen ][ "IndexName" ]

   FOR EACH tbl IN ::DetailSourceList
      IF hb_HHasKey( ::tableState[ ::tableStateLen ][ "DetailSourceList" ], tbl:ObjectH )
         tbl:StatePop()
      ENDIF
   NEXT

   ::FUndoList := ::tableState[ ::tableStateLen ][ "UndoList" ]

   --::tableStateLen

   ::Alias:Pop()

   RETURN

/*
    StatePush
    Teo. Mexico 2010
*/
METHOD PROCEDURE StatePush() CLASS TTable

   LOCAL fld
   LOCAL aCloneData := {}
   LOCAL hDSL := { => }
   LOCAL tbl

   IF ::FisMetaTable
      ::isMetaTable := .F.
   ENDIF

   IF Len( ::tableState ) < ++::tableStateLen
      AAdd( ::tableState, { => } )
   ENDIF

   FOR EACH fld IN ::FFieldList
      AAdd( aCloneData, fld:CloneData )
   NEXT

   ::tableState[ ::tableStateLen ][ "CloneData" ]        := aCloneData
   ::tableState[ ::tableStateLen ][ "RecNo" ]            := ::FRecNo
   ::tableState[ ::tableStateLen ][ "Bof" ]              := ::FBof
   ::tableState[ ::tableStateLen ][ "Eof" ]              := ::FEof
   ::tableState[ ::tableStateLen ][ "Found" ]            := ::FFound
   ::tableState[ ::tableStateLen ][ "State" ]            := ::FState
   ::tableState[ ::tableStateLen ][ "causeEditState" ]   := ::FcauseEditState
   ::tableState[ ::tableStateLen ][ "IndexName" ]        := ::IndexName
   ::tableState[ ::tableStateLen ][ "DetailSourceList" ] := hDSL
   ::tableState[ ::tableStateLen ][ "UndoList" ]         := ::FUndoList

   FOR EACH tbl IN ::DetailSourceList
      hDSL[ tbl:ObjectH ] := NIL
      tbl:StatePush()
   NEXT

   ::FState := dsBrowse
   ::FcauseEditState := NIL
   ::FUndoList := NIL

   ::Alias:Push()

   RETURN

/*
    SyncFromMasterSourceFields
    Teo. Mexico 2007
*/
METHOD PROCEDURE SyncFromMasterSourceFields() CLASS TTable

   IF ::FActive

      IF ::MasterSource != NIL

         IF ::MasterSource:Active

            ::OnSyncFromMasterSource()

            IF !::MasterSource:Eof() .AND. ::Alias != NIL

               IF ::InsideScope()
                  ::GetCurrentRecord()
               ELSE
                  ::dbGoTop()
               ENDIF

            ELSE

               ::FEof := .T.
               ::FBof := .T.
               ::FFound := .F.

               ::Reset()

               IF ::allowOnDataChange
                  ::OnDataChange()
               ENDIF

            ENDIF

         ENDIF

      ELSE

         ::GetCurrentRecord()

      ENDIF

   ENDIF

   RETURN

/*
    SyncRecNo
    Teo. Mexico 2007
*/
METHOD PROCEDURE SyncRecNo( fromAlias ) CLASS TTable

   IF fromAlias == .T.
      ::Alias:SyncFromAlias()
   ELSE
      ::Alias:SyncFromRecNo()
   ENDIF
   IF ::FRecNo = ::Alias:RecNo
      RETURN
   ENDIF
   ::GetCurrentRecord()

   RETURN

/*
    UpdateCustomIndexes
    Teo. Mexico 2012
*/
METHOD PROCEDURE UpdateCustomIndexes() CLASS TTable

   LOCAL INDEX

   FOR EACH INDEX IN ::FCustomIndexList
      index:CustomKeyUpdate()
   NEXT

   RETURN

/*
    Validate
    Teo. Mexico 2009
*/
METHOD FUNCTION Validate( showAlert ) CLASS TTable

   LOCAL AField

   FOR EACH AField IN ::FFieldList
      IF AField:Enabled .AND. AField:Validate( showAlert ) != NIL
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

/*
    End Class TTable
*/

/*
    FindTableBaseClass
    Teo. Mexico 2012
*/
STATIC FUNCTION FindTableBaseClass( AField )

   LOCAL n
   LOCAL t
   LOCAL clsName

   clsName := Upper( AField:ObjClass )

   n := AScan( BaseKeyFieldList, {| e| Upper( e[ 1 ] ) == clsName } )

   IF n = 0
      t := AField:LinkedTable
      n := AScan( BaseKeyFieldList, {| e| t:IsDerivedFrom( e[ 1 ] ) } )
   ENDIF

   RETURN n
