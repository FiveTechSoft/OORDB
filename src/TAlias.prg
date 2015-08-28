/*
 *
 */

/*
    TAlias
*/

#include "oordb.ch"
#include "xerror.ch"

THREAD STATIC __S_FInstances

CLASS TAlias FROM OORDBBASE

   PRIVATE:

   DATA FRecNo
   DATA FStack    INIT {}
   DATA FStackLen INIT 0
   DATA FTableName
   METHOD GetRecNo INLINE ::SyncFromRecNo(), ::FRecNo
   METHOD SetRecNo( RecNo ) INLINE ::dbGoto( RecNo )
   PROTECTED:
    METHOD FInstances BLOCK ;
        {||
            IF __S_FInstances = NIL
                __S_FInstances := hb_HSetCaseMatch( { => }, .F. )
            ENDIF
            RETURN __S_FInstances
        }
   DATA FstackLock INIT {}
   METHOD GetAliasName() INLINE ::FInstances[ ::FTableName, "aliasName" ]
   METHOD SetWorkArea( workArea )
   PUBLIC:

   DATA RddDriver
   DATA lReadOnly INIT .F.
   DATA lShared INIT .T.

   CONSTRUCTOR New( table, aliasName )

   METHOD __dbZap()
   METHOD AddRec( index )
   METHOD dbCloseArea()
   METHOD dbDelete()
   METHOD dbGoBottom( indexName )
   METHOD dbGoto( RecNo )
   METHOD dbGoTop( indexName )
   METHOD dbInfo( ... )
   METHOD DbOpen( table, aliasName )
   METHOD dbOrderInfo( ... )
   METHOD dbRecall()
   METHOD dbRLock( recNo ) INLINE ::recLock( recNo, noRetry )
   METHOD dbSkip( nRecords, indexName )
   METHOD dbStruct()
   METHOD dbUnlock() INLINE ::FstackLock := {}, ( ::workArea )->( dbUnlock() )
   METHOD Deleted()
   METHOD Eval( codeBlock, ... )
   METHOD ExistKey( KeyValue, IndexName, RecNo )
   METHOD FCount INLINE ( ::workArea )->( FCount() )
   METHOD FieldPos( FieldName ) INLINE ( ::workArea )->( FieldPos( FieldName ) )
   METHOD FLock() INLINE ( ::workArea )->( FLock() )
   METHOD Get4Seek( xVal, keyVal, indexName, softSeek )
   METHOD Get4SeekLast( xVal, keyVal, indexName, softSeek )
   METHOD GetFieldValue( fieldName )
   METHOD IsLocked( RecNo )
   METHOD KeyVal( indexName )
   METHOD LastRec INLINE ( ::workArea )->( LastRec() )
   METHOD ordCondSet( ... )
   METHOD ordCreate( ... )
   METHOD ordCustom( Name, cBag, KeyVal )
   METHOD ordDescend( Name, cBag, lDescend )
   METHOD ordKeyAdd( Name, cBag, KeyVal )
   METHOD ordKeyDel( Name, cBag, KeyVal )
   METHOD ordKeyNo( ... )
   METHOD ordKeyVal()
   METHOD ordNumber( ordName, ordBagName )
   METHOD ordSetFocus( Name, cBag )
   METHOD Pop()
   METHOD Push()
   METHOD RawGet4Seek( direction, xVal, keyVal, indexName, softSeek )
   METHOD RecCount INLINE ( ::workArea )->( RecCount() )
   METHOD RecLock( recNo, lNoRetry )
   METHOD RecUnLock( RecNo )
   METHOD Seek( cKey, indexName, softSeek )
   METHOD SeekLast( cKey, indexName, softSeek )
   METHOD SetFieldValue( fieldName, value )
   METHOD SyncFromAlias
   METHOD SyncFromRecNo

   MESSAGE DbSeek METHOD SEEK

    /*!
     * needed for tdbrowse.prg (oDBE:Alias)
     */
   PROPERTY ALIAS READ GetAliasName
   PROPERTY Instances READ FInstances
   PROPERTY workArea READ FInstances[ ::FTableName, "workArea" ] WRITE SetWorkArea

   PUBLISHED:
   PROPERTY Bof   INIT .T.
   PROPERTY Eof   INIT .T.
   PROPERTY Found INIT .F.
   PROPERTY Name READ GetAliasName
   PROPERTY RecNo READ GetRecNo WRITE SetRecNo

ENDCLASS

/*
    New
*/
METHOD New( table, aliasName ) CLASS TAlias

   LOCAL tableName

   IF Empty( table )
      RAISE ERROR "TAlias: Empty Table parameter."
   ENDIF

   IF HB_ISOBJECT( table )

      tableName := table:TableFileName

      IF Empty( tableName )
         RAISE ERROR "TAlias: Empty Table Name..."
      ENDIF

   ELSE

      tableName := table

   ENDIF

   IF !::DbOpen( table, aliasName )
      // RAISE ERROR "TAlias: Cannot Open Table '" + table:TableFileName + "'"
      Break( "TAlias: Cannot Open Table '" + tableName + "'" )
   ENDIF

   ::SyncFromAlias()

   RETURN Self

/*
    __DbZap
*/
METHOD FUNCTION __dbZap() CLASS TAlias
   RETURN ( ::workArea )->( __dbZap() )

/*
    AddRec
*/
METHOD FUNCTION AddRec( index ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( AddRec(, index ) )
   ::SyncFromAlias()

   RETURN Result

/*
    DbCloseArea
*/
METHOD PROCEDURE dbCloseArea() CLASS TAlias

   IF hb_HHasKey( ::FInstances, ::FTableName )
      ( ::workArea )->( dbCloseArea() )
      hb_HDel( ::FInstances, ::FTableName )
   ENDIF

   RETURN

/*
    DbDelete
*/
METHOD PROCEDURE dbDelete() CLASS TAlias

   ::SyncFromRecNo()
   ( ::workArea )->( dbDelete() )

   RETURN

/*
    DbGoBottom
*/
METHOD FUNCTION dbGoBottom( indexName ) CLASS TAlias

   LOCAL Result

   IF Empty( indexName )
      Result := ( ::workArea )->( dbGoBottom() )
   ELSE
      Result := ( ::workArea )->( DbGoBottomX( indexName ) )
   ENDIF
   ::SyncFromAlias()

   RETURN Result

/*
    DbGoTo
*/
METHOD FUNCTION dbGoto( RecNo ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( dbGoto( RecNo ) )
   ::SyncFromAlias()

   RETURN Result

/*
    DbGoTop
*/
METHOD FUNCTION dbGoTop( indexName ) CLASS TAlias

   LOCAL Result

   IF Empty( indexName )
      Result := ( ::workArea )->( dbGoTop() )
   ELSE
      Result := ( ::workArea )->( DbGoTopX( indexName ) )
   ENDIF
   ::SyncFromAlias()

   RETURN Result

/*
    DbInfo
*/
METHOD FUNCTION dbInfo( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( dbInfo( ... ) )

/*
    DbOpen
*/
METHOD DbOpen( table, aliasName ) CLASS TAlias

   LOCAL PATH
   LOCAL tableName
   LOCAL tableFullFileName
   LOCAL netIO
   LOCAL cPrefix
   LOCAL isTempTable := .F.
   LOCAL wa
   LOCAL result

   wa := Alias()

   IF HB_ISOBJECT( table )

      /* Check for a previously open workarea */
      IF hb_HHasKey( ::FInstances, table:TableFileName )
         ::FTableName := table:TableFileName
         table:fullFileName := ::FInstances[ table:TableFileName, "fullFileName" ]
         RETURN .T.
      ENDIF

      IF table:IsTempTable
         isTempTable := .T.
         IF !table:CreateTable()
            RETURN .F.
         ENDIF
         ::lShared := .F.
      ENDIF

      IF table:DataBase:OpenBlock != NIL
         IF !table:DataBase:OpenBlock:Eval( table, aliasName )
            ::FTableName := ""
            ::workArea := 0
            RETURN .F.
         ENDIF
         ::FTableName := table:TableFileName
         ::workArea := Select()
         IF !Empty( wa )
            dbSelectArea( wa )
         ENDIF
         RETURN .T.
      ENDIF

      IF !table:IsTempTable .AND. !Empty( path := LTrim( RTrim( table:TableFileName_Path ) ) )
         IF !Right( path, 1 ) == hb_osPathSeparator()
            path += hb_osPathSeparator()
         ENDIF
      ELSE
         path := ""
      ENDIF

      table:fullFileName := path + table:TableFileName

      tableFullFileName := table:fullFileName
      tableName := table:TableFileName

      netIO := table:DataBase:NetIO

   ELSE

      tableFullFileName := table
      tableName := table

   ENDIF

   cPrefix := iif( !isTempTable .AND. netIO == .T., "net:", "" )

   IF ! hb_dbExists( cPrefix + tableFullFileName ) .AND. HB_ISOBJECT( table ) .AND. table:AutoCreate
      IF !table:CreateTable( tableFullFileName )
         Break( "TAlias: Cannot Create Table '" + tableFullFileName + "'" )
      ENDIF
   ENDIF

   IF SELECT( tableName ) = 0
      dbUseArea( .T., ::RddDriver, cPrefix + tableFullFileName, aliasName, ::lShared, ::lReadOnly )
   ELSE
      dbSelectArea( tableName )
   ENDIF

   ::FTableName := tableName
   ::workArea := Select()
   ::FInstances[ tableName, "fullFileName" ] := tableFullFileName

   result := !NetErr()

   IF !Empty( wa )
      dbSelectArea( wa )
   ENDIF

   RETURN result

/*
    DbOrderInfo
*/
METHOD FUNCTION dbOrderInfo( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( dbOrderInfo( ... ) )

/*
    DbRecall
*/
METHOD PROCEDURE dbRecall() CLASS TAlias

   ::SyncFromRecNo()
   ( ::workArea )->( dbRecall() )

   RETURN

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( nRecords, indexName ) CLASS TAlias

   LOCAL Result

   ::SyncFromRecNo()

   IF Empty( indexName )
      Result := ( ::workArea )->( dbSkip( nRecords ) )
   ELSE
      Result := ( ::workArea )->( DbSkipX( nRecords, indexName ) )
   ENDIF

   ::SyncFromAlias()

   RETURN Result

/*
    DbStruct
*/
METHOD FUNCTION dbStruct() CLASS TAlias
   RETURN ( ::workArea )->( dbStruct() )

/*
    Deleted
*/
METHOD FUNCTION Deleted() CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( Deleted() )

/*
    Eval
*/
METHOD FUNCTION Eval( codeBlock, ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( codeBlock:Eval( ... ) )

/*
    ExistKey
*/
METHOD FUNCTION ExistKey( KeyValue, IndexName, RecNo ) CLASS TAlias
   RETURN ( ::workArea )->( ExistKey( KeyValue, IndexName, RecNo ) )

/*
    Get4Seek
*/
METHOD FUNCTION Get4Seek( xVal, keyVal, indexName, softSeek ) CLASS TAlias
   RETURN ::RawGet4Seek( 1, xVal, keyVal, indexName, softSeek )

/*
    Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( xVal, keyVal, indexName, softSeek ) CLASS TAlias
   RETURN ::RawGet4Seek( 0, xVal, keyVal, indexName, softSeek )

/*
    GetFieldValue
*/
METHOD FUNCTION GetFieldValue( fieldName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( FieldGet( FieldPos( fieldName ) ) )

/*
    IsLocked
*/
METHOD FUNCTION IsLocked( RecNo ) CLASS TAlias
   RETURN ( ::workArea )->( IsLocked( iif( RecNo == NIL, ::FRecNo, RecNo ) ) )

/*
    KeyVal
*/
METHOD FUNCTION KeyVal( indexName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( KeyVal( indexName ) )

/*
    OrdCondSet
*/
METHOD FUNCTION ordCondSet( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCondSet( ... ) )

/*
    OrdCreate
*/
METHOD FUNCTION ordCreate( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCreate( ... ) )

/*
    OrdCustom
*/
METHOD FUNCTION ordCustom( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordCustom( Name, cBag, KeyVal ) )

/*
    ordDescend
*/
METHOD FUNCTION ordDescend( Name, cBag, lDescend ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordDescend( Name, cBag, lDescend ) )

/*
    OrdKeyAdd
*/
METHOD FUNCTION ordKeyAdd( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyAdd( Name, cBag, KeyVal ) )

/*
    OrdKeyDel
*/
METHOD FUNCTION ordKeyDel( Name, cBag, KeyVal ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyDel( Name, cBag, KeyVal ) )

/*
    OrdKeyNo
*/
METHOD FUNCTION ordKeyNo( ... ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyNo( ... ) )

/*
    OrdKeyVal
*/
METHOD FUNCTION ordKeyVal() CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordKeyVal() )

/*
    OrdNumber
*/
METHOD FUNCTION ordNumber( ordName, ordBagName ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordNumber( ordName, ordBagName ) )

/*
    OrdSetFocus
*/
METHOD FUNCTION ordSetFocus( Name, cBag ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( ordSetFocus( Name, cBag ) )

/*
    Pop
*/
METHOD PROCEDURE Pop() CLASS TAlias

   IF ::FStackLen > 0
      ::FBof  := ::FStack[ ::FStackLen, 1 ]
      ::FEof  := ::FStack[ ::FStackLen, 2 ]
      ::FFound := ::FStack[ ::FStackLen, 3 ]
      ::FRecNo := ::FStack[ ::FStackLen, 4 ]
      ::ordSetFocus( ::FStack[ ::FStackLen, 5 ] )
      --::FStackLen
   ENDIF

   RETURN

/*
    Push
*/
METHOD PROCEDURE Push() CLASS TAlias

   IF Len( ::FStack ) < ++::FStackLen
      AAdd( ::FStack, { NIL, NIL, NIL, NIL, NIL } )
   ENDIF
   ::FStack[ ::FStackLen, 1 ] := ::FBof
   ::FStack[ ::FStackLen, 2 ] := ::FEof
   ::FStack[ ::FStackLen, 3 ] := ::FFound
   ::FStack[ ::FStackLen, 4 ] := ::FRecNo
   ::FStack[ ::FStackLen, 5 ] := ::ordSetFocus()

   RETURN

/*
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, xVal, keyVal, indexName, softSeek ) CLASS TAlias

   IF ValType( xVal ) = "O"
      xVal := xVal:FieldReadBlock
   END

   IF keyVal = NIL
      keyVal := ""
   ENDIF

   IF direction = 1
      RETURN ( ::workArea )->( Get4Seek( xVal, keyVal, indexName, softSeek ) )
   ENDIF

   RETURN ( ::workArea )->( Get4SeekLast( xVal, keyVal, indexName, softSeek ) )

/*
    RecLock
*/
METHOD FUNCTION RecLock( recNo, lNoRetry ) CLASS TAlias

   LOCAL n

   ::SyncFromRecNo()
   IF recNo = NIL
      recNo := ::FrecNo
   ENDIF
   IF ::IsLocked()
      n := AScan( ::FstackLock, {| e| e[ 1 ] = recNo } )
      IF n > 0
         ::FstackLock[ n, 2 ]++
      ELSE
         AAdd( ::FstackLock, { recNo, 1 } )
      ENDIF
      RETURN .T.
   ENDIF

   IF lNoRetry = noRetry
      RETURN ( ::workArea )->( dbRLock( recNo ) )
   ENDIF

   RETURN ( ::workArea )->( RecLock( recNo ) )

/*
    RecUnLock
*/
METHOD FUNCTION RecUnLock( RecNo ) CLASS TAlias

   LOCAL n

   ::SyncFromRecNo()
   IF RecNo = NIL
      RecNo := ::FRecNo
   ENDIF
   n := AScan( ::FstackLock, {| e| e[ 1 ] = RecNo } )
   IF n > 0 .AND. ::FstackLock[ n, 2 ] > 0
      ::FstackLock[ n, 2 ]--
      RETURN .T.
   ENDIF
   hb_ADel( ::FstackLock, n, .T. )

   RETURN ( ::workArea )->( RecUnLock( RecNo ) )

/*
    Seek
*/
METHOD FUNCTION SEEK( cKey, indexName, softSeek ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( Seek( cKey, indexName, softSeek ) )
   ::SyncFromAlias()

   RETURN Result

/*
    SeekLast
*/
METHOD FUNCTION SeekLast( cKey, indexName, softSeek ) CLASS TAlias

   LOCAL Result

   Result := ( ::workArea )->( SeekLast( cKey, indexName, softSeek ) )
   ::SyncFromAlias()

   RETURN Result

/*
    SetFieldValue
*/
METHOD FUNCTION SetFieldValue( fieldName, value ) CLASS TAlias

   ::SyncFromRecNo()

   RETURN ( ::workArea )->( FieldPut( FieldPos( fieldName ), value ) )

/*
    SetWorkArea
*/
METHOD PROCEDURE SetWorkArea( workArea ) CLASS TAlias

   ::FInstances[ ::FTableName ] := { => }
   ::FInstances[ ::FTableName, "workArea" ] := workArea
   ::FInstances[ ::FTableName, "aliasName" ] := Alias( workArea )

   RETURN

/*
    SyncFromAlias
*/
METHOD PROCEDURE SyncFromAlias CLASS TAlias

   ::FBof  := ( ::workArea )->( Bof() )
   ::FEof  := ( ::workArea )->( Eof() )
   ::FFound := ( ::workArea )->( Found() )
   ::FRecNo := ( ::workArea )->( RecNo() )

   RETURN

/*
    SyncFromRecNo
*/
METHOD PROCEDURE SyncFromRecNo CLASS TAlias

   IF ( ::workArea )->( RecNo() ) != ::FRecNo
      ::dbGoto( ::FRecNo )
   ENDIF

   RETURN

/*
    ENDCLASS TAlias
*/
