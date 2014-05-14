/*
 * $Id: TIndex.prg 143 2013-05-10 13:45:21Z tfonrouge $
 */

/*
    TIndex
*/

#include "oordb.ch"
#include "xerror.ch"

/*
    CLASS TIndex
*/
CLASS TIndex FROM OORDBBASE

   PRIVATE:

   DATA FAutoIncrementKeyField
   DATA FCaseSensitive INIT .T.
   DATA FCustom INIT .F.
   DATA FDescend INIT .F.
   DATA FDbFilter
   DATA FDbFilterStack INIT {}
   DATA FForKey
   DATA FKeyField
   DATA FName
   DATA FMasterKeyField
   DATA FRightJustified INIT .F.
   DATA FScopeBottom
   DATA FScopeTop
   DATA FTable
   DATA FTagName
   DATA FUniqueKeyField
   METHOD DbGoBottomTop( n )
   METHOD GetArrayKeyFields INLINE ::KeyField:FieldMethod
   METHOD GetAutoIncrement INLINE ::FAutoIncrementKeyField != NIL
   METHOD GetField
   METHOD GetIdxAlias()
   METHOD GetMasterKeyVal( keyField )
   METHOD GetScope INLINE iif( ::FScopeBottom == NIL .AND. ::FScopeTop == NIL, NIL, { ::FScopeTop, ::FScopeBottom } )
   METHOD GetScopeBottom INLINE iif( !Empty( ::FScopeBottom ), ::FScopeBottom, "" )
   METHOD GetScopeTop INLINE iif( !Empty( ::FScopeTop ), ::FScopeTop, "" )
   METHOD GetUnique INLINE ::FUniqueKeyField != NIL
   METHOD SetCaseSensitive( CaseSensitive ) INLINE ::FCaseSensitive := CaseSensitive
   METHOD SetCustom( Custom )
   METHOD SetDescend( Descend ) INLINE ::FDescend := Descend
   METHOD SetField( nIndex, XField )
   METHOD SetForKey( ForKey ) INLINE ::FForKey := ForKey
   METHOD SetIdxAlias( alias )
   METHOD SetRightJustified( rightJust ) INLINE ::FRightJustified := rightJust
   METHOD SetScope( value )
   METHOD SetScopeBottom( value )
   METHOD SetScopeTop( value )

   PROTECTED:

   DATA FBof INIT .T.
   DATA FCustomIndexExpression
   DATA FEof INIT .T.
   DATA FFound INIT .F.
   DATA FIndexType
   DATA FKeyFlags
   DATA FRecNo
   DATA FResetToMasterSourceFields INIT .T.
   DATA FTableBaseClass
   METHOD CustomKeyExpValue()
   METHOD SetCustomIndexExpression( customIndexExpression )

   PUBLIC:

   DATA associatedTable
   DATA FIdxAlias INIT .F.
   DATA getRecNoBlock
   DATA setRecNoBlock
   DATA temporary INIT .F.
   DATA useIndex

   DATA WarnMsg

   METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CONSTRUCTOR

   METHOD __Seek( direction, keyValue, lSoftSeek )
   METHOD AddIndex
   METHOD COUNT( bForCondition, bWhileCondition )
   METHOD CustomKeyUpdate
   METHOD DbFilterPush( ignoreMasterKey )
   METHOD DbFilterPull()
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs )
   METHOD GetKeyVal( keyVal )
   METHOD ExistKey( keyValue, recNo )
   METHOD FillCustomIndex()
   METHOD Get4Seek( blk, keyVal, softSeek )
   METHOD Get4SeekLast( blk, keyVal, softSeek )
   METHOD GetAlias()
   METHOD GetCurrentRecord()
   METHOD HasFilter() INLINE ::FDbFilter != NIL
   METHOD IndexExpression()
   METHOD InsideScope( ignoreFilters )
   METHOD KeyExpression()
   METHOD MasterKeyExpression()

   METHOD ordCondSet( ... ) INLINE ::FTable:ordCondSet( ... )
   METHOD ordCreate( ... ) INLINE ::FTable:ordCreate( ... )
   METHOD ordKeyNo() INLINE ::GetAlias():ordKeyNo()

   METHOD RawGet4Seek( direction, blk, keyVal, softSeek )
   METHOD RawSeek( Value )

   METHOD SetDbFilter( filter ) INLINE ::FDbFilter := filter
   METHOD SetKeyVal( keyVal, lSoftSeek )

   PROPERTY Bof READ FBof
   PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
   PROPERTY Eof READ FEof
   PROPERTY Found READ FFound
   PROPERTY IdxAlias READ GetIdxAlias WRITE SetIdxAlias
   PROPERTY IndexType READ FIndexType
   PROPERTY KeyFlags READ FKeyFlags
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY RecNo READ FRecNo
   PROPERTY Scope READ GetScope WRITE SetScope
   PROPERTY ScopeBottom READ GetScopeBottom WRITE SetScopeBottom
   PROPERTY ScopeTop READ GetScopeTop WRITE SetScopeTop

   METHOD SEEK( keyValue, lSoftSeek ) INLINE ::__Seek( 0, keyValue, lSoftSeek )
   METHOD SeekLast( keyValue, lSoftSeek ) INLINE ::__Seek( 1, keyValue, lSoftSeek )
   METHOD SetBof( bof ) INLINE ::FBof := bof
   METHOD SetEof( eof ) INLINE ::FEof := eof
   METHOD SetFound( found ) INLINE ::FFound := found
   METHOD SetRecNo( recNo ) INLINE ::FRecNo := recNo
   PUBLISHED:
   PROPERTY AutoIncrement READ GetAutoIncrement
   PROPERTY AutoIncrementKeyField INDEX 1 READ GetField WRITE SetField
   PROPERTY CaseSensitive READ FCaseSensitive WRITE SetCaseSensitive
   PROPERTY Custom READ FCustom
   PROPERTY CustomIndexExpression READ FCustomIndexExpression
   PROPERTY Descend READ FDescend WRITE SetDescend
   PROPERTY ForKey READ FForKey WRITE SetForKey
   PROPERTY IsPrimaryIndex
   PROPERTY KeyField INDEX 3 READ GetField WRITE SetField
   PROPERTY UniqueKeyField INDEX 2 READ GetField WRITE SetField
   PROPERTY Name READ FName
   PROPERTY MasterKeyField INDEX 0 READ GetField WRITE SetField
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY RightJustified READ FRightJustified WRITE SetRightJustified
   PROPERTY Table READ FTable
   PROPERTY TableBaseClass READ FTableBaseClass
   PROPERTY TagName READ FTagName
   PROPERTY UNIQUE READ GetUnique

ENDCLASS

/*
    New
*/
METHOD New( Table, tagName, name, indexType, curClass, warnMsg ) CLASS TIndex

   ::FTable := Table
   ::FIndexType := indexType
   ::WarnMsg := warnMsg

   IF Len( tagName ) > 10
      RAISE ERROR "TagName '" + tagName + "' exceeds lenght of 10..."
   ENDIF

   ::FTagName := tagName

   IF Empty( name )
      name := tagName
   ENDIF

   ::FName := name

   IF curClass = NIL
      curClass := ::FTable:ClassName()
   ENDIF

   ::FTableBaseClass := curClass

   IF !hb_HHasKey( ::FTable:IndexList, curClass )
      ::FTable:IndexList[ curClass ] := hb_HSetOrder( hb_HSetCaseMatch( { => }, .F. ), .T. )
   ENDIF

   ::FTable:IndexList[ curClass, name ] := Self

   ::FIsPrimaryIndex := indexType = "PRIMARY"

   IF ::FIsPrimaryIndex
      ::FTable:SetPrimaryIndexList( curClass, name )
      ::FTable:SetPrimaryIndex( Self )
   ENDIF

   RETURN Self

/*
    __Seek
*/
METHOD FUNCTION __Seek( direction, keyValue, lSoftSeek ) CLASS TIndex

   LOCAL ALIAS

   alias := ::GetAlias()

   IF AScan( { dsEdit, dsInsert }, ::FTable:State ) > 0
      ::FTable:Post()
   ENDIF

   keyValue := ::KeyField:GetKeyVal( keyValue )

   IF direction = 0
      alias:Seek( ::MasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ELSE
      alias:SeekLast( ::MasterKeyVal + keyValue, ::FTagName, lSoftSeek )
   ENDIF

   ::GetCurrentRecord()

   RETURN ::FFound

/*
    AddIndex
*/
METHOD AddIndex( cMasterKeyField, ai, un, cKeyField, keyFlags, ForKey, cs, de, acceptEmptyUnique, useIndex, temporary, rightJust /*, cu*/ )

   ::FKeyFlags := keyFlags

   ::MasterKeyField := cMasterKeyField

   /* Check if needs to add the primary index key */
   IF ::FTable:PrimaryIndex == Self
      IF ai == .T.
         ::AutoIncrementKeyField := cKeyField
      ELSE
         ::UniqueKeyField := cKeyField
      ENDIF
   ELSE
      DO CASE
         /* Check if index key is AutoIncrement */
      CASE ai == .T.
         ::AutoIncrementKeyField := cKeyField
         /* Check if index key is Unique */
      CASE un == .T.
         ::UniqueKeyField := cKeyField
         /* Check if index key is a simple index */
      OTHERWISE
         ::KeyField := cKeyField
      ENDCASE
   ENDIF

   IF acceptEmptyUnique != NIL
      ::UniqueKeyField:AcceptEmptyUnique := acceptEmptyUnique
   ENDIF

   ::ForKey := ForKey
   ::CaseSensitive := iif( HB_ISNIL( cs ), .F., cs )
   ::RightJustified := rightJust == .T.
   ::Descend := iif( HB_ISNIL( de ), .F., de )
   ::useIndex := useIndex
   ::temporary := temporary == .T.
   // ::Custom := iif( HB_ISNIL( cu ), .F. , cu )

   RETURN Self

/*
    Count
*/
METHOD FUNCTION COUNT( bForCondition, bWhileCondition ) CLASS TIndex

   LOCAL nCount := 0

   ::FTable:dbEval( {|| ++nCount }, bForCondition, bWhileCondition, Self )

   RETURN nCount

/*
    CustomKeyExpValue
*/
METHOD FUNCTION CustomKeyExpValue() CLASS TIndex

   IF ::MasterKeyField = NIL
      RETURN ::KeyVal
   ENDIF

   RETURN ::MasterKeyField:KeyVal + ::KeyVal

/*
    CustomKeyUpdate
*/
METHOD PROCEDURE CustomKeyUpdate CLASS TIndex

   IF ::FCustom
      WHILE ::FTable:Alias:ordKeyDel( ::FTagName ) ; ENDDO
      ::FTable:Alias:ordKeyAdd( ::FTagName, , ::CustomKeyExpValue() )
   ENDIF

   RETURN

/*
    DbFilterPull
*/
METHOD PROCEDURE DbFilterPull() CLASS TIndex

   ::FDbFilter := ATail( ::FDbFilterStack )
   hb_ADel( ::FDbFilterStack, Len( ::FDbFilterStack ), .T. )
   ::FTable:DbFilterPull()

   RETURN

/*
    DbFilterPush
*/
METHOD PROCEDURE DbFilterPush( ignoreMasterKey ) CLASS TIndex

   AAdd( ::FDbFilterStack, ::FDbFilter )
   ::FDbFilter := NIL
   ::FTable:DbFilterPush( ignoreMasterKey )

   RETURN

/*
    DbGoBottomTop
*/
METHOD FUNCTION DbGoBottomTop( n ) CLASS TIndex

   LOCAL masterKeyVal := ::MasterKeyVal
   LOCAL ALIAS

   alias := ::GetAlias()

   IF n = 1
      IF ::GetScopeTop() == ::GetScopeBottom()
         alias:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName )
      ELSE
         alias:Seek( masterKeyVal + ::GetScopeTop(), ::FTagName, .T. )
      ENDIF
   ELSE
      IF ::GetScopeTop() == ::GetScopeBottom()
         alias:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName )
      ELSE
         alias:SeekLast( masterKeyVal + ::GetScopeBottom(), ::FTagName, .T. )
      ENDIF
   ENDIF

   IF ::HasFilter() .OR. ::FTable:HasFilter()
      ::DbFilterPush()
      ::GetCurrentRecord()
      ::DbFilterPull()
      IF ::FEof() .OR. ( !::FTable:FilterEval( Self ) .AND. !::FTable:SkipFilter( n, Self ) )
         ::FTable:dbGoto( 0 )
         RETURN .F.
      ENDIF
   ENDIF

   RETURN ::GetCurrentRecord()

/*
    DbSkip
*/
METHOD FUNCTION dbSkip( numRecs ) CLASS TIndex

   LOCAL table
   LOCAL result

   IF ::associatedTable = NIL
      table := ::FTable
   ELSE
      table := ::associatedTable
   ENDIF

   IF !::HasFilter() .AND. !table:HasFilter()
      result := ::GetAlias():dbSkip( numRecs, ::FTagName ) /* because on Bof returns .F. */
      ::GetCurrentRecord()
      RETURN result .AND. ::InsideScope()
   ENDIF

   RETURN ::FTable:SkipFilter( numRecs, Self )

/*
    ExistKey
*/
METHOD FUNCTION ExistKey( keyValue, recNo ) CLASS TIndex
   RETURN ::GetAlias():ExistKey( ::MasterKeyVal + keyValue, ::FTagName, recNo )

/*
    FillCustomIndex
*/
METHOD PROCEDURE FillCustomIndex() CLASS TIndex

   LOCAL baseKeyIndex := ::FTable:BaseKeyIndex
   LOCAL resetToMasterSourceFields

   IF baseKeyIndex != NIL
      ::FTable:StatePush()
      resetToMasterSourceFields := ::FResetToMasterSourceFields
      ::FResetToMasterSourceFields := .F.
      ::FTable:DbFilterPush( .T. )
      baseKeyIndex:dbGoTop()
      WHILE !baseKeyIndex:Eof()
         ::CustomKeyUpdate()
         baseKeyIndex:dbSkip()
      ENDDO
      ::FTable:DbFilterPull()
      ::FResetToMasterSourceFields := resetToMasterSourceFields
      ::FTable:StatePull()
   ENDIF

   RETURN

/*
    Get4Seek
*/
METHOD FUNCTION Get4Seek( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 1, blk, keyVal, softSeek )

/*
    Get4SeekLast
*/
METHOD FUNCTION Get4SeekLast( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 0, blk, keyVal, softSeek )

/*
    GetAlias
*/
METHOD FUNCTION GetAlias() CLASS TIndex

   IF ::IdxAlias = NIL
      RETURN ::FTable:Alias
   ENDIF

   RETURN ::IdxAlias

/*
    GetCurrentRecord
*/
METHOD FUNCTION GetCurrentRecord() CLASS TIndex

   LOCAL result
   LOCAL index := ::FTable:Index

   ::FTable:Index := Self
   result := ::FTable:GetCurrentRecord( ::GetIdxAlias() )
   ::FTable:Index := index

   IF ::associatedTable != NIL
      ::associatedTable:ExternalIndexList[ ::ObjectH ] := NIL
      result := ::getRecNoBlock:Eval( ::associatedTable, ::FTable )
      ::associatedTable:ExternalIndexList[ ::ObjectH ] := Self
   ENDIF

   RETURN result

/*
    GetField
*/
METHOD FUNCTION GetField( nIndex ) CLASS TIndex

   LOCAL AField

   SWITCH nIndex
   CASE 0
      AField := ::FMasterKeyField
      EXIT
   CASE 1
      AField := ::FAutoIncrementKeyField
      EXIT
   CASE 2
      AField := ::FUniqueKeyField
      EXIT
   CASE 3
      AField := ::FKeyField
      EXIT
   ENDSWITCH

   RETURN AField

/*
    GetIdxAlias
*/
METHOD FUNCTION GetIdxAlias() CLASS TIndex

   IF ::temporary
      RETURN ::FTable:aliasTmp
   ENDIF

   RETURN ::FTable:aliasIdx

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TIndex

   IF ::FKeyField == NIL
      RETURN ""
   ENDIF

   RETURN ::FKeyField:GetKeyVal( keyVal, ::FKeyFlags )

/*
    GetMasterKeyVal
*/
METHOD FUNCTION GetMasterKeyVal( keyField ) CLASS TIndex

   LOCAL itm
   LOCAL FIELD
   LOCAL keyVal := ""

   IF keyField = NIL
      keyField := ::FMasterKeyField
   ENDIF

   IF keyField == NIL
      RETURN keyVal // ::FTable:MasterKeyString
   ENDIF

   IF keyField:FieldMethodType = "A"
      FOR EACH itm IN keyField:FieldArrayIndex
         keyVal += ::GetMasterKeyVal( ::FTable:FieldList[ itm ] )
      NEXT
   ELSE
      IF ::FTable:MasterSource = NIL
         RETURN keyField:GetKeyVal( keyField:DefaultValue, ::FKeyFlags )
      ELSE
         IF keyField:DefaultValue = NIL .AND. ( field := ::FTable:FindMasterSourceField( keyField ) ) != NIL
            RETURN field:GetKeyVal( NIL, ::FKeyFlags )
         ELSE
            RETURN keyField:GetKeyVal( keyField:DefaultValue, ::FKeyFlags )
         ENDIF
      ENDIF
   ENDIF

   RETURN keyVal

/*
    IndexExpression
*/
METHOD FUNCTION IndexExpression() CLASS TIndex

   LOCAL exp
   LOCAL keyExp

   IF ::FCustomIndexExpression = NIL
      exp := ::MasterKeyExpression
      keyExp := ::KeyExpression
      IF !Empty( keyExp )
         exp += iif( Len( exp ) = 0, "", "+" ) + keyExp
      ENDIF
   ELSE
      exp := ::FCustomIndexExpression
   ENDIF

   RETURN exp

/*
    InsideScope
*/
METHOD FUNCTION InsideScope( ignoreFilters ) CLASS TIndex

   LOCAL masterKeyVal
   LOCAL scopeVal
   LOCAL keyValue

   IF ::FTable:Alias:KeyVal( ::FTagName ) = NIL
      RETURN .F.
   ENDIF

   keyValue := ::GetAlias():KeyVal( ::FTagName )

   IF keyValue == NIL .OR. ( !ignoreFilters == .T. .AND. !::FTable:FilterEval( Self ) )
      RETURN .F.
   ENDIF

   masterKeyVal := ::MasterKeyVal

   scopeVal := ::GetScope()

   IF scopeVal == NIL
      RETURN masterKeyVal == "" .OR. keyValue = masterKeyVal
   ENDIF

   RETURN keyValue >= ( masterKeyVal + ::GetScopeTop() ) .AND. ;
      keyValue <= ( masterKeyVal + ::GetScopeBottom() )

/*
    KeyExpression
*/
METHOD FUNCTION KeyExpression() CLASS TIndex

   IF ::FKeyField != NIL
      RETURN ::FKeyField:IndexExpression
   ENDIF

   RETURN ""

/*
    MasterKeyExpression
*/
METHOD FUNCTION MasterKeyExpression() CLASS TIndex

   IF ::FMasterKeyField != NIL
      RETURN ::FMasterKeyField:IndexExpression( NIL, .T. )
   ENDIF

   RETURN ""

/*
    RawGet4Seek
*/
METHOD FUNCTION RawGet4Seek( direction, blk, keyVal, softSeek ) CLASS TIndex

   IF keyVal = NIL
      keyVal := ::MasterKeyVal
   ELSE
      keyVal := ::MasterKeyVal + keyVal
   ENDIF

   RETURN ::GetAlias():RawGet4Seek( direction, blk, keyVal, ::FTagName, softSeek )

/*
    RawSeek
*/
METHOD FUNCTION RawSeek( Value ) CLASS TIndex

   IF AScan( { dsEdit, dsInsert }, ::FTable:State ) > 0
      ::FTable:Post()
   ENDIF

   ::GetAlias():Seek( Value, ::FTagName )

   ::GetCurrentRecord()

   RETURN ::FTable:Found()

/*
    SetCustom
*/
METHOD PROCEDURE SetCustom( Custom ) CLASS TIndex

   ::FCustom := Custom

   ::FTable:Alias:ordCustom( ::FTagName, , Custom )

   RETURN

/*
    SetCustomIndexExpression
*/
METHOD PROCEDURE SetCustomIndexExpression( customIndexExpression ) CLASS TIndex

   ::FCustomIndexExpression := customIndexExpression
   ::FCustom := .T.
   ::FTable:AddCustomIndex( Self )

   RETURN

/*
    SetField
*/
METHOD PROCEDURE SetField( nIndex, XField ) CLASS TIndex

   LOCAL AField
   LOCAL fld
   LOCAL fieldBlock

   SWITCH ValType( XField )
   CASE 'C'
      AField := ::FTable:FieldByName( XField )
      IF AField = NIL .AND. ":" $ XField

          fieldBlock := ::FTable:BuildFieldBlockFromFieldExpression( XField, "Value", @fld )

         IF fieldBlock = NIL
            RAISE ERROR "Error building (COMPOUND) Index Field '" + XField + "' ..."
            RETURN
         ENDIF

         AField := __DynSN2Sym( fld:ClassName ):Exec():New( ::FTable, ::FTableBaseClass )
         AField:Name := StrTran( XField, ":", "_" )
         AField:FieldMethod := fieldBlock
         AField:Published := .F.

      ENDIF
      IF AField = NIL
         RAISE ERROR "Declared Index Field '" + XField + "' doesn't exist..."
         RETURN
      ENDIF
      IF AField:Calculated
         ::SetCustomIndexExpression( XField )
      ENDIF
      EXIT
   CASE 'O'
      IF !XField:IsDerivedFrom( "TField" )
         ::Error_Not_TField_Type_Object()
         RETURN
      ENDIF
      AField := XField
      EXIT
   CASE 'A'
      /* Array of fields are stored in a TFieldString (for the index nature) */
      AField := TFieldString():New( ::FTable, ::FTableBaseClass )
      AField:FieldMethod := XField
      AField:Published := .F.
      fld := ::FTable:FieldByName( AField:Name )
      IF fld = NIL
         AField:AddFieldMessage()
      ELSE
         AField := fld
      ENDIF
      EXIT
   CASE 'U'
      AField := NIL
      EXIT
   OTHERWISE
      SHOW WARN "! : Not a Valid Field Identifier..."
      RETURN
   ENDSWITCH

   /* Assign PrimaryKeyComponent value */
   IF ::FTable:PrimaryIndex == Self /* check if index is the Primary index */
      IF !HB_ISNIL( AField )
         AField:PrimaryKeyComponent := .T.
         /* Assign MasterField value to the TTable object field */
         IF nIndex = 0
            AField:IsMasterFieldComponent := .T.
         ENDIF
      ENDIF
   ELSE
      // AField:SecondaryKeyComponent := .T.
   ENDIF

   IF AField == NIL
      RETURN
   ENDIF

   SWITCH nIndex
   CASE 0  /* MasterKeyField */
      ::FMasterKeyField := AField
      EXIT
   CASE 1  /* AutoIncrementKeyField */
      IF AField:FieldMethodType = 'A'
         RAISE ERROR "Array of Fields are not Allowed as AutoIncrement Index Key..."
      ENDIF
      IF AField:IsDerivedFrom( "TFieldObject" )
         RAISE ERROR "TFieldObject's are not Allowed as AutoIncrement Index Key..."
      ENDIF
      AField:AutoIncrementKeyIndex := Self
      ::FAutoIncrementKeyField := AField
   CASE 2  /* UniqueKeyField */
      AAdd( AField:UniqueKeyIndexList, Self )
      ::FUniqueKeyField := AField
   CASE 3  /* KeyField */
      IF AField:IsDerivedFrom( "TFieldString" ) .AND. Len( AField ) = 0
         RAISE ERROR ::FTable:ClassName + ": Master key field <" + AField:Name + "> needs a size > zero..."
      ENDIF
      AField:AddKeyIndex( Self )
      ::FKeyField := AField
      IF ::FIsPrimaryIndex .AND. ::FTable:BaseKeyIndex = NIL
         ::FTable:SetBaseKeyIndex( Self )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN

/*
    SetIdxAlias
*/
METHOD PROCEDURE SetIdxAlias( alias ) CLASS TIndex

   IF ::temporary
      ::FTable:aliasTmp := alias
   ELSE
      ::FTable:aliasIdx := alias
   ENDIF

   RETURN

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal, lSoftSeek ) CLASS TIndex

   RETURN ::FKeyField:SetKeyVal( keyVal, lSoftSeek )

/*
    SetScope
*/
METHOD FUNCTION SetScope( value ) CLASS TIndex

   LOCAL oldValue := { ::FScopeTop, ::FScopeBottom }

   IF ValType( value ) = "A" // scope by field
      ::FScopeTop := value[ 1 ]
      ::FScopeBottom := value[ 2 ]
   ELSE
      ::FScopeTop := value
      ::FScopeBottom := value
   ENDIF

   RETURN oldValue

/*
    SetScopeBottom
*/
METHOD FUNCTION SetScopeBottom( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeBottom

   ::FScopeBottom := value

   RETURN oldValue

/*
    SetScopeTop
*/
METHOD FUNCTION SetScopeTop( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeTop

   ::FScopeTop := value

   RETURN oldValue

/*
    End Class TIndex
*/
