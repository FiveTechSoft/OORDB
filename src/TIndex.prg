/*
 * $Id: TIndex.prg 143 2013-05-10 13:45:21Z tfonrouge $
 */

/*
    TIndex
    Teo. Mexico 2007
*/

#include "oordb.ch"
#include "xerror.ch"

/*
    CLASS TIndex
    Teo. Mexico 2006
*/
CLASS TIndex FROM OORDBBASE

   PRIVATE:

   DATA FAutoIncrementKeyField
   DATA FCaseSensitive INIT .T.
   DATA FCustom INIT .F.
   DATA FDescend INIT .F.
   DATA FDbFilter
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
   METHOD Count( bForCondition, bWhileCondition )
   METHOD CustomKeyUpdate
   METHOD DbGoBottom INLINE ::DbGoBottomTop( -1 )
   METHOD DbGoTop INLINE ::DbGoBottomTop( 1 )
   METHOD dbSkip( numRecs )
   METHOD GetKeyVal()
   METHOD ExistKey( keyValue )
   METHOD FillCustomIndex()
   METHOD Get4Seek( blk, keyVal, softSeek )
   METHOD Get4SeekLast( blk, keyVal, softSeek )
   METHOD GetAlias()
   METHOD GetCurrentRecord()
   METHOD IndexExpression()
   METHOD InsideScope()
   METHOD KeyExpression()
   METHOD MasterKeyExpression()

   METHOD ordCondSet( ... ) INLINE ::FTable:ordCondSet( ... )
   METHOD ordCreate( ... ) INLINE ::FTable:ordCreate( ... )
   METHOD ordKeyNo() INLINE ::GetAlias():ordKeyNo()

   METHOD RawGet4Seek( direction, blk, keyVal, softSeek )
   METHOD RawSeek( Value )

   METHOD SetDbFilter( filter ) INLINE ::FDbFilter := filter
   METHOD SetKeyVal( keyVal )

   PROPERTY Bof READ FBof
   PROPERTY DbFilter READ FDbFilter WRITE SetDbFilter
   PROPERTY Eof READ FEof
   PROPERTY Found READ FFound
   PROPERTY IdxAlias READ GetIdxAlias WRITE SetIdxAlias
   PROPERTY IndexType READ FIndexType
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
   PROPERTY KeyField INDEX 3 READ GetField WRITE SetField
   PROPERTY UniqueKeyField INDEX 2 READ GetField WRITE SetField
   PROPERTY Name READ FName
   PROPERTY MasterKeyField INDEX 0 READ GetField WRITE SetField
   PROPERTY MasterKeyVal READ GetMasterKeyVal
   PROPERTY RightJustified READ FRightJustified WRITE SetRightJustified
   PROPERTY Table READ FTable
   PROPERTY TagName READ FTagName
   PROPERTY UNIQUE READ GetUnique

ENDCLASS

/*
    New
    Teo. Mexico 2006
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

   IF "PRIMARY" = indexType
      ::FTable:SetPrimaryIndexList( curClass, name )
      ::FTable:SetPrimaryIndex( Self )
   ENDIF

   RETURN Self

/*
    __Seek
    Teo. Mexico 2007
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
    Teo. Mexico 2008
*/
METHOD AddIndex( cMasterKeyField, ai, un, cKeyField, ForKey, cs, de, acceptEmptyUnique, useIndex, temporary, rightJust /*, cu*/ )

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
    Teo. Mexico 2013
*/
METHOD FUNCTION Count( bForCondition, bWhileCondition ) CLASS TIndex

   LOCAL nCount := 0

   ::FTable:dbEval( {|| ++nCount }, bForCondition, bWhileCondition, Self )

   RETURN nCount

/*
    CustomKeyExpValue
    Teo. Mexico 2012
*/
METHOD FUNCTION CustomKeyExpValue() CLASS TIndex

   IF ::MasterKeyField = NIL
      RETURN ::KeyVal
   ENDIF

   RETURN ::MasterKeyField:KeyVal + ::KeyVal

/*
    CustomKeyUpdate
    Teo. Mexico 2012
*/
METHOD PROCEDURE CustomKeyUpdate CLASS TIndex

   IF ::FCustom
      WHILE ::FTable:Alias:ordKeyDel( ::FTagName ) ; ENDDO
      ::FTable:Alias:ordKeyAdd( ::FTagName, , ::CustomKeyExpValue() )
   ENDIF

   RETURN

/*
    DbGoBottomTop
    Teo. Mexico 2008
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

   ::GetCurrentRecord()

   IF !::FTable:FilterEval( Self ) .AND. !::FTable:SkipFilter( n, Self )
      ::FTable:dbGoto( 0 )
   ENDIF

   RETURN !::FEof

/*
    DbSkip
    Teo. Mexico 2007
*/
METHOD FUNCTION dbSkip( numRecs ) CLASS TIndex

   LOCAL table
   LOCAL result

   IF ::associatedTable = NIL
      table := ::FTable
   ELSE
      table := ::associatedTable
   ENDIF

   IF ::FDbFilter = NIL .AND. !table:HasFilter
      result := ::GetAlias():dbSkip( numRecs, ::FTagName )
      ::GetCurrentRecord()
      RETURN result
   ENDIF

   RETURN ::FTable:SkipFilter( numRecs, Self )

/*
    ExistKey
    Teo. Mexico 2007
*/
METHOD FUNCTION ExistKey( keyValue ) CLASS TIndex
   RETURN ::GetAlias():ExistKey( ::MasterKeyVal + keyValue, ::FTagName, ;
      {||
IF ::IdxAlias = NIL
RETURN ::FTable:RecNo
ENDIF

   RETURN ( ::IdxAlias:workArea )->RecNo
} )

/*
    FillCustomIndex
    Teo. Mexico 2012
*/
METHOD PROCEDURE FillCustomIndex() CLASS TIndex

   LOCAL baseKeyIndex := ::FTable:BaseKeyIndex
   LOCAL resetToMasterSourceFields
   LOCAL dbFilter

   IF baseKeyIndex != NIL
      resetToMasterSourceFields := ::FResetToMasterSourceFields
      ::FResetToMasterSourceFields := .F.
      dbFilter := ::FTable:DbFilter
      baseKeyIndex:dbGoTop()
      /* TODO: Loop to remove all the custom keys ? */
      WHILE baseKeyIndex:InsideScope()
         IF ::FTable:FilterEval( Self )
            ::CustomKeyUpdate()
         ELSE
            WHILE ::FTable:Alias:ordKeyDel( ::FTagName ) ; ENDDO
         ENDIF
         baseKeyIndex:dbSkip()
      ENDDO
      ::FTable:SetDbFilter( dbFilter )
      ::FResetToMasterSourceFields := resetToMasterSourceFields
   ENDIF

   RETURN

/*
    Get4Seek
    Teo. Mexico 2009
*/
METHOD FUNCTION Get4Seek( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 1, blk, keyVal, softSeek )

/*
    Get4SeekLast
    Teo. Mexico 2009
*/
METHOD FUNCTION Get4SeekLast( blk, keyVal, softSeek ) CLASS TIndex
   RETURN ::RawGet4Seek( 0, blk, keyVal, softSeek )

/*
    GetAlias
    Teo. Mexico 2010
*/
METHOD FUNCTION GetAlias() CLASS TIndex

   IF ::IdxAlias = NIL
      RETURN ::FTable:Alias
   ENDIF

   RETURN ::IdxAlias

/*
    GetCurrentRecord
    Teo. Mexico 2010
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
    Teo. Mexico 2006
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
    Teo. Mexico 2010
*/
METHOD FUNCTION GetIdxAlias() CLASS TIndex

   IF ::temporary
      RETURN ::FTable:aliasTmp
   ENDIF

   RETURN ::FTable:aliasIdx

/*
    GetKeyVal
    Teo. Mexico 2009
*/
METHOD FUNCTION GetKeyVal() CLASS TIndex

   IF ::FKeyField == NIL
      RETURN ""
   ENDIF

   RETURN ::FKeyField:GetKeyVal

/*
    GetMasterKeyVal
    Teo. Mexico 2009
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
         RETURN keyField:GetKeyVal( keyField:DefaultValue )
      ELSE
         IF keyField:FieldType = ftObject .AND. ( field := ::FTable:FindMasterSourceField( keyField ) ) != NIL
            RETURN field:KeyVal
         ELSE
            RETURN keyField:GetKeyVal( keyField:DefaultValue )
         ENDIF
      ENDIF
   ENDIF

   RETURN keyVal

/*
    IndexExpression
    Teo. Mexico 2010
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
    Teo. Mexico 2008
*/
METHOD FUNCTION InsideScope() CLASS TIndex

   LOCAL masterKeyVal
   LOCAL scopeVal
   LOCAL keyValue

   IF ::FTable:Alias:KeyVal( ::FTagName ) = NIL
      RETURN .F.
   ENDIF

   keyValue := ::GetAlias():KeyVal( ::FTagName )

   IF keyValue == NIL
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
    Teo. Mexico 2010
*/
METHOD FUNCTION KeyExpression() CLASS TIndex

   IF ::FKeyField != NIL
      RETURN ::FKeyField:IndexExpression
   ENDIF

   RETURN ""

/*
    MasterKeyExpression
    Teo. Mexico 2011
*/
METHOD FUNCTION MasterKeyExpression() CLASS TIndex

   IF ::FMasterKeyField != NIL
      RETURN ::FMasterKeyField:IndexExpression( NIL, .T. )
   ENDIF

   RETURN ""

/*
    RawGet4Seek
    Teo. Mexico 2009
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
    Teo. Mexico 2008
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
    Teo. Mexico 2006
*/
METHOD PROCEDURE SetCustom( Custom ) CLASS TIndex

   ::FCustom := Custom

   ::FTable:Alias:ordCustom( ::FTagName, , Custom )

   RETURN

/*
    SetCustomIndexExpression
    Teo. Mexico 2012
*/
METHOD PROCEDURE SetCustomIndexExpression( customIndexExpression ) CLASS TIndex

   ::FCustomIndexExpression := customIndexExpression
   ::FCustom := .T.
   ::FTable:AddCustomIndex( Self )

   RETURN

/*
    SetField
    Teo. Mexico 2006
*/
METHOD PROCEDURE SetField( nIndex, XField ) CLASS TIndex

   LOCAL AField
   LOCAL fld
   LOCAL fieldBlock
   LOCAL customExpression

   SWITCH ValType( XField )
   CASE 'C'
      AField := ::FTable:FieldByName( XField )
      IF ":" $ XField
         customExpression := XField
      ELSEIF AField != NIL .AND. AField:FieldExpression != NIL .AND. ":" $ AField:FieldExpression
         customExpression := AField:FieldExpression
      ENDIF
      IF customExpression != NIL
         fieldBlock := ::FTable:BuildFieldBlockFromFieldExpression( customExpression, "Value" )
         IF fieldBlock = NIL
            RAISE ERROR "Declared Index (COMPOUND) Field '" + XField + "' doesn't exist..."
            RETURN
         ENDIF
         AField := TStringField():New( ::FTable, ::FTableBaseClass )
         AField:Name := StrTran( XField, ":", "_" )
         AField:FieldMethod := fieldBlock
         AField:Published := .F.
         ::SetCustomIndexExpression( XField )
      ELSEIF AField != NIL .AND. AField:Calculated .AND. AField:IndexExpression = NIL
         ::SetCustomIndexExpression( XField )
      ELSEIF AField = NIL
         RAISE ERROR "Declared Index Field '" + XField + "' doesn't exist..."
         RETURN
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
      /* Array of fields are stored in a TStringField (for the index nature) */
      AField := TStringField():New( ::FTable, ::FTableBaseClass )
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
      IF AField:IsDerivedFrom( "TObjectField" )
         RAISE ERROR "TObjectField's are not Allowed as AutoIncrement Index Key..."
      ENDIF
      AField:AutoIncrementKeyIndex := Self
      ::FAutoIncrementKeyField := AField
   CASE 2  /* UniqueKeyField */
      AAdd( AField:UniqueKeyIndexList, Self )
      ::FUniqueKeyField := AField
   CASE 3  /* KeyField */
      IF AField:IsDerivedFrom( "TStringField" ) .AND. Len( AField ) = 0
         RAISE ERROR ::FTable:ClassName + ": Master key field <" + AField:Name + "> needs a size > zero..."
      ENDIF
      AField:AddKeyIndex( Self )
      ::FKeyField := AField
      IF ::FIndexType == "PRIMARY" .AND. ::FTable:BaseKeyIndex = NIL
         ::FTable:SetBaseKeyIndex( Self )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN

/*
    SetIdxAlias
    Teo. Mexico 2010
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
    Teo. Mexico 2010
*/
METHOD FUNCTION SetKeyVal( keyVal ) CLASS TIndex

   ::FKeyField:SetKeyVal( keyVal )

   RETURN Self

/*
    SetScope
    Teo. Mexico 2008
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
    Teo. Mexico 2008
*/
METHOD FUNCTION SetScopeBottom( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeBottom

   ::FScopeBottom := value

   RETURN oldValue

/*
    SetScopeTop
    Teo. Mexico 2008
*/
METHOD FUNCTION SetScopeTop( value ) CLASS TIndex

   LOCAL oldValue := ::FScopeTop

   ::FScopeTop := value

   RETURN oldValue

/*
    End Class TIndex
*/
