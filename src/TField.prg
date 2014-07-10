/*
 *
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TODO: Check for a correct validation for FieldExpression, it can contain any valid
                Harbour statement/formula, and loose checking is done on SetFieldMethod
*/

/*
    TField
*/
CLASS TField FROM OORDBBASE

   PRIVATE:

   DATA FEnabled INIT .F.
   DATA FAutoIncrementKeyIndex
   DATA FDescription INIT ""
   DATA FFieldCodeBlock         // Code Block
   DATA FFieldWriteBlock         // Code Block to do WRITE
   DATA FGroup              // A Text label for grouping
   DATA FIsMasterFieldComponent INIT .F. // Field is a MasterField component
   DATA FPrimaryKeyComponent INIT .F.  // Field is included in a Array of fields for a Primary Index Key
   DATA FPublished INIT .T.       // Logical: Appears in user field selection
   DATA FReadOnly INIT .F.
   DATA FRequired INIT .F.
   /* TODO: remove or fix validations, i.e. when reusing a primary index field */
   DATA FReUseField INIT .F.
   DATA FReUseFieldIndex
   DATA FUniqueKeyIndexList INIT {}

   METHOD GetAutoIncrement INLINE ::FAutoIncrementKeyIndex != NIL
   METHOD GetAutoIncrementValue()
   METHOD GetFieldMethod
   METHOD GetIsPrimaryKeyField INLINE ::Table:KeyField == Self
   METHOD GetReadOnly INLINE ::FReadOnly
   METHOD GetUnique INLINE !Empty( ::FUniqueKeyIndexList )
   METHOD SetAutoIncrementKeyIndex( index ) INLINE ::FAutoIncrementKeyIndex := index
   METHOD SetDescription( Description ) INLINE ::FDescription := Description
   METHOD SetGroup( Group ) INLINE ::FGroup := Group
   METHOD SetIsMasterFieldComponent( IsMasterFieldComponent )
   METHOD SetName( name )
   METHOD SetPrimaryKeyComponent( PrimaryKeyComponent )
   METHOD SetPublished( Published ) INLINE ::FPublished := Published
   METHOD SetReadOnly( ReadOnly ) INLINE ::FReadOnly := ReadOnly
   METHOD SetUsingField( usingField )

   PROTECTED:

   DATA FBuffer
   DATA FcalcResult
   DATA FCalculated INIT .F.
   DATA FChanged INIT .F.
   DATA FCheckEditable INIT .F.  // intended to be used by TUI/GUI
   DATA FDefaultValue
   DATA FDBS_DEC INIT 0
   DATA FDBS_LEN
   DATA FDBS_NAME
   DATA FDBS_TYPE
   DATA FdefaultValueList
   DATA FEditable
   DATA FEvtOnBeforeChange
   DATA FFieldArrayIndex        // Array of TField's indexes in FieldList
   DATA FFieldExpression         // Literal Field expression on the Database
   DATA FFieldMethodType
   DATA FFieldReadBlock        // Code Block to do READ
   DATA FFieldType     INIT ftBase
   DATA FIndexExpression
   DATA FIndexKeyList  INIT {}
   DATA FLabel
   DATA FModStamp INIT .F.       // Field is automatically mantained (dbf layer)
   DATA FName INIT ""
   DATA FNewValue
   DATA FOnEvalFieldWriteBlock
   DATA FOnReset INIT .F.
   DATA FOnSetKeyValBlock
   DATA FOnSetValue
   DATA FTable
   DATA FTableBaseClass
   DATA FType INIT "TField"
   DATA FtypeNameList
   DATA FUsingField      // Field used on Calculated Field
   DATA FValidValues
   DATA FValType INIT "U"
   DATA FWrittenValue

   METHOD CheckForValidValue( value, showAlert, errorStr )
   METHOD GetAsExpression INLINE hb_StrToExp( ::GetAsString )
   METHOD GetCloneData( cloneData )
   METHOD GetDBS_LEN INLINE ::FDBS_LEN
   METHOD GetDBS_TYPE INLINE ::FDBS_TYPE
   METHOD GetDefaultNewValue( index )
   METHOD GetEditable
   METHOD GetEnabled()
   METHOD GetEmptyValue BLOCK {|| NIL }
   METHOD GetFieldArray()
   METHOD GetFieldReadBlock()
   METHOD GetIsKeyIndex INLINE ::KeyIndex != NIL
   METHOD GetKeyIndex()
   METHOD GetLabel INLINE iif( ::FLabel == NIL, ::FName, ::FLabel )
   METHOD GetLinkedTable() INLINE NIL
   METHOD GetUndoValue()
   METHOD GetValidValues()
   METHOD OnSetKeyVal( lSeek, keyVal )
   METHOD SetAsString( string ) INLINE ::SetAsVariant( string )
   METHOD SetBuffer( value )
   METHOD SetDBS_DEC( dec ) INLINE ::FDBS_DEC := dec
   METHOD SetDBS_LEN( dbs_Len ) INLINE ::FDBS_LEN := dbs_Len
   METHOD SetCloneData( cloneData )
   METHOD SetDefaultNewValue( index, value )
   METHOD SetEditable( editable ) INLINE ::FEditable := editable
   METHOD SetEnabled( enabled )
   METHOD SetLabel( label ) INLINE ::FLabel := label
   METHOD SetRequired( Required ) INLINE ::FRequired := Required
   METHOD SetReUseField( reUseField ) INLINE ::FReUseField := reUseField
   METHOD TranslateToFieldValue( value ) INLINE value
   METHOD TranslateToValue( value ) INLINE value
   METHOD WriteToTable( value, initialize )

   PUBLIC:

   DATA AcceptEmptyUnique INIT .F.
   DATA nameAlias
   DATA PICTURE

   CONSTRUCTOR New( Table, curBaseClass )

   // ON ERROR FUNCTION OODB_ErrorHandler( ... )

   METHOD AddFieldMessage()
   METHOD AddKeyIndex( index )
   METHOD CheckEditable( flag )
   METHOD CLEAR()
   METHOD DefaultValuePull()
   METHOD DefaultValuePush( newDefaultValue )
   METHOD DELETE()
   METHOD GetAsString() INLINE "<" + ::ClassName + ">"
   METHOD GetAsUTF8 INLINE hb_StrToUTF8( ::GetAsString() )
   METHOD GetAsDisplay( ... )
   METHOD GetAsDisplayEmptyValue INLINE ::GetEmptyValue
   METHOD GetAsVariant( ... )
   METHOD GetBuffer()
   METHOD GetData()
   METHOD GetKeyVal( keyVal, keyFlags )
   METHOD IndexExpression VIRTUAL
   METHOD IsReadOnly() INLINE ::FTable:READONLY .OR. ::FReadOnly .OR. ( ::FTable:State != dsBrowse .AND. ::AutoIncrement )
   METHOD IsTableField()
   METHOD Reset()
   METHOD RevertValue()
   METHOD SetAsVariant( value )
   METHOD SetData( value, initialize )
   METHOD SetDbStruct( aStruct )
   METHOD SetFieldMethod( FieldMethod, calculated )
   METHOD SetFieldReadBlock( readBlock ) INLINE ::FFieldReadBlock := readBlock
   METHOD SetFieldWriteBlock( writeBlock )
   METHOD SetIndexExpression( indexExpression ) INLINE ::FIndexExpression := indexExpression
   METHOD SetKeyVal( keyVal, lSoftSeek )
   METHOD SetKeyValBlock( keyValBlock ) INLINE ::FOnSetKeyValBlock := keyValBlock
   METHOD SetValidValues( validValues, ignoreUndetermined )
   METHOD Validate( showAlert ) INLINE ::ValidateResult( showAlert ) = NIL
   METHOD ValidateResult( showAlert, value ) BLOCK ;
      {|Self,showAlert,value|
         LOCAL result

         IF value = NIL
            value := ::GetAsVariant()
         ENDIF

         result := ::ValidateResult_TableLogic( showAlert, value )

         IF Empty( result )
            result := ::ValidateResult_OnValidate( showAlert, value )
         ENDIF

         RETURN result
      }

   METHOD ValidateResult_TableLogic( showAlert, value )
   METHOD ValidateResult_OnValidate( showAlert, value )
   METHOD ValidateFieldInfo VIRTUAL

   PROPERTY AsDisplay READ GetAsDisplay
   PROPERTY AsExpression READ GetAsExpression
   PROPERTY AsString READ GetAsString WRITE SetAsString
   PROPERTY AsUTF8 READ GetAsUTF8
   PROPERTY AsVariant READ GetAsVariant WRITE SetAsVariant
   PROPERTY Calculated READ FCalculated
   PROPERTY CloneData READ GetCloneData WRITE SetCloneData
   PROPERTY DisplayBlock READWRITE
   PROPERTY EmptyValue READ GetEmptyValue
   PROPERTY FieldArrayIndex READ FFieldArrayIndex
   PROPERTY ignoreUndetermined
   PROPERTY KeyVal READ GetKeyVal WRITE SetKeyVal
   PROPERTY LinkedTable READ GetLinkedTable
   PROPERTY ReUseField READ FReUseField WRITE SetReUseField
   PROPERTY ReUseFieldIndex READ FReUseFieldIndex
   PROPERTY IsKeyIndex READ GetIsKeyIndex
   PROPERTY IsMasterFieldComponent READ FIsMasterFieldComponent WRITE SetIsMasterFieldComponent
   PROPERTY IsPrimaryKeyField READ GetIsPrimaryKeyField
   PROPERTY RawDefaultValue READ FDefaultValue
   PROPERTY RawNewValue READ FNewValue
   PROPERTY Size READ FSize
   PROPERTY UndoValue READ GetUndoValue
   PROPERTY ValidValues READ GetValidValues WRITE SetValidValues
   PROPERTY Value READ GetAsVariant( ... ) WRITE SetAsVariant
   PROPERTY WrittenValue READ FWrittenValue

   PUBLISHED:

   DATA IncrementBlock
   DATA nameAliasPublished INIT .T.
    /*
     * Event holders
     */
   DATA OnGetText   // Params: Sender: TField, Text: String
   DATA OnSearch   // Search in indexed field
   DATA OnSetText   // Params: Sender: TField, Text: String
   DATA OnAfterChange  // Params: Sender: Table
   DATA OnAfterPostChange  // executes after Table post and only if field has been changed
   DATA OnBeforeChange     // executes before assign value to buffer, must return logical value on success
   DATA OnValidate   // Params: Sender: Table
   DATA OnValidateWarn     // message if OnValidate == FALSE

   PROPERTY AutoIncrement READ GetAutoIncrement
   PROPERTY AutoIncrementKeyIndex READ FAutoIncrementKeyIndex WRITE SetAutoIncrementKeyIndex
   PROPERTY AutoIncrementValue READ GetAutoIncrementValue
   PROPERTY Changed READ FChanged
   PROPERTY DBS_DEC READ FDBS_DEC WRITE SetDBS_DEC
   PROPERTY DBS_LEN READ GetDBS_LEN WRITE SetDBS_LEN
   PROPERTY DBS_NAME READ FDBS_NAME
   PROPERTY DBS_TYPE READ GetDBS_TYPE
   PROPERTY DefaultValue INDEX 1 READ GetDefaultNewValue WRITE SetDefaultNewValue
   PROPERTY Description READ FDescription WRITE SetDescription
   PROPERTY Editable READ GetEditable WRITE SetEditable
   PROPERTY Enabled READ GetEnabled WRITE SetEnabled
   PROPERTY FieldArray READ GetFieldArray WRITE SetFieldMethod
   PROPERTY FieldCodeBlock READ FFieldCodeBlock WRITE SetFieldMethod
   PROPERTY FieldExpression READ FFieldExpression WRITE SetFieldMethod
   PROPERTY FieldMethod READ GetFieldMethod WRITE SetFieldMethod
   PROPERTY FieldMethodType READ FFieldMethodType
   PROPERTY FieldReadBlock READ GetFieldReadBlock WRITE SetFieldReadBlock
   PROPERTY FieldType READ FFieldType
   PROPERTY FieldWriteBlock READ FFieldWriteBlock WRITE SetFieldWriteBlock
   PROPERTY Group READ FGroup WRITE SetGroup
   PROPERTY IndexKeyList READ FIndexKeyList
   PROPERTY KeyIndex READ GetKeyIndex
   PROPERTY LABEL READ GetLabel WRITE SetLabel
   PROPERTY Name READ FName WRITE SetName
   PROPERTY NewValue INDEX 2 READ GetDefaultNewValue WRITE SetDefaultNewValue
   PROPERTY PrimaryKeyComponent READ FPrimaryKeyComponent WRITE SetPrimaryKeyComponent
   PROPERTY PrimaryKeyIndex
   PROPERTY Published READ FPublished WRITE SetPublished
   PROPERTY READONLY READ GetReadOnly WRITE SetReadOnly
   PROPERTY Required READ FRequired WRITE SetRequired
   PROPERTY Table READ FTable
   PROPERTY TableBaseClass READ FTableBaseClass
   METHOD Type( locale )
   PROPERTY UNIQUE READ GetUnique
   PROPERTY UniqueKeyIndexList READ FUniqueKeyIndexList
   PROPERTY UsingField READ FUsingField WRITE SetUsingField
   PROPERTY ValType READ FValType

ENDCLASS

/*
    New
*/
METHOD New( Table, curBaseClass ) CLASS TField

   ::FTable := Table
   ::FTableBaseClass := curBaseClass

   ::FEnabled := .T.

   RETURN Self

/*
    AddFieldMessage
*/
METHOD PROCEDURE AddFieldMessage() CLASS TField

   ::FTable:AddFieldMessage( ::Name, Self )

   RETURN

/*
   AddKeyIndex
*/
METHOD PROCEDURE AddKeyIndex( index ) CLASS TField

    IF index:IsPrimaryIndex
        ::FPrimaryKeyIndex := index
    ENDIF

   IF AScan( ::FIndexKeyList, {| e| e == index } ) = 0
      hb_AIns( ::FIndexKeyList, 1, index, .T. )
   ELSE
      ::ERROR_ATTEMPT_TO_REASIGN_INDEX_TO_FIELD()
   ENDIF

   RETURN

/*
    CheckEditable
*/
METHOD FUNCTION CheckEditable( flag ) CLASS TField

   LOCAL oldFlag := ::FCheckEditable

   ::FCheckEditable := flag

   RETURN oldFlag

/*
    CheckForValidValue
*/
METHOD FUNCTION CheckForValidValue( value, showAlert, errorStr ) CLASS TField
    LOCAL result := .T.
    LOCAL validValues

    IF ::FValidValues != NIL

        BEGIN SEQUENCE WITH ::FTable:ErrorBlock

            validValues := ::GetValidValues()

            SWITCH ValType( validValues )
            CASE 'A'
                result := AScan( validValues, {| e| e == value } ) > 0
                EXIT
            CASE 'H'
                result := AScan( hb_HKeys( validValues ), {| e| e == value } ) > 0
                EXIT
            OTHERWISE
                result := NIL
            ENDSWITCH

        RECOVER

            result := NIL

        END SEQUENCE

    ENDIF

    IF ! result == .T.

        IF result = NIL
            errorStr := ::FTable:ClassName + ": '" + ::Name + "' <Illegal data in 'ValidValues'> "
            IF showAlert == .T.
                SHOW WARN errorStr
            ENDIF
        ELSE
            errorStr := ::FTable:ClassName + ": '" + ::Name + "' <value given not in 'ValidValues'> : '" + AsString( value ) + "'"
            IF showAlert == .T. .AND. !::FignoreUndetermined
                SHOW WARN errorStr
            ENDIF
        ENDIF

   ENDIF

RETURN result

/*
    Clear
*/
METHOD PROCEDURE CLEAR() CLASS TField

   ::SetBuffer( ::EmptyValue )
   ::FChanged := .F.
   ::FWrittenValue := NIL

   RETURN

/*
    DefaultValuePull
*/
METHOD FUNCTION DefaultValuePull() CLASS TField
    LOCAL oldDefaultValue := ::FDefaultValue

    ::DefaultValue := ATail( ::FdefaultValueList )

    HB_ADel( ::FdefaultValueList, Len( ::FdefaultValueList ), .T. )

RETURN oldDefaultValue

/*
    DefaultValuePush
*/
METHOD PROCEDURE DefaultValuePush( newDefaultValue ) CLASS TField

    IF ::FdefaultValueList = NIL
        ::FdefaultValueList := {}
    ENDIF

    AAdd( ::FdefaultValueList, ::FDefaultValue )

    IF PCount() > 0
        ::DefaultValue := newDefaultValue
    ENDIF

RETURN

/*
    Delete
*/
METHOD PROCEDURE DELETE() CLASS TField

   LOCAL errObj

   IF AScan( { dsEdit, dsInsert }, ::Table:State ) = 0
      ::Table:Table_Not_In_Edit_or_Insert_mode()
      RETURN
   ENDIF

   IF !::FFieldMethodType = 'C' .OR. ::FCalculated .OR. ::FFieldWriteBlock == NIL .OR. ::FModStamp .OR. ::FUsingField != NIL
      RETURN
   ENDIF

   BEGIN SEQUENCE WITH ::FTable:ErrorBlock

      ::WriteToTable( ::EmptyValue )

   RECOVER USING errObj

      SHOW ERROR errObj

   END SEQUENCE

   RETURN

/*
    GetAsDisplay
*/
METHOD FUNCTION GetAsDisplay( ... ) CLASS TField

   LOCAL validValues
   LOCAL value

   IF ::FDisplayBlock = NIL

      value := ::GetAsVariant( ... )

      IF ::FcalcResult = NIL
         validValues := ::GetValidValues()
      ELSE
         validValues := ::FcalcResult:ValidValues()
      ENDIF

      IF HB_ISHASH( validValues )

         IF hb_HHasKey( validValues, value )
            RETURN validValues[ value ]
         ENDIF

         RETURN "<!>"

      ENDIF

   ENDIF

   RETURN ::FDisplayBlock:Eval( ::FTable )

/*
    GetAsVariant
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TField

   LOCAL AField
   LOCAL i
   LOCAL result
   LOCAL value

   IF ::FTable:isMetaTable
      ::FTable:isMetaTable := .F.
   ENDIF

   ::FcalcResult := NIL

   IF ::FUsingField != NIL
      RETURN ::FUsingField:GetAsVariant( ... )
   ENDIF

   // ::SyncToContainerField()

   IF ::FFieldMethodType = "B" .OR. ::FCalculated
      IF ::FTable:Alias != NIL
         result := ::FTable:Alias:Eval( ::FieldReadBlock, ::FTable, ... )
         IF HB_ISOBJECT( result ) .AND. result:IsDerivedFrom( "TField" )
            ::FcalcResult := result
            result := result:Value
         ENDIF
      ENDIF
   ELSE
      SWITCH ::FFieldMethodType
      CASE "A"
            /*
             * This will ONLY work when all the items are of TFieldString type
             */
         result := ""
         FOR EACH i IN ::FFieldArrayIndex
            AField := ::FTable:FieldList[ i ]
            value := AField:GetAsVariant()
            IF !HB_ISSTRING( value )
               result += AField:AsString
            ELSE
               result += value
            ENDIF
         NEXT
         EXIT
      CASE "C"
         result := ::TranslateToValue( ::GetBuffer() )
         EXIT
      OTHERWISE
         THROW ERROR OODB_ERR__FIELD_METHOD_TYPE_NOT_SUPPORTED ARGS ::FFieldMethodType
      ENDSWITCH
   ENDIF

   RETURN result

/*
    GetAutoIncrementValue
*/
METHOD FUNCTION GetAutoIncrementValue() CLASS TField

   LOCAL AIndex
   LOCAL value

   IF ::FAutoIncrementKeyIndex = NIL
      AIndex := ::KeyIndex
   ELSE
      AIndex := ::FAutoIncrementKeyIndex
   ENDIF

   value := ::Table:Alias:Get4SeekLast( ::FieldReadBlock, AIndex:MasterKeyVal, AIndex:TagName )

   IF HB_ISCHAR( value ) .AND. Len( value ) > ::Size
      value := Left( value, ::Size )
   ENDIF

   IF ::IncrementBlock = NIL
      value := Inc( value )
   ELSE
      value := ::IncrementBlock:Eval( value )
   ENDIF

   RETURN value

/*
    GetBuffer
*/
METHOD FUNCTION GetBuffer() CLASS TField

   LOCAL i

   IF !::FCalculated
      /* FieldArray's doesn't have a absolute FBuffer */
      IF ::FFieldMethodType = "A"
         IF ::FBuffer = NIL
            ::FBuffer := Array( Len( ::FFieldArrayIndex ) )
         ENDIF
         FOR EACH i IN ::FFieldArrayIndex
            ::FBuffer[ i:__enumIndex ] := ::FTable:FieldList[ i ]:GetBuffer()
         NEXT
         RETURN ::FBuffer
      ENDIF

      IF ::FBuffer == NIL
         ::Reset()
      ENDIF
   ENDIF

   RETURN ::FBuffer

/*
    GetCloneData
*/
METHOD FUNCTION GetCloneData( cloneData ) CLASS TField

   IF cloneData = NIL
      cloneData := { => }
   ENDIF

   cloneData[ "Buffer" ] := ::FBuffer
   cloneData[ "Changed" ] := ::FChanged
   cloneData[ "DefaultValue" ] := ::FDefaultValue
   cloneData[ "NewValue" ] := ::FNewValue
   cloneData[ "WrittenValue" ] := ::FWrittenValue

   RETURN cloneData

/*
    GetData
*/
METHOD FUNCTION GetData() CLASS TField
   LOCAL i
   LOCAL result := .T.

   /* this is called for Table:GetCurrentRecord, used field has been read */
   IF ::FUsingField != NIL
      RETURN result
   ENDIF

   SWITCH ::FFieldMethodType
   CASE 'B'
      result := ::SetBuffer( ::GetAsVariant() )
      EXIT
   CASE 'C'
      IF ::FCalculated
         result := ::SetBuffer( ::GetAsVariant() )
      ELSE
         result := ::SetBuffer( ::Table:Alias:Eval( ::FieldReadBlock ) )
         ::FChanged := .F.
      ENDIF
      EXIT
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         IF !::FTable:FieldList[ i ]:GetData()
            result := .F.
            EXIT
         ENDIF
      NEXT
      EXIT
   ENDSWITCH

   IF result
      ::FWrittenValue := NIL
   ENDIF

   RETURN result

/*
    GetDefaultNewValue
*/
METHOD FUNCTION GetDefaultNewValue( index ) CLASS TField

   LOCAL i
   LOCAL validValues
   LOCAL value
   LOCAL FValue
   LOCAL labelValue

   IF index = 1
      FValue := ::FDefaultValue
      labelValue := "default"
   ELSEIF index = 2
      IF ::FNewValue = NIL
         FValue := ::FDefaultValue
      ELSE
         FValue := ::FNewValue
      ENDIF
      labelValue := "new"
   ENDIF

   IF ::FFieldMethodType = 'A'
      FOR EACH i IN ::FFieldArrayIndex
         IF index = 1
            ::FTable:FieldList[ i ]:DefaultValue()
         ELSEIF index = 2
            ::FTable:FieldList[ i ]:NewValue()
         ENDIF
      NEXT
      // RETURN NIL
   ENDIF

   value := ::TranslateToValue( iif( ValType( FValue ) = "B", FValue:Eval( Self:FTable ), FValue ) )

   validValues := ::GetValidValues()

   IF ! Empty( validValues )
      SWITCH ValType( validValues )
      CASE 'A'
         IF value != NIL .AND. AScan( validValues, {| e| e == value } ) = 0
            RAISE ERROR "On field <" + ::Table:ClassName() + ":" + ::Name + ">, " + labelValue + " value '" + value + "' is not in valid values array list"
         ENDIF
         EXIT
      CASE 'H'
         IF value != NIL .AND. !hb_HHasKey( validValues, value )
            RAISE ERROR "On field <" + ::Table:ClassName() + ":" + ::Name + ">, " + labelValue + " value '" + value + "' is not in valid values hash list"
         ENDIF
         EXIT
      ENDSWITCH
   ENDIF

   RETURN value

/*
    GetEditable
*/
METHOD FUNCTION GetEditable CLASS TField

   IF HB_ISLOGICAL( ::FEditable )
      RETURN ::FEditable
   ENDIF
   IF HB_ISBLOCK( ::FEditable )
      RETURN ::FEditable:Eval( ::FTable )
   ENDIF

   RETURN .T.

/*
    GetEnabled
*/
METHOD FUNCTION GetEnabled() CLASS TField

   IF HB_ISLOGICAL( ::FEnabled )
      RETURN ::FEnabled
   ENDIF
   IF HB_ISBLOCK( ::FEnabled )
      RETURN ::FEnabled:Eval( ::FTable )
   ENDIF

   RETURN .F.

/*
    GetFieldArray
*/
METHOD FUNCTION GetFieldArray() CLASS TField

   LOCAL a := {}
   LOCAL i

   FOR EACH i IN ::FFieldArrayIndex
      AAdd( a, ::FTable:FieldList[ i ] )
   NEXT

   RETURN a

/*
    GetFieldMethod
*/
METHOD FUNCTION GetFieldMethod CLASS TField

   SWITCH ::FFieldMethodType
   CASE 'A'
      RETURN ::FFieldArrayIndex
   CASE 'B'
      RETURN ::FFieldCodeBlock
   CASE 'C'
      RETURN ::FFieldExpression
   ENDSWITCH

   RETURN NIL

/*
    GetFieldReadBlock
*/
METHOD FUNCTION GetFieldReadBlock() CLASS TField

   LOCAL block

   IF ::FFieldReadBlock == NIL .AND. ::FCalculated
      IF !Empty( ::FFieldExpression ) .AND. ":" $ ::FFieldExpression
         block := ::FTable:BuildFieldBlockFromFieldExpression( ::FFieldExpression, iif( ::FieldType = ftTable, NIL, "Value" ) )
         IF block != NIL
            ::FFieldReadBlock := block
            RETURN ::FFieldReadBlock
         ENDIF
      ENDIF
      IF ::FFieldCodeBlock = NIL
         IF __objHasMsgAssigned( ::FTable, "CalcField_" + ::FName )
            ::FFieldReadBlock := &( "{|o,...|" + "o:CalcField_" + ::FName + "( ... ) }" )
         ELSE
            IF __objHasMsgAssigned( ::FTable:MasterSource, "CalcField_" + ::FName )
               ::FFieldReadBlock := &( "{|o,...|" + "o:MasterSource:CalcField_" + ::FName + "( ... ) }" )
            ELSE
               IF !::IsDerivedFrom( "TFieldTable" )
                  THROW ERROR OODB_ERR__CALCULATED_FIELD_CANNOT_BE_SOLVED
               ENDIF
            ENDIF
         ENDIF
      ELSE
         ::FFieldReadBlock := ::FFieldCodeBlock
      ENDIF
   ENDIF

   RETURN ::FFieldReadBlock

/*
    GetKeyIndex
*/
METHOD FUNCTION GetKeyIndex() CLASS TField

    IF ::FPrimaryKeyIndex != NIL
        RETURN ::FPrimaryKeyIndex
    ENDIF

   IF Len( ::FIndexKeyList ) > 0
      RETURN ::FIndexKeyList[ 1 ]
   ENDIF

   RETURN NIL

/*
    GetKeyVal
*/
METHOD FUNCTION GetKeyVal( keyVal, keyFlags ) CLASS TField

   LOCAL AField
   LOCAL i, start, size, value

   IF ::FFieldMethodType = "A"
      IF keyVal = NIL
         keyVal := ""
         FOR EACH i IN ::FFieldArrayIndex
            keyVal += ::FTable:FieldList[ i ]:GetKeyVal( NIL, keyFlags )
         NEXT
      ELSE
         keyVal := PadR( keyVal, Min( Len( keyVal ), ::Size ) )
         start := 1
         FOR EACH i IN ::FFieldArrayIndex
            AField := ::FTable:FieldList[ i ]
            size := AField:Size
            value := SubStr( keyVal, start, size )
            value := AField:GetKeyVal( value, keyFlags )
            keyVal := Stuff( keyVal, start, size, value )
            start += Len( value )
         NEXT
      ENDIF
   ELSE
      IF keyVal = NIL
         keyVal := ::GetAsVariant()
      ENDIF
      IF HB_ISCHAR( keyVal )
         IF keyFlags != NIL .AND. hb_HHasKey( keyFlags, ::Name )
            SWITCH keyFlags[ ::Name ]
            CASE "U"
               keyVal := Upper( keyVal )
               EXIT
            ENDSWITCH
         ELSEIF ::IsKeyIndex
            IF !::KeyIndex:CaseSensitive
               keyVal := Upper( keyVal )
            ENDIF
            IF ::KeyIndex:RightJustified
               keyVal := PadL( RTrim( keyVal ), ::DBS_LEN )
            ENDIF
         ENDIF
         IF Len( keyVal ) < ::DBS_LEN
            keyVal := PadR( keyVal, ::DBS_LEN )
         ENDIF
      ENDIF
   ENDIF

   RETURN keyVal

/*
    GetUndoValue
*/
METHOD FUNCTION GetUndoValue() CLASS TField

   IF !Empty( ::FTable:UndoList ) .AND. hb_HHasKey( ::FTable:UndoList, ::FName )
      RETURN ::FTable:UndoList[ ::FName ]
   ENDIF

   RETURN NIL

/*
    GetValidValues
*/
METHOD FUNCTION GetValidValues() CLASS TField

   LOCAL validValues

   SWITCH ValType( ::FValidValues )
   CASE "A"
   CASE "H"
      RETURN ::FValidValues
   CASE "B"
      validValues := ::FValidValues:Eval( Self:FTable )
      IF HB_ISOBJECT( validValues )
         validValues := validValues:ValidValues()
      ENDIF
      RETURN validValues
   CASE "O"
      IF ::FValidValues:IsDerivedFrom( "TFieldTable" )
         RETURN ::FValidValues:ValidValues()
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*
    IsTableField
*/
METHOD FUNCTION IsTableField() CLASS TField
   RETURN ::FFieldMethodType = "C" .AND. !::FCalculated .AND. ::FUsingField = NIL

/*
    OnSetKeyVal
*/
METHOD PROCEDURE OnSetKeyVal( lSeek, keyVal ) CLASS TField

    IF __objHasMsgAssigned( ::FTable, "OnSetKeyVal_Field" )
        __objSendMsg( ::FTable, "OnSetKeyVal_Field", Self, lSeek, keyVal )
    ELSEIF __objHasMsgAssigned( ::FTable, "OnSetKeyVal_Field_" + ::Name )
        __objSendMsg( ::FTable, "OnSetKeyVal_Field_" + ::Name, lSeek, keyVal )
    ELSEIF ::FOnSetKeyValBlock != NIL
        ::FOnSetKeyValBlock:Eval( ::FTable, Self, lSeek, keyVal )
    ENDIF

RETURN

/*
    Reset
*/
METHOD FUNCTION Reset() CLASS TField

   LOCAL AField
   LOCAL i
   LOCAL value
   LOCAL result := .F.
   LOCAL FValue

   IF ::FOnReset
      RETURN .F. /* ::Value of field can be NIL */
   ENDIF

   ::FOnReset := .T.

   IF ::FTable:State = dsInsert .AND. ::FNewValue != NIL
      FValue := ::FNewValue
   ELSE
      FValue := ::FDefaultValue
   ENDIF

   IF !::FCalculated

      IF FValue = NIL

         /* if is a masterfield component, then *must* resolve it in the MasterSource(s) */
         IF ( result := ::IsMasterFieldComponent )

            result := ( AField := ::FTable:FindMasterSourceField( Self ) ) != NIL

            IF !result .AND. ::FFieldType == ftTable .AND. ::FTable:MasterSource:ClassName == Upper( ::ObjClass )
               result := ( AField := ::FTable:MasterSource:KeyField ) != NIL
            ENDIF

            IF result

               value := ::TranslateToFieldValue( AField:GetBuffer() )
                    /*
                     * if there is a DefaultValue this is ignored (may be a warning is needed)
                     */
            ENDIF

         ENDIF

         /* reset was not succesfull yet */
         IF !result
            /* resolve each field on a array of fields */
            IF ::FFieldMethodType = 'A'

               result := .T.

               FOR EACH i IN ::FFieldArrayIndex
                  result := ::FTable:FieldList[ i ]:Reset() .AND. result
               NEXT

               ::FOnReset := .F.

               RETURN result

            ELSE

               IF ::IsDerivedFrom( "TFieldTable" ) .AND. ::IsMasterFieldComponent
                  IF ::FTable:MasterSource = NIL
                     // RAISE ERROR "MasterField component '" + ::Table:ClassName + ":" + ::Name + "' needs a MasterSource Table."
                  ELSE
                     // RAISE ERROR "MasterField component '" + ::Table:ClassName + ":" + ::Name + "' cannot be resolved in MasterSource Table (" + ::FTable:MasterSource:ClassName() + ") ."
                  ENDIF
               ENDIF

               IF ::IsDerivedFrom( "TFieldTable" ) .AND. ::LinkedTable:KeyField != NIL
                  IF ::FTable:State = dsInsert
                     value := ::LinkedTable:BaseKeyField:NewValue
                  ELSE
                     value := ::LinkedTable:BaseKeyField:DefaultValue
                  ENDIF
                  IF value == NIL
                     value := ::LinkedTable:BaseKeyField:GetEmptyValue()
                  ENDIF
               ELSE
                  IF ::FTable:State = dsInsert
                     value := ::NewValue
                  ELSE
                     value := ::DefaultValue
                  ENDIF
                  IF value = NIL
                     value := ::GetEmptyValue()
                  ENDIF
               ENDIF

               result := .T.

            ENDIF
         ENDIF

      ELSE

         /* On a TFieldTable with a Table with MasterSource in the same Table, allows to Reset it first */
         IF ::FieldType = ftTable .AND. ::LinkedTable:MasterSource != NIL .AND. ::LinkedTable:MasterSource:LinkedObjField != NIL .AND. ::LinkedTable:MasterSource:LinkedObjField:Table == ::FTable
            ::LinkedTable:MasterSource:LinkedObjField:Reset()
         ENDIF

         IF ::FTable:State = dsInsert
            value := ::NewValue
         ELSE
            value := ::DefaultValue
         ENDIF

         result := .T.

      ENDIF

      ::SetBuffer( value )

      ::FChanged := .F.
      ::FWrittenValue := NIL

   ENDIF

   ::FOnReset := .F.

   RETURN result

/*
    RevertValue
*/
METHOD PROCEDURE RevertValue() CLASS TField
    LOCAL undoValue

    undoValue := ::GetUndoValue()

    IF undoValue != NIL
        ::WriteToTable( undoValue )
        ::FChanged := .F.
    ENDIF

RETURN

/*
    SetAsVariant
*/
METHOD FUNCTION SetAsVariant( value ) CLASS TField

   LOCAL oldState

   IF ::FTable:isMetaTable
      ::FTable:isMetaTable := .F.
   ENDIF

   IF ::IsReadOnly .OR. ::FTable:State = dsInactive .OR. !::Enabled
      RETURN value
   ENDIF

   IF ::FOnSetValue == NIL
      ::FOnSetValue := __objHasMsg( ::FTable, "OnSetValue_Field_" + ::FName )
   ENDIF

   IF ::FOnSetValue
      __objSendMsg( ::FTable, "OnSetValue_Field_" + ::Name, @value )
   ENDIF

   IF ::FCalculated
      IF ::FFieldWriteBlock != NIL .AND. ::FOnEvalFieldWriteBlock = NIL
         ::FOnEvalFieldWriteBlock := .T.
         ::FTable:Alias:Eval( ::FFieldWriteBlock, ::FTable, value )
         ::FOnEvalFieldWriteBlock := NIL
      ENDIF
      RETURN value
   ENDIF

   IF ( ::FTable:LinkedObjField = NIL .OR. ::FTable:LinkedObjField:Table:State = dsBrowse ) .AND. ::FTable:State = dsBrowse .AND. ::FTable:autoEdit
      oldState := ::FTable:State
      ::FTable:Edit()
   ENDIF

   SWITCH ::FTable:State
   CASE dsBrowse

      ::SetKeyVal( value )

      EXIT

   CASE dsEdit
   CASE dsInsert

      SWITCH ::FFieldMethodType
      CASE "A"

         RAISE TFIELD ::Name ERROR "Trying to assign a value to a compound TField."

         EXIT

      CASE "C"

         /* Check if we are really changing values here */
         IF !::GetBuffer() == ::TranslateToFieldValue( value )
            ::SetData( value )
         ENDIF

      ENDSWITCH

      EXIT

   OTHERWISE

      RAISE TFIELD ::Name ERROR "Table not in Edit or Insert or Reading mode"

   ENDSWITCH

   IF oldState != NIL
      ::FTable:Post()
   ENDIF

   RETURN value

/*
    SetBuffer
*/
METHOD FUNCTION SetBuffer( value ) CLASS TField
    LOCAL result
    LOCAL itm

    result := ::CheckForValidValue( ::TranslateToValue( value ), .T. ) == .T.

    IF result

        IF !::FCalculated
            /* FieldArray's doesn't have a absolute FBuffer */
            IF ::FFieldMethodType = "A"
                SWITCH ValType( value )
                CASE 'A'
                    FOR EACH itm IN value
                       IF !::FTable:FieldList[ ::FFieldArrayIndex[ itm:__enumIndex ] ]:SetBuffer( itm )
                          result := .F.
                          EXIT
                       ENDIF
                     NEXT
                    EXIT
                ENDSWITCH
                RETURN result
            ENDIF
            IF !( HB_ISNIL( value ) .OR. ValType( value ) = ::FValType ) .AND. ( ::IsDerivedFrom( "TFieldString" ) .AND. AScan( { "C", "M" }, ValType( value ) ) = 0 )
                RAISE TFIELD ::Name ERROR "Wrong Type Assign: [" + value:ClassName + "] to <" + ::ClassName + ">"
            ENDIF
        ENDIF

        ::FBuffer := ::TranslateToFieldValue( value )

    ENDIF

    RETURN result

/*
    SetCloneData
*/
METHOD PROCEDURE SetCloneData( cloneData ) CLASS TField

   ::FBuffer := cloneData[ "Buffer" ]
   ::FChanged := cloneData[ "Changed" ]
   ::FDefaultValue := cloneData[ "DefaultValue" ]
   ::FNewValue := cloneData[ "NewValue" ]
   ::FWrittenValue := cloneData[ "WrittenValue" ]

   RETURN

/*
    SetData
*/
METHOD PROCEDURE SetData( value, initialize ) CLASS TField
   LOCAL i
   LOCAL nTries
   LOCAL errObj
   LOCAL result
   LOCAL index

   IF ::FUsingField != NIL
      ::FUsingField:SetData( value )
      RETURN
   ENDIF

   /* SetData is only for the physical fields on the database */
   SWITCH ::FFieldMethodType
   CASE 'A'

      IF value != NIL
         RAISE TFIELD ::Name ERROR "SetData: Not allowed custom value in a compound TField..."
      ENDIF

      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:SetData( , initialize )
      NEXT

      RETURN

   CASE 'B'

      ::FTable:Alias:Eval( ::FFieldCodeBlock, ::FTable, value )

      RETURN

   CASE 'C'

      EXIT

   OTHERWISE

      RETURN

   ENDSWITCH

   IF AScan( { dsEdit, dsInsert }, ::Table:State ) = 0
      RAISE TFIELD ::Name ERROR "SetData(): Table not in Edit or Insert mode..."
      RETURN
   ENDIF

   IF ::FModStamp
      RETURN
   ENDIF

   IF ::AutoIncrement

      IF value != NIL
         RAISE TFIELD ::Name ERROR "Not allowed custom value in AutoIncrement Field..."
      ENDIF

        /*
         *AutoIncrement field writting allowed only in Adding
         */
      IF !( ::FTable:SubState = dssAdding )
         RETURN
      ENDIF

      /* Try to obtain a unique key */
      nTries := 1000
      WHILE .T.
         value := ::GetAutoIncrementValue()
         IF !::FAutoIncrementKeyIndex:ExistKey( ::GetKeyVal( value, ::FAutoIncrementKeyIndex:KeyFlags ) )
            EXIT
         ENDIF
         IF ( --nTries = 0 )
            RAISE TFIELD ::Name ERROR "Can't create AutoIncrement Value..."
            RETURN
         ENDIF
      ENDDO

   ELSE

      IF value == NIL
         value := ::TranslateToValue( ::GetBuffer() )
      ENDIF

   ENDIF

   /* Don't bother... all except ftArray, ftHash : always must be written */
   IF !(::FieldType = ftArray .OR. ::FieldType = ftHash) .AND. value == ::FWrittenValue
      RETURN
   ENDIF

   IF ::FCheckEditable .AND. !::Editable()
      SHOW WARN ::FTable:ClassName + ": '" + ::Name + "' <field is not editable>"
      RETURN
   ENDIF

    IF ! initialize == .T.

        IF ::OnBeforeChange != NIL
            BEGIN SEQUENCE WITH ::FTable:ErrorBlock
                result := ::OnBeforeChange:Eval( ::FTable, @value )
            RECOVER
                SHOW WARN ::FTable:ClassName + ": '" + ::Name + "' <Error at 'OnBeforeChange'>"
                result := .F.
            END SEQUENCE
            IF !result
                RETURN
            ENDIF
        ELSE
            IF ::FEvtOnBeforeChange = NIL
                ::FEvtOnBeforeChange := __objHasMsgAssigned( ::FTable, "OnBeforeChange_Field_" + ::Name )
            ENDIF
            IF ::FEvtOnBeforeChange .AND. !__objSendMsg( ::FTable, "OnBeforeChange_Field_" + ::Name, Self, @value )
                RETURN
            ENDIF
        ENDIF

        IF !Empty( ::ValidateResult_TableLogic( .T., value ) )
            RETURN
        ENDIF

    ENDIF

    BEGIN SEQUENCE WITH ::FTable:ErrorBlock

        /*
         * Check for a key violation
         */
        FOR EACH index IN ::FUniqueKeyIndexList
            IF ::IsPrimaryKeyField .AND. index:ExistKey( ::GetKeyVal( value, index:KeyFlags ), ::FTable:RecNo )
                RAISE TFIELD ::Name ERROR "Key violation."
            ENDIF
        NEXT

        ::WriteToTable( value, initialize )

        IF ! initialize == .T. .AND. !Empty( result := ::ValidateResult_OnValidate( .F. ) )

            ::RevertValue()

            SHOW WARN result

        ELSE
            IF ::FTable:LinkedObjField != NIL  .AND. ::FTable:BaseKeyField == Self

                ::FTable:LinkedObjField:SetAsVariant( ::GetAsVariant() )

            ENDIF

            /* sync with re-used field in db */
            IF ::FReUseFieldIndex != NIL
                ::FTable:FieldList[ ::FReUseFieldIndex ]:GetData()
            ENDIF

            IF !initialize == .T. .AND. ::OnAfterChange != NIL
                ::OnAfterChange:Eval( ::FTable )
            ENDIF
        ENDIF


    RECOVER USING errObj

        SHOW ERROR errObj

    END SEQUENCE

RETURN

/*
    SetDbStruct
*/
METHOD PROCEDURE SetDbStruct( aStruct ) CLASS TField

   ::FModStamp := aStruct[ 2 ] $ "=^+"

   IF !::IsDerivedFrom("TFieldFloat")
      ::SetDBS_LEN( aStruct[ 3 ] )
   ENDIF

   ::SetDBS_DEC( aStruct[ 4 ] )

   RETURN

/*
    SetDefaultNewValue
*/
METHOD PROCEDURE SetDefaultNewValue( index, value ) CLASS TField

   IF index = 1
      ::FDefaultValue := value
   ELSEIF index = 2
      ::FNewValue := value
   ENDIF

   RETURN

/*
    SetEnabled
*/
METHOD PROCEDURE SetEnabled( enabled ) CLASS TField

   IF ::FIsMasterFieldComponent .OR. ::FPrimaryKeyComponent
      RAISE TFIELD ::FName ERROR "Cannot disable a Master/Primary key component..."
   ENDIF
   ::FEnabled := enabled

   RETURN

/*
    SetFieldMethod
*/
METHOD PROCEDURE SetFieldMethod( FieldMethod, calculated ) CLASS TField

   LOCAL itm, fieldName
   LOCAL AField
   LOCAL i
   LOCAL fieldBlock

   SWITCH ( ::FFieldMethodType := ValType( FieldMethod ) )
   CASE "A"

      // ::FReadOnly := .T.
      ::FFieldArrayIndex := {}
      fieldName := ""
      FOR EACH itm IN FieldMethod
         AField := ::FTable:FieldByName( itm, @i )
         IF AField != NIL
            AAdd( ::FFieldArrayIndex, i )
            fieldName += itm + ";"
         ELSE
            RAISE TFIELD itm ERROR "Field is not defined yet..."
         ENDIF
      NEXT
      ::Name := Left( fieldName, Len( fieldName ) - 1 )
      ::FFieldCodeBlock  := NIL
      ::FFieldReadBlock  := NIL
      ::FFieldWriteBlock := NIL
      ::FFieldExpression := NIL

      EXIT

   CASE "B"
      // ::FReadOnly := .T.
      ::FFieldArrayIndex := NIL
      ::FFieldCodeBlock  := FieldMethod
      ::FFieldReadBlock  := NIL
      ::FFieldWriteBlock := NIL
      ::FFieldExpression := NIL

      ::FCalculated := .T.

      EXIT

   CASE "C"

      ::FFieldArrayIndex := NIL
      ::FFieldCodeBlock := NIL

      FieldMethod := LTrim( RTrim( FieldMethod ) )

      ::FCalculated := calculated == .T. .OR. ":" $ FieldMethod

      IF ! ::FCalculated

         ::FDBS_NAME := FieldMethod

         /* Check if the same FieldExpression is declared redeclared in the same table baseclass */
         FOR EACH AField IN ::FTable:FieldList
            IF !Empty( AField:FieldExpression ) .AND. !AField:Calculated .AND. ;
                  Upper( AField:FieldExpression ) == Upper( FieldMethod ) .AND. ;
                  AField:TableBaseClass == ::FTableBaseClass
               IF !::FReUseField
                  RAISE TFIELD ::Name ERROR "Atempt to Re-Use FieldExpression (same field on db) <" + ::ClassName + ":" + FieldMethod + ">"
               ELSE
                  ::FReUseFieldIndex := AField:__enumIndex()
               ENDIF
            ENDIF
         NEXT

         fieldBlock := FieldBlock( FieldMethod )

         IF Empty( fieldBlock )
            /* this can happen when creating/adding fields to table */
            ::FFieldReadBlock := &( "{|| FIELD->" + FieldMethod + " }" )
            ::FFieldWriteBlock := &( "{|__x_val| FIELD->" + FieldMethod + " := __x_val }" )
         ELSE
            ::FFieldReadBlock := fieldBlock
            ::FFieldWriteBlock := fieldBlock
         ENDIF

      ENDIF

      fieldName := iif( Empty( ::FName ), FieldMethod, ::FName )

      IF Empty( fieldName )
         RAISE TFIELD "<Empty>" ERROR "Empty field name and field method."
      ENDIF

      ::FFieldExpression := FieldMethod
      ::Name := FieldMethod

      EXIT

   ENDSWITCH

   RETURN

/*
    SetFieldWriteBlock
*/
METHOD PROCEDURE SetFieldWriteBlock( writeBlock ) CLASS TField

   SWITCH ValType( writeBlock )
   CASE "C"
      writeBlock := &( "{|Self,value| ::CalcField_" + writeBlock + "( value )}" )
      EXIT
   CASE "B"
      EXIT
   OTHERWISE
      RETURN
   ENDSWITCH

   ::FFieldWriteBlock := writeBlock

   RETURN

/*
    SetIsMasterFieldComponent
*/
METHOD PROCEDURE SetIsMasterFieldComponent( IsMasterFieldComponent ) CLASS TField

   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:IsMasterFieldComponent := IsMasterFieldComponent
      NEXT
   CASE 'C'
      ::FIsMasterFieldComponent := IsMasterFieldComponent
      ::FEnabled := .T.
   ENDSWITCH

   IF ::IsDerivedFrom( "TFieldTable" ) .AND. Empty( ::FTable:GetMasterSourceClassName() )
      // RAISE TFIELD ::Name ERROR "FieldTable's needs a valid MasterSource table."
   ENDIF

   RETURN

/*
    SetKeyVal
*/
METHOD FUNCTION SetKeyVal( keyVal, lSoftSeek ) CLASS TField

   IF !::FTable:OnActiveSetKeyVal()

      ::FTable:OnActiveSetKeyVal( .T. )

      IF ::IsKeyIndex

         IF ::OnSearch != NIL
            ::OnSearch:Eval( Self )
         ENDIF

         IF !Empty( keyVal )
            keyVal := ::GetKeyVal( keyVal, ::KeyIndex:KeyFlags )
            IF !::KeyIndex:KeyVal == keyVal
               ::OnSetKeyVal( ::KeyIndex:Seek( keyVal, lSoftSeek ), keyVal )
            ENDIF
         ELSE
            ::FTable:dbGoto( 0 )
         ENDIF

         IF ::FTable:LinkedObjField != NIL .AND. (::FTable:LinkedObjField:Calculated .OR. ::FTable:LinkedObjField:Table:State > dsBrowse .OR. ::FTable:LinkedObjField:Table:autoEdit)

            ::FTable:LinkedObjField:SetAsVariant( ::FTable:BaseKeyField:GetAsVariant() )

         ENDIF

      ELSE

         SHOW WARN "Field '" + ::Name + "' has no Index in the '" + ::FTable:ClassName() + "' Table..."

      ENDIF

      ::FTable:OnActiveSetKeyVal( .F. )

   ENDIF

   RETURN !::FTable:Eof

/*
    SetName
*/
METHOD PROCEDURE SetName( name ) CLASS TField

   IF Empty( name ) .OR. !Empty( ::FName )
      RETURN
   ENDIF

   IF ":" $ name
      name := StrTran( name, ":", "_" )
   ENDIF

   ::FName := name

   RETURN

/*
    SetPrimaryKeyComponent
*/
METHOD PROCEDURE SetPrimaryKeyComponent( PrimaryKeyComponent ) CLASS TField

   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         ::FTable:FieldList[ i ]:PrimaryKeyComponent := PrimaryKeyComponent
      NEXT
      EXIT
   CASE 'C'
      ::FPrimaryKeyComponent := PrimaryKeyComponent
      ::FEnabled := .T.
   ENDSWITCH

   RETURN

/*
    SetUsingField
*/
METHOD PROCEDURE SetUsingField( usingField ) CLASS TField

   LOCAL AField := ::FTable:FieldByName( usingField )

   IF AField != NIL
      ::FUsingField := AField
   ENDIF

   RETURN

/*
    SetValidValues
*/
METHOD PROCEDURE SetValidValues( validValues, ignoreUndetermined ) CLASS TField

   ::FValidValues := validValues
   ::FignoreUndetermined := ignoreUndetermined

   RETURN

/*
    Type
*/
METHOD Type( locale ) CLASS TField
    LOCAL type := ::FType

    IF !Empty( locale ) .AND. !Empty( ::FtypeNameList ) .AND. hb_HHasKey( ::FtypeNameList, locale )
        type := ::FtypeNameList[ locale ]
    ENDIF

RETURN type

/*
    ValidateResult_TableLogic
*/
METHOD FUNCTION ValidateResult_TableLogic( showAlert, value ) CLASS TField
    LOCAL result
    LOCAL index
    LOCAL indexWarnMsg

    IF value = NIL
        value := ::GetAsVariant()
    ENDIF

    IF ::FRequired .AND. Empty( value )
        result := ::FTable:ClassName + ": '" + ::Name + "' <empty field required value>"
        IF showAlert == .T.
            SHOW WARN result
        ENDIF
        RETURN result
    ENDIF

    IF ::Unique
        IF Empty( value ) .AND. !::AcceptEmptyUnique
            result := ::FTable:ClassName + ": '" + ::Name + "' <empty UNIQUE INDEX key value>"
            IF showAlert == .T.
                SHOW WARN result
            ENDIF
            RETURN result
        ENDIF
        FOR EACH index IN ::FUniqueKeyIndexList
            indexWarnMsg := index:WarnMsg
            IF !Empty( value ) .AND. index:ExistKey( ::GetKeyVal( value, index:KeyFlags ), ::FTable:RecNo )
                result := ::FTable:ClassName + ": " + iif( !Empty( indexWarnMsg ), indexWarnMsg, "'" + ::Name + "' <key value already exists> '" + AsString( value ) + "'" )
                IF showAlert == .T.
                    SHOW WARN result
                ENDIF
                RETURN result
            ENDIF
        NEXT
    ENDIF

RETURN result

/*
    ValidateResult_OnValidate
*/
METHOD FUNCTION ValidateResult_OnValidate( showAlert, value ) CLASS TField
    LOCAL result
    LOCAL l

    IF value = NIL
        value := ::GetAsVariant()
    ENDIF

    l := ::CheckForValidValue( value, showAlert, @result )

    IF ! l == .T.
        RETURN result
    ENDIF

    IF ::OnValidate != NIL
        BEGIN SEQUENCE WITH ::FTable:ErrorBlock
            l := ::OnValidate:Eval( ::FTable )
        RECOVER
            l := NIL
        END SEQUENCE
        IF l = NIL
            result := ::FTable:ClassName + ": '" + ::Name + "' <Error at 'OnValidate'> "
            IF showAlert == .T.
                SHOW WARN result
            ENDIF
        ELSEIF !l
            result := ::FTable:ClassName + ": '" + ::Name + E"' OnValidate:\n<" + iif( ::OnValidateWarn = NIL, "Value Not Valid", ::OnValidateWarn ) + "> "
            IF showAlert == .T.
            SHOW WARN result
            ENDIF
        ENDIF
    ENDIF

RETURN result

/*
    WriteToTable
*/
METHOD PROCEDURE WriteToTable( value, initialize ) CLASS TField
    LOCAL oldBuffer

    oldBuffer := ::GetBuffer()
    
    value := ::TranslateToFieldValue( value )

    ::SetBuffer( value )

    /* The physical write to the field */
    ::FTable:Alias:Eval( ::FFieldWriteBlock, value )

    ::FWrittenValue := ::GetBuffer()

    /* fill undolist */
    IF ! initialize == .T.
        IF ::FTable:UndoList != NIL .AND. !hb_HHasKey( ::FTable:UndoList, ::FName )
            ::FTable:UndoList[ ::FName ] := oldBuffer
            ::FChanged := ! value == oldBuffer
        ENDIF
    ENDIF

RETURN

/*
    ENDCLASS TField
*/
