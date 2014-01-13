/*
 * $Id: TField.prg 155 2013-06-06 03:29:32Z tfonrouge $
 */

#include "oordb.ch"
#include "xerror.ch"

/*
    TODO: Check for a correct validation for FieldExpression, it can contain any valid
                Harbour statement/formula, and loose checking is done on SetFieldMethod
*/

#xcommand RAISE TFIELD <name> ERROR <cDescription> => ;
      RAISE ERROR E"\nTable: <" + ::FTable:ClassName() + ">, FieldExpression: <" + < name > + ">" + ;
      E"\n" + ;
      < cDescription > + E"\n" ;
      SUBSYSTEM ::ClassName + "<'" + ::GetLabel() + "'>" ;
      OPERATION E"\n" + ProcName( 0 ) + "(" + LTrim( Str( ProcLine( 0 ) ) ) + ")"

/*
    TField
    Teo. Mexico 2009
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
   METHOD GetAutoIncrementValue
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

   METHOD CheckForValidValue( value )
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
   METHOD WriteToTable( value, oldBuffer )

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
   METHOD SetKeyVal( keyVal )
   METHOD SetKeyValBlock( keyValBlock ) INLINE ::FOnSetKeyValBlock := keyValBlock
   METHOD SetValidValues( validValues )
   METHOD Validate( showAlert, value )
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
    Teo. Mexico 2006
*/
METHOD New( Table, curBaseClass ) CLASS TField

   ::FTable := Table
   ::FTableBaseClass := curBaseClass

   ::FEnabled := .T.

   RETURN Self

/*
    AddFieldMessage
    Teo. Mexico 2010
*/
METHOD PROCEDURE AddFieldMessage() CLASS TField

   ::FTable:AddFieldMessage( ::Name, Self )

   RETURN

/*
   AddKeyIndex
   Teo. Mexico 2013
*/
METHOD PROCEDURE AddKeyIndex( index ) CLASS TField

   IF AScan( ::FIndexKeyList, {| e| e == index } ) = 0
      hb_AIns( ::FIndexKeyList, 1, index, .T. )
   ELSE
      ::ERROR_ATTEMPT_TO_REASIGN_INDEX_TO_FIELD()
   ENDIF

   RETURN

/*
    CheckEditable
    Teo. Mexico 2011
*/
METHOD FUNCTION CheckEditable( flag ) CLASS TField

   LOCAL oldFlag := ::FCheckEditable

   ::FCheckEditable := flag

   RETURN oldFlag

/*
    CheckForValidValue
    Teo. Mexico 2013
*/
METHOD FUNCTION CheckForValidValue( value ) CLASS TField
    LOCAL result := .T.
    LOCAL validValues

    IF ::FValidValues != NIL

        validValues := ::GetValidValues()

        BEGIN SEQUENCE WITH ::FTable:ErrorBlock

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

RETURN result

/*
    Clear
    Teo. Mexico 2012
*/
METHOD PROCEDURE CLEAR() CLASS TField

   ::SetBuffer( ::EmptyValue )
   ::FChanged := .F.
   ::FWrittenValue := NIL

   RETURN

/*
    Delete
    Teo. Mexico 2009
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
    Teo. Mexico 2013
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
    Teo. Mexico 2006
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
             * This will ONLY work when all the items are of TStringField type
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
         result := ::GetBuffer()
         EXIT
      OTHERWISE
         THROW ERROR OODB_ERR__FIELD_METHOD_TYPE_NOT_SUPPORTED ARGS ::FFieldMethodType
      ENDSWITCH
   ENDIF

   RETURN result

/*
    GetAutoIncrementValue
    Teo. Mexico 2009
*/
METHOD FUNCTION GetAutoIncrementValue CLASS TField

   LOCAL AIndex
   LOCAL value

   AIndex := ::FAutoIncrementKeyIndex

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
    Teo. Mexico 2009
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
    Teo. Mexico 2010
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
    Teo. Mexico 2006
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
    Teo. Mexico 2009
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
    Teo. Mexico 2011
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
    Teo. Mexico 2011
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
    Teo. Mexico 2010
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
    Teo. Mexico 2006
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
    Teo. Mexico 2010
*/
METHOD FUNCTION GetFieldReadBlock() CLASS TField

   LOCAL block

   IF ::FFieldReadBlock == NIL .AND. ::FCalculated
      IF ":" $ ::FFieldExpression
         block := ::FTable:BuildFieldBlockFromFieldExpression( ::FFieldExpression, iif( ::FieldType = ftObject, NIL, "Value" ) )
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
               IF !::IsDerivedFrom( "TObjectField" )
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
    Teo. Mexico 2012
*/
METHOD FUNCTION GetKeyIndex() CLASS TField

   IF Len( ::FIndexKeyList ) > 0
      RETURN ::FIndexKeyList[ 1 ]
   ENDIF

   RETURN NIL

/*
    GetKeyVal
    Teo. Mexico 2010
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
    Teo. Mexico 2009
*/
METHOD FUNCTION GetUndoValue() CLASS TField

   IF !Empty( ::FTable:UndoList ) .AND. hb_HHasKey( ::FTable:UndoList, ::FName )
      RETURN ::FTable:UndoList[ ::FName ]
   ENDIF

   RETURN NIL

/*
    GetValidValues
    Teo. Mexico 2009
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
      IF ::FValidValues:IsDerivedFrom( "TObjectField" )
         RETURN ::FValidValues:ValidValues()
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*
    IsTableField
    Teo. Mexico 2010
*/
METHOD FUNCTION IsTableField() CLASS TField
   RETURN ::FFieldMethodType = "C" .AND. !::FCalculated .AND. ::FUsingField = NIL

/*
    OnSetKeyVal
    Teo. Mexico 2013
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
    Teo. Mexico 2009
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

            IF !result .AND. ::FFieldType == ftObject .AND. ::FTable:MasterSource:ClassName == Upper( ::ObjClass )
               result := ( AField := ::FTable:MasterSource:KeyField ) != NIL
            ENDIF

            IF result

               value := AField:GetBuffer()
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
                  IF result
                     result := ::FTable:FieldList[ i ]:Reset()
                  ENDIF
               NEXT

               ::FOnReset := .F.

               RETURN result

            ELSE

               IF ::IsDerivedFrom( "TObjectField" ) .AND. ::IsMasterFieldComponent
                  IF ::FTable:MasterSource = NIL
                     // RAISE ERROR "MasterField component '" + ::Table:ClassName + ":" + ::Name + "' needs a MasterSource Table."
                  ELSE
                     // RAISE ERROR "MasterField component '" + ::Table:ClassName + ":" + ::Name + "' cannot be resolved in MasterSource Table (" + ::FTable:MasterSource:ClassName() + ") ."
                  ENDIF
               ENDIF

               IF ::IsDerivedFrom( "TObjectField" ) .AND. ::LinkedTable:KeyField != NIL
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
    Teo. Mexico 2010
*/
METHOD PROCEDURE RevertValue() CLASS TField

   ::WriteToTable( ::GetUndoValue() )

   RETURN

/*
    SetAsVariant
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetAsVariant( value ) CLASS TField

   LOCAL oldState

   IF ::FTable:isMetaTable
      ::FTable:isMetaTable := .F.
   ENDIF

   IF ::IsReadOnly .OR. ::FTable:State = dsInactive .OR. !::Enabled
      RETURN
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
      RETURN
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
         IF !::GetBuffer() == value
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

   RETURN

/*
    SetBuffer
    Teo. Mexico 2009
*/
METHOD FUNCTION SetBuffer( value ) CLASS TField
    LOCAL result
    LOCAL itm

    value := ::TranslateToValue( value )

    result := ::CheckForValidValue( value ) == .T.

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
            IF !( HB_ISNIL( value ) .OR. ValType( value ) = ::FValType ) .AND. ( ::IsDerivedFrom( "TStringField" ) .AND. AScan( { "C", "M" }, ValType( value ) ) = 0 )
                RAISE TFIELD ::Name ERROR "Wrong Type Assign: [" + value:ClassName + "] to <" + ::ClassName + ">"
            ENDIF
        ENDIF

        ::FBuffer := value

    ENDIF

    RETURN result

/*
    SetCloneData
    Teo. Mexico 2010
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
    Teo. Mexico 2006
*/
METHOD PROCEDURE SetData( value, initialize ) CLASS TField

   LOCAL i
   LOCAL nTries
   LOCAL errObj
   LOCAL buffer
   LOCAL result
   LOCAL INDEX

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
         ::FTable:FieldList[ i ]:SetData()
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
         value := ::GetBuffer()
      ENDIF

   ENDIF

   /* Don't bother... */
   IF value == ::FWrittenValue
      RETURN
   ENDIF

   IF ::FCheckEditable .AND. !::Editable()
      SHOW WARN ::FTable:ClassName + ": '" + ::GetLabel() + "' <field is not editable>"
      RETURN
   ENDIF

   /* Check if field is a masterkey in child tables */
    /* TODO: Check childs for KeyField
    IF ::FTable:PrimaryIndex != NIL .AND. ::FTable:PrimaryIndex:UniqueKeyField == Self .AND. ::FWrittenValue != NIL
        IF !Empty( ::FTable:Childs() )
            SHOW WARN "Can't modify key <'"+::GetLabel()+"'> with " + AsString( Value ) + ";Has dependant child tables."
            RETURN
        ENDIF
    ENDIF
    */

   IF ::OnBeforeChange != NIL
      BEGIN SEQUENCE WITH ::FTable:ErrorBlock
         result := ::OnBeforeChange:Eval( ::FTable, value )
      RECOVER
         SHOW WARN ::FTable:ClassName + ": '" + ::GetLabel() + "' <Error at 'OnBeforeChange'>"
         result := .F.
      END SEQUENCE
      IF !result
         RETURN
      ENDIF
   ELSE
      IF ::FEvtOnBeforeChange = NIL
         ::FEvtOnBeforeChange := __objHasMsgAssigned( ::FTable, "OnBeforeChange_Field_" + ::Name )
      ENDIF

      IF ::FEvtOnBeforeChange .AND. !initialize == .T. .AND. !__objSendMsg( ::FTable, "OnBeforeChange_Field_" + ::Name, Self, value )
         RETURN
      ENDIF
   ENDIF

   buffer := ::GetBuffer()

   ::SetBuffer( value )

   /* Validate before the physical writting */
   IF !initialize == .T. .AND. !Empty( ::Validate( .T., value ) )
      ::SetBuffer( buffer )  // revert the change
      RETURN
   ENDIF

   BEGIN SEQUENCE WITH ::FTable:ErrorBlock

        /*
         * Check for a key violation
         */
      FOR EACH INDEX IN ::FUniqueKeyIndexList
         IF ::IsPrimaryKeyField .AND. index:ExistKey( ::GetKeyVal( NIL, index:KeyFlags ) )
            RAISE TFIELD ::Name ERROR "Key violation."
         ENDIF
      NEXT

      IF initialize == .T.
         ::WriteToTable( value )
      ELSE
         ::WriteToTable( value, buffer )
      ENDIF

      IF ::FTable:LinkedObjField != NIL  .AND. ::FTable:BaseKeyField == Self

         ::FTable:LinkedObjField:SetAsVariant( ::GetAsVariant() )

      ENDIF

      /* sync with re-used field in db */
      IF ::FReUseFieldIndex != NIL
         ::FTable:FieldList[ ::FReUseFieldIndex ]:GetData()
      ENDIF

      IF !initialize == .T. .AND. ::OnAfterChange != NIL
         ::OnAfterChange:Eval( ::FTable, buffer )
      ENDIF

   RECOVER USING errObj

      SHOW ERROR errObj

   END SEQUENCE

   /* masterkey field's aren't changed here */
   IF !::AutoIncrement .AND. ::IsMasterFieldComponent
      ::Reset()  /* retrieve the masterfield value */
   ENDIF

   RETURN

/*
    SetDbStruct
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetDbStruct( aStruct ) CLASS TField

   ::FModStamp := aStruct[ 2 ] $ "=^+"
   ::SetDBS_LEN( aStruct[ 3 ] )
   ::SetDBS_DEC( aStruct[ 4 ] )

   RETURN

/*
    SetDefaultNewValue
    Teo. Mexico 2012
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
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetEnabled( enabled ) CLASS TField

   IF ::FIsMasterFieldComponent .OR. ::FPrimaryKeyComponent
      RAISE TFIELD ::FName ERROR "Cannot disable a Master/Primary key component..."
   ENDIF
   ::FEnabled := enabled

   RETURN

/*
    SetFieldMethod
    Teo. Mexico 2006
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
    Teo. Mexico 2013
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
    Teo. Mexico 2009
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

   IF ::IsDerivedFrom( "TObjectField" ) .AND. Empty( ::FTable:GetMasterSourceClassName() )
      // RAISE TFIELD ::Name ERROR "ObjectField's needs a valid MasterSource table."
   ENDIF

   RETURN

/*
    SetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION SetKeyVal( keyVal ) CLASS TField

   IF !::FTable:OnActiveSetKeyVal()

      ::FTable:OnActiveSetKeyVal( .T. )

      IF ::IsKeyIndex

         IF ::OnSearch != NIL
            ::OnSearch:Eval( Self )
         ENDIF

         IF !Empty( keyVal )
            keyVal := ::GetKeyVal( keyVal, ::KeyIndex:KeyFlags )
            IF !::KeyIndex:KeyVal == keyVal
               ::OnSetKeyVal( ::KeyIndex:Seek( keyVal ), keyVal )
            ENDIF
         ELSE
            ::FTable:dbGoto( 0 )
         ENDIF

         IF ::FTable:LinkedObjField != NIL .AND. (::FTable:LinkedObjField:Calculated .OR. ::FTable:LinkedObjField:Table:State > dsBrowse)

            ::FTable:LinkedObjField:SetAsVariant( ::FTable:BaseKeyField:GetAsVariant() )

         ENDIF

      ELSE

         SHOW WARN "Field '" + ::GetLabel() + "' has no Index in the '" + ::FTable:ClassName() + "' Table..."

      ENDIF

      ::FTable:OnActiveSetKeyVal( .F. )

   ENDIF

   RETURN !::FTable:Eof

/*
    SetName
    Teo. Mexico 2006
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
    Teo. Mexico 2009
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
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetUsingField( usingField ) CLASS TField

   LOCAL AField := ::FTable:FieldByName( usingField )

   IF AField != NIL
      ::FUsingField := AField
   ENDIF

   RETURN

/*
    SetValidValues
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetValidValues( validValues ) CLASS TField

   ::FValidValues := validValues

   RETURN

/*
    Type
    Teo. Mexico 2013
*/
METHOD Type( locale ) CLASS TField
    LOCAL type := ::FType

    IF !Empty( locale ) .AND. !Empty( ::FtypeNameList ) .AND. hb_HHasKey( ::FtypeNameList, locale )
        type := ::FtypeNameList[ locale ]
    ENDIF

RETURN type

/*
    Validate
    Teo. Mexico 2011
*/
METHOD FUNCTION Validate( showAlert, value ) CLASS TField

   LOCAL result := NIL
   LOCAL l
   LOCAL INDEX
   LOCAL indexWarnMsg

   IF ::Enabled

      IF PCount() < 2
         value := ::GetAsVariant()
      ENDIF

      IF ::FRequired .AND. Empty( value )
         result := ::FTable:ClassName + ": '" + ::GetLabel() + "' <empty field required value>"
         IF showAlert == .T.
            SHOW WARN result
         ENDIF
         RETURN result
      ENDIF

      IF ::Unique
         IF Empty( value ) .AND. !::AcceptEmptyUnique
            result := ::FTable:ClassName + ": '" + ::GetLabel() + "' <empty UNIQUE INDEX key value>"
            IF showAlert == .T.
               SHOW WARN result
            ENDIF
            RETURN result
         ENDIF
         FOR EACH INDEX IN ::FUniqueKeyIndexList
            indexWarnMsg := index:WarnMsg
            IF !Empty( value ) .AND. index:ExistKey( ::GetKeyVal( value, index:KeyFlags ) )
               result := ::FTable:ClassName + ": " + iif( !Empty( indexWarnMsg ), indexWarnMsg, "'" + ::GetLabel() + "' <key value already exists> '" + AsString( value ) + "'" )
               IF showAlert == .T.
                  SHOW WARN result
               ENDIF
               RETURN result
            ENDIF
         NEXT
      ENDIF

      l := ::CheckForValidValue( value )

      IF l = NIL
         result := ::FTable:ClassName + ": '" + ::GetLabel() + "' <Illegal value in 'ValidValues'> "
         IF showAlert == .T.
            SHOW WARN result
         ENDIF
         RETURN result
      ENDIF

      IF !l
         result := ::FTable:ClassName + ": '" + ::GetLabel() + "' <value given not in 'ValidValues'> : '" + AsString( value ) + "'"
         IF showAlert == .T.
            SHOW WARN result
         ENDIF
         RETURN result
      ENDIF

      IF ::OnValidate != NIL
         BEGIN SEQUENCE WITH ::FTable:ErrorBlock
            l := ::OnValidate:Eval( ::FTable )
         RECOVER
            l := NIL
         END SEQUENCE
         IF l = NIL
            result := ::FTable:ClassName + ": '" + ::GetLabel() + "' <Error at 'OnValidate'> "
            IF showAlert == .T.
               SHOW WARN result
            ENDIF
         ELSEIF !l
            result := ::FTable:ClassName + ": '" + ::GetLabel() + E"' OnValidate:\n<" + iif( ::OnValidateWarn = NIL, "Value Not Valid", ::OnValidateWarn ) + "> "
            IF showAlert == .T.
               SHOW WARN result
            ENDIF
         ENDIF
      ENDIF

   ENDIF

   RETURN result

/*
    WriteToTable
    Teo. Mexico 2010
*/
METHOD PROCEDURE WriteToTable( value, oldBuffer ) CLASS TField

   /* The physical write to the field */
   ::FTable:Alias:Eval( ::FFieldWriteBlock, ::TranslateToFieldValue( value ) )

   ::FWrittenValue := ::GetBuffer()

   /* fill undolist */
   IF ::FTable:UndoList != NIL .AND. !hb_HHasKey( ::FTable:UndoList, ::FName ) .AND. PCount() > 1
#ifdef __DEBUG
      IF HB_ISHASH( ::ValidValues ) .AND. !hb_HHasKey( ::ValidValues, oldBuffer )
         ::TRHOW_SOME_ERROR()
      ENDIF
#endif
      ::FTable:UndoList[ ::FName ] := oldBuffer
      ::FChanged := ! value == oldBuffer
   ENDIF

   RETURN

/*
    ENDCLASS TField
*/

/*
    TStringField
    Teo. Mexico 2006
*/
CLASS TStringField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 0
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "C"
   DATA FSize
   DATA FType INIT "String"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Texto"} )
   DATA FValType INIT "C"
   METHOD GetEmptyValue INLINE Space( ::Size )
   METHOD GetAsNumeric INLINE Val( ::GetAsVariant() )
   METHOD GetSize()
   METHOD SetAsNumeric( n ) INLINE ::SetAsVariant( LTrim( Str( n ) ) )
   METHOD SetBuffer( buffer )
   METHOD SetDBS_LEN( dbs_Len )
   METHOD SetDefaultValue( DefaultValue )
   METHOD SetSize( size )
   PUBLIC:
   METHOD GetAsString()
   METHOD IndexExpression( fieldName, isMasterFieldComponent )
   PROPERTY AsNumeric READ GetAsNumeric WRITE SetAsNumeric
   PUBLISHED:
   PROPERTY Size READ GetSize WRITE SetSize

ENDCLASS

/*
    GetAsString
    Teo. Mexico 2009
*/
METHOD FUNCTION GetAsString() CLASS TStringField

   LOCAL Result := ""
   LOCAL i

   SWITCH ::FFieldMethodType
   CASE 'A'
      FOR EACH i IN ::FFieldArrayIndex
         Result += ::FTable:FieldList[ i ]:AsString()
      NEXT
      EXIT
   OTHERWISE
      Result := ::GetAsVariant()
   ENDSWITCH

   RETURN Result

/*
    GetSize
    Teo. Mexico 2010
*/
METHOD FUNCTION GetSize() CLASS TStringField

   LOCAL i

   IF ::FSize = NIL
      IF ::FFieldMethodType = "A"
         ::FSize := 0
         FOR EACH i IN ::FFieldArrayIndex
            ::FSize += ::FTable:FieldList[ i ]:Size
         NEXT
      ENDIF
   ENDIF

   RETURN ::FSize

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName, isMasterFieldComponent ) CLASS TStringField

   LOCAL exp
   LOCAL i

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF

   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   IF ::FFieldMethodType = "A"
      exp := ""
      FOR EACH i IN ::FFieldArrayIndex
         exp += iif( Len( exp ) = 0, "", "+" ) + ::FTable:FieldList[ i ]:IndexExpression( NIL, isMasterFieldComponent == .T. .OR. ( ::IsKeyIndex .AND. !::KeyIndex:CaseSensitive ) )
      NEXT
   ELSE
      IF isMasterFieldComponent == .T. .OR. ::IsMasterFieldComponent .OR. ( ::IsKeyIndex .AND. ::KeyIndex:CaseSensitive )
         exp := fieldName
      ELSE
         IF ::FFieldExpression = NIL
            exp := "<error: IndexExpression on '" + ::Name + "'>"
         ELSE
            exp := "Upper(" + fieldName + ")"
         ENDIF
      ENDIF
   ENDIF

   RETURN exp

/*
    SetBuffer
    Teo. Mexico 2009
*/
METHOD FUNCTION SetBuffer( buffer ) CLASS TStringField

   LOCAL size := ::Size

   IF ::FFieldType = ftMemo .OR. Len( buffer ) = size
      RETURN ::Super:SetBuffer( buffer )
   ELSE
      RETURN ::Super:SetBuffer( PadR( buffer, size ) )
   ENDIF

   RETURN .F.

/*
    SetDBS_LEN
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetDBS_LEN( dbs_Len ) CLASS TStringField

   ::FDBS_LEN := dbs_Len
   IF ::FSize = NIL
      ::FSize := dbs_Len
   ENDIF

   RETURN

/*
    SetDefaultValue
    Teo. Mexico 2006
*/
METHOD PROCEDURE SetDefaultValue( DefaultValue ) CLASS TStringField

   ::FDefaultValue := DefaultValue

   ::FBuffer := NIL /* to force ::Reset on next read */

   RETURN

/*
    SetSize
    Teo. Mexico 2010
*/
METHOD PROCEDURE SetSize( size ) CLASS TStringField

   ::FSize := size
   ::FDBS_LEN := size

   RETURN

/*
    ENDCLASS TStringField
*/

/*
    TMemoField
    Teo. Mexico 2006
*/
CLASS TMemoField FROM TStringField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "M"
   DATA FFieldType INIT ftMemo
   DATA FSize INIT 0
   DATA FType INIT "Memo"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Memo"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TMemoField
*/

/*
    TNumericField
    Teo. Mexico 2006
*/
CLASS TNumericField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 15   // 000000000000.00
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "N"
   DATA FSize
   DATA FType INIT "Numeric"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Numero"} )
   DATA FValType INIT "N"
   METHOD GetEmptyValue BLOCK {|| 0 }
   METHOD StrFormat( value ) INLINE Str( value )
   PUBLIC:

   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsNumeric READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

ENDCLASS

/*
    GetAsString
    Teo. Mexico 2009
*/
METHOD FUNCTION GetAsString( value ) CLASS TNumericField

   LOCAL Result

   IF value == NIL
      value := ::GetAsVariant()
   ENDIF

   IF ::OnGetText != NIL
      Result := Value
      ::OnGetText:Eval( Self, @Result )
   ELSE
      IF ::Picture = NIL
         Result := ::StrFormat( value )
      ELSE
         Result := Transform( value, ::Picture )
      ENDIF
   ENDIF

   RETURN Result

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TNumericField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'N'
      RETURN Str( keyVal, ::FDBS_LEN )
   CASE 'U'
      RETURN Str( ::GetAsVariant(), ::FDBS_LEN )
   ENDSWITCH

   RAISE TFIELD ::GetLabel() ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TNumericField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "Str(" + fieldName + ")"

/*
    SetAsVariant
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TNumericField

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( Val( variant ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( variant )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TNumericField
*/

/*
    TIntegerField
    Teo. Mexico 2009
*/
CLASS TIntegerField FROM TNumericField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "I"
   DATA FSize INIT 4
   DATA FType INIT "Integer"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Numero Entero"} )
   METHOD StrFormat( value ) INLINE hb_StrFormat( "%d", value )
   PUBLIC:

   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsInteger READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

ENDCLASS

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TIntegerField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'N'
      RETURN hb_NumToHex( keyVal, 8 )
   CASE 'U'
      RETURN hb_NumToHex( ::GetAsVariant(), 8 )
   ENDSWITCH

   RAISE TFIELD ::GetLabel() ERROR "Don't know how to convert to key value ('" + ValType( keyVal ) + "')..."

   RETURN NIL

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TIntegerField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_NumToHex(" + fieldName + ",8)"

/*
    SetAsVariant
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TIntegerField

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( Int( Val( variant ) ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( Int( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TIntegerField
*/

/*
    TAutoIncField
    Teo. Mexico 2012
*/
CLASS TAutoIncField FROM TIntegerField

   PRIVATE:

   PROTECTED:
   DATA FDBS_TYPE INIT "+"
   DATA FType INIT "AutoInc"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"AutoInc"} )
   DATA FFieldType INIT ftAutoInc
   PUBLIC:
   PUBLISHED:

ENDCLASS
/*
    ENDCLASS TAutoIncField
*/

/*
    TTimeField
    Teo. Mexico 2012
*/
CLASS TTimeField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "I"
   DATA FFieldType INIT ftTime
   DATA FSize INIT 4
   DATA FTime AS OBJECT
   DATA FType INIT "Time"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Tiempo"} )
   METHOD GetAs( index )
   METHOD GetAsVariant( ... )
   METHOD GetEmptyValue INLINE ::Time:AsSeconds := 0, ::Time
   METHOD GetTime INLINE iif( ::FTime = NIL, ::FTime := TTime():New( "00:00:00", "HH:MM:SS" ), ::FTime )
   METHOD TranslateToFieldValue( value )
   METHOD TranslateToValue( value )
   PUBLIC:

   METHOD GetAsDisplay() INLINE ::GetAsVariant():AsString
   METHOD GetAsDisplayEmptyValue INLINE ::GetEmptyValue:AsString
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )

   PROPERTY AsHours   INDEX 1 READ GetAs
   PROPERTY AsMinutes INDEX 2 READ GetAs
   PROPERTY AsSeconds INDEX 3 READ GetAs
   PROPERTY Hours READ Time:Hours
   PROPERTY Minutes READ Time:Minutes
   PROPERTY Seconds READ Time:Seconds
   PROPERTY TimeFormat READ Time:FORMAT WRITE Time:SetFormat
   PROPERTY Time READ GetTime

   PUBLISHED:

ENDCLASS

/*
    GetAs
    Teo. Mexico 2012
*/
METHOD FUNCTION GetAs( index ) CLASS TTimeField

   ::Time:AsString := ::GetAsVariant()
   SWITCH INDEX
   CASE 1
      RETURN ::Time:AsHours
   CASE 2
      RETURN ::Time:AsMinutes
   CASE 3
      RETURN ::Time:AsSeconds
   ENDSWITCH

   RETURN NIL

/*
    GetAsVariant
    Teo. Mexico 2013
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TTimeField

   LOCAL time

   time := ::Super:GetAsVariant( ... )

   SWITCH ValType( time )
   CASE "N"
      ::time:AsSeconds := time
      EXIT
   CASE "C"
      ::time:AsString := time
      EXIT
   CASE "O"
      ::time:AsSeconds := time:AsSeconds
      EXIT
   ENDSWITCH

   RETURN ::time

/*
    GetKeyVal
    Teo. Mexico 2012
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TTimeField

   IF PCount() = 0
      keyVal := ::GetAsVariant()
   ENDIF

   RETURN hb_NumToHex( ::TranslateToFieldValue( keyVal ), 8 )

/*
    IndexExpression
    Teo. Mexico 2012
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TTimeField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_NumToHex(" + fieldName + ",8)"

/*
    TranslateToFieldValue
    Teo. Mexico 2013
*/
METHOD FUNCTION TranslateToFieldValue( value ) CLASS TTimeField
   RETURN value:AsSeconds

/*
    TranslateToValue
    Teo. Mexico 2013
*/
METHOD FUNCTION TranslateToValue( value ) CLASS TTimeField

   SWITCH ValType( value )
   CASE "C"
      ::Time:AsString := value
      EXIT
   CASE "N"
      ::Time:AsSeconds := value
      EXIT
   CASE "O"
      ::FTime := value
      EXIT
   ENDSWITCH

   RETURN ::FTime

/*
    TFloatField
    Teo. Mexico 2010
*/
CLASS TFloatField FROM TNumericField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 2
   DATA FDBS_TYPE INIT "B"
   DATA FType INIT "Float"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Numerico Decimal"} )
   METHOD StrFormat( value ) INLINE hb_StrFormat( "%10." + Chr( 48 + ::FDBS_DEC ) + "f", value )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TFloatField
*/

/*
    TLogicalField
    Teo. Mexico 2006
*/
CLASS TLogicalField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 1
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "L"
   DATA FSize INIT 1
   DATA FType INIT "Logical"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Logico"} )
   DATA FValType INIT "L"
   METHOD GetEmptyValue BLOCK {|| .F. }
   PUBLIC:

   METHOD GetAsString()
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )

   PROPERTY AsBoolean READ GetAsVariant WRITE SetAsVariant

   PUBLISHED:

ENDCLASS

/*
    GetAsString
    Teo. Mexico 2012
*/
METHOD FUNCTION GetAsString() CLASS TLogicalField
   RETURN iif( ::Value, ".T.", ".F." )

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TLogicalField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'L'
      RETURN iif( keyVal, "T", "F" )
   CASE 'U'
      RETURN iif( ::GetAsVariant(), "T", "F" )
   ENDSWITCH

   RAISE TFIELD ::GetLabel() ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TLogicalField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "iif(" + fieldName + ",'T','F')"

/*
    ENDCLASS TLogicalField
*/

/*
    TDateField
    Teo. Mexico 2006
*/
CLASS TDateField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 4
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "D"
   DATA FNewValue INIT {|| CToD( "" ) }
   DATA FDefaultValue INIT {|| Date() }
   DATA FSize INIT 8     // Size on index is 8 = len of DToS()
   DATA FType INIT "Date"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Fecha"} )
   DATA FValType INIT "D"
   METHOD GetEmptyValue BLOCK {|| CToD( "" ) }
   PUBLIC:

   METHOD GetAsString() INLINE FDateS( ::GetAsVariant() )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PUBLISHED:

ENDCLASS

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TDateField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'D'
      RETURN DToS( keyVal )
   CASE 'U'
      RETURN DToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::GetLabel() ERROR "Don't know how to convert to key value..."

   RETURN NIL

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TDateField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "DToS(" + fieldName + ")"

/*
    SetAsVariant
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TDateField

   SWITCH ValType( variant )
   CASE 'C'
      ::Super:SetAsVariant( AsDate( variant ) )
      EXIT
   CASE 'D'
      ::Super:SetAsVariant( variant )
      EXIT
   ENDSWITCH

   RETURN

/*
    ENDCLASS TDateField
*/

/*
    TDateTimeField
    Teo. Mexico 2009
*/
CLASS TDateTimeField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FSize INIT 23
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "@"
   DATA FNewValue INIT {|| hb_CToT( "" ) }
   DATA FDefaultValue INIT {|| hb_DateTime() }
   DATA FFormatDate
   DATA FFormatTime
   DATA FType INIT "DateTime"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Fecha Hora"} )
   DATA FValType INIT "C"
   METHOD GetAsDate() INLINE hb_TToD( ::Value )
   METHOD GetEmptyValue BLOCK {|| hb_CToT( "" ) }
   METHOD GetFormatDate INLINE iif( ::FFormatDate = NIL, ::ClsFmtDate, ::FFormatDate )
   METHOD GetFormatTime INLINE iif( ::FFormatTime = NIL, ::ClsFmtTime, ::FFormatTime )
   METHOD GetTime
   METHOD SetFormatDate( formatDate ) INLINE ::FFormatDate := formatDate
   METHOD SetFormatTime( formatTime ) INLINE ::FFormatTime := formatTime
   METHOD SetTime( cTime )
   PUBLIC:

   CLASSDATA ClsFmtDate INIT "YYYY-MM-DD"
   CLASSDATA ClsFmtTime INIT "HH:MM"

   METHOD DiffSeconds( dateTimePrev )
   METHOD GetAsDisplay INLINE ::GetAsString
   METHOD GetAsDisplayEmptyValue INLINE ::GetAsString( ::GetEmptyValue )
   METHOD GetAsString( value )
   METHOD GetKeyVal( keyVal )
   METHOD IndexExpression( fieldName )
   METHOD SetAsVariant( variant )

   PROPERTY AsDate READ GetAsDate
   PROPERTY FormatDate READ GetFormatDate WRITE SetFormatDate
   PROPERTY FormatTime READ GetFormatTime WRITE SetFormatTime
   PROPERTY Time READ GetTime WRITE SetTime

   PUBLISHED:

ENDCLASS

/*
    DiffSeconds
    Teo. Mexico 2011
*/
METHOD FUNCTION DiffSeconds( dateTimePrev ) CLASS TDateTimeField

   LOCAL t1, t2
   LOCAL t

   IF dateTimePrev = NIL
      dateTimePrev := hb_DateTime()
   ENDIF

   IF dateTimePrev = ::Value
      RETURN 0.0
   ENDIF

   IF dateTimePrev < ::Value
      t1 := ::Value
      t2 := dateTimePrev
   ELSE
      t2 := ::Value
      t1 := dateTimePrev
   ENDIF

   RETURN ( hb_TToD( hb_NToT( t1 - t2 ), @t ) - CToD( "" ) ) * 86400 + t

/*
    GetAsString
    Teo. Mexico 2012
*/
METHOD FUNCTION GetAsString( value ) CLASS TDateTimeField

   IF value = NIL
      value := ::Value
   ENDIF
   IF ::FormatDate = NIL .AND. ::FormatTime = NIL
      RETURN hb_TSToStr( value )
   ENDIF

   RETURN hb_TToC( value, ::FormatDate, ::FormatTime )

/*
    GetKeyVal
    Teo. Mexico 2010
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TDateTimeField

   SWITCH ValType( keyVal )
   CASE 'C'
      RETURN keyVal
   CASE 'T'
      RETURN hb_TToS( keyVal )
   CASE 'U'
      RETURN hb_TToS( ::GetAsVariant() )
   ENDSWITCH

   RAISE TFIELD ::GetLabel() ERROR "Don't know how to convert to key value..."

   RETURN hb_TToS( keyVal )

/*
    GetTime
    Teo. Mexico 2011
*/
METHOD GetTime CLASS TDateTimeField

   LOCAL cTime := "00:00:00"
   LOCAL time

   time := ::GetAsVariant()

   IF !Empty( time )
      cTime := SubStr( hb_TToS( time ), 9, 6 )
      cTime := Left( cTime, 2 ) + ":" + SubStr( cTime, 3, 2 ) + ":" + SubStr( cTime, 5, 2 )
   ENDIF

   RETURN cTime

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TDateTimeField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF
   IF fieldName = NIL
      fieldName := ::FFieldExpression
   ENDIF

   RETURN "HB_TToS(" + fieldName + ")"

/*
    SetAsVariant
    Teo. Mexico 2009
*/
METHOD PROCEDURE SetAsVariant( variant ) CLASS TDateTimeField

   LOCAL cTime

   SWITCH ValType( variant )
   CASE 'T'
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'C'
      variant := RTrim( variant )
      IF NumToken( variant ) > 1
         variant := hb_CToT( variant, ::ClsFmtDate, ::ClsFmtTime )
      ELSE
         variant := hb_SToT( variant )
      ENDIF
      ::Super:SetAsVariant( variant )
      EXIT
   CASE 'D'
      cTime := ::GetTime()
      ::Super:SetAsVariant( hb_DToT( variant, cTime ) )
      EXIT
   CASE 'N'
      ::Super:SetAsVariant( hb_NToT( variant ) )
      EXIT
   ENDSWITCH

   RETURN

/*
    SetTime
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetTime( cTime ) CLASS TDateTimeField

   ::SetAsVariant( hb_DToT( ::Value, cTime ) )

   RETURN

/*
    EndClass TDateTimeField
*/

/*
    TModTimeField
    Teo. Mexico 2009
*/
CLASS TModTimeField FROM TDateTimeField

   PRIVATE:

   PROTECTED:
   DATA FDBS_LEN INIT 8
   DATA FDBS_DEC INIT 0
   DATA FDBS_TYPE INIT "="
   DATA FModStamp INIT .T.        // Field is automatically mantained (dbf layer)
   DATA FType INIT "ModTime"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"ModTime"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    EndClass TModTimeField
*/

/*
    TObjectField
    Teo. Mexico 2009
*/
CLASS TObjectField FROM TField

   PRIVATE:

   DATA FObjClass
   DATA FLinkedTable          /* holds the Table object */
   DATA FLinkedTableMasterSource
   METHOD BuildLinkedTable()
   METHOD SetLinkedTableMasterSource( linkedTable )
   METHOD SetObjClass( objClass ) INLINE ::FObjClass := objClass

   PROTECTED:

   DATA buildingLinkedTable
   DATA FCalcMethod
   DATA FcalculatingLinkedTable INIT .F.
   DATA FClassInit
   DATA FFieldType INIT ftObject
   DATA FMasterKeyVal
   DATA FonDataChangeBlock
   DATA FonDataObj
   DATA FType INIT "ObjectField"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Documento"} )
   DATA FValidValuesLabelField
   DATA FValType INIT "O"
   METHOD GetDBS_LEN INLINE ::BaseKeyField():DBS_LEN
   METHOD GetDBS_TYPE INLINE iif( ::BaseKeyField():DBS_TYPE = "+", "I", ::BaseKeyField():DBS_TYPE )
   METHOD GetLabel()
   METHOD GetLinkedTable
   METHOD GetEmptyValue() INLINE ::BaseKeyField():EmptyValue
   METHOD GetFieldReadBlock()
   METHOD GetOnDataChange()
   METHOD GetValidValues()
   METHOD SetOnDataChange( onDataChangeBlock )

   PUBLIC:

   METHOD BaseKeyField() // Returns the non-TObjectField associated to this obj
   METHOD DataObj
   METHOD GetAsDisplay() INLINE ::GetKeyVal()
   METHOD GetKeyVal( keyVal )
   METHOD GetAsString()       // INLINE ::LinkedTable:KeyField:AsString()
   METHOD GetAsVariant( ... )
   METHOD IndexExpression( fieldName )
   METHOD SetClassInit( clsInit ) INLINE ::FClassInit := clsInit
   METHOD SetValidValues( validValues, labelField )
   PROPERTY LinkedTable READ GetLinkedTable
   PROPERTY LinkedTableAssigned READ FLinkedTableMasterSource != NIL
   PROPERTY LinkedTableMasterSource READ FLinkedTableMasterSource WRITE SetLinkedTableMasterSource
   PROPERTY ObjClass READ FObjClass WRITE SetObjClass
   PROPERTY OnDataChange READ GetOnDataChange WRITE SetOnDataChange
   PROPERTY Size READ BaseKeyField():Size
   PROPERTY ValidValuesLabelField READ FValidValuesLabelField

ENDCLASS

/*
    BaseKeyField
    Teo. Mexico 2011
*/
METHOD FUNCTION BaseKeyField() CLASS TObjectField

   LOCAL baseKeyField

   IF ::FLinkedTable != NIL .OR. !Empty( ::GetLinkedTable() )
      baseKeyField := ::FLinkedTable:BaseKeyField
      IF baseKeyField = NIL
         ::No_BaseKeyField_Defined()
      ENDIF
   ENDIF

   RETURN baseKeyField

/*
    BuildLinkedTable
    Teo. Mexico 2011
*/
METHOD PROCEDURE BuildLinkedTable() CLASS TObjectField

   LOCAL masterSource
   LOCAL className
   LOCAL fld
   LOCAL classInit

   IF ::FLinkedTable = NIL .AND. ::buildingLinkedTable = NIL

      ::buildingLinkedTable := .T.

      IF Empty( ::FObjClass )
         RAISE TFIELD ::Name ERROR "TObjectField has not a ObjClass value."
      ENDIF

        /*
         * Solve using the default ObjClass
         */
      IF ::FTable:MasterSource != NIL .AND. ::FTable:MasterSource:IsDerivedFrom( ::FObjClass ) .AND. ::IsMasterFieldComponent
         ::FLinkedTable := ::FTable:MasterSource
      ELSE
         IF ::FLinkedTableMasterSource != NIL
            masterSource := ::FLinkedTableMasterSource
         ELSEIF ::FTable:IsDerivedFrom( ::Table:GetMasterSourceClassName() ) // ( ::FObjClass ) )
            masterSource := ::FTable
         ENDIF

         ::FLinkedTable := __ClsInstFromName( ::FObjClass )

         IF ::FLinkedTable:IsDerivedFrom( ::FTable:ClassName() )
            RAISE TFIELD ::Name ERROR "Denied: To create TObjectField's linked table derived from the same field's table class."
         ENDIF

         IF !::FLinkedTable:IsDerivedFrom( "TTable" )
            RAISE TFIELD ::Name ERROR "Denied: To create TObjectField's linked table NOT derived from a TTable class."
         ENDIF

         /* check if we still need a mastersource and it exists in TObjectField's Table */
         IF Empty( masterSource )
            className := ::FLinkedTable:GetMasterSourceClassName()
            IF ::FTable:IsDerivedFrom( className )
               masterSource := ::FTable
            ELSEIF !Empty( className ) .AND. ! Empty( fld := ::FTable:FieldByObjClass( className, .T. ) )
               masterSource := fld
            ENDIF
         ENDIF
         ::FLinkedTable:New( masterSource )
         classInit := ::FClassInit
      ENDIF

      IF !HB_ISOBJECT( ::FLinkedTable ) .OR. ! ::FLinkedTable:IsDerivedFrom( "TTable" )
         RAISE TFIELD ::Name ERROR "Default value is not a TTable object."
      ENDIF

        /*
         * Attach the current DataObj to the one in table to sync when table changes
         * MasterFieldComponents are ignored, a child cannot change his parent :)
         */
      IF !::IsMasterFieldComponent .AND. ::FLinkedTable:LinkedObjField == NIL
            /*
             * LinkedObjField is linked to the FIRST TObjectField were it is referenced
             * this has to be the most top level MasterSource table
             */
         ::FLinkedTable:LinkedObjField := Self
      ELSE
            /*
             * We need to set this field as READONLY, because their LinkedTable
             * belongs to a some TObjectField in some MasterSource table
             * so this TObjectField cannot modify the physical database here
             */
         // ::ReadOnly := .T.
      ENDIF

      IF classInit != NIL
         classInit:Eval( ::FLinkedTable )
      ENDIF

      ::buildingLinkedTable := NIL

   ENDIF

   RETURN

/*
    DataObj
    Syncs the Table with the key in buffer
    Teo. Mexico 2009
*/
METHOD FUNCTION DataObj CLASS TObjectField

   LOCAL linkedTable
   LOCAL linkedObjField
   LOCAL keyVal

   linkedTable := ::GetLinkedTable()

   IF ::FonDataObj = NIL

      ::FonDataObj := .T.

      IF linkedTable != NIL

         IF linkedTable:isMetaTable
            linkedTable:isMetaTable := .F.
         ENDIF

         IF ::FonDataChangeBlock != NIL
            linkedTable:OnDataChangeBlock := ::FonDataChangeBlock
            linkedTable:OnDataChangeBlock_Param := ::Table
            ::FonDataChangeBlock := NIL
         ENDIF

         IF linkedTable:State = dsBrowse
            IF ::IsMasterFieldComponent .AND. ::FTable:FUnderReset

            ELSE
                /*
                    to sure a resync with linkedTable mastersource table
                    on TObjectField's that have a mastersource field (another TObjectField)
                    in the same table
                */
               IF !Empty( linkedTable:MasterSource ) .AND. !Empty( linkedTable:MasterSource:LinkedObjField ) .AND. linkedTable:MasterSource:LinkedObjField:Table == ::FTable
                  linkedTable:MasterSource:LinkedObjField:DataObj()
               ENDIF
               /* to be sure of mastersource synced with linkedTable */
               IF linkedTable:MasterSource != NIL .AND. !linkedTable:MasterSource:BaseKeyField:KeyVal == ::FMasterKeyVal
                  IF linkedTable:InsideScope()
                     // linkedTable:GetCurrentRecord()
                  ELSE
                     linkedTable:dbGoTop()
                  ENDIF
                  ::FMasterKeyVal := linkedTable:MasterSource:BaseKeyField:KeyVal
               ENDIF
               keyVal := ::GetKeyVal()
               /* Syncs with the current value */
               IF !::FTable:MasterSource == linkedTable .AND. !linkedTable:BaseKeyField:KeyVal == keyVal
                  linkedObjField := linkedTable:LinkedObjField
                  linkedTable:LinkedObjField := NIL
                  linkedTable:BaseKeyField:SetKeyVal( keyVal )
                  linkedTable:LinkedObjField := linkedObjField
               ENDIF
            ENDIF
         ELSE
            IF linkedTable:MasterSource != NIL .AND. AScan( { dsEdit, dsInsert }, linkedTable:State ) > 0
               ::FMasterKeyVal := linkedTable:MasterSource:BaseKeyField:KeyVal
            ENDIF
         ENDIF
      ENDIF

      ::FonDataObj := NIL

   ENDIF

   RETURN linkedTable

/*
    GetAsString
    Teo. Mexico 2009
*/
METHOD FUNCTION GetAsString() CLASS TObjectField
   RETURN ::DataObj:GetAsString()

/*
    GetAsVariant
    Teo. Mexico 2010
*/
METHOD FUNCTION GetAsVariant( ... ) CLASS TObjectField

   LOCAL variant

   variant := ::Super:GetAsVariant( ... )

   IF HB_ISOBJECT( variant )

      IF variant:IsDerivedFrom( "TObjectField" )
         // RETURN variant:DataObj:GetAsVariant()
         RETURN variant:GetAsVariant()
      ELSEIF variant:IsDerivedFrom( "TTable" )
         IF variant:BaseKeyField = NIL

            THROW ERROR OODB_ERR__NO_BASEKEYFIELD ON variant

         ENDIF
         RETURN variant:BaseKeyField:GetAsVariant()
      ENDIF

   ENDIF

   RETURN variant

/*
    GetFieldReadBlock
    Teo. Mexico 2010
*/
METHOD FUNCTION GetFieldReadBlock() CLASS TObjectField

   IF ::FFieldReadBlock = NIL .AND. ::Super:GetFieldReadBlock() = NIL
      IF ::FLinkedTable = NIL
         IF ::FcalculatingLinkedTable
            ::BuildLinkedTable() /* no result from calculating, so create table from ObjClass name */
         ELSE
            ::GetLinkedTable()
         ENDIF
      ENDIF
      ::FFieldReadBlock := {|| ::FLinkedTable }
   ENDIF

   RETURN ::FFieldReadBlock

/*
    GetKeyVal
    Teo. Mexico 2009
*/
METHOD FUNCTION GetKeyVal( keyVal ) CLASS TObjectField

   IF keyVal = NIL
      keyVal := ::GetAsVariant()
   ENDIF

   RETURN ::BaseKeyField:GetKeyVal( keyVal )

/*
    GetLabel
    Teo. Mexico 2012
*/
METHOD FUNCTION GetLabel() CLASS TObjectField

   IF !Empty( ::FLabel )
      RETURN ::FLabel
   ENDIF
   IF ::BaseKeyField != NIL
      RETURN ::BaseKeyField:Label
   ENDIF

   RETURN ""

/*
    GetLinkedTable
    Teo. Mexico 2009
*/
METHOD FUNCTION GetLinkedTable CLASS TObjectField

   LOCAL result

   IF ::FCalculated

      IF !::FcalculatingLinkedTable

         ::FcalculatingLinkedTable := .T.

         /* Alias can be NIL if table cannot be instanced yet */
         IF ::FTable:Alias == NIL
            result := ::FieldReadBlock:Eval( ::FTable )
         ELSE
            result := ::FTable:Alias:Eval( ::FieldReadBlock, ::FTable )
         ENDIF

         IF result != NIL
            IF HB_ISOBJECT( result )
               IF result:IsDerivedFrom( "TTable" )
                  ::FLinkedTable := result
               ELSEIF result:IsDerivedFrom( "TObjectField" )
                  ::FLinkedTable := result:DataObj()
               ENDIF
            ELSE /* the basekey field value is returned for the calculated field */
               IF ::FLinkedTable = NIL
                  ::BuildLinkedTable()
               ENDIF
               ::FLinkedTable:BaseKeyField:Value := result
            ENDIF
         ENDIF

         IF !::IsMasterFieldComponent .AND. ::FLinkedTable:LinkedObjField == NIL
            ::FLinkedTable:LinkedObjField := Self
         ENDIF

         ::FcalculatingLinkedTable := .F.

      ELSE

         ::BuildLinkedTable()

      ENDIF

   ELSE

      IF ::FLinkedTable == NIL
         ::BuildLinkedTable()
      ENDIF

   ENDIF

   RETURN ::FLinkedTable

/*
    GetOnDataChange
    Teo. Mexico 2011
*/
METHOD FUNCTION GetOnDataChange() CLASS TObjectField
   RETURN ::GetLinkedTable:OnDataChangeBlock

/*
    GetValidValues
    Teo. Mexico 2009
*/
METHOD FUNCTION GetValidValues() CLASS TObjectField

   LOCAL hValues
   LOCAL fld

   IF ::FValidValuesLabelField == .T.
      hValues := { => }
      fld := ::LinkedTable:FieldByName( ::FValidValues )
      ::LinkedTable:StatePush()
      ::LinkedTable:dbGoTop()
      WHILE !::LinkedTable:Eof()
         hValues[ ::LinkedTable:KeyVal ] := fld:Value
         ::LinkedTable:dbSkip()
      ENDDO
      ::LinkedTable:StatePull()
      RETURN hValues
   ENDIF

   RETURN ::Super:GetValidValues()

/*
    IndexExpression
    Teo. Mexico 2010
*/
METHOD FUNCTION IndexExpression( fieldName ) CLASS TObjectField

   IF ::FIndexExpression != NIL
      RETURN ::FIndexExpression
   ENDIF

   IF ::Calculated
      RETURN NIL
   ENDIF

   IF fieldName = NIL
      IF ::FUsingField = NIL
         fieldName := ::FFieldExpression
      ELSE
         fieldName := ::FUsingField:FieldExpression
      ENDIF
   ENDIF

   RETURN ::BaseKeyField:IndexExpression( fieldName )

/*
    SetLinkedTableMasterSource
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetLinkedTableMasterSource( linkedTable ) CLASS TObjectField

   SWITCH ValType( linkedTable )
   CASE "C"
      linkedTable := ::Table:FieldByName( linkedTable )
   CASE "O"
      IF linkedTable:IsDerivedFrom( "TObjectField" ) .OR. linkedTable:IsDerivedFrom( "TTable" )
         EXIT
      ENDIF
   CASE "B"
      EXIT
   OTHERWISE
      RAISE ERROR "Invalid master source value..."
   ENDSWITCH

   ::FLinkedTableMasterSource := linkedTable

   RETURN

/*
    SetOnDataChange
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetOnDataChange( onDataChangeBlock ) CLASS TObjectField

   ::FonDataChangeBlock := onDataChangeBlock

   RETURN

/*
    SetValidValues
    Teo. Mexico 2011
*/
METHOD PROCEDURE SetValidValues( validValues, labelField ) CLASS TObjectField

   ::Super:SetValidValues( validValues )
   ::FValidValuesLabelField := labelField

   RETURN

/*
    ENDCLASS TObjectField
*/

/*
    TVariantField
    Teo. Mexico 2010
*/
CLASS TVariantField FROM TField

   PRIVATE:

   PROTECTED:
   DATA FType INIT "Variant"
   DATA FtypeNameList INIT hb_hSetCaseMatch( {"es"=>"Variante"} )
   PUBLIC:
   PUBLISHED:

ENDCLASS

/*
    ENDCLASS TFloatField
*/
