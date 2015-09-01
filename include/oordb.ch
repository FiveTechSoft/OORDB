/*
 *
 */

/*
    Arelds.ch
*/

#ifndef _OORDB_H_
#define _OORDB_H_

#include "hbclass.ch"
#include "property.ch"

#include "oordbdefs.h"

#define dsInactive  0
#define dsBrowse    1
#define dsInsert    2
#define dsEdit      4

#define dssNone     0
#define dssAdding   1
#define dssPosting  2

#define ftBase      0
#define ftArray     1
#define ftDate      2
#define ftDateTime  3
#define ftHash      4
#define ftLogical   5
#define ftNumeric   6
#define ftString    7
#define ftTable     8
#define ftMemo      9
#define ftTime      10
#define ftVariant   11

#define ftAutoInc   101
#define ftRowVer    102
#define ftFloat     103
#define ftInteger   104
#define ftModTime   105

#define noRetry     .T.


/* Events for TTable */
#xtranslate EVENT ONAFTEROPEN => METHOD OnAfterOpen()
#xtranslate EVENT ONAFTEROPEN CLASS <className> => METHOD PROCEDURE OnAfterOpen() CLASS <className>

#xtranslate EVENT ONAFTERINSERT => METHOD OnAfterInsert()
#xtranslate EVENT ONAFTERINSERT CLASS <className> => METHOD PROCEDURE OnAfterInsert() CLASS <className>

/* Events for TField's */
#xtranslate EVENT ONBEFORECHANGE FIELD <!name!> ;
    => ;
    METHOD OnBeforeChange_Field_<name>( field_<name>, value )

#xtranslate DEFINE RELATIONS ;
    => ;
    METHOD DefineRelations()

#xtranslate BEGIN RELATIONS CLASS <className> ;
    => ;
    METHOD PROCEDURE DefineRelations() CLASS <className>

#xtranslate END RELATIONS [CLASS] ;
    => ;
    RETURN

#xtranslate ADD TABLE <tableName> [ <vt: VIRTUAL> ] [ INDEX <indexName> ] [ <auto: AUTODELETE> ] ;
            => ;
            ::cmdAddTable( <tableName>, [ <indexName> ], <.vt.>, <.auto.> )

#xtranslate DEFINE CHILD ;
            => ;
            ::cmdDefineChild()

#xtranslate END CHILD ;
            => ;
            ::cmdEndChild()

#xtranslate ADD TABLE <parentTableName> CHILD <childTableName> [ INDEX <indexName> ];
            => ;
            ::AddParentChild( <parentTableName>, <childTableName>, [ <indexName> ] )

/* To REQUIRE FORMAT in TFieldTime */
#xtranslate ADD [<clauses0,...>] TIME FIELD <xFieldMethod> [<clauses1,...>] FORMAT <timeFormat> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _TIME FIELD <xFieldMethod> [<clauses1>] FORMAT <timeFormat> [<clauses2>]
#xtranslate TField_Time => TFieldTime

/* HTIME fields (float type based time) */
#xtranslate ADD [<clauses0,...>] HTIME FIELD <xFieldMethod> [<clauses1,...>] ;
                        => ;
                        ADD [<clauses0>] TIME FIELD <xFieldMethod> FTYPE "B" LEN 8 DEC 2 [<clauses1>]

/* To REQUIRE SIZE in TFieldString */
#xtranslate ADD [<clauses0,...>] STRING FIELD <xFieldMethod> [<clauses1,...>] SIZE <nsize> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _STRING FIELD <xFieldMethod> [<clauses1>] SIZE <nsize> [<clauses2>]
#xtranslate TField_String => TFieldString

/* To REQUIRE LEN DEC in TFieldNumeric */
#xtranslate ADD [<clauses0,...>] NUMERIC FIELD <xFieldMethod> [<clauses1,...>] LEN <nLen> DEC <nDec> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _NUMERIC FIELD <xFieldMethod> [<clauses1>] LEN <nLen> DEC <nDec> [<clauses2>]
#xtranslate TField_Numeric => TFieldNumeric

/* To REQUIRE CLASS in TFieldTable */
#xtranslate ADD [<clauses0,...>] TABLE FIELD <xFieldMethod> [<clauses1,...>] CLASS <objClass> [<clauses2,...>] ;
                        => ;
                        ADD [<clauses0>] _TABLE FIELD <xFieldMethod> [<clauses1>] CLASS <objClass> [<clauses2>]
#xtranslate TField_Table => TFieldTable

#xtranslate ADD [<calc: CALCULATED>] <type: _STRING, MEMO, _NUMERIC, FLOAT, INTEGER, AUTOINC, ROWVER, LOGICAL, _TIME, DATE, DATETIME, MODTIME, _TABLE, VARIANT, ARRAY, HASH> FIELD [<xFieldMethod>] ;
                        [ NAME <cName> ] ;
                        [ LABEL <label> ] ;
                        [ <ro: READONLY> ] ;
                        [ DEFAULTVALUE <xDefaultValue> ] ;
                        [ NEWVALUE <xNewValue> ] ;
                        [ READ <readblock,...> ] ;
                        [ WRITE <writeblock,...> ] ;
                        [ <rq: REQUIRED> ] ;
                        [ GROUP <cGroup> ] ;
                        [ DESCRIPTION <cDesc> ] ;
                        [ SIZE <nSize> ] ;
                        [ FORMAT <timeFormat> ] ;
                        [ FTYPE <fType> ] ;
                        [ LEN <nLen> ] ;
                        [ DEC <nDec> ] ;
                        [ PICTURE <pict> ] ;
                        [ FORMATDATE <fmtDate> ] ;
                        [ FORMATTIME <fmtTime> ] ;
                        [ <pv: PRIVATE> ] ;
                        [ INCREMENT <incrementBlock> ] ;
                        [ MASTERSOURCE <linkedTableMasterSource> ] ;
                        [ CLASS <objClass> ] ;
                        [ CLASSINIT <clsInit> ] ;
                        [ ON GETTEXT <bOnGetText> ] ;
                        [ ON SETTEXT <bOnSetText> ] ;
                        [ ON VALIDATE <bOnValidate> [ WARN <warnMsg> ] ] ;
                        [ ON SEARCH <bOnSearch> ] ;
                        [ ON BEFORE CHANGE <bOnBeforeChange> ] ;
                        [ ON AFTER CHANGE <bOnAfterChange> ] ;
                        [ ON AFTER POST CHANGE <bOnAfterPostChange> ] ;
                        [ ON DATA CHANGE <bOnDataChange> ] ;
                        [ ON SETKEY <bOnSetKey> ] ;
                        [ VALIDVALUES [<vvl: LABEL>] <validValues> [<igUndet: IGNORE_UNDETERMINED>] ] ;
                        [ USING <usingField> ] ;
                        [ <ruf: REUSEFIELD> ] ;
                        [ ENABLED <enabled> ] ;
                        [ EDITABLE <editable> ] ;
                        [ INDEXEXPRESSION <indexExp> ] ;
                        [ DEFAULTINDEX <defaultIndexName> ] ;
                        [ DISPLAY <dispBlock> ] ;
                        [ <aeu: ACCEPT_EMPTY_UNIQUE> ] ;
                     => ;
                        WITH OBJECT TField<type>():New( Self, ::curClassField ) ;;
                            [ :Name := <cName> ] ;;
                            [ :Label := <label> ] ;;
                            [ :ReadOnly := <.ro.> ] ;;
                            [ :ReUseField := <.ruf.> ] ;;
                            [ :ObjClass := <objClass> ] ;;
                            [ :SetClassInit( <clsInit> ) ] ;;
                            [ :Size := <nSize> ] ;;
                            [ :TimeFormat := <timeFormat> ] ;;
                            [ :SetFieldMethod( <xFieldMethod>, <.calc.> ) ] ;;
                            [ :FieldReadBlock := <readblock> ] ;;
                            [ :FieldWriteBlock := <writeblock> ] ;;
                            [ :DefaultValue := <xDefaultValue> ] ;;
                            [ :NewValue := <xNewValue> ] ;;
                            [ :Required := <.rq.> ] ;;
                            [ :Group := <cGroup> ] ;;
                            [ :Description := <cDesc> ] ;;
                            [ :DBS_TYPE := <fType> ] ;;
                            [ :DBS_LEN := <nLen> ] ;;
                            [ :DBS_DEC := <nDec> ] ;;
                            [ :Picture := <pict> ] ;;
                            [ :FormatDate := <fmtDate> ] ;;
                            [ :FormatTime := <fmtTime> ] ;;
                            [ :Published := !<.pv.> ] ;;
                            [ :IncrementBlock := <incrementBlock> ] ;;
                            [ :LinkedTableMasterSource := <linkedTableMasterSource> ] ;;
                            [ :OnGetText := {|field,Text| <bOnGetText> } ] ;;
                            [ :OnSetText := {|field,Text| <bOnSetText> } ] ;;
                            [ :OnValidate := <bOnValidate> ] ;;
                            [ :OnValidateWarn := <warnMsg> ] ;;
                            [ :OnSearch := <bOnSearch> ] ;;
                            [ :OnBeforeChange := <bOnBeforeChange> ] ;;
                            [ :OnAfterChange := <bOnAfterChange> ] ;;
                            [ :OnAfterPostChange := <bOnAfterPostChange> ] ;;
                            [ :OnDataChange := <bOnDataChange> ] ;;
                            [ :SetKeyValBlock( <bOnSetKey> ) ] ;;
                            [ :SetValidValues( <validValues>, <.igUndet.>, <.vvl.> ) ] ;;
                            [ :UsingField := <usingField> ] ;;
                            [ :Enabled := <enabled> ] ;;
                            [ :Editable := <editable> ] ;;
                            [ :SetIndexExpression( <indexExp> ) ] ;;
                            [ :SetDefaultIndexName( <defaultIndexName> ) ] ;;
                            [ :DisplayBlock := <dispBlock> ] ;;
                            [ :acceptEmptyUnique := <.aeu.> ] ;;
                            :AddFieldMessage() ;;
                            :ValidateFieldInfo() ;;
                        ENDWITH
                        
/* Calculated field with CALCREAD method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> CALCREAD <mthd> [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> READ {|Self,...| ::<mthd>(<fldName>,...) } [<clauses1>]

/* Calculated field with CALCREADWRITE method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> CALCREADWRITE <mthd> [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> READ {|Self| ::<mthd>(<fldName>) } WRITE {|Self,...| ::<mthd>( <fldName>,... ) } [<clauses1>]

/* Calculated field with ALLOW_WRITE method */
#xtranslate ADD CALCULATED [<clauses0,...>] FIELD <fldName> ALLOW_WRITE [<clauses1,...>] ;
                        => ;
                        ADD CALCULATED [<clauses0>] FIELD <fldName> WRITE <fldName> [<clauses1>]

#xtranslate ADD ALIAS NAME <aliasFldName> TO FIELD <fld> [<noPub> PRIVATE ] ;
            => ;
            ::AddFieldAlias( <aliasFldName>, <fld>, [!<.noPub.>] )

#xtranslate DEFINE DATABASE WITH <methodDatabase> CLASS ;
    => ;
    METHOD InitDataBase INLINE <methodDatabase>():New()

#xtranslate DEFINE BASEDOCUMENT ;
    => ;
    DATA FbaseDocument INIT oClass:cName

#xtranslate DEFINE FIELDS ;
    => ;
    DATA curClassField HIDDEN ;;
    METHOD __DefineFields()

#xtranslate DEFINE PRIMARY INDEX ;
    => ;
    DATA curClassPrimaryIndex HIDDEN ;;
    METHOD __DefinePrimaryIndex()

#xtranslate DEFINE SECONDARY INDEX ;
    => ;
    DATA curClassIndex HIDDEN ;;
    METHOD __DefineSecondaryIndexes()

#xtranslate BEGIN FIELDS CLASS <className>;
                        => ;
                        METHOD PROCEDURE __DefineFields() CLASS <className> ;;
                        ::curClassField := <(className)> ;;
                        ::Super:__DefineFields()

#xtranslate END FIELDS [CLASS] ;
                        => ;
                        ::__DefineFields_Exit() ;;
                        RETURN

#xtranslate BEGIN PRIMARY INDEX CLASS <className> ;
                        => ;
            METHOD PROCEDURE __DefinePrimaryIndex() CLASS <className> ;;
            LOCAL __typeIndex__ := "PRIMARY" ;;
            ::curClassPrimaryIndex := <(className)> ;;
            ::Super:__DefinePrimaryIndex() ;;

#xtranslate END PRIMARY INDEX [CLASS] ;
                        => ;
            HB_SYMBOL_UNUSED( __typeIndex__ ) ;;
            RETURN
            
#xtranslate BEGIN SECONDARY INDEX CLASS <className> ;
                        => ;
            METHOD PROCEDURE __DefineSecondaryIndexes() CLASS <className> ;;
            LOCAL __typeIndex__ := "SECONDARY" ;;
            ::curClassIndex := <(className)> ;;
            ::Super:__DefineSecondaryIndexes() ;;

#xtranslate END SECONDARY INDEX [CLASS] ;
                        => ;
            RETURN
            
#xtranslate METHOD CALCFIELD <clcField> => METHOD CalcField_<clcField>
#xtranslate METHOD CALCFIELD <clcField> CLASS <className> ;
    => ;
    METHOD FUNCTION CalcField_<clcField> CLASS <className>
#xtranslate METHOD CALCFIELD <clcField>( [<params,...>] ) CLASS <className> ;
    => ;
    METHOD FUNCTION CalcField_<clcField>( [<params>] ) CLASS <className>

#xtranslate ON SETVALUE FIELD <svField> ;
    => ;
    METHOD OnSetValue_Field_<svField>
#xtranslate ON SETVALUE FIELD <svField> CLASS <className> ;
    => ;
    METHOD PROCEDURE OnSetValue_Field_<svField>( value ) CLASS <className>

/* TODO: Implement this, needs to use a index declared in ancestor class
#xtranslate DEFINE PRIMARY INDEX <cName> ;
            => ;
            TIndex():New( Self, <cName>, "PRIMARY" )
*/

#xtranslate DEFINE INDEX [TAG] <tagName> [NAME <name>] ;
                        [ MASTERKEYFIELD <cMasterKeyField> ] ;
                        [ KEYFIELD <cKeyField> ] ;
                        [ KEYFLAGS <keyFlags> ] ;
                        [ FOR <ForKey> ] ;
                        [ <ncs: NO_CASE_SENSITIVE> ] ;
                        [ <rj: RJUST> ] ;
                        [ <de: DESCENDING> ] ;
                        [ <cu: CUSTOM> ] ;
                        [ <un: UNIQUE> ] ;
                        [ <ai: AUTOINCREMENT> ] ;
                        [ <tm: TEMPORARY> ] ;
                        [ USEINDEX <useIndex> ] ;
                        [ <acceptEmptyUnique: ACCEPT_EMPTY_UNIQUE> ] ;
                        [ ON KEYVIOLATION WARN <errorMsg> ] ;
                        => ;
                        WITH OBJECT TIndex():New( Self , <tagName>, [<name>], __typeIndex__, iif( __typeIndex__ == "PRIMARY", ::curClassPrimaryIndex, ::curClassIndex ), <errorMsg> ) ;;
                            :AddIndex( [<cMasterKeyField>], [<.ai.>], [<.un.>], [<cKeyField>], [<keyFlags>], [<ForKey>], !<.ncs.>, [<.de.>], [<.acceptEmptyUnique.>], [<useIndex>], [<.tm.>], [<.rj.>], [<.cu.>] ) ;;
                        ENDWITH
                        
#xtranslate CREATE <type: PRIMARY,SECONDARY> INDEX [TAG] <tagName> [NAME <name>] ;
                        [ MASTERKEYFIELD <cMasterKeyField> ] ;
                        [ KEYFIELD <cKeyField> ] ;
                        [ FOR <ForKey> ] ;
                        [ KEYFLAGS <keyFlags> ] ;
                        [ <ncs: NO_CASE_SENSITIVE> ] ;
                        [ <rj: RJUST> ] ;
                        [ <de: DESCENDING> ] ;
                        [ <cu: CUSTOM> ] ;
                        [ <un: UNIQUE> ] ;
                        [ <ai: AUTOINCREMENT> ] ;
                        [ <tm: TEMPORARY> ] ;
                        [ USEINDEX <useIndex> ] ;
                        [ <acceptEmptyUnique: ACCEPT_EMPTY_UNIQUE> ] ;
                        => ;
                        WITH OBJECT TIndex():New( Self , <tagName>, [<name>], <"type">, ::curClassIndex ) ;;
                                :AddIndex( [<cMasterKeyField>], [<.ai.>], [<.un.>], [<cKeyField>], [<keyFlags>], [<ForKey>], !<.ncs.>, [<.de.>], [<.acceptEmptyUnique.>], [<useIndex>], [<.tm.>], [<.rj.>], [<.cu.>] ) ;;
                        ENDWITH
                        
#xtranslate DEFINE EXTERNAL INDEX <name> WITH <table> GET_RECNO <getRecNo> SET_RECNO <setRecNo> ;
    => ;
    ::AssociateTableIndex( <table>, <name>, <getRecNo>, <setRecNo> )

#xtranslate CREATE [<custom: CUSTOM>] INDEX ON <expKey> TAG <ordName> ;
    [BAG <bagName>] ;
    [FOR <forKey>] ;
    [WHILE <whileBlk>] ;
    [<unique: UNIQUE>] ;
    [EVAL <evalBlk>] ;
    [EACH <intVal>] ;
    [<descend: DESCENDING>] ;
    [<additive: ADDITIVE>] ;
    [<current: USECURRENT>] ;
    [<temporary: TEMPORARY>] ;
    => ;
    OrdCondSet( ;
        <(forKey)>, ;
        iif(<.forKey.>, <{forKey}>, NIL ), ;
        NIL, ;
        <{whileBlk}>, ;
        <{evalBlk}>, ;
        <intVal>, ;
        NIL, ;
        NIL, ;
        NIL, ;
        NIL, ;
        <.descend.>, ;
        NIL, ;
        <.additive.>, ;
        <.current.>, ;
        <.custom.>, ;
        NIL, ;
        NIL, ;
        <.temporary.> ) ;;
    OrdCreate( [<bagName>], <ordName>, <expKey>, <expKey>, <.unique.> )
