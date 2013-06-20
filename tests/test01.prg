
/*
    Checks for creating ciclic table references
*/

#include "oordb.ch"

PROCEDURE Main()
    LOCAL registro

    SetMode( 40, 100 )

    ? "Starting..."

    registro := TRegistro():New()

    ? registro:Count
    ? registro:Field_Inspection:DataObj:Count

    WAIT "end test..."

RETURN

CLASS TRegistro FROM TTable
    PROPERTY TableFileName INIT "regs"
    DEFINE FIELDS
    DEFINE PRIMARY INDEX
ENDCLASS

BEGIN FIELDS CLASS TRegistro

    ADD AUTOINC FIELD "RegistroId"

    ADD DATETIME FIELD "FechaHora"

    ADD FLOAT FIELD "Cantidad"

    ADD OBJECT FIELD "Inspection" ;
	CLASS "TInspection"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS TRegistro

    DEFINE INDEX "Primary" KEYFIELD "RegistroId"

END PRIMARY INDEX CLASS

CLASS TInspection FROM TTable
    PROPERTY TableFileName INIT "inspect"
    DEFINE FIELDS
    DEFINE PRIMARY INDEX
ENDCLASS

BEGIN FIELDS CLASS TInspection

    ADD AUTOINC FIELD "InspId" NAME "InspectionId"

    ADD DATETIME FIELD "DateTime"

    ADD STRING FIELD "St" SIZE 1

    ADD OBJECT FIELD "Registro" ;
	CLASS "TRegistro"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS TInspection

    DEFINE INDEX "Primary" KEYFIELD "InspectionId"

END PRIMARY INDEX CLASS
