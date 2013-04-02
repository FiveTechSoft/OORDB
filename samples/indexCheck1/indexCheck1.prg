
#include "oordb.ch"

REQUEST DBFCDX

FUNCTION Main()
  LOCAL machine
  LOCAL tool
  LOCAL wo
  LOCAL i

  SetMode( 40, 100 )
  
  IF !HB_DirExists("data")
    HB_DirCreate( "data" )
  ENDIF

  RddSetDefault("DBFCDX")

  DbDrop( "data/Machine" )
  DbDrop( "data/Tool" )
  DbDrop( "data/WorkOrder" )

  machine := TMachine():New()
  tool := TTool():New()
  wo := TWorkOrder():New()

  CLS

  /* Create/Seek 10 Machines */
  ?
  ? "Ten Machines..."
  FOR i:=1 TO 10
    machine:Value := i
    IF machine:Eof()
      machine:Insert()
      machine:Field_Name:Value := "Machine #" + LTrim( Str( i ) )
      machine:Post()
    ENDIF
    ? "Machine:", machine:Field_Name:Value
  NEXT

  /* Create/Seek 10 Tools */
  ?
  ? "Ten Tools..."
  FOR i:=1 TO 10
    tool:Value := i
    IF tool:Eof()
      tool:Insert()
      tool:Field_Name:Value := "Tool #" + LTrim( Str( i ) )
      tool:Post()
    ENDIF
    ? "Tool:", tool:Field_Name:Value
  NEXT

  ?
  ?
  ? "Creating Order"

  wo:Insert()
  wo:Field_Machine:Value := 5
  wo:Field_Tool:Value := 6
  wo:Field_Status:Value := "1"
  ? "Post", wo:Post()

  ?
  ? "Trying to use again Machine 5 (must fail)"
  wo:Insert()
  wo:Field_Machine:Value := 5
  wo:Field_Tool:Value := 7
  wo:Field_Status:Value := "1"
  ? "Post", wo:Post()

  ?
  ? "Trying to use Machine 3 (must success)"
  wo:Insert()
  wo:Field_Machine:Value := 3
  wo:Field_Tool:Value := 7
  wo:Field_Status:Value := "1"
  ? "Post", wo:Post()

  WAIT

RETURN NIL
