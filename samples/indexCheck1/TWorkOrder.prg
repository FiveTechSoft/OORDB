/*
 * $Id:$
 */

#include "oordb.ch"

CLASS TWorkOrder FROM TTable

  DEFINE FIELDS
  DEFINE PRIMARY INDEX
  DEFINE SECONDARY INDEX
  
  PROPERTY TableFileName VALUE "data/WorkOrder"

ENDCLASS

BEGIN FIELDS CLASS TWorkOrder

  ADD INTEGER FIELD "WrkOrdId"
  
  ADD STRING FIELD "Status" SIZE 1 ;
    VALIDVALUES {" "=>"Unknown","1"=>"Active","0"=>"Terminated"}
  
  ADD OBJECT FIELD "Machine" ;
    REQUIRED ;
    CLASS "TMachine"

  ADD OBJECT FIELD "Tool" ;
    REQUIRED ;
    CLASS "TTool"

END FIELDS CLASS

BEGIN PRIMARY INDEX CLASS TWorkOrder
  DEFINE INDEX "Primary" KEYFIELD "WrkOrdId" AUTOINCREMENT
END PRIMARY INDEX CLASS

BEGIN SECONDARY INDEX CLASS TWorkOrder

  /* index to create restriction to just one Machine active at time */
  DEFINE INDEX "XMachineSt" MASTERKEYFIELD "Machine" KEYFIELD "Status" UNIQUE ;
    FOR "Status = '1'"

  /* index to create restriction to just one Tool active at time */
  DEFINE INDEX "XToolSt" MASTERKEYFIELD "Tool" KEYFIELD "Status" UNIQUE ;
    FOR "Status = '1'"

END SECONDARY INDEX CLASS
