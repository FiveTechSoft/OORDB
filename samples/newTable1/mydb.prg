/*
 * $Id:$
 */

#include "oordb.ch"

CLASS MyDb FROM TDatabase

  PROPERTY Directory VALUE "data"

ENDCLASS

CLASS MyTableBase INHERIT TTable
  DEFINE DATABASE WITH MyDb CLASS
ENDCLASS
