/*
 *
 */

#include "hbclass.ch"

CLASS OORDBBASE

   METHOD ObjectH

ENDCLASS

#pragma BEGINDUMP

#include "hbapi.h"

HB_FUNC( OORDBBASE_OBJECTH )
{
  PHB_ITEM pSelf = hb_stackSelfItem();
  if( pSelf )
  {
    hb_retptr( hb_arrayId( pSelf ) );
  }
}

#pragma ENDDUMP
