/*
 * $Id: xerror.ch 54 2012-09-19 19:43:28Z tfonrouge $
 */

/*
    xerror.ch - eXtended error manager
*/

#ifndef __XERROR_CH__
#define __XERROR_CH__

/* THROW => generate error */
#xtranslate THROW(<oErr>) => (Eval(ErrorBlock(), <oErr>), Break(<oErr>))

#xcommand RAISE ERROR <cDescription> ;
                                        [ SUBSYSTEM <cSubsystem> ] ;
                                        [ OPERATION <cOperation> ] ;
                                        [ ARGS <aArgs> ] ;
                    => ;
                    Throw( MyErrorNew( [<cSubsystem>], ;
                                                         [<cOperation>], ;
                                                            <cDescription>, ;
                                                         [<aArgs>], ;
                                                            ProcFile(), ;
                                                            ProcName(), ;
                                                            ProcLine() ;
                                                        ) ;
                                )

#xcommand SHOW ERROR <errObj> => ui_ShowError( <errObj> )
#xcommand SHOW WARN <msgWarn> => ui_Alert( <msgWarn> )

/* TField errors */
//#define OODB_ERR__FIELD_METHOD_TYPE_NOT_SUPPORTED       1000
//#define OODB_ERR__CALCULATED_FIELD_CANNOT_BE_SOLVED     1001

/* TTable errors */
//#define OODB_ERR__NO_BASEKEYFIELD                       2000

#xcommand THROW ERROR <errId> [ ON <obj> ] [ ARGS <args,...> ] ;
          => ;
          Throw( OordbErrorNew( iif( <.obj.>, <obj>, Self ), <"errId">, <args> ) )
//          IF ::IsDerivedFrom("TField") ;;
//            __objSendMsg( iif( <.obj.>, <obj>, Self ), "<" + ::Table:ClassName() + ":" + ::Name + ">_" + <"errId">, 0 ) ;;
//          ELSE ;;
//            ::<errId>( <args> ) ;;
//          ENDIF

          //::<errId>( <args> )
          //__objSendMsg( iif( <.obj.>, <obj>, Self ), "__Err_"[ , <args> ], <errId> )
#endif
