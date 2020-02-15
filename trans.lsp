(defun C:trans1()
  (setq DVBFILE  "c:/program files/autocad 2002/djdg/trans.dvb"  ;CHANGE DVB FILEMNAMEHERE!!!!!!!!!!!!!!!!!!!!
     DVBMACRO "hanja2hangul"  ;CHANGE MACRO NAME HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!
        MSG (strcat "\n" DVBFILE " not found"))

  (setvar "CMDECHO" 0) ;switch off command echo
  (if (findfile DVBFILE) ;if the project file is found
  ;in your AutoCAD search path
    (progn
      (vl-load-com)

      (vl-vbaload (findfile DVBFILE)) ;load the project file
      (vl-vbarun DVBMACRO) ;run the project macro
      ) ;progn
    (princ MSG) ;if project not found, inform the user
    ) ;if
  (command "_VBAUNLOAD" DVBMACRO) ;unload the macro
  (princ) ;finish clean
) ;defun
(princ)