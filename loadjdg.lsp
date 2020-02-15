;****************************************
; Program : LOADJDG
;           AutoLOAD DJDG
;           By Suk-Jong Yi
;           1997/5/30
;****************************************

;AutoLoad、록

;-------------------------------------------------------
; initialize
;-------------------------------------------------------
(autoload (strcat (prefix) "djdg/ddscl") '("ddscl"))


;-------------------------------------------------------
; [다정다감] 일반도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/SPILE1")  '("SPILE1" ))
(autoload (strcat (prefix) "DJDG/SPILE2")  '("SPILE2" ))
(autoload (strcat (prefix) "DJDG/RCD")     '("RCD"    ))
(autoload (strcat (prefix) "DJDG/ROUND")   '("ROUND"  ))
(autoload (strcat (prefix) "DJDG/SARW")    '("SARW"   ))
(autoload (strcat (prefix) "DJDG/ELM.fas")     '("ELM" "pel" "melm"   ))
(autoload (strcat (prefix) "DJDG/THUN")    '("THUN"   ))
(autoload (strcat (prefix) "DJDG/TRI")     '("TRI"    ))
(autoload (strcat (prefix) "DJDG/PLMARK")  '("PLMARK" ))
(autoload (strcat (prefix) "DJDG/SLOP")    '("SLOP"   ))
(autoload (strcat (prefix) "DJDG/GRNDMRK") '("GRNDMRK"))
(autoload (strcat (prefix) "DJDG/WATERM")  '("WATERM" ))
(autoload (strcat (prefix) "DJDG/SLOPL")   '("SLOPL"  ))
(autoload (strcat (prefix) "DJDG/RTEXT")   '("RTEXT"  ))
(autoload (strcat (prefix) "DJDG/WAVE")    '("WAVE"   ))
(autoload (strcat (prefix) "DJDG/cutpipe") '("cutpipe"   ))  
(autoload (strcat (prefix) "DJDG/cutpipe1") '("cutpipe1"   ))
(autoload (strcat (prefix) "DJDG/MLEAD")   '("MLEAD"  ))
(autoload (strcat (prefix) "DJDG/SHOE")    '("SHOE"   ))
(autoload (strcat (prefix) "DJDG/LEAD.fas")    '("LEAD"  "LEADC" ))
(autoload (strcat (prefix) "DJDG/LEAD2.fas")    '("LEAD2"   ))
(autoload (strcat (prefix) "DJDG/aLEAD.fas")    '("ALEAD"   ))
(autoload (strcat (prefix) "DJDG/mtLEAD")  '("mtLEAD"   ))
(autoload (strcat (prefix) "DJDG/DIRMRK")  '("DIRMRK" ))
(autoload (strcat (prefix) "DJDG/BOR")     '("BOR"    ))
(autoload (strcat (prefix) "DJDG/ALL-ULT") '("ALL-ULT"))
(autoload (strcat (prefix) "DJDG/lslop")   '("lslop"  ))
(autoload (strcat (prefix) "DJDG/brse")    '("brse"   ))
(autoload (strcat (prefix) "DJDG/wflow")   '("wflow"  ))
(autoload (strcat (prefix) "DJDG/north")   '("north"  ))
(autoload (strcat (prefix) "DJDG/boring")  '("boring" ))
(autoload (strcat (prefix) "DJDG/conc")    '("conc"   ))
(autoload (strcat (prefix) "DJDG/sungto")  '("sungto" ))
(autoload (strcat (prefix) "DJDG/rnori.fas")   '("rnori" ))
(autoload (strcat (prefix) "DJDG/pnori")   '("pnori" ))
(autoload (strcat (prefix) "DJDG/noripf")   '("noripf" ))    
(autoload (strcat (prefix) "DJDG/notebox") '("notebox" ))
(autoload (strcat (prefix) "DJDG/jump")    '("jump" ))
(autoload (strcat (prefix) "DJDG/lanemark")    '("lanemark" ))
(autoload (strcat (prefix) "DJDG/pole")    '("pole" ))
  
;-------------------------------------------------------
; [다정다감] 구조물 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/pscbeam") '("pscbeam"))
(autoload (strcat (prefix) "DJDG/pier")    '("pier"))
(autoload (strcat (prefix) "DJDG/spiles")  '("spiles"))
(autoload (strcat (prefix) "DJDG/piles.fas")  '("piles"))
(autoload (strcat (prefix) "DJDG/abut")    '("abut"))
(autoload (strcat (prefix) "DJDG/rwall")   '("ddrwall"))
(autoload (strcat (prefix) "DJDG/brr")     '("brr"))
(autoload (strcat (prefix) "DJDG/brr1")     '("brr1"))
(autoload (strcat (prefix) "DJDG/jung")    '("jung"))
(autoload (strcat (prefix) "DJDG/jung1")   '("jung1"))
(autoload (strcat (prefix) "DJDG/jung2")   '("jung2"))
(autoload (strcat (prefix) "DJDG/bracket") '("bracket"))
(autoload (strcat (prefix) "DJDG/basec")   '("basec"))
(autoload (strcat (prefix) "DJDG/aslab")   '("aslab"))
(autoload (strcat (prefix) "DJDG/aslab1")   '("aslab1"))
(autoload (strcat (prefix) "DJDG/bearing.fas")   '("bearing"))
(autoload (strcat (prefix) "DJDG/dlive.fas")   '("dlive"))
(autoload (strcat (prefix) "DJDG/fanch.fas")   '("fanch"))
(autoload (strcat (prefix) "DJDG/ddrect.fas")   '("ddrect"))
(autoload (strcat (prefix) "DJDG/sheet.fas")   '("sheet"))

;-------------------------------------------------------
; [다정다감] 배근도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/BM1")     '("BM1"))
(autoload (strcat (prefix) "DJDG/BM2")     '("BM2"))
(autoload (strcat (prefix) "DJDG/BM3")     '("BM3"))
(autoload (strcat (prefix) "DJDG/BM4")     '("BM4"))
(autoload (strcat (prefix) "DJDG/BM5")     '("BM5"))
(autoload (strcat (prefix) "DJDG/BM6")     '("BM6"))
(autoload (strcat (prefix) "DJDG/B1")      '("B1"))
(autoload (strcat (prefix) "DJDG/B3")      '("B3"))
(autoload (strcat (prefix) "DJDG/B4")      '("B4"))
(autoload (strcat (prefix) "DJDG/B5")      '("B5"))
(autoload (strcat (prefix) "DJDG/B6")      '("B6"))
(autoload (strcat (prefix) "DJDG/CB")      '("CB"))
(autoload (strcat (prefix) "DJDG/JEON")    '("JEON"))
(autoload (strcat (prefix) "DJDG/BAE")     '("BAE"))
(autoload (strcat (prefix) "DJDG/NAE")     '("NAE"))
(autoload (strcat (prefix) "DJDG/WUE")     '("WUE"))
(autoload (strcat (prefix) "DJDG/SANG")    '("SANG"))
(autoload (strcat (prefix) "DJDG/HA")      '("HA"))
(autoload (strcat (prefix) "DJDG/RBRP")    '("RBRP"))
(autoload (strcat (prefix) "DJDG/RBRF")    '("RBRF"))
(autoload (strcat (prefix) "DJDG/STRP1")   '("STRP1"))
(autoload (strcat (prefix) "DJDG/STRP2")   '("STRP2"))
(autoload (strcat (prefix) "DJDG/RBAR")    '("RBAR"))
(autoload (strcat (prefix) "DJDG/HOOK")    '("HOOK"))
(autoload (strcat (prefix) "DJDG/HBLIST")  '("HBLIST"))
(autoload (strcat (prefix) "DJDG/BAR")     '("BAR" "BARV"))
(autoload (strcat (prefix) "DJDG/SB")      '("SB"))
(autoload (strcat (prefix) "DJDG/SBLOCK")  '("SBLOCK"))
(autoload (strcat (prefix) "DJDG/ARD")     '("ARD"))
(autoload (strcat (prefix) "DJDG/ARSLOP")  '("ARSLOP"))
(autoload (strcat (prefix) "DJDG/RCORNER")  '("RCORNER"))
(autoload (strcat (prefix) "DJDG/ALIGNBAR.FAS")  '("ALIGNBAR"))
(autoload (strcat (prefix) "DJDG/LAP.FAS")  '("LAP" "MLAP"))
(autoload (strcat (prefix) "DJDG/TBAR.FAS")  '("TBAR"))
(autoload (strcat (prefix) "DJDG/TRIM2.FAS")  '("TRIM2" "TRIMS"))
(autoload (strcat (prefix) "DJDG/DREBAR.FAS")  '("DREBAR" "CPDIM" "D2L"))
(autoload (strcat (prefix) "DJDG/FCORNER.FAS")  '("FCORNER"))
(autoload (strcat (prefix) "DJDG/EREBAR.FAS")  '("ER" "ERA"))

;-------------------------------------------------------
; [다정다감] 강재도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/splice")    '("splice"))
(autoload (strcat (prefix) "DJDG/trimb1")    '("trimb1"))
(autoload (strcat (prefix) "DJDG/trimb2")    '("trimb2" "mtrimb"))
(autoload (strcat (prefix) "DJDG/trimb3")    '("trimb3"))
(autoload (strcat (prefix) "DJDG/ff2")       '("ff2"))
(autoload (strcat (prefix) "DJDG/tc15")      '("tc15"))
(autoload (strcat (prefix) "DJDG/weld")      '("weld"))
(autoload (strcat (prefix) "DJDG/gusset")    '("gusset"))
(autoload (strcat (prefix) "DJDG/scallop.fas")    '("scallop"))
(autoload (strcat (prefix) "DJDG/rib.fas")    '("rib" "scallop1" "scallop2"))
(autoload (strcat (prefix) "DJDG/stud.fas")    '("stud"))
(autoload (strcat (prefix) "DJDG/hstiff.fas")    '("hstiff"))
(autoload (strcat (prefix) "DJDG/lstiff.fas")    '("lstiff"))
(autoload (strcat (prefix) "DJDG/mhole.fas")    '("mhole"))
(autoload (strcat (prefix) "DJDG/truss.fas")   '("truss"))

;-------------------------------------------------------
; [다정다감] PSC 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/tdpf")    '("tdpf"))
(autoload (strcat (prefix) "DJDG/anch1.fas")   '("anch1"))
(autoload (strcat (prefix) "DJDG/ahole.fas")   '("ahole"))


;-------------------------------------------------------
; [다정다감] 치수선
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/dh")        '("dh" "dv" "da" "arwh" "ldim" "dss" "dt" "dsm"))
;(autoload (strcat (prefix) "DJDG/dv")        '("dv"))
;(autoload (strcat (prefix) "DJDG/da.fas")        '("da" "ldim"))
(autoload (strcat (prefix) "DJDG/do")        '("do"))
(autoload (strcat (prefix) "DJDG/dc.fas")        '("dc" "dima"))
(autoload (strcat (prefix) "DJDG/nt.fas")        '("nt"))
(autoload (strcat (prefix) "DJDG/dud")       '("dud"))
(autoload (strcat (prefix) "DJDG/cds")       '("cds"))
(autoload (strcat (prefix) "DJDG/ob")        '("ob"))
;(autoload (strcat (prefix) "DJDG/dss")        '("dss"))
(autoload (strcat (prefix) "DJDG/dimdot")        '("dimdot"))
(autoload (strcat (prefix) "DJDG/rmark1")        '("rmark1"))
(autoload (strcat (prefix) "DJDG/dsk")        '("dsk" "ddr"))  
(autoload (strcat (prefix) "DJDG/edim.fas")        '("edim"))  
(autoload (strcat (prefix) "DJDG/pldim.fas")       '("pldim" "pln"))



;-------------------------------------------------------
; [다정다감] 계산서
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/modelm.fas")        '("modelm" "extip" "makeattblock" "insertattblock"))  
(autoload (strcat (prefix) "DJDG/ldd.fas")        '("ldd"))  


;-------------------------------------------------------
; [다정다감] 고치기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/measa")     '("measa"))
(autoload (strcat (prefix) "DJDG/diva")      '("diva"))
(autoload (strcat (prefix) "DJDG/measl")     '("measl"))
(autoload (strcat (prefix) "DJDG/divl")      '("divl"))
(autoload (strcat (prefix) "DJDG/CTH")       '("CTH"))
(autoload (strcat (prefix) "DJDG/tav")       '("tav"))
(autoload (strcat (prefix) "DJDG/tah")       '("tah"))
(autoload (strcat (prefix) "DJDG/bdash")     '("bdash"))
(autoload (strcat (prefix) "DJDG/tbreak")    '("tbreak"))
(autoload (strcat (prefix) "DJDG/txtgap")    '("txtgap"))
(autoload (strcat (prefix) "DJDG/mcht")      '("mcht"))
(autoload (strcat (prefix) "DJDG/ddmcht")    '("ddmcht"))
(autoload (strcat (prefix) "DJDG/linext")    '("linext"))
(autoload (strcat (prefix) "DJDG/txtr")      '("txtr"))
(autoload (strcat (prefix) "DJDG/ffp")       '("ffp"))
(autoload (strcat (prefix) "DJDG/arp")       '("arp"))
(autoload (strcat (prefix) "DJDG/bp")        '("bp"))
(autoload (strcat (prefix) "DJDG/crc")       '("crc"))
(autoload (strcat (prefix) "DJDG/cdd")       '("cdd"))
(autoload (strcat (prefix) "DJDG/ctxt")      '("ctxt" "meq"))
(autoload (strcat (prefix) "DJDG/delshort")  '("delshort"))
(autoload (strcat (prefix) "DJDG/addtext")   '("addtext"))
(autoload (strcat (prefix) "DJDG/rref")      '("rref"))
(autoload (strcat (prefix) "DJDG/mtxt")      '("mtxt"))
(autoload (strcat (prefix) "DJDG/challsty")      '("challsty"))
(autoload (strcat (prefix) "DJDG/pfix.fas")      '("pfix"))

;-------------------------------------------------------
; [다정다감] 도우미
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/tm")        '("tm"))
(autoload (strcat (prefix) "DJDG/bcn")       '("bcn"))
(autoload (strcat (prefix) "DJDG/calt")      '("calt"))
(autoload (strcat (prefix) "DJDG/qtable")    '("qtable"))
(autoload (strcat (prefix) "DJDG/coord")     '("coord"))
(autoload (strcat (prefix) "DJDG/coord2")     '("coord2"))
(autoload (strcat (prefix) "DJDG/coord3")     '("coord3"))
(autoload (strcat (prefix) "DJDG/numcoor")   '("numcoor"))
(autoload (strcat (prefix) "DJDG/2pd")       '("2pd"))
(autoload (strcat (prefix) "DJDG/wlen")      '("wlen" "wcpl" "wcpl2"))
(autoload (strcat (prefix) "DJDG/aplot")     '("aplot"))
(autoload (strcat (prefix) "DJDG/allplot")   '("allplot"))
(autoload (strcat (prefix) "DJDG/allplotb.fas")   '("allplotb" "aq"))
(autoload (strcat (prefix) "DJDG/allplotn.fas")   '("allplotn"))
(autoload (strcat (prefix) "DJDG/allplotx.fas")   '("allplotx"))
(autoload (strcat (prefix) "DJDG/xypline")   '("xypline"))
(autoload (strcat (prefix) "DJDG/table")     '("table"))
(autoload (strcat (prefix) "DJDG/earth")     '("earth"))
(autoload (strcat (prefix) "DJDG/camber")    '("camber"))
(autoload (strcat (prefix) "DJDG/bcross")    '("bcross"))
(autoload (strcat (prefix) "DJDG/mkbdr")     '("mkbdr"))
(autoload (strcat (prefix) "DJDG/plotbdr")   '("plotbdr"))
(autoload (strcat (prefix) "DJDG/wdwgn")     '("wdwgn"))
(autoload (strcat (prefix) "DJDG/bmd")       '("bmd"))
(autoload (strcat (prefix) "DJDG/wbname")    '("wbname"))
(autoload (strcat (prefix) "DJDG/shatch")    '("shatch"))
(autoload (strcat (prefix) "DJDG/solidbk")    '("solidbk"))
(autoload (strcat (prefix) "DJDG/txtfil")    '("txtfil" "textx"))
(autoload (strcat (prefix) "DJDG/gsection")    '("gsection"))
(autoload (strcat (prefix) "DJDG/db24")    '("db24"))
(autoload (strcat (prefix) "DJDG/para")    '("para")) 
(autoload (strcat (prefix) "DJDG/s2kcomp")    '("s2kcomp")) 
(autoload (strcat (prefix) "DJDG/ceh")    '("ceh")) 
(autoload (strcat (prefix) "DJDG/pcacol")    '("pcacol")) 
(autoload (strcat (prefix) "DJDG/bi")    '("bi")) 
(autoload (strcat (prefix) "DJDG/dwgrtxt")    '("dwgrtxt")) 
(autoload (strcat (prefix) "DJDG/plote")    '("plote")) 
(autoload (strcat (prefix) "DJDG/ploteps")    '("ploteps")) 
(autoload (strcat (prefix) "DJDG/sarea.fas")    '("sarea" "cala")) 
(autoload (strcat (prefix) "DJDG/trs")    '("trs"))
(autoload (strcat (prefix) "DJDG/iblk.fas")    '("iblk"))
(autoload (strcat (prefix) "DJDG/sscl.fas")    '("sscl"))
(autoload (strcat (prefix) "DJDG/psnp")    '("aa1" "aa2" "aa3" "setatt"))
(autoload (strcat (prefix) "DJDG/gel.fas")    '("gel"))
(autoload (strcat (prefix) "DJDG/chbdrtxt")    '("chbdrtxt"))
(autoload (strcat (prefix) "DJDG/checkdate")    '("checkdate"))
(autoload (strcat (prefix) "DJDG/ve")    '("ve"))