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
(autoload (strcat (prefix) "DJDG/ELM")     '("ELM" "pel" "melm"   ))
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
(autoload (strcat (prefix) "DJDG/LEAD")    '("LEAD"  "LEADC" ))
(autoload (strcat (prefix) "DJDG/LEAD2")    '("LEAD2"   ))
(autoload (strcat (prefix) "DJDG/aLEAD")    '("ALEAD"   ))
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
(autoload (strcat (prefix) "DJDG/rnori")   '("rnori" ))
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
(autoload (strcat (prefix) "DJDG/piles")  '("piles"))
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
(autoload (strcat (prefix) "DJDG/bearing")   '("bearing"))
(autoload (strcat (prefix) "DJDG/dlive")   '("dlive"))
(autoload (strcat (prefix) "DJDG/fanch")   '("fanch"))
(autoload (strcat (prefix) "DJDG/ddrect")   '("ddrect"))
(autoload (strcat (prefix) "DJDG/sheet")   '("sheet"))

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
(autoload (strcat (prefix) "DJDG/ALIGNBAR")  '("ALIGNBAR"))
(autoload (strcat (prefix) "DJDG/LAP")  '("LAP" "MLAP"))
(autoload (strcat (prefix) "DJDG/TBAR")  '("TBAR"))
(autoload (strcat (prefix) "DJDG/TRIM2")  '("TRIM2" "TRIMS"))
(autoload (strcat (prefix) "DJDG/DREBAR")  '("DREBAR" "CPDIM" "D2L"))
(autoload (strcat (prefix) "DJDG/FCORNER")  '("FCORNER"))
(autoload (strcat (prefix) "DJDG/EREBAR")  '("ER" "ERA"))

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
(autoload (strcat (prefix) "DJDG/scallop")    '("scallop"))
(autoload (strcat (prefix) "DJDG/rib")    '("rib" "scallop1" "scallop2"))
(autoload (strcat (prefix) "DJDG/stud")    '("stud"))
(autoload (strcat (prefix) "DJDG/hstiff")    '("hstiff"))
(autoload (strcat (prefix) "DJDG/lstiff")    '("lstiff"))
(autoload (strcat (prefix) "DJDG/mhole")    '("mhole"))
(autoload (strcat (prefix) "DJDG/truss")   '("truss"))

;-------------------------------------------------------
; [다정다감] PSC 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/tdpf")    '("tdpf"))
(autoload (strcat (prefix) "DJDG/anch1")   '("anch1"))
(autoload (strcat (prefix) "DJDG/ahole")   '("ahole"))


;-------------------------------------------------------
; [다정다감] 치수선
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/dh")        '("dh" "dv" "da" "arwh" "ldim" "dss" "dt" "dsm"))
;(autoload (strcat (prefix) "DJDG/dv")        '("dv"))
;(autoload (strcat (prefix) "DJDG/da")        '("da" "ldim"))
(autoload (strcat (prefix) "DJDG/do")        '("do"))
(autoload (strcat (prefix) "DJDG/dc")        '("dc" "dima"))
(autoload (strcat (prefix) "DJDG/nt")        '("nt"))
(autoload (strcat (prefix) "DJDG/dud")       '("dud"))
(autoload (strcat (prefix) "DJDG/cds")       '("cds"))
(autoload (strcat (prefix) "DJDG/ob")        '("ob"))
;(autoload (strcat (prefix) "DJDG/dss")        '("dss"))
(autoload (strcat (prefix) "DJDG/dimdot")        '("dimdot"))
(autoload (strcat (prefix) "DJDG/rmark1")        '("rmark1"))
(autoload (strcat (prefix) "DJDG/dsk")        '("dsk" "ddr"))  
(autoload (strcat (prefix) "DJDG/edim")        '("edim"))  
(autoload (strcat (prefix) "DJDG/pldim")       '("pldim" "pln"))



;-------------------------------------------------------
; [다정다감] 계산서
;-------------------------------------------------------
(autoload (strcat (prefix) "DJDG/modelm")        '("modelm" "extip" "makeattblock" "insertattblock"))  
(autoload (strcat (prefix) "DJDG/ldd")        '("ldd"))  


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
(autoload (strcat (prefix) "DJDG/addtext1")   '("addtext1"))
(autoload (strcat (prefix) "DJDG/rref")      '("rref"))
(autoload (strcat (prefix) "DJDG/mtxt")      '("mtxt"))
(autoload (strcat (prefix) "DJDG/challsty")      '("challsty"))
(autoload (strcat (prefix) "DJDG/pfix")      '("pfix"))

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
(autoload (strcat (prefix) "DJDG/allplotb")   '("allplotb" "aq"))
(autoload (strcat (prefix) "DJDG/allplotn")   '("allplotn"))
(autoload (strcat (prefix) "DJDG/allplotx")   '("allplotx"))
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
(autoload (strcat (prefix) "DJDG/sarea")    '("sarea" "cala")) 
(autoload (strcat (prefix) "DJDG/trs")    '("trs"))
(autoload (strcat (prefix) "DJDG/iblk")    '("iblk"))
(autoload (strcat (prefix) "DJDG/sscl")    '("sscl"))
(autoload (strcat (prefix) "DJDG/psnp")    '("aa1" "aa2" "aa3" "setatt"))
(autoload (strcat (prefix) "DJDG/gel")    '("gel"))
(autoload (strcat (prefix) "DJDG/chbdrtxt")    '("chbdrtxt"))
(autoload (strcat (prefix) "DJDG/checkdate")    '("checkdate"))
(autoload (strcat (prefix) "DJDG/ve")    '("ve"))
(autoload (strcat (prefix) "DJDG/PREG")    '("PREG"))
(autoload (strcat (prefix) "DJDG/MESH")    '("MESHM" "MESHW"))
(autoload (strcat (prefix) "DJDG/PPL")    '("PPL"))
