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
(autoload (strcat (prefix) "src/ddscl") '("ddscl"))


;-------------------------------------------------------
; [다정다감] 일반도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/SPILE1")  '("SPILE1" ))
(autoload (strcat (prefix) "SRC/SPILE2")  '("SPILE2" ))
(autoload (strcat (prefix) "SRC/RCD")     '("RCD"    ))
(autoload (strcat (prefix) "SRC/ROUND")   '("ROUND"  ))
(autoload (strcat (prefix) "SRC/SARW")    '("SARW"   ))
(autoload (strcat (prefix) "SRC/ELM")     '("ELM" "pel" "melm"   ))
(autoload (strcat (prefix) "SRC/THUN")    '("THUN"   ))
(autoload (strcat (prefix) "SRC/TRI")     '("TRI"    ))
(autoload (strcat (prefix) "SRC/PLMARK")  '("PLMARK" ))
(autoload (strcat (prefix) "SRC/SLOP")    '("SLOP"   ))
(autoload (strcat (prefix) "SRC/GRNDMRK") '("GRNDMRK"))
(autoload (strcat (prefix) "SRC/WATERM")  '("WATERM" ))
(autoload (strcat (prefix) "SRC/SLOPL")   '("SLOPL"  ))
(autoload (strcat (prefix) "SRC/RTEXT")   '("RTEXT"  ))
(autoload (strcat (prefix) "SRC/WAVE")    '("WAVE"   ))
(autoload (strcat (prefix) "SRC/cutpipe") '("cutpipe"   ))  
(autoload (strcat (prefix) "SRC/cutpipe1") '("cutpipe1"   ))
(autoload (strcat (prefix) "SRC/MLEAD")   '("MLEAD"  ))
(autoload (strcat (prefix) "SRC/SHOE")    '("SHOE"   ))
(autoload (strcat (prefix) "SRC/LEAD")    '("LEAD"  "LEADC" ))
(autoload (strcat (prefix) "SRC/LEAD2")    '("LEAD2"   ))
(autoload (strcat (prefix) "SRC/aLEAD")    '("ALEAD"   ))
(autoload (strcat (prefix) "SRC/mtLEAD")  '("mtLEAD"   ))
(autoload (strcat (prefix) "SRC/DIRMRK")  '("DIRMRK" ))
(autoload (strcat (prefix) "SRC/BOR")     '("BOR"    ))
(autoload (strcat (prefix) "SRC/ALL-ULT") '("ALL-ULT"))
(autoload (strcat (prefix) "SRC/lslop")   '("lslop"  ))
(autoload (strcat (prefix) "SRC/brse")    '("brse"   ))
(autoload (strcat (prefix) "SRC/wflow")   '("wflow"  ))
(autoload (strcat (prefix) "SRC/north")   '("north"  ))
(autoload (strcat (prefix) "SRC/boring")  '("boring" ))
(autoload (strcat (prefix) "SRC/conc")    '("conc"   ))
(autoload (strcat (prefix) "SRC/sungto")  '("sungto" ))
(autoload (strcat (prefix) "SRC/rnori")   '("rnori" ))
(autoload (strcat (prefix) "SRC/pnori")   '("pnori" ))
(autoload (strcat (prefix) "SRC/noripf")   '("noripf" ))    
(autoload (strcat (prefix) "SRC/notebox") '("notebox" ))
(autoload (strcat (prefix) "SRC/jump")    '("jump" ))
(autoload (strcat (prefix) "SRC/lanemark")    '("lanemark" ))
(autoload (strcat (prefix) "SRC/pole")    '("pole" ))
  
;-------------------------------------------------------
; [다정다감] 구조물 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/pscbeam") '("pscbeam"))
(autoload (strcat (prefix) "SRC/pier")    '("pier"))
(autoload (strcat (prefix) "SRC/spiles")  '("spiles"))
(autoload (strcat (prefix) "SRC/piles")  '("piles"))
(autoload (strcat (prefix) "SRC/abut")    '("abut"))
(autoload (strcat (prefix) "SRC/rwall")   '("ddrwall"))
(autoload (strcat (prefix) "SRC/brr")     '("brr"))
(autoload (strcat (prefix) "SRC/brr1")     '("brr1"))
(autoload (strcat (prefix) "SRC/jung")    '("jung"))
(autoload (strcat (prefix) "SRC/jung1")   '("jung1"))
(autoload (strcat (prefix) "SRC/jung2")   '("jung2"))
(autoload (strcat (prefix) "SRC/bracket") '("bracket"))
(autoload (strcat (prefix) "SRC/basec")   '("basec"))
(autoload (strcat (prefix) "SRC/aslab")   '("aslab"))
(autoload (strcat (prefix) "SRC/aslab1")   '("aslab1"))
(autoload (strcat (prefix) "SRC/bearing")   '("bearing"))
(autoload (strcat (prefix) "SRC/dlive")   '("dlive"))
(autoload (strcat (prefix) "SRC/fanch")   '("fanch"))
(autoload (strcat (prefix) "SRC/ddrect")   '("ddrect"))
(autoload (strcat (prefix) "SRC/sheet")   '("sheet"))

;-------------------------------------------------------
; [다정다감] 배근도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/BM1")     '("BM1"))
(autoload (strcat (prefix) "SRC/BM2")     '("BM2"))
(autoload (strcat (prefix) "SRC/BM3")     '("BM3"))
(autoload (strcat (prefix) "SRC/BM4")     '("BM4"))
(autoload (strcat (prefix) "SRC/BM5")     '("BM5"))
(autoload (strcat (prefix) "SRC/BM6")     '("BM6"))
(autoload (strcat (prefix) "SRC/B1")      '("B1"))
(autoload (strcat (prefix) "SRC/B3")      '("B3"))
(autoload (strcat (prefix) "SRC/B4")      '("B4"))
(autoload (strcat (prefix) "SRC/B5")      '("B5"))
(autoload (strcat (prefix) "SRC/B6")      '("B6"))
(autoload (strcat (prefix) "SRC/CB")      '("CB"))
(autoload (strcat (prefix) "SRC/JEON")    '("JEON"))
(autoload (strcat (prefix) "SRC/BAE")     '("BAE"))
(autoload (strcat (prefix) "SRC/NAE")     '("NAE"))
(autoload (strcat (prefix) "SRC/WUE")     '("WUE"))
(autoload (strcat (prefix) "SRC/SANG")    '("SANG"))
(autoload (strcat (prefix) "SRC/HA")      '("HA"))
(autoload (strcat (prefix) "SRC/RBRP")    '("RBRP"))
(autoload (strcat (prefix) "SRC/RBRF")    '("RBRF"))
(autoload (strcat (prefix) "SRC/STRP1")   '("STRP1"))
(autoload (strcat (prefix) "SRC/STRP2")   '("STRP2"))
(autoload (strcat (prefix) "SRC/RBAR")    '("RBAR"))
(autoload (strcat (prefix) "SRC/HOOK")    '("HOOK"))
(autoload (strcat (prefix) "SRC/HBLIST")  '("HBLIST"))
(autoload (strcat (prefix) "SRC/BAR")     '("BAR" "BARV"))
(autoload (strcat (prefix) "SRC/SB")      '("SB"))
(autoload (strcat (prefix) "SRC/SBLOCK")  '("SBLOCK"))
(autoload (strcat (prefix) "SRC/ARD")     '("ARD"))
(autoload (strcat (prefix) "SRC/ARSLOP")  '("ARSLOP"))
(autoload (strcat (prefix) "SRC/RCORNER")  '("RCORNER"))
(autoload (strcat (prefix) "SRC/ALIGNBAR")  '("ALIGNBAR"))
(autoload (strcat (prefix) "SRC/LAP")  '("LAP" "MLAP"))
(autoload (strcat (prefix) "SRC/TBAR")  '("TBAR"))
(autoload (strcat (prefix) "SRC/TRIM2")  '("TRIM2" "TRIMS"))
(autoload (strcat (prefix) "SRC/DREBAR")  '("DREBAR" "CPDIM" "D2L"))
(autoload (strcat (prefix) "SRC/FCORNER")  '("FCORNER"))
(autoload (strcat (prefix) "SRC/EREBAR")  '("ER" "ERA"))

;-------------------------------------------------------
; [다정다감] 강재도 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/splice")    '("splice"))
(autoload (strcat (prefix) "SRC/trimb1")    '("trimb1"))
(autoload (strcat (prefix) "SRC/trimb2")    '("trimb2" "mtrimb"))
(autoload (strcat (prefix) "SRC/trimb3")    '("trimb3"))
(autoload (strcat (prefix) "SRC/ff2")       '("ff2"))
(autoload (strcat (prefix) "SRC/tc15")      '("tc15"))
(autoload (strcat (prefix) "SRC/weld")      '("weld"))
(autoload (strcat (prefix) "SRC/gusset")    '("gusset"))
(autoload (strcat (prefix) "SRC/scallop")    '("scallop"))
(autoload (strcat (prefix) "SRC/rib")    '("rib" "scallop1" "scallop2"))
(autoload (strcat (prefix) "SRC/stud")    '("stud"))
(autoload (strcat (prefix) "SRC/hstiff")    '("hstiff"))
(autoload (strcat (prefix) "SRC/lstiff")    '("lstiff"))
(autoload (strcat (prefix) "SRC/mhole")    '("mhole"))
(autoload (strcat (prefix) "SRC/truss")   '("truss"))

;-------------------------------------------------------
; [다정다감] PSC 그리기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/tdpf")    '("tdpf"))
(autoload (strcat (prefix) "SRC/anch1")   '("anch1"))
(autoload (strcat (prefix) "SRC/ahole")   '("ahole"))


;-------------------------------------------------------
; [다정다감] 치수선
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/dh")        '("dh" "dv" "da" "arwh" "ldim" "dss" "dt" "dsm"))
(autoload (strcat (prefix) "SRC/do")        '("do"))
(autoload (strcat (prefix) "SRC/dc")        '("dc" "dima"))
(autoload (strcat (prefix) "SRC/nt")        '("nt"))
(autoload (strcat (prefix) "SRC/dud")       '("dud"))
(autoload (strcat (prefix) "SRC/cds")       '("cds"))
(autoload (strcat (prefix) "SRC/ob")        '("ob"))
(autoload (strcat (prefix) "SRC/dimdot")        '("dimdot"))
(autoload (strcat (prefix) "SRC/rmark1")        '("rmark1"))
(autoload (strcat (prefix) "SRC/dsk")        '("dsk" "ddr"))  
(autoload (strcat (prefix) "SRC/edim")        '("edim"))  
(autoload (strcat (prefix) "SRC/pldim")       '("pldim" "pln"))



;-------------------------------------------------------
; [다정다감] 계산서
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/modelm")        '("modelm" "extip" "makeattblock" "insertattblock"))  
(autoload (strcat (prefix) "SRC/ldd")        '("ldd"))  


;-------------------------------------------------------
; [다정다감] 고치기
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/measa")     '("measa"))
(autoload (strcat (prefix) "SRC/diva")      '("diva"))
(autoload (strcat (prefix) "SRC/measl")     '("measl"))
(autoload (strcat (prefix) "SRC/divl")      '("divl"))
(autoload (strcat (prefix) "SRC/CTH")       '("CTH"))
(autoload (strcat (prefix) "SRC/tav")       '("tav"))
(autoload (strcat (prefix) "SRC/tah")       '("tah"))
(autoload (strcat (prefix) "SRC/bdash")     '("bdash"))
(autoload (strcat (prefix) "SRC/tbreak")    '("tbreak"))
(autoload (strcat (prefix) "SRC/txtgap")    '("txtgap"))
(autoload (strcat (prefix) "SRC/mcht")      '("mcht"))
(autoload (strcat (prefix) "SRC/ddmcht")    '("ddmcht"))
(autoload (strcat (prefix) "SRC/linext")    '("linext"))
(autoload (strcat (prefix) "SRC/txtr")      '("txtr"))
(autoload (strcat (prefix) "SRC/ffp")       '("ffp"))
(autoload (strcat (prefix) "SRC/arp")       '("arp"))
(autoload (strcat (prefix) "SRC/bp")        '("bp"))
(autoload (strcat (prefix) "SRC/crc")       '("crc"))
(autoload (strcat (prefix) "SRC/cdd")       '("cdd"))
(autoload (strcat (prefix) "SRC/ctxt")      '("ctxt" "meq"))
(autoload (strcat (prefix) "SRC/delshort")  '("delshort"))
(autoload (strcat (prefix) "SRC/addtext")   '("addtext"))
(autoload (strcat (prefix) "SRC/addtext1")   '("addtext1"))
(autoload (strcat (prefix) "SRC/rref")      '("rref"))
(autoload (strcat (prefix) "SRC/mtxt")      '("mtxt"))
(autoload (strcat (prefix) "SRC/challsty")      '("challsty"))
(autoload (strcat (prefix) "SRC/pfix")      '("pfix"))

;-------------------------------------------------------
; [다정다감] 도우미
;-------------------------------------------------------
(autoload (strcat (prefix) "SRC/tm")        '("tm"))
(autoload (strcat (prefix) "SRC/bcn")       '("bcn"))
(autoload (strcat (prefix) "SRC/calt")      '("calt"))
(autoload (strcat (prefix) "SRC/qtable")    '("qtable"))
(autoload (strcat (prefix) "SRC/coord")     '("coord"))
(autoload (strcat (prefix) "SRC/coord2")     '("coord2"))
(autoload (strcat (prefix) "SRC/coord3")     '("coord3"))
(autoload (strcat (prefix) "SRC/numcoor")   '("numcoor"))
(autoload (strcat (prefix) "SRC/2pd")       '("2pd"))
(autoload (strcat (prefix) "SRC/wlen")      '("wlen" "wcpl" "wcpl2"))
(autoload (strcat (prefix) "SRC/aplot")     '("aplot"))
(autoload (strcat (prefix) "SRC/allplot")   '("allplot"))
(autoload (strcat (prefix) "SRC/allplotb")   '("allplotb" "aq"))
(autoload (strcat (prefix) "SRC/allplotn")   '("allplotn"))
(autoload (strcat (prefix) "SRC/allplotx")   '("allplotx"))
(autoload (strcat (prefix) "SRC/xypline")   '("xypline"))
(autoload (strcat (prefix) "SRC/table")     '("table"))
(autoload (strcat (prefix) "SRC/earth")     '("earth"))
(autoload (strcat (prefix) "SRC/camber")    '("camber"))
(autoload (strcat (prefix) "SRC/bcross")    '("bcross"))
(autoload (strcat (prefix) "SRC/mkbdr")     '("mkbdr"))
(autoload (strcat (prefix) "SRC/plotbdr")   '("plotbdr"))
(autoload (strcat (prefix) "SRC/wdwgn")     '("wdwgn"))
(autoload (strcat (prefix) "SRC/bmd")       '("bmd"))
(autoload (strcat (prefix) "SRC/wbname")    '("wbname"))
(autoload (strcat (prefix) "SRC/shatch")    '("shatch"))
(autoload (strcat (prefix) "SRC/solidbk")    '("solidbk"))
(autoload (strcat (prefix) "SRC/txtfil")    '("txtfil" "textx"))
(autoload (strcat (prefix) "SRC/gsection")    '("gsection"))
(autoload (strcat (prefix) "SRC/db24")    '("db24"))
(autoload (strcat (prefix) "SRC/para")    '("para")) 
(autoload (strcat (prefix) "SRC/s2kcomp")    '("s2kcomp")) 
(autoload (strcat (prefix) "SRC/ceh")    '("ceh")) 
(autoload (strcat (prefix) "SRC/pcacol")    '("pcacol")) 
(autoload (strcat (prefix) "SRC/bi")    '("bi")) 
(autoload (strcat (prefix) "SRC/dwgrtxt")    '("dwgrtxt")) 
(autoload (strcat (prefix) "SRC/plote")    '("plote")) 
(autoload (strcat (prefix) "SRC/ploteps")    '("ploteps")) 
(autoload (strcat (prefix) "SRC/sarea")    '("sarea" "cala")) 
(autoload (strcat (prefix) "SRC/trs")    '("trs"))
(autoload (strcat (prefix) "SRC/iblk")    '("iblk"))
(autoload (strcat (prefix) "SRC/sscl")    '("sscl"))
(autoload (strcat (prefix) "SRC/psnp")    '("aa1" "aa2" "aa3" "setatt"))
(autoload (strcat (prefix) "SRC/gel")    '("gel"))
(autoload (strcat (prefix) "SRC/chbdrtxt")    '("chbdrtxt"))
(autoload (strcat (prefix) "SRC/checkdate")    '("checkdate"))
(autoload (strcat (prefix) "SRC/ve")    '("ve"))
(autoload (strcat (prefix) "SRC/PREG")    '("PREG"))
(autoload (strcat (prefix) "SRC/MESH")    '("MESHM" "MESHW"))
(autoload (strcat (prefix) "SRC/PPL")    '("PPL"))
