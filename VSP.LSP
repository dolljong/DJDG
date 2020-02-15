;**************************   PSIN.LSP  ******************************
;**                                                                 **
;**                   SAMWOO ENGINEERING CO., LTD.                  **
;**                      TEL. (02) 3453-8011                        **
;**                                                                 **
;**                          by PARK, J.S                           **
;**                                                                 **
;**                           Version 1.0                           **
;**                                                                 **
;**                       AutoCAD  lisp file.                       **
;**                                                                 **
;*********************************************************************

(defun c:pgset ()
(setq # setq)
(setq @ command)
(setvar "osmode" 0)
(setvar "cmdecho" 0)
(setvaR "blipmode" 0) 
(# sclv nil sclh nil)
(# base_h nil tdis nil)
(# sclh  (getint "\n [ H_SCALE ] 100/200/400/500/1000/1500/3000/5000/User  : <default 500>  : 
"))       
(if (= sclh nil)
    (# sclh 500.0))
(if (= sclh U)
    (# sclh (getreal"\nEnter user H_SCALE : "))) 
(# sclv (getreal  "\n [ V_SCALE ]  100/200/400/500/1000/1500/3000/5000/User : <default 200> : 
"))       
(if (= sclv nil)
    (# sclv 200.0))
(if (= sclv U)
    (# sclv (getreal"\n Enter user V_SCALE : "))) 

(# t_st1 1)
(while t_st1
(# base_h (getreal "\n Enter base Pole height..<m> : <default 0m> : "))
  (if (= base_h nil)
      (progn
           (# t_st1 nil)
           (# base_h 0));progn
      (progn
           (# bs__1 (fix (/ base_h 2)))
           (# bs__2 (/ base_h 2))
           (if (= bs__1 bs__2)
               (# t_st1 nil)));progn
   );if   
 );while

(# scl (/ sclv sclh))
(# tsize (* 2.5 sclv))
(# tdis (getreal "\n Unit chain distance.. <m> : <default 20m> : "))
(if (= tdis nil)
    (# tdis 20))
(@ "layer" "m" "z_el" "c" "g" "" "")
(@ "layer" "m" "z_rl" "c" "m" "" "")
(@ "layer" "m" "z_txt" "c" "c" "" "")
(@ "layer" "m" "z_cl" "c" "y" "" "")
(@ "layer" "m" "z_tl" "c" "r" "" "") 
(@ "layer" "s" "0" "")
(# rail_he (strcase (getstring "\n Write R.L <on> <off> : [default <on>] :")))
(if (= rail_he "")
    (# rail_he "ON"));if
(# fora_he (strcase (getstring "\n Write F.L <on> <off> : [default <on>] :")))
(if (= fora_he "")
    (# fora_he "ON"));if

(# dim-si (* 1 sclv))
(setvar "dimasz" dim-si)
(setvar "dimdli" 0)
(setvar "dimaso" 0)
(setvar "dimclrt" 4)
(setvar "dimclrd" 1)
(setvar "dimclre" 1)
(# dim-si (* 2 sclv))
(setvar "dimcen" dim-si)
(# dim-si (* 1.5 sclv))
(setvar "dimexe" dim-si)
(# dim-sio (* 10.0 sclv))
(setvar "dimexo" dim-sio)
(setvar "dimtad" 1)
(setvar "dimtih" 0)
(setvar "dimtix" 1)
(setvar "dimtofl" 1)
(# dim-si (* 2.5 sclv))
(setvar "dimtxt" dim-si)
(# dim-si (* 1.25 sclv))
(setvar "dimgap" dim-si) 
(# dim-si (/ 1 (* scl 1000)))
(setvar "dimlfac" dim-si)
(princ)
)
;;=================================================
(defun c:pla ()

; ### general form ###
;(setvar "cmdecho" 0)
(@ "layer" "s" "z_tl" "")
(# dis1 nil)

(while (or (= dis1 nil) (<= dis1 0)) 
(# dis1 (getreal "\n Total distance  <m>: "))) ;;while

(# dis1 (* dis1 1000))
(# dis (* dis1 scl))
(# fix1 (getpoint "\n Start point:"))
(# test_p 1)
(while test_p
   (# gpj2 (getint "\n Enter Number of table..  <1-8>: ")) 
   (if (and (>= gpj2 1) (<= gpj2 8))
      (# test_p nil)  
   );if
);while
(princ)
(prompt "\nLine drawing......<<Park J. s.>>......")
(# dx1 (car fix1))
(# dy1 (cadr fix1))
(# gpj1 (* 60 sclv))     ;;;60->50
(# dx2 (+ dx1 gpj1 dis))
(# fix2 (list dx2 dy1))
(@ "line" fix1 fix2 "")
(# ss1 (entlast))

(# arr_15 (* 15 sclv))
(@ "array" ss1 "" "r" (+ gpj2 1) "" arr_15)
(# bbw (entlast))
(# gpj3 (* 5 sclv))
(# gpj4 (* gpj2 arr_15))
(# fix3 (list dx1 (+ dy1 gpj4)))
(@ "line" fix1 fix3 "")
(# fix5 (+ (car fix1) (* 40 sclv)))
(# fix6 (list fix5 (cadr fix1)))
(# fix7 (list fix5 (cadr fix3)))
(@ "line" fix6 fix7 "") 
(# fix4 (list dx2 (+ dy1 gpj4)))
(@ "line" fix2 fix4 "")

(# con_y1 (+ dy1 (* gpj2 arr_15) gpj3))
(# con_1 (list fix5 con_y1))
(# con_2 (list dx2 con_y1))
(@ "line" con_1 con_2 "")
(@ "change" "L" "" "P" "la" "z_el" "")


(@ "layer" "s" "z_cl" "")
(# llx (+ dx1 (* 50 sclv)))
(# ll1 (list llx dy1))

(# juu1 (* 0.75 sclv))
(# juu2 (* 1.5 sclv))
(# juu1_1 (rtos juu1))
(# Juu2_1 (rtos juu2))
(# juu3 (strcat "@" juu1_1 "<90"))
(# juu4 (strcat "@" juu2_1 "<90"))

(@ "line" ll1 juu3 "")
(# ay1 (entlast))

(# juuu (* 15 sclv))
(# juu5 (- juuu juu1))
(# juu6 (+ juu5 juuu))
(# juu7 (+ juu6 juuu))
(# juu8 (+ juu7 juuu))
(# juu9 (+ juu8 juuu))
(# juu10 (+ juu9 juuu))
(# juu11 (+ juu10 juuu))
(# juu12 (+ juu11 juuu))

(# ll2 (list llx (+ dy1 juu5)))
(# ll3 (list llx (+ dy1 juu6))) 
(# ll4 (list llx (+ dy1 juu7)))
(# ll5 (list llx (+ dy1 juu8))) 
(# ll6 (list llx (+ dy1 juu9)))
(# ll7 (list llx (+ dy1 juu10)))
(# ll8 (list llx (+ dy1 juu11)))
(# ll9 (list llx (+ dy1 juu12)))

(if (<= 2 gpj2) 
     (progn      (@ "line" ll2 juu4 "")
                 (# ay2 (entlast))))
(if (<= 3 gpj2) 
     (progn      (@ "line" ll3 juu4 "")
                 (# ay3 (entlast))))
(if (<= 4 gpj2) 
     (progn      (@ "line" ll4 juu4 "")
                 (# ay4 (entlast))))
(if (<= 5 gpj2) 
     (progn      (@ "line" ll5 juu4 "")
                 (# ay5 (entlast))))
(if (<= 6 gpj2) 
     (progn      (@ "line" ll6 juu4 "")
                 (# ay6 (entlast))))
(if (<= 7 gpj2) 
     (progn      (@ "line" ll7 juu4 "")
                 (# ay7 (entlast))))
(if (<= 8 gpj2) 
     (progn      (@ "line" ll8 juu4 "")
                 (# ay8 (entlast))))
(if (<= 9 gpj2) 
     (progn      (@ "line" ll9 juu4 "")
                 (# ay9 (entlast))))

(cond 
    ((= 1 gpj2)  (@ "line" ll2 juu3 ""))
    ((= 2 gpj2)  (@ "line" ll3 juu3 ""))
    ((= 3 gpj2)  (@ "line" ll4 juu3 ""))
    ((= 4 gpj2)  (@ "line" ll5 juu3 ""))
    ((= 5 gpj2)  (@ "line" ll6 juu3 ""))
    ((= 6 gpj2)  (@ "line" ll7 juu3 ""))
    ((= 7 gpj2)  (@ "line" ll8 juu3 ""))
    ((= 8 gpj2)  (@ "line" ll9 juu3 ""))
);;cond    
(# aa_la (entlast))
         

(# gpj5 (* tdis 1000))
(# hf1 (fix (+ 1 (/ dis1 gpj5))))
(# aryd (* gpj5 scl))

(cond 
    ((= 1 gpj2)  
     (@ "array" ay1  aa_la "" "R" "" hf1 aryd))
    ((= 2 gpj2)  
     (@ "array" ay1 ay2 aa_la "" "R" "" hf1 aryd))
    ((= 3 gpj2)  
     (@ "array" ay1 ay2 ay3 aa_la "" "R" "" hf1 aryd))
    ((= 4 gpj2)  
     (@ "array" ay1 ay2 ay3 ay4 aa_la "" "R" "" hf1 aryd))
    ((= 5 gpj2)  
     (@ "array" ay1 ay2 ay3 ay4 ay5 aa_la "" "R" "" hf1 aryd))
    ((= 6 gpj2)  
     (@ "array" ay1 ay2 ay3 ay4 ay5 ay6 aa_la "" "R" "" hf1 aryd))
    ((= 7 gpj2)  
     (@ "array" ay1 ay2 ay3 ay4 ay5 ay6 ay7 aa_la "" "R" "" hf1 aryd))
    ((= 8 gpj2)  
     (@ "array" ay1 ay2 ay3 ay4 ay5 ay6 ay7 ay8 aa_la "" "R" "" hf1 aryd))
);;cond

;; =========  insert =======
(@ "layer" "s" "0" "")
;(if (= gpj2 1)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H1" fix1 sclv sclv "")) 
;(if (= gpj2 2)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H2" fix1 sclv sclv "")) 
;(if (= gpj2 3)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H3" fix1 sclv sclv "")) 
;(if (= gpj2 4)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H4" fix1 sclv sclv "")) 
;(if (= gpj2 5)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H5" fix1 sclv sclv "")) 
;(if (= gpj2 6)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H6" fix1 sclv sclv "")) 
;(if (= gpj2 7)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H7" fix1 sclv sclv "")) 
;(if (= gpj2 8)
;    (@ "insert" "c:\\acad12\\utility\\lib\\BLK_H8" fix1 sclv sclv "")) 
;(# phsuk (entlast))
;(@ "explode" phsuk)

; ### station ###
(setvar "lunits" 2)
(setvar "luprec" 3)
(@ "layer" "s" "z_txt" "")
(# st11 (getint "\n Starting station <km> .... <default:0> :"))
(if (= st11 nil)
    (# st11 0))
(# st22 (getint "\n Starting station <m>.... <default:0> : "))
(if (= st22 nil)
    (# st22 0))
(# st1 (rtos st11 2 0))
(# st1 (strcat st1 "K" "M"))
(# st2 (rtos st22 2 0))
(if (= st22 0)
    (# st2 "000"))

(# tch_1 (* 50 sclv))
(# tch_2 (* 1.25 sclv))
(# tch_3 (* 4 sclv))
(# tch_4 (* tch_2 2))
(# tch_5 (* 4 sclv))
(# tch_6 (* tdis 1000))
(# tch_7 (* 0.5 sclv))


(# ttx1 (+ dx1 tch_1 tch_2))
(# tty1 (+ dy1 tch_3))
(# ttxy (list ttx1 tty1))
(# st_len (strlen st2))
(if (= st_len 4)
           (# ttxy (list (car ttxy) (- (cadr ttxy) (* 0.9 sclv)))))  
(@ "text" ttxy tch_4 "90" st2) 
(# ttxy1 (list (- ttx1 tch_5) (- tty1 tch_7)))
(# st_len (strlen st1))
(if (= st_len 4)
           (# ttxy1 (list (car ttxy1) (- (cadr ttxy1) (* 0.9 sclv)))))  
(@ "text" ttxy1 tch_4 "90" st1)
(princ)
(prompt "\nText writing.....<<Park J. s.>>")
(repeat (- hf1 1)
(# tx_00 (- 1000 tdis))
(if (or (= st22 tx_00) (= st22 -20)) 
(progn 
  (# st22 0)
  (# st2 "000")
  (# ttx1 (+ ttx1 (* tch_6 scl)))
  (# ttxy (list ttx1 tty1))
  (@ "text" ttxy tch_4 "90" st2)
  
  (# st11 (+ st11 1))
  (# st1 (rtos st11 2 0))
  (# st1 (strcat st1 "K" "M"))
  (# st_len (strlen st1))
  (# ttxy1 (list (- ttx1 tch_5) (- tty1 tch_7)))
  (if (= st_len 4)
           (# ttxy1 (list (car ttxy1) (- (cadr ttxy1) (* 0.9 sclv)))))  
  (@ "text" ttxy1 tch_4 "90" st1)
 );progn
(progn
  (# st22 (+ st22 tdis))
  
  (if (and (> st22 -100) (< st22 0))
        (# st2 (rtos st22 2 0)))
  (if (and (< st22 100) (> st22 0))
        (# st2 (strcat "0" (rtos st22 2 0))))
  (if (or (<= st22 -100) (>= st22 100))  
        (# st2 (rtos st22 2 0)))

  (# ttx1 (+ ttx1 (* tch_6 scl)))
  (# ttxy (list ttx1 tty1)) 
  (# st_len (strlen st2))
  (if (= st_len 4)
           (# ttxy (list (car ttxy) (- (cadr ttxy) (* 0.9 sclv)))))  
  (@ "text" ttxy tch_4 "90" st2) 
  );progn
 );if  
);repeat
(princ)
;=======================================================================
;;---------------> pole
(# pol_tcc 10)  ;;text between <m>
(# pol_tc (* 1000 pol_tcc)) 
(# p_dis (* 22 sclv))

(# pol_bs con_1)  ;;;  table -> base point
(# pol_bsm base_h)

;;(# pol_bs (getpoint "\n Pick base point of pole : "))
;;(# tet_s 1)
;;(while tet_s
;;   (# pol_bsm (getint "\n Enter height base_pole <m> : "))
;;   (# tet_1 (/ pol_bsm 2.0))
;;   (# tet_2 (fix tet_1))
;;   (if (= tet_2 tet_1)
;;       (# tet_s nil))  ;if  
;; );while

(# pol_txt (* pol_bsm 1000))
(# tet_s1 1)
(while tet_s1
    (# pol_tl (getint "\n Enter Total length of pole : "))
    (# tet_3 (/ pol_tl))
    (# tet_4 (fix tet_3))
    (if (= tet_4 tet_3)
        (# tet_s1 nil)) ;if
 );while        

;;(# pol_ut (getint "\n Enter UNIT length of pole : "))
  (# pol_ut 2)

(# pol_uts (* pol_ut 1000))
(# pol_tls (* pol_tl 1000))
(# pol_w sclv) 
(# pf1 (list (- (car pol_bs) sclv) (cadr pol_bs)))
(# pf2 (list (+ (car pol_bs) sclv) (cadr pol_bs)))
(# pf1_f (list (car pf1) (+ (cadr pf1) pol_tls)))
(# pf2_f (list (car pf2) (+ (cadr pf1) pol_tls)))
(# pf_c (list (car pol_bs) (+ (cadr pol_bs) pol_tls)))
(@ "layer" "s" "z_cl" "")
(@ "line" pf1 pf2 pf2_f pf1_f "c")
(@ "line" pol_bs pf_c "")

(# kal_1 (fix (/ pol_bsm 10)))
(# kal_2 (+ (* kal_1 10) 10))
(# kal_3 (- kal_2 pol_bsm))   ;;;; ex) 8m
(# kal_4 (* kal_3 1000))   ;;; ex) 8000mm

(# p_arrb (list (- (car pf2) p_dis) (+ (cadr pf2) kal_4))) 
(# p_arrc (list (car pf2) (+ (cadr pf2) kal_4)))
(@ "line" p_arrc p_arrb "") 

(# p_ssa (entlast))
(# p_rof (fix (/ pol_tl 10)))
(if (< base_h 0)
    (progn
          (# yhn (fix (/ (abs base_h) 10)))
          (# p_rof (- p_rof yhn))
    );progn
 );;if   
      
(@ "array" p_ssa "" "" p_rof "" "10000")

(# t_siz (* 2.5 sclv))    ;;;;text height
(# tbx1 (car pol_bs))
(# tby1 (cadr pol_bs))
(# ttbx (* 17 sclv))
(# ttby (* 1 sclv))
(# tbxy (list (- tbx1 ttbx) (+ tby1 ttby kal_4)))
(# pol_bb (+ pol_bsm kal_3))
(@ "layer" "s" "z_txt" "")
(# ttbx_3 (* 2.2 sclv))  ;;;;<<3section>> 2.2
(repeat p_rof
       (if (= pol_bb 100)
           (# tbxy (list (- (car tbxy) ttbx_3) (cadr tbxy))));;if
       (# pol_ttt (rtos pol_bb 2 2))
       (@ "text" tbxy t_siz "0" pol_ttt)
       (# pol_bb (+ pol_bb pol_tcc))
       (# tbxy (list (car tbxy) (+ (cadr tbxy) pol_tc))) 
  );repeat  
;       =========
;;;     = solid =
;       =========
(@ "layer" "s" "z_cl" "")
(# sodc pol_bs)
(# na12 (* pol_ut 2))
(# sod1 (list (- (car sodc) sclv) (cadr sodc)))
(# sod2 (list (+ (car sodc) sclv) (cadr sodc)))
(# sod3 (list (car sod1) (+ (cadr sod1) pol_uts)))
(# sod4 (list (car sod2) (+ (cadr sod1) pol_uts)))
(# sodcc (list (car sodc) (+ (cadr sodc) pol_uts))) 
(# pl_bs pol_bsm)
(# fix_r (fix (/ pol_tl pol_ut)))
(repeat fix_r 
     (@ "line" sod3 sod4 "")
     (# n12n (rem pl_bs na12))  
     (if (= n12n 0)
                  (@ "solid" sod1 sodc sod3 sodcc ""))
     (if (/= n12n 0) 
                  (@ "solid" sod2 sodc sod4 sodcc ""))
     (# sodc sodcc)
     (# sod1 sod3)
     (# sod2 sod4)
     (# sod3 (list (car sod1) (+ (cadr sod1) pol_uts)))
     (# sod4 (list (car sod2) (+ (cadr sod1) pol_uts)))
     (# sodcc (list (car sodc) (+ (cadr sodc) pol_uts))) 
     (# pl_bs (+ pl_bs pol_ut))
  );repeat         
(princ)
);;defun

;;;============================
(defun c:pl1 ()
(setvar "cmdecho" 0)
(setvar "osmode" 512)
(setq # setq)
(setq @ command)
(# b2 (getpoint "\n pick base point of G.L : "))
(# base_pc nil)
(# base_pc (getreal "\n Input base line height..<m> : <default 0m> : "))
(if (= base_pc nil)
    (# base_pc 0))
(# stp 1 gxy2 nil)
(setvar "osmode" 32)
(# b1 (getpoint "\n Pick base point <text>:"))
(# th_1 (* 1.25 sclv))   ;;250
(# th_2 (* 2  sclv))   ;;400
(# th_3 (* 2.5 sclv))    ;;500

(# p_if 1)
(while stp
(# t0 (getreal "\n Input the Ground hight <m>:"))
(# base_h1 (- t0 base_pc)) 
(# t1 (* 1000 t0))
(# t2 (rtos t0 2 2))
(setvar "osmode" 0)
(# bx1 (+ th_1 (car b1)))
(# by1 (+ th_2 (cadr b1)))
(# bxy (list bx1 by1))

(# t0_if (fix t0))
(# t0_if1 (* sclv 1.15))     
(if (>= t0_if 100)
    (# bxy (list bx1 (- by1 t0_if1))))

(@ "layer" "s" "z_txt" "")
(@ "text" bxy th_3 "90" t2)

(# ggxx gxy2)
(# gy1 (cadr b2))
(# gxy1 (list (car b1) gy1))
(# base_h1 (* 1000 base_h1))
(# gy2 (+ (cadr b2) base_h1))
(# gxy2 (list (car b1) gy2))
(@ "layer" "s" "z_cl" "")
(@ "line" gxy1 gxy2 "")
(@ "layer" "s" "z_el" "")
(if (/= p_if 1)
      (@ "line" ggxx gxy2 ""))
(# b1 (list (+ (car b1) (* (* tdis 1000) scl)) (cadr b1)))
(# p_if 2)
(# sst 1)
(# sst (strcase (getstring "\n Continuous <Yes> or <No> ....<Y> : << default <Y> >> :  ")))
(if (= sst "N") (# stp nil))
);;while
(princ)
);end

;;;=====================================
(defun c:ral1 ()

(# RTH_1 (* 1.25 SCLV)) ;;250
(# RTH_2 (/ (* 15 SCLV) 2)) ;;1500
(# RTH_3 (* 2.5 SCLV)) ;;500
(# RTH_4 (* 3 SCLV)) ;;600
(# RTH_5 (* 11 SCLV)) ;;2200
(# RTH_6 (* 25 SCLV)) ;;5000
(# RTH_7 (* 6.4 SCLV)) ;;1280
(# RTH_8 (* 30 SCLV)) ;;6000
(# RTH_9 (* TDIS 1000)) ;;20000

(setvar "cmdecho" 0)
(setvar "osmode" 32)
(setq # setq)
(setq @ command)
(@ "layer" "s" "z_cl" "")
(# p1 (getpoint "\nPick first point...<int of>:"))
(# h1 (getreal "\nInput first rail level:"))
(setvar "osmode" 0)
(# px1 (car p1))
(# py1 (+ (cadr p1) RTH_2))
(# pc1 (list px1 py1))
(# pxt1 (+ px1 RTH_1))
(# pyt1 (+ (cadr p1) RTH_1))
(# pt1 (list pxt1 pyt1))
(# hh1 (rtos h1 2 3))
(@ "text" pt1 RTH_3 90 hh1)
(@ "change" "l" "" "p" "la" "z_txt" "")
(@ "circle" pc1 RTH_2)

(setvar "osmode" 32)
(# p2 (getpoint "\nPick second point...<int of>:"))
(# h2 (getreal "\nInput second rail level:"))
(setvar "osmode" 0)
(# px2 (car p2))
(# py2 (+ (cadr p2) RTH_2))
(# pc2 (list px2 py2))
(# pxt2 (+ px2 RTH_1)) 
(# pyt2 (+ (cadr p2) RTH_1))
(# pt2 (list pxt2 pyt2))
(# hh2 (rtos h2 2 3))
(@ "text" pt2 RTH_3 "90" hh2)
(@ "change" "l" "" "P" "la" "z_txt" "")
(@ "circle" pc2 RTH_2)

(setvar "osmode" 0)
; ### line ###
(# pxx1 (+ px1 RTH_2))
(# wp1 (list pxx1 py1))
(# pxx2 (- px2 RTH_2))
(# wp2 (list pxx2 py2))
(@ "line" wp1 wp2 "")

; ### text distance ###
(@ "layer" "s" "z_txt" "")
(# dp (- px2 px1))
(# dp5 (/ (/ dp 1000) scl))
(# dp1 (rtos dp5 2 3))
(# dp1 (strcat "L=" dp1 "M"))
(# dpp (- (+ (/ dp 2) px1) RTH_5))
(# dpp1 (+ (cadr p1) RTH_4))
(# ddpp (list dpp dpp1))
(@ "text" ddpp RTH_4 "0" dp1)

;; ### slope determine ###
(# dh1 (- h2 h1)) 
(# sd19 (* (/ dh1 dp5) 1000))
(# sd0 (rtos sd19 2 1))
(# sd1 (strcat "S=" sd0 "%")) 
(# sd2 (strcat "S=+" sd0 "%"))
(# sdd1 (- (+ (/ dp 2) px1) RTH_6))
(# sdd2 (+ dpp1 RTH_7))
(# sdd3 (list sdd1 sdd2))
(if (< dh1 0) 
   (@ "text" sdd3 RTH_4 "0" sd1)
   (@ "text" sdd3 RTH_4 "0" sd2)
   );if

;;#### pamilli ####
(# sdd3_7 (list (+ sdd1 (* 21.498 sclv)) (+ sdd2 (* 0.5 sclv))))
(# sdd3_8 (list (+ sdd1 (* 24.257 sclv)) (+ sdd2 (* 0.5 sclv))))
(# cnt_r (* 0.5 sclv))
(if  (or (= (strlen sd1) 7) (= (strlen sd2) 7))
     (@ "circle" sdd3_7 cnt_r))
(if  (or (= (strlen sd1) 8) (= (strlen sd2) 8))
     (@ "circle" sdd3_8 cnt_r))

;; ### -h ###
(# dhh (abs dh1))
(# dhh (rtos dhh 2 3))
(# dht (strcat "H=" dhh "M"))
(# dhtx (list (+ sdd1 RTH_8) sdd2))
(@ "text" dhtx RTH_4 "0" dht)

;--------------------------------------------------------------------
;; ### rail text 20m ###
(if (= rail_he "ON")
(progn
(setvar "osmode" 512)
(# bar1 (getpoint "\n Pick base line of R.L <nea of>:"))
(setvar "osmode" 0)
(# bary1 (+ (cadr bar1) RTH_1)) 
(# hj1 (rtos h1 2 3)) 
(# barxy (list pxt1 bary1))
(@ "text" barxy RTH_3 "90" hj1)
(# hpjs (/ (* sd19 TDIS) 1000))
(# hpjss hpjs)
;;;; <<chain distance>> 
(# wooka 1 cd1 nil)
(while wooka
  (# cd1 (getreal "\nChain distance ..(Right)....<m>: "))
  (if (/= cd1 nil)
    (progn
         (# cd2 (* cd1 1000))
         (# cd3 (* cd2 scl))
         (# pxt1 (+ pxt1 cd3))
         (# barxy (list pxt1 bary1))
         (# hhq (/ (* hpjss cd1) TDIS))
         (# hSW1 (+ h1 hhq))
         (# hj1 (rtos hSW1 2 3))
         (@ "text" barxy RTH_3 "90" hj1)
         (# wooka nil)
     );progn
   );if
 );while
;;;; <<Chain distance -END->>
;; === rrp ===
(# rrpp1 (- dp (* cd2 scl)))
(# rrpp2 (fix (/ rrpp1 (* RTH_9 scl)))) 
(# rrpp3 (rem rrpp1 (* RTH_9 scl)))
(# rrpp4 (/ rrpp3 1000))
;; === rrp end ===
(repeat rrpp2
   (# hj1 (+ hsw1 hpjs))   
   (# hj1 (rtos hj1 2 3))
   (# pxt1 (+ pxt1 (* RTH_9 scl)))
   (# barxy (list pxt1 bary1)) 
   (@ "text" barxy RTH_3 "90" hj1) 
   (# hpjs (+ hpjs hpjss))
   );;repeat

;; << chain distance after >>
(if (/= cd1 nil)
  (progn
  (# hj1 (rtos h2 2 3))
  (# pxt1 (+ pxt1 rrpp3))
  (# barxy (list pxt1 bary1))
  (@ "text" barxy RTH_3 "90" hj1)
  );progn
);if
;; << chain distance after [END] >>
);progn
);if (rail level)

;; ### formation height ###
(if (= fora_he "ON")
(progn
(setvar "osmode" 512)
(# bar1 (getpoint "\n Pick base line of F.L <nea of>:"))
(setvar "osmode" 0)
(# bary1 (+ (cadr bar1) RTH_1)) 
(# rrp1 (fix (/ dp (* RTH_9 scl)))) 
(# h1 (- h1 0.55))
(# hj1 (rtos h1 2 3)) 
(# ppxx1 (+ (car p1) RTH_1))
(# parxy (list ppxx1 bary1))
(@ "text" parxy RTH_3 "90" hj1)
(# hpjs (/ (* sd19 TDIS) 1000))
(# hpjss hpjs)
;;;<<Chain distance>>
  (if (/= cd1 nil)
    (progn
    (# ppxx1 (+ ppxx1 cd3))
    (# parxy (list ppxx1 bary1))
    (# hhp (/ (* hpjss cd1) TDIS))
    (# hjpp1 (+ h1 hhp))
    (# hj1 (rtos hjpp1 2 3))
    (@ "text" parxy RTH_3 "90" hj1)
    );progn
  );if
;;<<Chain distance  [END] >>
(repeat rrpp2
   (# hj1 (+ hjpp1 hpjs))   
   (# hj1 (rtos hj1 2 3))
   (# ppxx1 (+ ppxx1 (* RTH_9 scl)))
   (# parxy (list ppxx1 bary1)) 
   (@ "text" parxy RTH_3 "90" hj1) 
   (# hpjs (+ hpjs hpjss))
   );;repeat

;; << chain distance after >>
(if (/= cd1 nil)
  (progn
  (# h2 (- h2 0.55))
  (# hj1 (rtos h2 2 3))
  (# ppxx1 (+ ppxx1 rrpp3))
  (# barxy (list ppxx1 bary1))
  (@ "text" barxy RTH_3 "90" hj1)
  );progn
);if
;; << chain distance after [END] >>
);progn
(progn
       (# h2 (- h2 0.55))
       (# h1 (- h1 0.55))
);progn
);if (formation height)

;-------------------------------------------------------------------


;; ### formation line ###
(# h1 (+ h1 (* 0.02 SCLV)))
(# h2 (+ h2 (* 0.02 SCLV)))
(# hbb1 (+ (* (- h1 base_h) 1000) (cadr p1)))
(# hbb2 (car p1))
(# hbb3 (list hbb2 hbb1))
(# hbh1 (+ (* (- h2 base_h) 1000) (cadr p1)))
(# hbh2 (car p2))
(# hbh3 (list hbh2 hbh1))
(@ "line" hbb3 hbh3 "")
(@ "change" "l" "" "p" "la" "z_rl" "")
(@ "copy" "p" "" hbb3 "@550<90")
(@ "change" "p" "" "p" "la" "z_el" "")
;; ### formation line [END] ###
(princ)
);defun
;end
                 





;;====================================================
;;This program is vertical curve
;;date : 1994. 10. 15
;;Programmer : Park jong seok
;;===================================================

(defun c:vcc()

(setvar "osmode" 32)
(# p_base (getpoint "\n Pick rail level intersection point <int of> : "))
(setvar "osmode" 0)
(# slope_f (getreal "\nInput first rail slope <left> : "))
(# slope_l (getreal "\nInput second rail slope <right> : "))
(# r_1 3000) ;  <<define>>
(# slo_f (/ slope_f 1000.0)) 
(# slo_l (/ slope_l 1000.0))
(# sp_ca (abs (- slo_f slo_l)))

(if (> sp_ca 0.005)    ;;;;;; N.G ==> Program endding
 (progn

 (# len_s (* (/ r_1 2.0) sp_ca))    ; L value
 (# len_d (fix len_s))
 (if (< len_d len_s)
    (progn   (# len_d (+ len_d 1))
             (# r_1 (fix (/ (* 2 len_d) sp_ca)))
             (# r_1 (+ r_1))    
    );progn
 );if
 (# len_dd (* (* len_d scl) 1000))

 ;;============= sortting ===============
 (# p 0 gro_g nil)        
 (# gro_s (ssget'((8 . "z_cl")))) 

 (# hk1 (car p_base))
(repeat (sslength gro_s)
        (# oo1 (cadr (assoc 10 (entget (ssname gro_s p)))))
        (if (/= hk1 oo1) 
               (# gro_g (cons oo1 gro_g)))
        (# p (+ p 1))
);repeat             
(# gro_g (cons hk1 gro_g))

(# n 0 tt nil)
(repeat (length gro_g)
        (# mx (apply 'max gro_g))
        (# tt (cons mx tt))
        (# ee1 (cdr (member mx gro_g)))
        (# ee2 (cdr (member mx (reverse gro_g))))
        (# gro_g (append ee1 ee2))
);repeat           
(# gro_g tt)
(# p_bx (car p_base))
(# p_by (cadr p_base))
(# pq11 (- p_bx len_dd)) 
(# pq22 (+ p_bx len_dd)) 
(# pq1_f (list pq11 p_by)) 
(# pq1_s (list pq22 p_by)) 
(# pq_ff pq1_f)

(# tx_ga (* 32.5 sclv))   ;;dim_sio=2.5->10==+7.5  ,  tx_ga 25->32.5   
(# p 0 cout 0)
(repeat (length gro_g)
        (# a1 (nth p gro_g))
        (# a2 (abs (- a1 p_bx)))
        (# p (+ p 1))
        (if (< a2 len_dd)
            (progn
                 (# a3 (list a1 p_by)) 
                 (# tx_ga1 (list p_bx (- p_by tx_ga)))   
                 (@ "dim" "hor" pq_ff a3 tx_ga1 "" "e")
                 (# pq_ff a3)
            );progn
         );if
);repaet                   
(@ "dim" "hor" pq_ff pq1_s tx_ga1 "" "e") 
;;;;===========================  radius
(setvar "clayer" "z_txt")
(# r_he (* 2.5 sclv))
(# pp_rx (- p_bx (* 7.5 sclv)))
(# pp_rY (- (- p_by tx_ga) (* 5 sclv)))
(# pp_rxy (list pp_rx pp_ry))
(# red_t (strcat "R" "=" (rtos r_1 2 0))) 
(@ "text" pp_rxy r_he  "0" red_t)   
(# hh_h (* 2.5 sclv))   ;;text height
(# hh_g (* 3.5 sclv))
(# hh_g1 (* 1.0 sclv))
(# m_y (+ (- p_by tx_ga) (* 7.5 sclv)))
(# m_st (+ m_y (* 3.0 sclv)))
(# m_sts (list (+ pq11 hh_g) m_st))
(# m_ste (list (- pq22 hh_g1) m_st))
(@ "text" m_sts hh_h "90" "0.000")
(@ "text" m_ste hh_h "90" "0.000")
(# p 0)
(repeat (length gro_g)
        (# a1 (nth p gro_g))
        (# a2 (abs (- a1 p_bx)))
        (# p (+ p 1))
        (if (and (< a2 len_dd) (< a1 p_bx))
            (progn
                (# l_t (/ (- a1 pq11) (* 1000 scl)))
                (# t_y (/ (* l_t l_t) (* 2 r_1)))
                (# t_y1 (rtos t_y 2 3)) 
                (if (< slope_f slope_l)
                    (# t_y1 (strcat "+" t_y1));if
                    (# t_y1 (strcat "-" t_y1)));if
                (# xx1 (list (+ a1 hh_g) m_y))     
                (@ "text" xx1 hh_h "90" t_y1)    
            );progn
        );;if    

        (if (and (< a2 len_dd) (>= a1 p_bx))
            (progn
                (# l_t (/ (- a1 pq22) (* 1000 scl)))
                (# t_y (/ (* l_t l_t) (* 2 r_1)))
                (# t_y1 (rtos t_y 2 3)) 
                (if (< slope_f slope_l)
                    (# t_y1 (strcat "+" t_y1));if
                    (# t_y1 (strcat "-" t_y1)));if
                (# xx1 (list (- a1 hh_g1) m_y))     
                (@ "text" xx1 hh_h "90" t_y1)    
            );progn
        );;if    
   );;repeat
 );progn ;;;; N.G   
(prompt " Need not vortical curve : <<  (first slope - second slope) < 0.005  >>\n")
);if
(prin1)
);defun
;=======================================================================
;;---------------> pole
(defun c:pol()

(# pol_tcc 10)  ;;text between <m>
(# pol_tc (* 1000 pol_tcc)) 
(# p_dis (* 22 sclv))
(# pol_bs (getpoint "\n Pick base point of pole "))

(# t_st1 1)
(while t_st1
(# pol_bsm (getint "\n Enter height of base point : <default 0m> : "))
  (if (= pol_bsm)
      (progn
           (# t_st1 nil)
           (# pol_bsm 0));progn
      (progn
           (# bs__1 (fix (/ pol_bsm 2)))
           (# bs__2 (/ pol_bsm 2))
           (if (= bs__1 bs__2)
               (# t_st1 nil)));progn
   );if   
 );while

(# pol_txt (* pol_bsm 1000))
(# tet_s1 1)
(while tet_s1
    (# pol_tl (getint "\n Enter TOTAL length of pole : "))
    (# tet_3 (/ pol_tl))
    (# tet_4 (fix tet_3))
    (if (= tet_4 tet_3)
        (# tet_s1 nil)) ;if
 );while        

;;(# pol_ut (getint "\n Enter UNIT length of pole : "))
  (# pol_ut 2)

(# pol_uts (* pol_ut 1000))
(# pol_tls (* pol_tl 1000))
(# pol_w sclv) 
(# pf1 (list (- (car pol_bs) sclv) (cadr pol_bs)))
(# pf2 (list (+ (car pol_bs) sclv) (cadr pol_bs)))
(# pf1_f (list (car pf1) (+ (cadr pf1) pol_tls)))
(# pf2_f (list (car pf2) (+ (cadr pf1) pol_tls)))
(# pf_c (list (car pol_bs) (+ (cadr pol_bs) pol_tls)))
(@ "layer" "s" "z_cl" "")
(@ "line" pf1 pf2 pf2_f pf1_f "c")
(@ "line" pol_bs pf_c "")

(# kal_1 (fix (/ pol_bsm 10)))
(# kal_2 (+ (* kal_1 10) 10))
(# kal_3 (- kal_2 pol_bsm))   ;;;; ex) 8m
(# kal_4 (* kal_3 1000))   ;;; ex) 8000mm

(# p_arrb (list (- (car pf2) p_dis) (+ (cadr pf2) kal_4))) 
(# p_arrc (list (car pf2) (+ (cadr pf2) kal_4)))
(@ "line" p_arrc p_arrb "") 

(# p_ssa (entlast))
(# p_rof (fix (/ pol_tl 10)))
(if (< base_h 0)
    (progn
          (# yhn (fix (/ (abs base_h) 10)))
          (# p_rof (- p_rof yhn))
    );progn
 );;if   
      
(@ "array" p_ssa "" "" p_rof "" "10000")

(# t_siz (* 2.5 sclv))    ;;;;text height
(# tbx1 (car pol_bs))
(# tby1 (cadr pol_bs))
(# ttbx (* 17 sclv))
(# ttby (* 1 sclv))
(# tbxy (list (- tbx1 ttbx) (+ tby1 ttby kal_4)))
(# pol_bb (+ pol_bsm kal_3))
(@ "layer" "s" "z_txt" "")
(# ttbx_3 (* 2.2 sclv))  ;;;;<<3section>> 2.2
(repeat p_rof
       (if (= pol_bb 100)
           (# tbxy (list (- (car tbxy) ttbx_3) (cadr tbxy))));;if
       (# pol_ttt (rtos pol_bb 2 2))
       (@ "text" tbxy t_siz "0" pol_ttt)
       (# pol_bb (+ pol_bb pol_tcc))
       (# tbxy (list (car tbxy) (+ (cadr tbxy) pol_tc))) 
  );repeat  
;       =========
;;;     = solid =
;       =========
(@ "layer" "s" "z_cl" "")
(# sodc pol_bs)
(# na12 (* pol_ut 2))
(# sod1 (list (- (car sodc) sclv) (cadr sodc)))
(# sod2 (list (+ (car sodc) sclv) (cadr sodc)))
(# sod3 (list (car sod1) (+ (cadr sod1) pol_uts)))
(# sod4 (list (car sod2) (+ (cadr sod1) pol_uts)))
(# sodcc (list (car sodc) (+ (cadr sodc) pol_uts))) 
(# pl_bs pol_bsm)
(# fix_r (fix (/ pol_tl pol_ut)))
(repeat fix_r 
     (@ "line" sod3 sod4 "")
     (# n12n (rem pl_bs na12))  
     (if (= n12n 0)
                  (@ "solid" sod1 sodc sod3 sodcc ""))
     (if (/= n12n 0) 
                  (@ "solid" sod2 sodc sod4 sodcc ""))
     (# sodc sodcc)
     (# sod1 sod3)
     (# sod2 sod4)
     (# sod3 (list (car sod1) (+ (cadr sod1) pol_uts)))
     (# sod4 (list (car sod2) (+ (cadr sod1) pol_uts)))
     (# sodcc (list (car sodc) (+ (cadr sodc) pol_uts))) 
     (# pl_bs (+ pl_bs pol_ut))
  );repeat         
(princ)
);;defun

