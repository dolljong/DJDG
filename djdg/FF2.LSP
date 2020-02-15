;****************************************
; Program : FFP
;           oFFset 2
;           By Yi Suk-Jong
;           1995/12/28
;****************************************

(defun C:FF2(/
              ffd  ff_dst  newc  newlt  ss_lst  ss_num  index
              entl entype  s_pnt e_pnt  ang     mid_pnt ff_pnt1
              ff_pnt2      pick_pnt     last_ent        cen_pnt
              r    s_ang   s_pnt

)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                               ;환경변수 대피

  (setq ffd (getvar "OFFSETDIST"))         ;offset 거리 취함

  (princ "\nOffset distance<")             ;offset 거리 물어봄
  (princ ffd)
  (princ ">: ")
  (setq ff_dst (getdist))
  (if (= ff_dst nil)
    (setq ff_dst ffd)
  )

  (setq newc (getint "\nColor number<7>: "))   ;새로운 색깔 물어봄
  (if (= newc nil) (setq newc 7))              ;default = 7 (white)

  (initget "Continuous Hidden Center Dashed")
  (setq newlt (getkword "\nContinuous/Hidden/CEnter/Dashed<Continuous>: "))
  (if (= newlt nil) (setq newlt "Continuous"))  ;새로운 line-type
                                                ;default = continuous
  (setq ss_lst (ssget))                         ;entity선택
  (setq ss_num (sslength ss_lst))               ;entity갯수

  (setq index 0)

  (repeat ss_num                                ;entity갯수만큼 반복
    (setq entl (entget (ssname ss_lst index)))  ;entity그룹중에 한개 취함
    (setq entype (cdr (assoc 0 entl)))          ;entity Type
    (cond
      ((= entype "LINE")                        ;eneity가 LINE일 경우
        (setq s_pnt (cdr (assoc 10 entl))       ;line의 시작점
              e_pnt (cdr (assoc 11 entl)))      ;line의 끝점
        (setq ang (angle s_pnt e_pnt))          ;line이 이루는 각
        (setq mid_pnt (mid-point s_pnt e_pnt))  ;라인의 중간점
        (setq ff_pnt1 (polar mid_pnt (+ ang (* pi 0.5)) ff_dst))   ;offset점-1
        (setq ff_pnt2 (polar mid_pnt (- ang (* pi 0.5)) ff_dst))   ;offset점-2
        (setq pick_pnt (list (cdr (assoc -1 entl)) mid_pnt))       ;선택점
        (command "OFFSET" ff_dst pick_pnt ff_pnt1 "")   ;offset명령-1
        (setq last_ent (entget (entlast)))              ;offset된 entity의
        (modi_clt last_ent newc newlt)                  ;색과 linetype변경
        (command "OFFSET" ff_dst pick_pnt ff_pnt2 "")   ;offset명령-2
        (setq last_ent (entget (entlast)))              ;offset된 entity의
        (modi_clt last_ent newc newlt)                  ;색과 linetype변경
      )
      ((= entype "ARC")
        (setq cen_pnt (cdr (assoc 10 entl)))
        (setq r (cdr (assoc 40 entl)))
        (setq s_ang (cdr (assoc 50 entl)))
        (setq s_pnt (polar cen_pnt s_ang r))
        (setq pick_pnt (list (cdr (assoc -1 entl)) s_pnt))
        (setq ff_pnt1 (polar cen_pnt s_ang (+ r ff_dst)))
        (setq ff_pnt2 (polar cen_pnt s_ang (- r ff_dst)))
        (command "OFFSET" ff_dst pick_pnt ff_pnt1 "")
        (setq last_ent (entget (entlast)))
        (modi_clt last_ent newc newlt)
        (command "OFFSET" ff_dst pick_pnt ff_pnt2 "")
        (setq last_ent (entget (entlast)))
        (modi_clt last_ent newc newlt)
      )
    ) ;of cond
    (setq index (1+ index))
  ) ;of repea t
  (setq *error* oer seterr nil)
) ;of defun


;*****************************************************
; Function : MODI_CLT (Entity list, Color, Linetype)
;            MODify Color and Line-Type
;            By Yi Suk-Jong
;            1995/12/28
;*****************************************************

(defun modi_clt( ent c lt /
                            ent   c   lt  newlt  newc  oldc oldlt
 )

   (setq oldc (assoc 62 ent))
   (setq oldlt (assoc 6 ent))
   (if (= oldc nil)
     (setq ent (append ent (list (cons 62 c))))
     (setq newc (cons 62 c)
           ent (subst newc oldc ent))
   ) ;of if
   (if (= oldlt nil)
     (setq ent (append ent (list (cons 6 lt))))
     (setq newlt (cons 6 lt)
           ent (subst newlt oldlt ent))
   ) ;of if

   (entmod ent)
) ;of defun

(princ)
