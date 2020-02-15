;**************************************
; program : RCORNER
;           Rebar for Corner
;           Yi Suk Jong
;           03/10/25
;**************************************

(defun c:RCORNER( /
 l1s  l2s  lds1  seld ldpnt rdist nlayer dist l1  l2  ld1  l1_s  l1_e  ang_l1
 l2_s  l2_e  ang_l2  ld1_s  ld1_e  lng_ld1 ld1_sdst ld1_edst spnt ang  bpnt ang90 
 index vpnt1 vpnt2 rpnt1 rpnt2
)

  (defun SETERR(s)                              ;내장에러루틴 하
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)

  (push-env)                            ;환경저장


  (setq l1s (car (entsel "\nPick Rebar Line-1: ")))       ;외곽선1 선택
  (redraw l1s 3)
  (setq l2s (car (entsel "\nPick Rebar Line-2: ")))       ;외곽선2 선택
  (redraw l2s 3)
  (setq seld (entsel "\nPick diagonal Line: ")) 	;사선 선택
  (setq lds1 (car seld)					; 사선 선정보	
        ldpnt (cadr seld))    				;사선 선택점 정보
  (redraw lds1 3)
  (setq rdist (getreal "\nEnter Distance Ratio: "))     ;떨어진 거리 정보 받기
  (setq nlayer (getint "\nEnter Number of Layer: "))    ;층수 받기
  (setq dist  (getreal "\nEnter Distance: "))           ;간격받기
   
  (setq l1 (entget l1s)                            ;선택된 3개 선의 정보축출
        l2 (entget l2s)
        ld1 (entget lds1))

  (setq l1_s (cdr (assoc 10 l1))            ;2개의 line의 시작점 끝점 구함
        l1_e (cdr (assoc 11 l1))
        

        l2_s (cdr (assoc 10 l2))
        l2_e (cdr (assoc 11 l2))
        

        ld1_s (cdr (assoc 10 ld1))          ;사선1의 시작점
        ld1_e (cdr (assoc 11 ld1))          ;사선1의 끝점
        lng_ld1 (distance ld1_s ld1_e)      ;두선의 거리
        ld1_sdst (distance ld1_s ldpnt)     ;선택점에서 시작점까지 거리
        ld1_edst (distance ld1_e ldpnt)
  );setq
  (if (< ld1_sdst ld1_edst)		;시작점 설정(선택점과 가까운점을 시작점으로 정함)		
    (setq spnt ld1_s ang (angle ld1_s ld1_e))     ;각도는 시작점에서 끝점으로 가능방향
    (setq spnt ld1_e ang (angle ld1_e ld1_s))
  );if
  (setq 	
	bpnt (polar spnt ang (* lng_ld1 rdist));기준점
;	ld1_m (mid-point ld1_s ld1_e)       ;사선1의 중간점
;        ang (angle ld1_s ld1_e)             ;사선1의 각도
        ang90 (+ ang (* 0.5 pi))            ;사선1의 각도+90도

;        ld2_s (cdr (assoc 10 ld2))                              ;사선2의 시작점
;        ld2_e (cdr (assoc 11 ld2))                              ;사선2의 끝점
;        ld2_m (inters ld2_s ld2_e ld1_m (polar ld1_m ang90 100) nil)
        ;(polar~)는 가상점
  );setq
  (setq bpnt1 (polar bpnt (+ ang pi) (* 0.5 (1- nlayer) dist)))          ;철근점
  (setq index 0)
  (repeat nlayer
    (setq vpnt1 (polar bpnt1 ang (* index dist)) 		;가상점1  (철근과의 교차점을 찾기위한 점)
	  vpnt2 (polar vpnt1 ang90 100)		;가상점2
          rpnt1 (inters vpnt1 vpnt2 l1_s l1_e nil) 	;외곽선1과의 교점
	  rpnt2 (inters vpnt1 vpnt2 l2_s l2_e nil)    ;외곽선2와의 교점
    );setq
    (command "LINE" rpnt1 rpnt2 "")
    (setq index (1+ index))
  );repeat  

  (redraw l1s 4)                               ;강조해제
  (redraw l2s 4)
  (redraw lds1 4)

  (pop-env)                                    ;환경복귀

  (setq *error* oer seterr nil)
  (prin1)
) ;of defun                                                         ]
