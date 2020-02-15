;*********************************
; Program : DH
;           Dimension Horizontal
;           Jong-Suk Yi
;           1995. 3. 7, 7/5
;*********************************

(defun C:DH(/
th ds sp dsel sent pnt1 pnt2 ppnt                       ;지역변수 정의
next ep dp dty sgn fst dy dxy dx
txt txt1 divl divn txtlen dtxt1 dtxt2
dtxt1p dtxt2p
)

(setq th (getvar "DIMTXT")
      dim_gap 10.0)                                         ;글자 크기 지정
(setq ds (getvar "DIMSCALE"))                           ;scale factor

(push-env)                                              ;환경값 지정

(initget "Object")
(setq sp (getpoint "\nPick first point/Object: "))      ;시작점 입력
(if (= sp "Object")                                       ;기존 치수선 참조
  (progn
    (setq dsel (entsel "\nSelect Dimension Entity: "))    ;dimension entity선택
    (setq sent (entget (car dsel)))
    (setq pnt1 (cdr (assoc 13 sent)))
    (setq pnt2 (cdr (assoc 14 sent)))
    (setq ppnt (cadr dsel))                                 ;선택점
    (if (> (distance ppnt pnt1) (distance ppnt pnt2))
      (setq sp pnt2) (setq sp pnt1))                         ;선택점과 가까운 쪽
  ) ;of progn THEN
) ;of IF(sp=Object)

(setq next (getdist sp "\nDistance or RETURN to Pick point: ")) ;끝점까지 거리
(cond                                               ;점을 입력하려면 리턴입력
  ((= next nil)
    (setq ep (getpoint "\nPick point: "))                ;끝점을 입력
    (setq ep (list (car ep) (cadr sp)))                  ;수정된 끝점
  ) ;cond(next=nil)
  ((numberp next)                                          ;dx가 숫자인 경우
    (setq ep (list (+ (car sp) next) (cadr sp)))             ;ep 위치계산
    (if (> next 0) (setq lsgn 1) (setq lsgn -1))           ;치수선 진행방향
  ) ;cond(next=number)
) ;of cond


(setq dp (getpoint "\nPick Dimension line side: ")) ;치수선의 위치(위,아래)
(setq dty (- (cadr dp) (cadr ep)))                  ;y값 차 구함
(setq sgn (/ dty (abs dty)))                        ;y값 차의 부호구함

(setq fst (getint "\n1/2/3<1>: "))                  ;치수선 단계입력
(if (= fst nil) (setq fst 1))                       ;리턴입력시 1단계로
(setq dy (* ds (+ 20 (* dim_gap (- fst 1)))))             ;치수선 위치 계산

(while (/= ep nil)                                  ;ep가 nil이 아니면 반복

  (setq dxy (list (car ep) (+ (cadr ep) (* dy sgn)) 0.0))  ;치수선 위치

  (setq dx (distance sp ep))                          ;거리 계산
  (if (< dx 1000.0)
    (setq txt (rtos dx 2 0))                          ;1000미만일 때
    (setq txt (rtos (* dx 0.001) 2 3))                ;1000이상일 때
  ) ;of if(dx < 1000)

  (princ "\nDimension text <")                        ;Dimension text표시
  (princ txt)
  (setq txt1 (getstring T ">: "))                     ;새로운 dimension text입력
  (if (= txt1 "@")
    (progn
      (setq divl (getint "\nDivision length: "))      ;나누는 길이 입력
      (setq divn (rtos (/ dx divl) 2 0))              ;나눈 갯수계산
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;1000미만일 때
        (setq divl (rtos (* 0.001 divl) 2 3))) ;of if ;1000이상일 때
      (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds  ;text전체길이
                   (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
      (if (>= txtlen dx)                       ;치수보조선 내에 text 안들어가면
        (progn
          (setq dtxt1 (strcat divn "@" divl))       ;위 아래 두줄로 나눈다
          (setq dtxt2 (strcat "=" txt))
          (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (+ (* dy sgn) (* ds th)) 0.0)))
          (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                  (list 0.0 (- (* dy sgn) (* ds th)) 0.0)))
          (command "TEXT" "M" dtxt1p (* th ds) "0" dtxt1)
          (command "TEXT" "M" dtxt2p (* th ds) "0" dtxt2)
          (command "DIM1" "HOR" sp ep dxy " ")               ;DIM명령 내림
        ) ;of progn THEN
        (progn                                 ;치수보조선 내에 text 들어가면
          (setq dtxt1 (strcat divn "@" divl "=" txt))
          (command "DIM1" "HOR" sp ep dxy dtxt1)               ;DIM명령 내림
        ) ;of progn ELSE
      ) ;of IF
    ) ;of progn THEN
    (progn
      (if (= txt1 "") (setq txt1 txt))                  ;리턴입력시 옛 text를 씀
      (command "DIM1" "HOR" sp ep dxy txt1)             ;DIM명령 내림
    ) ;of progn ELSE
  ) ;of if(txt1=@)

  (setq sp ep)                                          ;끝점을 첫점으로
  (initget "eXit Undo")
  (setq next (getdist "\nDistance or RETURN to Pick point/eXit: ")) ;끝점까지 거리
  (cond                                               ;점을 입력하려면 리턴입력
    ((= next nil)
      (setq ep (getpoint "\nPick point: "))                ;끝점을 입력
      (setq ep (list (car ep) (cadr sp)))                  ;수정된 끝점
    ) ;cond(next=nil)
    ((= next "eXit")                                       ;eXit입력시 ep=nil
      (setq ep nil)
    ) ;cond(next="eXit")
    ((numberp next)                                             ;dx가 숫자인 경우
      (setq ep (list (+ (car sp) (* next lsgn)) (cadr sp)))     ;ep 위치계산
    ) ;cond(next=number)
  ) ;of cond

) ;of while

(pop-env)
(prin1)
) ;defun
