;***********************************
; Program ; MTLEAD
;           MulTi LEADer line
;           Yi -Suk-Jong
;           99/11/9
;***********************************
; 다중지시선을 그려준다.
;***********************************

(defun C:mtLEAD(/
ds p1 p2 ang w4 ys tp tbox tl p3                            ;지역변수 정의
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 대피
  (setq ds (getvar "DIMSCALE")                             ;스케일 값
        th (getvar "DIMTXT"))

  (setq plist nil)
  (while (/= nil (setq p1 (getpoint "\nFirst point: ")))                    ;첫째점
    (setq plist (append plist (list p1)))                                   ;점더하기
  );while

  
  (setq nplist (length plist))            ;number of plist
	
  
  (setq p2 (getpoint "\nSecond point: "))                ;두째점
  (setq ang (angle p2 (nth 0 plist)))        ;첫점의 각을 구한다
  (setq minang ang)                           ;최대각과 최소각을 첫점의 각으로한다
  (setq maxang ang)
  
  (setq index 0)
  (repeat nplist
    (setq p1 (nth index plist))
    (setq ang (angle p1 p2)                                   ;두점이 이루는 각
        w4  (which4 ang))                                   ;몇사분면가?
    (if (<= ang minang) (setq minang ang))    ;최대각과 최소각 찾기
    (if (>= ang maxang) (setq maxang ang))
    (if (or (= w4 1) (= w4 4))                                ;1,4분면일경우
      (setq ys (* 1 ds))                                             ;y-scale =  1
      (setq ys (* -1 ds))                                            ;y-scale = -1
    ) ;of if
    (command "INSERT" (strcat (prefix) "blocks/arw1") p1 ds ys (rtod ang))
    (setvar "CECOLOR" "RED")
    (command "LINE" p1 p2 "")                                 ;leader line 
    (setq index (1+ index))
  );repeat

  

  (setq dtang (dang minang maxang))           ;최대각과 최소각의 차이
  (setq mang (+ minang (/ dtang 2.0) pi))        ;최대각과 최소각의 중간각
  (setq w4  (which4 mang))

  (setvar "CECOLOR" "WHITE")

  (if (or (= w4 1) (= w4 4))
    (progn                                                  ; 1, 4사분면일 때 왼쪽에서 오른쪽으로
      (setq tp (list (+ (car p2) (* ds th)) (+ (cadr p2) (* ds 1.25)) 0.0))
      (command "DTEXT" tp (* ds th) "0.0")
    ) ;of progn
    (progn                                                  ; 2 ,3사분면일 때 오른쪽에서 왼쪽으로
      (setq tp (list (- (car p2) (* ds th)) (+ (cadr p2) (* ds 1.25)) 0.0))
      ;(command "DTEXT" "R" tp (* ds th) "0.0" /)            ; text 입력대기
      (command "DTEXT" "R" tp (* ds th) "0.0")            ; text입력대
      ) ;progn
  ) ;of if

(command)

  (setq tbox (textbox (entget (entlast))))                  ;쓰여진 글자 크기 정보
  (setq tl (- (car (nth 1 tbox)) (car (nth 0 tbox))))       ;쓰여진 글자 크기
  (setq p3 (list (+ (car p2) (* 5.0 ys) (* tl (/ (abs ys) ys)))     ;밑줄
                 (cadr p2) 0.0))

  (setvar "CECOLOR" "RED")                                  ;색을 빨간색으로
  (command "LINE" p2 p3 "")                                 ;line그리기
                                  ;색을 흰색으로

  (pop-env)                                                 ;환경변수 돌리기
  (setq *error* oer seterr nil)
  (princ)

) ;of defun
