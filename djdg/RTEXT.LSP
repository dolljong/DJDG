;*************************************
; Program : RTEXT
;           Round TEXT
;           By Suk-Jong Yi
;           1995/7/1
;*************************************
; 원상에 글자를 배열

(defun C:RTEXT(/

)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

(setq cpnt (getpoint "\nPick center point: "))          ; 원의 중심점
(setq ipnt (getpoint cpnt "\nPick insert point: "))     ; 삽입점
(setq th (getreal "\n Text height: "))                  ; text 높이
(setq txt (getstring T "\nTEXT: "))                       ; text입력
(setq r (distance cpnt ipnt))                           ; 원중심~삽입점 거리

(setq ang (angle cpnt ipnt))                               ; 텍스트위치의 절대각
(setq tn (strlen txt))                                  ; 글자 갯수
(setq tw (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))) ; 글자폭
(setq ta (* (/ (* th tw) (* 2.0 pi r)) 2.0 pi))         ;한글자가 차지하는 각도
(setq tta (* tn ta))                                    ; 전체글자의 각도



(if (> ang pi) (setq sgn 1 tb 3) (setq sgn -1 tb 0))              ; 글자 쓰는 방향
(setq sta (+ ang (/ tta -2.0 sgn) (* 0.5 ta sgn)))

(setq header (list '(0 . "BLOCK") '(2 . "*JONG") '(70 . 1) (cons 10 ipnt)))
(entmake header)    ;블럭설정시작하기

(setq count 1)
(repeat tn
  (setq t1a (+ sta (* (1- count) ta sgn)))              ;각 글자 절대각
  (setq ra (+ t1a (* pi 0.5 sgn)))                      ;글자의 회전각도
  (setq ip (polar cpnt t1a r))                          ;글자의 위치
  (setq txt1 (substr txt count 1))                      ;글자 하나 축출
                                                ;*** text 엔티티 만들기
  (setq tent (list '(0 . "TEXT")                  ;엔티티명
                    (cons 1 txt1)                 ;텍스트값
                    (cons 7 (getvar "TEXTSTYLE")) ;텍스트 스타일
                    (cons 10 ip)                  ;기준점
                    (cons 11 ip)                  ;삽입점
                    (cons 40 th)                  ;텍스트 높이
                    (cons 50 ra)                  ;회전각도
                    (cons 72 1)                   ;중앙정렬
                    (cons 73 tb)))                ;상단, 하단
  (setq subent (entmake tent))                    ;block table에 글자 등록
;  (princ subent)
;  (command "TEXT" ip th (rtod ra) txt1)
  (setq count (1+ count))
) ;of repeat

(setq blknm (entmake '((0 . "ENDBLK"))))

(entmake (list '(0 . "INSERT") (cons 2 blknm) (cons 10 ipnt)))
;(princ blknm)
  (setq *error* oer seterr nil)
(prin1)
) ;of defun

