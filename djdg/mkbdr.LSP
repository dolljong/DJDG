;**************************************************************
; Program : MKBDR
;           MaKe BorDeR
;           By Suk-Jong Yi
;           1997/1/8
;**************************************************************
; 1. 자동 출력프로그램인 PLOTBDR.LSP을 위한 프로그램으로서
;    border를 만들어준다. border는 plot시 window를 지정할 때
;    필요한 두개의 점(point entity)과 box로 이루어져있다.
; 2. border를 만들때마다 block으로 만들어주는데 block의 이름은
;    차례로 BDR$1, BDR$2 와 같이 BDR$에 붙여서 만들어진다.
;    사용자가 만든 block이름과 겹치는 것을 방지하기 위해 $기호
;    를 사용하였다.
;**************************************************************

(defun C:MKBDR(/
                p1    p2   dx       dy      dst     p11   p12     p13   p14
                bk1h  bkh  bkh_lst  bdr_nm  header  pent  subent  lent  blknm

)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  ;*** 두 점 입력
  (setq p1 (getpoint "\nPick point: "))                 ; 첫점
  (setq p2 (getcorner p1 "\nPick Second point: "))      ; 끝점

  (setq dx (- (car p2) (car p1))                        ;X 길이
        dy (- (cadr p2) (cadr p1)))                     ;Y 길이

  (if (< dx 0) (setq xsgn -1)                           ;X부호
               (setq xsgn 1))

  (if (< dy 0) (setq ysgn -1)                           ;Y부호
               (setq ysgn 1))

  (if (< (abs dx) (abs dy)) (setq dst (abs (/ dx 10.0)));작은길이의 1/10
                            (setq dst (abs (/ dy 10.0))))

  (setq p11 (list (- (car p1) (* dst xsgn))
                  (- (cadr p1) (* dst ysgn)) 0)
        p14 (list (+ (car p2) (* dst xsgn))
                  (+ (cadr p2) (* dst ysgn)) 0)
        p12 (list (car p14) (cadr p11) 0)
        p13 (list (car p11) (cadr p14) 0))

  ;기존 block list를 만든다.
  (setq bk1h (tblnext "BLOCK" T))                    ;첫번째 블럭 table header
  (if (/= bk1h nil)
    (if (= (strcase (substr (cdr (assoc 2 bk1h)) 1 4)) "BDR$")
      (setq bkh_lst (list bk1h))
      (setq bkh_lst nil)
    ) ;of if
  ) ;of if

  (while (/= (setq bkh (tblnext "BLOCK")) nil)
    (if (= (strcase (substr (cdr (assoc 2 bkh)) 1 4)) "BDR$")
      (setq bkh_lst (append bkh_lst (list bkh)))
    ) ;of if
  ) ;of while

  (if (= bkh_lst nil)
    (setq bdr_nm "BDR$1")
    (setq bdr_nm (strcat "BDR$" (rtos (1+ (length bkh_lst)) 2 0)))
  ) ;of if

  (setq header (list '(0 . "BLOCK") (cons 2 bdr_nm) '(70 . 64) (cons 10 p1)))
  (entmake header)                                      ;블럭시작

  (setq pent (list '(0 . "POINT")                       ;첫번째점
                      (cons 10 p1)))                    ;삽입점
  (setq subent (entmake pent))                          ;block table에 글자 등록

  (setq pent (list '(0 . "POINT")                       ;둘째점
                      (cons 10 p2)))                    ;삽입점
  (setq subent (entmake pent))                          ;block table에 글자 등록

  (setq lent (list '(0 . "LINE")                        ;첫점box 수직선
                    (cons 10 p1)                        ;시작점
                    (cons 11 (list (car p1) (cadr p11) 0)))) ;끝점
  (setq subent (entmake lent))                          ;block table에 글자 등록

  (setq lent (list '(0 . "LINE")                        ;첫점box 수평선
                    (cons 10 p1)                        ;시작점
                    (cons 11 (list (car p11) (cadr p1) 0)))) ;끝점
  (setq subent (entmake lent))                          ;block table에 글자 등록

  (setq lent (list '(0 . "LINE")                        ;둘째점box 수직선
                    (cons 10 p2)                        ;시작점
                    (cons 11 (list (car p2) (cadr p14) 0)))) ;끝점
  (setq subent (entmake lent))                          ;block table에 글자 등록

  (setq lent (list '(0 . "LINE")                        ;둘째점box 수평선
                    (cons 10 p2)                        ;시작점
                    (cons 11 (list (car p14) (cadr p2) 0)))) ;끝점
  (setq subent (entmake lent))                          ;block table에 글자 등록

  (setq lent (list '(0 . "LINE")                        ;전체 box 위 수직선
                    (cons 10 p11)                       ;시작점
                    (cons 11 p12)))                     ;끝점
  (setq subent (entmake lent))                          ;block table에 등록

  (setq lent (list '(0 . "LINE")                        ;전체 box 오른쪽 수직선
                    (cons 10 p12)                       ;시작점
                    (cons 11 p14)))                     ;끝점
  (setq subent (entmake lent))                          ;block table에 등록

  (setq lent (list '(0 . "LINE")                        ;전체 box 아래 수평선
                    (cons 10 p14)                       ;시작점
                    (cons 11 p13)))                     ;끝점
  (setq subent (entmake lent))                          ;block table에 등록

  (setq lent (list '(0 . "LINE")                        ;전체 box 왼쪽 수평선
                    (cons 10 p13)                       ;시작점
                    (cons 11 p11)))                     ;끝점
  (setq subent (entmake lent))                          ;block table에 등록

  (setq blknm (entmake '((0 . "ENDBLK"))))              ;블럭끝

  (entmake (list '(0 . "INSERT") (cons 2 blknm) (cons 10 p1)))

  (setq *error* oer seterr nil)
  (prin1)

) ;of defun

