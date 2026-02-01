;****************************************
;*    CDD
;*              Change Dia Donut
;*              By Suk-Jong Yi
;*              1998. 2. 22
;****************************************
; Donut의 직경을 바꿈


(defun C:CDD()

  (defun SETERR(s)                              ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)             ;내장 에러루틴 가동

  (push-env)

   (initget "Dia All Select")
   (setq ans (getkword "\nDia/All/<Select>: "))

   (cond
       ((or (= ans nil) (= ans "Select"))
           (princ "\nSelect Rebar(Donut): ")
           (setq ss1 (ssget))
       ) ;of ans=nil
       ((= ans "All")
           (setq ss1 (ssget "X" '((0 . "LWPOLYLINE") (-4 . ">") (40 . 0))))
       ) ;of ans=All
       ((= ans "Dia")
         (initget "Pick")  (princ "\n")
         (setq kword (getreal "Dia <Pick>: "))                   ;원하는 도넛 dia
         (cond
           ((numberp  kword)
             (setq rod (/ kword 2.0))                             ;도넛반지름
           ) ;of sub cond

           ((or (= kword "Pick") (= kword nil))
             (setq entl (entget (car (entsel "\nPick a Donut: "))))
             (setq rod (distance                                  ;도나스반지름
                         (getLwVert entl 0)
                         (getLwVert entl 1)
                       );distance
             );setq 
           ) ;of sub cond
         ) ;of cond
         (setq sslst (list (cons 40 rod) '(0 . "LWPOLYLINE")))    ;해당 도넛 선택
         (setq ss1 (ssget "X" sslst))
	     
       ) ;of ans="Radius"
   ); of cond


   (setq num (sslength ss1))                                ;도넛 갯수

   (print num) (princ " found\n")
;   (princ "Old dia: ") (princ (* 2 rod))

   (initget "Dia Pick")
   (setq ndia (getreal "\nEnter New Dia/Pick: "))

   (cond
     ((numberp ndia)
       (setq ndia ndia)                                     ;도나스반지름
     ) ;of sub cond

     ((or (= ndia "Pick") (= ndia nil))
       (setq entl (entget (car (entsel "\nPick a Donut: "))))
       (setq ndia (* 2 (distance                            ;도나스반지름
                    (getLwVert entl 0)
                    (getLwVert entl 1)
                  )))
     ) ;of sub cond
   ) ;of cond

   (setq plst nil)                                          ;삽입점 리스트지정
   (setq index 0)
   (repeat num                                              ;갯수만큼 반복
     (setq entl (entget (ssname ss1 index)))
     (setq pnt1 (getLwVert entl 0))                             ;도나스의 첫점
     (setq pnt2 (getLwVert entl 1))                             ;도나스의 둘째점
     (setq plst (append plst (list (mid-point pnt1 pnt2))))     ;도나스의 중심점
     (setq index (1+ index))
   ) ;of repeat

   (command "ERASE" ss1 "")                                 ;기존도넛 지우기

   (command "DONUT" "0" ndia)                               ;새도넛 명령 시작

   (setq index 0)
   (repeat num                                              ;갯수만큼 반복
     (command (nth index plst))
     (setq index (1+ index))
   ) : of repeat

   (command "")

   (princ num)
   (princ " Modified")

  (setq *error* oer seterr nil)                             ;에러루틴 복귀

  (pop-env)                                                 ;환경변수 복귀

) ;of defun



; -------------------------------------
; function : getLwVert
; LwPolyline의 Vertex를 척아냄
; 인수: vlist  : vertext list
;       tmpctr : 접근할 vertext 번호 0,1,2
; -------------------------------------

  (defun getLwVert (vlist tmpctr /
  count tmp pt1
  )

    (setq count 0)                                      ;첫 vertex 찾아감
    (while (/= (car (nth count vlist)) 10)
        (setq count (+ count 1))
    )
    ;; If the counter reaches the number of vertices,
    ;; reset ctr and tmpctr to zero again.
    (if (= tmpctr (cdr (assoc 90 vlist)))
        (progn
        (setq ctr 0)
        (setq tmpctr 0)
        )
    )
    (setq tmp (nth (+ count (* tmpctr 4)) vlist))
    (setq tmp (append tmp (list(cdr (assoc 38 vlist)))))
    (setq pt1 (trans (cdr tmp) (cdr (assoc -1 vlist)) 1))
    (setq pt1 pt1)
  ) ;of defun

