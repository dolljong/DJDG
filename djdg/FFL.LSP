;***************************************
; Program : FFL
;           oFFset to current Layer
;           By Suk-Jong Yi
;           95/5/26
;***************************************

; 보통 강재도면을 그릴때 center선을 그려놓고 양쪽으로 offset시킨다.
; 하지만 나중에 center선을 지우는 것도 만만치 않은 작업이다.
; 이 프로그램은 entity를 offset시키되 현재의 layer로 offset시킨다.
; 즉 center line을 위한 layer와 구조물을 위한 layer를 만든후
; center line layer에 center line들을 그리고 Current layer를 구조물
; layer로 바꾸고 offset을 한다. 나중에 출력할 때 center line layer는
; offset 시킨후에 print한다.
; 모든 option은 원래 offset명령과 같다.

(defun C:FFL(/
ffdst ans ffpnt ent oldl newl                          ;지역변수 정의
)
  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)
(if (< (setq ffdst (getvar "OFFSETDIST")) 0)           ;환경변수값 구함
 (progn                                   ;환경변수 OFFSETDIST가 음수일 경우
   (initget "Trough")
   (setq ans (getreal "\nOffset distance or Throgh <Trough>: "))
   (if (= ans nil) (setq ans "T"))
 ) ;of progn
 (progn                                   ;환경변수 OFFSETDIST가 양수일 경우
   (princ "\nOffset distance or Through <")
   (princ ffdst)
   (initget "Through")
   (setq ans (getreal ">: "))
   (if (= ans nil) (setq ans ffdst))
 ) ;of progn
) ;of if
(while (setq ents (entsel "\nSelect object: "))        ;offset 반복
  (setq ffpnt (getpoint "\nPick side: "))
  (command "OFFSET" ans ents ffpnt "")                 ;offset 실행
  (setq ent (entget (entlast)))
  (setq oldl (assoc 8 ent))
  (setq newl (cons 8 (getvar "CLAYER")))    ; offset된 entity의 layer를-
  (setq ent (subst newl oldl ent))          ; 현재 layer로 변경
  (entmod ent)
) ;of while
  (setq *error* oer seterr nil)
(princ)
);of defun
