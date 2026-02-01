

;------------------------------------------------------------
; frunction : circle2mark
;             By Yi Seok Jong
;             2000/9/1
;------------------------------------------------------------
; argument: entityname
; return : marking
;ex) (circle2mark entname)  --> A1-1
;------------------------------------------------------------
(defun circle2mark(centname /
		   centname cent cc rc ix iy p1 p2 ss ssn count tlst sent etype l1 l2 end_p nt txt1 txt2 mark)
  (setq cent (entget centname))             ;circle정보
  (setq cc (cdr (assoc 10 cent)))                     ;center 좌표
  (setq rc (cdr (assoc 40 cent)))                     ;radius (반지름)
  (setq ix (car cc)                         ;삽입점 x
        iy (cadr cc)                        ;       y
        p1 (list (- ix rc) (+ iy rc))         ;
        p2 (list (+ ix rc) (- iy rc))         ;
        ss (ssget "C" p1 p2)                ; MARKING/LINE 잡아내기
        ssn (sslength ss)
        count 0
        tlst nil)

  (repeat ssn                                 ;엔티티 갯수만큼 반복
    (setq sent (entget (ssname ss count)))
    (setq etype (cdr (assoc 0 sent)))
    (cond
      ((= etype "TEXT")                       ;엔티티가 텍스트인 경우
        (if (< (distance (cdr (assoc 10 sent)) cc) rc)
          (setq tlst (append tlst (list (cdr (assoc 1 sent))))) ;텍스트 리스트에 추가
        );if
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;엔티티가 라인인 경우
        (progn
          (setq l1 (cdr (assoc 10 sent))          ;첫점
                l2 (cdr (assoc 11 sent)))         ;끝점
          (if (> (car l1) (car l2))             ;끝점찾기
            (setq end_p l1)
            (setq end_p l2)
          ) ;of if
;          (print tlst)(print sent)	  
        ) ;of progn
      ) ;of etype=LINE
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat                             
  
  (setq nt (length tlst))                       ;text갯수
  (if (= nt 2)                                  ;text갯수가 두개면 (예: A1-1)
    (progn
      (setq txt1 (car (sp-trunc (nth 0 tlst)))
            txt2 (car (sp-trunc (nth 1 tlst))))
      (if (= (substr txt1 1 1) "-")               ;-가 붙은 쪽을 뒤로해서 더하기
        (setq MARK (strcat txt2 txt1))
        (setq MARK (strcat txt1 txt2))
      ) ;of if
    ) ;of progn
    (setq MARK (nth 0 tlst))                     ;text갯수가 한개면 (예: A2)
  ) ;of if
  mark
);defun

;------------------------------------------------------------
; frunction : CB
;             Change BarMarking type
;             By Yi Seok Jong
;             2000/9/1
;------------------------------------------------------------
(defun c:CB(
	    /  ssc nc tlst count ent etype l1 l2 cc rc ntext mklst textcount enttxt
	       mklst diatxt nt txt1 txt2 mark ipnt sidepnt)
  (setq ssc (ssget))      ;circle들을 입력
  (setq nc (sslength ssc))                  ;entity의 갯수

  (setq tlst nil)                          ;빈 line-list 만듬

  (setq count 0)
  (repeat nc                                            ;entity갯수만큼 반복
    (setq ent (entget (ssname ssc count)))             ;entity정보
    (setq etype (cdr (assoc 0 ent)))                   ;entity type
    (cond
      ((= etype "TEXT")                       ;엔티티가 텍스트인 경우
        (setq tlst (append tlst (list (ssname ssc count)))) ;텍스트 리스트에 추가
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;엔티티가 라인인 경우
        (setq l1 (cdr (assoc 10 ent))          ;첫점
              l2 (cdr (assoc 11 ent)))         ;끝점
      ) ;of etype=LINE
      ((= etype "CIRCLE")
        (setq cc (cdr (assoc 10 ent)))                     ;center 좌표
        (setq rc (cdr (assoc 40 ent)))                     ;radius (반지름)
      );of etype=circle 
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat

  (setq ntext (length tlst)
	mklst nil
	textcount 0)    ;number of text entity
  (repeat ntext
    (setq enttxt (entget (nth textcount tlst)))
    (if (< (distance (cdr (assoc 10 enttxt)) cc) rc)
      (setq mklst (append mklst (list (cdr (assoc 1 enttxt)))))
      (setq diatxt (cdr (assoc 1 enttxt)))
    );if
    (setq textcount (1+ textcount))
  );repeat

  (setq nt (length mklst))                       ;text갯수
  (if (= nt 2)                                  ;text갯수가 두개면 (예: A1-1)
    (progn
      (setq txt1 (car (sp-trunc (nth 0 mklst)))
            txt2 (car (sp-trunc (nth 1 mklst))))
      (if (= (substr txt1 1 1) "-")               ;-가 붙은 쪽을 뒤로해서 더하기
        (setq MARK (strcat txt2 txt1))
        (setq MARK (strcat txt1 txt2))
      ) ;of if
    ) ;of progn
    (setq MARK (nth 0 mklst))                     ;text갯수가 한개면 (예: A2)
  ) ;of if

  (setq ipnt (getpoint "\nPick basepoint: ")
	sidepnt (getpoint ipnt "\nPick sidepoint: "))
  
  (barmark ipnt sidepnt mark diatxt)
  (command "ERASE" ssc "")
;  (print mark )  (print diatxt)
  
  (princ)
);defun


;------------------------------------------------------
; function : barmark
;            By Yi Seok Jong
;            2000/9/1
;------------------------------------------------------
(defun barmark(ep p3 mark diatxt /
	       ep p3 mark diatxt th  ds rc dx xsgn dy ysgn blen p4 ip cp diaxy txtrot oldclr)
;  (setq p3 (getpoint ep "\nPick base line: "))              ;base line point
  (setq th (getvar "DIMTXT")
	ds (getvar "DIMSCALE"))           ;text height
  
  (setq rc (* ds 3.5))                  ;반지름

  (setq dx (- (car p3) (car ep)))
  (if (< dx 0)                                          ;base line의 x방향인식
    (setq xsgn -1)
    (setq xsgn 1)
  ) ;of if

  (setq dy (- (cadr p3) (cadr ep)))
  (if (<  dy 0)                                         ;base line의 y방향인식
    (setq ysgn -1)
    (setq ysgn  1)
  ) ;of if

  (setq blen (+ (* ds 7) (* 4 ds th)))                     ;base line의 길이

  (if (> (abs dx) (abs dy))                                             ;누워있나 서있나?
    (progn
      (setq p4 (list (+ (car ep) (* blen xsgn)) (cadr ep)))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dx 0)
        (setq ip p4)
        (setq ip ep)
      ) ;of if
      (setq cp (list (+ (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 4 ds)) (- (cadr cp) (* 3 ds)) 0.0))      ;dia표시 위치
      (setq txtrot 0)                                   ;text회전각
    ) ;of progn
    (progn
      (setq p4 (list (car ep) (+ (cadr ep) (* blen ysgn))))
      (cecolor "1")
      (command "LINE" ep p4 "")
      (popcolor)
      (if (< dy 0)
        (setq ip p4)	
        (setq ip ep)
      ) ;of if
      (setq cp (list (- (car ip) (* ds 3.5))
                     (+ (cadr ip) (* ds 3.5))))
      (setq diaxy (list (+ (car cp) (* 3 ds)) (+ (cadr cp) (* 4 ds)) 0.0))      ;dia표시 위치
      (setq txtrot 90)
    ) ;of progn
  ) ;of if


  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "1")
  (command "CIRCLE" cp rc)
  (setvar "CECOLOR" oldclr)
  
  (setq oldclr (getvar "CECOLOR"))
  (setvar "CECOLOR" "7")
  (txtinc mark cp txtrot)
  (command "TEXT" diaxy (* th ds) txtrot (strcase diatxt))
  (setvar "CECOLOR" oldclr)
);

;********************************************
; FUCTION : TXTINC
;           TeXT IN Circle
;           Suk-Jong Yi
;           96/5/7
;********************************************
; 원 안에 철근번호를 기입해준다.
; 넘어오는 값
;         TXT: TEXT
;        IPNT: Insert point
;      TXTROT: TeXT ROTation
;********************************************

(defun TXTINC(TXT IPNT TXTROT / th ipnt  txtl count)

  (setq th (getvar "DIMTXT"))               ;text크기=치수크기

  (setq txtl (strlen txt))

  (if (> txtl 3)
    (progn
      (setq count 1)
      (while (and (/= (substr txt count 1) "-")
                 (< count txtl))
        (setq count (1+ count)))
      (if (= count txtl)
        (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
        (progn
          (command "TEXT" "C" ipnt (* th ds) TXTROT
                   (strcase (substr txt 1 (- count 1))))
          (command "TEXT" "TC" ipnt (* th ds) TXTROT
                   (strcase (substr txt count (+ (- txtl count) 1))))
        ) ;of progn
      ) ;of IF
    ) ;of PROGN
    (command "TEXT" "M" ipnt (* th ds) TXTROT (strcase txt))
  ) ;of IF
) ;of DEFUN

;**************************************************************************
; Function : SP-TRUNC
;            SPace TRUNCation
;            By Suk-Jong Yi
;            1995/6/1
;**************************************************************************
; 입력문자열의 앞,뒤에 있는 빈칸을 짤라낸다.
; 리턴값은
; (짤라낸 문자열,
;  첫 문자 나오는 위치,
;  마지막 문자 나오는 위치,
;  숫자인가?)
;***************************************************************************

(defun SP-TRUNC(txt
/               txtl        frntn       backn       txt1
)

(setq txtl (strlen txt))
(setq frntn 1)
(while (= (substr txt frntn 1) " ") (setq frntn (+ frntn 1)))
(if (<= frntn txtl)
  (progn
    (setq backn txtl)
    (while (= (substr txt backn 1) " ")
     (setq backn (- backn 1))
    )
    (setq txt1 (substr txt frntn (- backn frntn -1)))
    (list txt1 frntn backn (is-num txt1))
  ) ;progn
) ;of if
);defun

;************************************
; Function : IS-NUM
;            IS NUMber ?
;            By Suk-Jong Yi
;            1996/2/23
;************************************
; 문자열이 숫자인가?를 판단해준다.
;************************************

(defun IS-NUM(str
/ str strl count ch )

  (setq strl (strlen str))
  (setq count 1)
  (while (or (and (>= (setq ch (ascii (substr str count 1))) 48)
                  (<= ch 57))
             (= ch 44)
             (= ch 46)
             (and (= count 1) (= ch 43))
             (and (= count 1) (= ch 45))
         )
    (setq count (+ count 1))
  ) ;of while

  (if (= count (+ strl 1)) strl NIL)

) ;of defun


