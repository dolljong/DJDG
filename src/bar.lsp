;Program : BAR 	: 철근상세도를 이용하여 철근재료표 만들기
;Program : BARV : 철근데이터파일을 이용하여 변수처리된 철근재료표 만들기
;----------------------------------------------------------
; Function : B_MARK ; 철근 상세도에서 marking(철근번호)과 철근데이타를 찾기 위한
; 		      두점을 되돌려준다. (두점은 원의 오른쪽 상단과 밑줄의 끝점)
; 		      넘어오는 값:  IP : Insert point (원의 중심)
;       			    R : Radius       (원의 반지름)
;     				    A34 : 3사4입 여부(Yes/No)
; 			넘어가는 값:
;     				    MARK : 철근 마킹
;      				    DIA : 철근의 직경
;        			    L : 철근의 길이(L,L',A.V.L이 동시에 존재할 경우 제일 큰 것으로)
;        			    N : 철근의 갯수
;**************************************
; Function : ATOF1  ;            ATOF-1 (STRING --> REAL)  
; Function : ATOI1  ;            ATOI-1 (STRING --> INTEGER)
; Function : DATA-IN;                DATA file IN
; Function : SP-TRUNC;            SPace TRUNCation
; Function : IS-NUM;            IS NUMber ?
; Function : SORT_LIST;           SORT LIST
; Function : LT;           Less Then
; Function : DEL_ATOM;           DELete ATOM
; Function : BAR_LIST;            draw BAR LIST
; Function : WRITE_DATA;            WRITE DATA file
; Function : LT;           Less Then
; function : str_position
; function: split_blist
; function : bardlg (bar dialog)
; function : djdg_lengthtxt;		Length of text ; text길이 구하기


;**************************************************
; Program : BAR
;           BAR
;           Yi Suk-Jong
;           96/9/4
;**************************************************
; 철근상세도에서 철근재료표용 data를 만들어준다.
; 실수 갯수 인식하도록 고침 98/7/11
; Dialog box를 통해 옵션 선택하도록 수정 01/12/06
; Attribute를 이용한 bar detail도 인식하도록 수정.
;**************************************************

(defun C:BAR( /
             sblist bblist 
	     scl ans ssc nc llist count cent cc rc fn ipnt ans34)

  (defun SETERR(s)                                      ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                     ;내장에러루틴 기동

  (setq th (getvar "DIMTXT"))                                 ;글자 크기
  (setq scl (getvar "DIMSCALE"))            ;scale값 잡아내기
  
  (setq #TH (* th scl))                     ;text높이

  (setq secondtbl nil)
  
  (push-env)                                ;환경변수 대피

  (bardlg)					;다이얼로그로 입력받음.


  (setq scl (getvar "DIMSCALE"))            ;scale값 잡아내기

   (setq ssc (ssget '((-4 . "<OR")           ;circle들과 block이름이 djdg_ratt인 insert 선택
                         (0 . "CIRCLE")
                         (-4 . "<AND")
                           (0 . "INSERT")
                           (2 . "djdg_ratt")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  
  (setq nc (sslength ssc))                  ;circle 및 att의 갯수

  (setq llist nil)                          ;빈 line-list 만듬

  (setq count 0)
  (repeat nc                                            ;circle갯수만큼 반복
    (setq cent (entget (ssname ssc count)))             ;circle정보
    (setq cc (cdr (assoc 10 cent)))                     ;center 좌표
    (setq rc (cdr (assoc 40 cent)))                     ;radius (반지름)
    (setq etype (cdr (assoc 0 cent)))			;entity의 type(Circle or insert(att))
    (if (= etype "CIRCLE")				;원인 경우와 attribute insert인 경우 분리해서 호출.
      (setq llist (append llist (list (b_mark cc rc #roundup))))   ;Marking 찾아서 (("A1" "D29" 12.5 50) ... )와 같이 철근정보 만들기.
      (setq llist (append llist (list (b_marka (cdr (assoc -1 cent )) #roundup)))) ;attribute블럭에서 철근정보 찾아 list에 추가.
    );if  
    (setq count (1+ count))                             ;다음 circle로
  ) ;of repeat

  (cond
    ((= #BAR_outtype "FILE")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file이름 입력받음
        (write_data llist fn)                                 ;파일로 만들기
      ) ;of progn
    ) ;of sub_cond
    ((or (= #BAR_outype "DRAW") (= #BAR_outype nil))
      (if (= #addg "1")
	(progn
          (setq spblist (split_blist llist 22))				;22기준으로 자르기
          (setq bblist (car spblist))   				;앞에것 22~32
          (setq sblist (cadr spblist))					;뒤에것 13~19
          (if bblist
            (progn
              (setq ipnt (getpoint "\nInsert point(D22~D32): "))             ;삽입점 입력받음
              (setq bound (bar_list bblist #addb ipnt scl))                  ;bar list그리기
	      (command "line" (car bound) (cadr bound))				;text용 bound check
            );progn
          );if
          (if sblist
            (progn
	      (if secondtbl 
	        (setq ipnt (list (car ipnt) (- #YBL (* #TH 15))))                           ;삽입점구하기 전테이블의 마지막 y값-
                (setq ipnt (getpoint "\nInsert point(D10~D19): "))             ;삽입점 입력받음
              );if
	      (bar_list sblist #adds ipnt scl)                             ;bar list그리기
	    );progn
          );if
        );progn
        (progn
          (setq ipnt (getpoint "\nInsert point: "))             ;삽입점 입력받음
          (bar_list llist #addratio ipnt scl)                             ;bar list그리기
        ) ;of progn
      );if
    ) ;of sub_cond
    ((= #BAR_outtype "ALL")
      (progn
        (setq fn (getfiled "Open data file" "" "dat" 1))      ;file이름 입력받음
        (write_data llist fn)                                 ;file로 만들기
        (setq ipnt (getpoint "\nInsert point: "))             ;삽입점 입력받음
        (bar_list llist #addratio ipnt scl)                             ;bar list그리기
      ) ;of progn
    ) ;of sub_cond
  ) ;of cond

  (pop-env)                                             ;환경변수 복귀

  (princ "\nNormal terminated")                         ;정상적으로 끝남 표시
  (setq *error* oer seterr nil)
  (princ)

) ;of defun



;**************************************************
; Function : B_MARK
;            Bar MARKing
;            Yi Suk-Jong
;            96/9/5
;**************************************************
; 철근 상세도에서 marking(철근번호)과 철근데이타를 찾아 철근정보들을 넘겨준다.
; (두점은 원의 오른쪽 상단과 밑줄의 끝점)
; 넘어오는 값:
;      IP : Insert point (원의 중심)
;       R : Radius       (원의 반지름)
;     A34 : 3사4입 여부(Yes/No)
; 넘어가는 값:
;     MARK : 철근 마킹
;      DIA : 철근의 직경
;        L : 철근의 길이(L,L',A.V.L이 동시에 존재할 경우 제일 큰 것으로)
;        N : 철근의 갯수
;**************************************************

(defun B_MARK(IP R A34 /
  A34 ix iy p1 p2 p3 ss ssn count tlst sent etype l1 l2 end_p nt
  txt1 txt2 MARK LD LD1 AVL L txt txtl N maxl
  )

  (setq ix (car IP)                         ;삽입점 x
        iy (cadr IP)                        ;       y
        p1 (list (- ix R) (+ iy R))         ;
        p2 (list (+ ix R) (- iy R))         ;
        p3 (list (+ ix R) (+ iy R))         ;
        ss (ssget "C" p1 p2)                ; MARKING/LINE 잡아내기
        ssn (sslength ss)
        count 0
        tlst nil)

  (cond
    ((= A34 4) (setq roundv 0.0035))
    ((= A34 5) (setq roundv 0.0045))
  );cond  
    
  (repeat ssn                                 ;엔티티 갯수만큼 반복
    (setq sent (entget (ssname ss count)))
    (setq etype (cdr (assoc 0 sent)))
    (cond
      ((= etype "TEXT")                       ;엔티티가 텍스트인 경우
        (progn
           (setq tlst (append tlst (list (cdr (assoc 1 sent))))) ;텍스트 리스트에 추가
        ) ;of progn
      ) ;of etype=TEXT
      ((= etype "LINE")                       ;엔티티가 라인인 경우
        (progn
          (setq l1 (cdr (assoc 10 sent))          ;첫점
                l2 (cdr (assoc 11 sent)))         ;끝점
          (if (> (car l1) (car l2))             ;끝점찾기
            (setq end_p l1)
            (setq end_p l2)
          ) ;of if
        ) ;of progn
      ) ;of etype=LINE
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat                                  ;다음 엔티티로

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

  ;---------------------------
  ; DIA , 길이, 갯수 잡아내기
  ;---------------------------

  (setq ss (ssget "C" p3 end_p '(( 0 . "TEXT"))))
  (setq ssn (sslength ss)
        count          0
        DIA            ""
        LD             0.0
        LD1            0.0
        AVL            0.0
        L              0.0
        N                0)

  (repeat ssn
    (setq txt (car (sp-trunc (cdr (assoc 1 (entget (ssname ss count))))))  ;텍스트 내용
          txtl                                        (strlen txt))        ;텍스트 길이

    (cond
      ((or (= (setq txt3 (substr txt 1 3)) "D13")
           (= txt3 "D10") (= txt3 "D16")  (= txt3 "D19") (= txt3 "D22")
           (= txt3 "D25") (= txt3 "D29") (= txt3 "D32")
           (= txt3 "H10") (= txt3 "H13")  (= txt3 "H16") (= txt3 "H19")
           (= txt3 "H22") (= txt3 "H25") (= txt3 "H29")
           (= txt3 "H32"))                                      ; DIA
        (setq DIA TXT)
      ) ;of sub_cond
      ((= (substr txt 1 2) "L=")                                ; L=
        (setq L (atof1 (substr txt 3 (- txtl 2))))
      ) ;of sub_cond
      ((= (substr txt 1 3) "L'=")                               ; L'=
        (setq LD (atof1 (substr txt 4 (- txtl 3))))
      ) ;of sub_cond
      ((= (substr txt 1 3) "L`=")                               ; L`=
        (setq LD1 (atof1 (substr txt 4 (- txtl 3))))
      ) ;of sub_cond
      ((= (substr txt 1 6) "A.V.L=")                            ;A.V.L=
        (setq AVL (atof1 (substr txt 7 (- txtl 6))))
      ) ;of sub_cond
      ((= (substr txt 1 2) "N=")                                ;N=
;        (setq N (atoi (substr txt 3 (- txtl 2))))                     ;정수로바꿈
        (setq N (atof (substr txt 3 (- txtl 2))))                     ;정수로바꿈
      ) ;of sub_cond
    ) ;of cond
    (setq count (1+ count))
  ) ;of repeat

  (setq maxl L)
  (if (> LD maxl) (setq maxl LD))
  (if (> LD1 maxl) (setq maxl LD1))             ; L, L'중 가장 큰 값 사용
  (if (> AVL 0.0) (setq maxl AVL))              ;A.V.L이 있으면 A.V.L 을 길이로

  (if (= maxl 0.0) (alert (strcat "<" MARK ">" " LENGTH NOT FOUND")))
  (if (= DIA "") (alert (strcat "<" MARK ">" " DIA NOT FOUND")))
  (if (= N 0) (alert (strcat "<" MARK ">" " NUMBER NOT FOUND")))

  (if (> A34 0)                                 ;3사4입의 적용여부
    (progn
      (setq rm (rem maxl 0.01))                     ; 3사 4입
      (if (>= rm roundv)                            ;나머지가 3.5MM이상일 때
        (setq L (+ maxl (- 0.01 rm)))                  ;올림
        (setq L (- maxl rm))                        ;나머지가 3.5MM이하일 때
      ) ;of if                                         ;버림
    );progn
    (setq L maxl)
  );if

  (list MARK DIA L N)

) ;of defun


;**************************************************
; Function : B_MARKA
;            get Bar MARKing from Attribute
;            Yi Suk-Jong
;            05/12/22
;**************************************************
; Attribute로 만들어진 철근 상세도에서 marking(철근번호)과 철근데이타를 찾아
; 철근정보들을 넘겨준다.
; (두점은 원의 오른쪽 상단과 밑줄의 끝점)
; (b_marka ename a34)
;       ename : entity name
;       A34 : 3사4입 여부(Yes/No)
; 결과값(MARK DIA L N)
;     MARK : 철근 마킹(string)
;      DIA : 철근의 직경(string)
;        L : 철근의 길이(L,L',A.V.L이 동시에 존재할 경우 제일 큰 것으로)(실수)
;        N : 철근의 갯수(실수)
;**************************************************

(defun B_MARKA(ename A34 /
  		attv roundv txt1 txt2 nt diastr lenstr lentstr
	       numstr txt3 splen splent spnum maxl rm )

  (setq attv (djdg_getattv ename))	;전체 attribute 잡아내기.
  
  (cond					
    ((= A34 4) (setq roundv 0.0035))
    ((= A34 5) (setq roundv 0.0045))
  );cond  
  
  (setq txt1 (cdr (assoc "MARK1" attv))		;marking text-1
	txt2 (cdr (assoc "MARK2" attv)))	;marking text-1

  (setq tlst nil)
  (if (/= txt1 "") (setq tlst (append tlst (list txt1))))	;text list에 추가
  (if (/= txt2 "") (setq tlst (append tlst (list txt2))))
  
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

  ;---------------------------
  ; DIA , 길이, 갯수 잡아내기
  ;---------------------------
  
  (setq DIA            ""
        LD             0.0
        L              0.0
        N                0)

  (setq diastr (cdr (assoc "DIA" attv)))  	;철근 dia
  (setq lenstr (cdr (assoc "LEN" attv)))	;철근 net길이
  (setq lentstr (cdr (assoc "LENT" attv)))	;철근 total길이.
  (setq numstr (cdr (assoc "NUM" attv)))	;갯수.

  ;----- Dia체크 
      (if (or (= (setq txt3 (substr diastr 1 3)) "D13")
           (= txt3 "D10") (= txt3 "D16")  (= txt3 "D19") (= txt3 "D22")
           (= txt3 "D25") (= txt3 "D29") (= txt3 "D32")
           (= txt3 "H10") (= txt3 "H13")  (= txt3 "H16") (= txt3 "H19")
           (= txt3 "H22") (= txt3 "H25") (= txt3 "H29")
           (= txt3 "H32"))                                      ; DIA
        (setq DIA txt3)
      ) ;if
  
	(setq splen (djdg_splitstr lenstr "="))
  	(if (= (length splen) 2)
	  (setq L (atof1 (cadr splen)))
	);if
  
  	(setq splent (djdg_splitstr lentstr "="))
  	(if (= (length splent) 2)
	  (setq LD (atof1 (cadr splent)))
	);if  
  
  	(setq spnum (djdg_splitstr numstr "="))
  	(if (= (length spnum) 2)
	  (setq N (atof (cadr spnum)))
	);if  

  
  (setq maxl L)
  (if (> LD maxl) (setq maxl LD))

  (if (= maxl 0.0) (alert (strcat "<" MARK ">" " LENGTH NOT FOUND")))
  (if (= DIA "") (alert (strcat "<" MARK ">" " DIA NOT FOUND")))
  (if (= N 0) (alert (strcat "<" MARK ">" " NUMBER NOT FOUND")))

  (if (> A34 0)                                 ;3사4입의 적용여부
    (progn
      (setq rm (rem maxl 0.01))                     ; 3사 4입
      (if (>= rm roundv)                            ;나머지가 3.5MM이상일 때
        (setq L (+ maxl (- 0.01 rm)))                  ;올림
        (setq L (- maxl rm))                        ;나머지가 3.5MM이하일 때
      ) ;of if                                         ;버림
    );progn
    (setq L maxl)
  );if

  (list MARK DIA L N)

) ;of defun


;**************************************
; Function : ATOF1
;            ATOF-1 (STRING --> REAL)
;            Yi Suk-Jong
;            96/9/5
;**************************************

(defun atof1(STR / n count dot)
  (setq     n (strlen str)
        count            1
        dot              0)
  (repeat n
    (if (= (substr str count 1) ".") (setq DOT count))
    (setq count (1+ count))
  ) ;of repeat
  (if (= dot 0) (* 0.001 (atof str))
                (atof str))
) ;of defun


;**************************************
; Function : ATOI1
;            ATOI-1 (STRING --> INTEGER)
;            Yi Suk-Jong
;            96/9/5
;**************************************
;
(defun ATOI1( STR / n count str chr)
  (setq     n (strlen str)
        count            1)
  (while (/= (setq chr (substr str count 1)) "")
    (if (= chr ",")
      (setq str (strcat (substr str 1 (1- count))
                        (substr str (1+ count) (- n count)))
              n (1- n))
    ) ;of if
    (setq count (1+ count))
  ) ;of while
  (atoi str)
) ;of defun


;*******************************************************************
;     Function : DATA-IN
;                DATA file IN
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************

(defun DATA-IN(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC


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

);of defun


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


;******************************************************
; Function : SORT_LIST
;           SORT LIST
;           Yi Suk-Jong
;           1996/2/23
;******************************************************
; list를 sort해준다.
; 넘어오는 값
;     ALIST : SORT되어야할 LIST
;      AIDX : 기준이 되는 sub list (첫 sub list = 0)
; 넘겨지는 값
;             SORT된 LIST
;******************************************************

(defun SORT_LIST(alist aidx
/       alist       nl       rlist       slist        count      min1
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq min1 (nth aidx (nth 0 rlist)))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (nth count1 rlist)))    ;현재 값
      (if (lt c_val min1)                             ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq min1 c_val)                          ;최소값을 현재 값으로
        ) ;of progn
      ) ;of if
      (setq count1 (1+ count1))                     ;다음 list로
    ) ;of repeat
    (setq slist (append slist (list (nth min_th rlist)))) ;최소값을 sort된 list에 추가
    (setq rlist (del_atom rlist min_th))            ;남은list에서 최소 list 제거
    (setq count (1- count))                         ;한개 줄여서
  ) ;of repeat
  (setq slist slist)
) ;of defun


;************************************************
; Function : LT
;           Less Then
;           Yi Suk-Jong
;           1996/2/27
;************************************************
; 인수-1이 인수-2보다 작은가를 판단해준다.
; 넘어오는 값:
;       ARG1 : 인수-1
;       ARG2 : 인수-2
; 넘어가는 값:
;       T    : 인수-1이 인수-2보다 작을 때
;       nil  : 인수-1이 인수-2보다 작지 않을때
;************************************************

(defun LT1(arg1 arg2)
  (if (and (is-num arg1) (is-num arg2))
    (< (atof arg1) (atof arg2))
    (< arg1 arg2)
  ) ;of if
) ;of defun


;************************************************
; Function : DEL_ATOM
;           DELete ATOM
;           Yi Suk-Jong
;           1996/2/23
;************************************************
; list에서 특정 atom을 지운다
; 넘어오는 값
;             b_list : 축출전 list
;               anth : 축출되야할 atom의 위치
; 넘겨가는 값
;                    : 축출후 list
;************************************************

(defun DEL_ATOM(b_list anth
/       b_list      mlist       a_list      count   ;지역변수
)

  (setq nlist (length b_list))                      ;list의 갯수

  (setq a_list nil)                                 ;빈 list생성
  (setq count 0)                                    ;첫번째 list부터

  (repeat nlist                                     ;list갯수만큼 반복
    (if (/= count anth)                             ;지정된 atom이 아닌경우만
      (setq a_list (append a_list (list (nth count b_list))))   ;list에다 추가
    ) ;of if
    (setq count (1+ count))
  ) ;of repeat

  (setq a_list a_list)

) ;of defun


;*******************************************
; Function : BAR_LIST
;            draw BAR LIST
;            Yi Suk-Jong
;            1996/2/26
;*******************************************
; 주어진 bar data로 bar list를 그림
; 넘어오는 값:
;      BLIST : bar data
;      IPNT  : insert point
;      S     : Scale factor
;*******************************************

(defun BAR_LIST(blist add ipnt s
/
       add     ipnt    s
       bd      bdx     bl      blist    blt     blx     bn      bnx     cdia
       cl      count   dcount  fdia     gtw     gtwa    hy      ix      iy
       lh      ln      ly      nb       nbx     ndia    oldc    sbcount sbn
       slist   stl     th      tl       tlist   tlx     tw      twa     twat
       twax    twt     twx     uwx      vy1     vy1     vy2     y       add_factor
)


  (setq th (getvar "DIMTXT"))                           ;글자 크기
  (setq lh 8)                                   	;한 line의 높이
  (setq secondtbl T)
;;;  (setq add (getreal "\n할증률<3%>: "))
;;;  (if (= add nil)                               ;엔터입력시 할증률은 3%
;;;    (setq add_factor 1.03)
    (setq add_factor (+ 1.0 (/ add 100.0)))
;;;  );if

  (setq th (* th s))                            ;글자크기 scale대로
  (setq #TH th)
  (setq lh (* lh s))                            ;줄높이 scale대로

  (setq ln (length blist))                      ;철근 data 갯수

  (setq blist (reverse (sort_list blist 1)))    ;철근 직경을 기준으로 sort

  (setq fdia (nth 1 (nth 0 blist)))             ;첫 철근 직경

  (setq ix   (car ipnt)
        iy   (cadr ipnt)
        bnx  (+ ix (* 10.0 s))
        bdx  (+ ix (* 30.0 s))
        blx  (+ ix (* 68 s))
        nbx  (+ ix (* 88 s))
        tlx  (+ ix (* 118 s))
        uwx  (+ ix (* 138 s))
        twx  (+ ix (* 168 s))
        twax (+ ix (* 198 s)))

  (setq slist nil                               ;sub list
        tlist nil)                              ;total list

  (setq tl   0.0                                ;같은 철근 길이 합
        tw   0.0)                               ;총 중량

  (setq count 0)                                ;두번째 철근부터
  (repeat ln                                    ;끝까지 반복
    (setq cdia (nth 1 (nth count blist)))       ;현재의 철근직경
    (if (= fdia cdia)                           ;현재직경이 이전직경과 같을 때
      (setq slist (append slist (list (nth count blist))))
      (progn
        (setq tlist (append tlist (list slist)))
        (setq slist (list (nth count blist)))
        (setq fdia cdia)
      ) ;of progn ELSE
    ) ;of if
    (setq count (1+ count))                         ;다음 철근으로
  ) ;of repeat

  (setq tlist (append tlist (list slist)))          ;마지막 철근묶음 추가

  (setq hy (- iy (/ lh 2.0)))                 ;Header의 y위치

  (command "TEXT" "M" (list (+ ix (* 100.0 s)) (+ iy (* th 5)))
                  (* 6.0 s) "0.0" "철 근 재 료 표")
  (command "TEXT" "M" (list (+ ix (*  10.0 S)) hy) th "0.0" "번 호")
  (command "TEXT" "M" (list (+ ix (*  30.0 S)) hy) th "0.0" "직 경")
  (command "TEXT" "M" (list (+ ix (*  55.0 S)) hy) th "0.0" "길    이")
  (command "TEXT" "M" (list (+ ix (*  80.0 S)) hy) th "0.0" "갯 수")
  (command "TEXT" "M" (list (+ ix (* 105.0 S)) hy) th "0.0" "총  길  이")
  (command "TEXT" "M" (list (+ ix (* 130.0 S)) hy) th "0.0" "단위중량")
  (command "TEXT" "M" (list (+ ix (* 155.0 S)) hy) th "0.0" "총  중  량")
  (command "TEXT" "M" (list (+ ix (* 185.0 S)) hy) th "0.0" "비 고")


;  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0" "ADD 3%")

  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0"
           (strcat "ADD " (rtos (* (- add_factor 1.0) 100) 2 0) "%"))

;   (command "INSERT" (strcat (prefix) "blocks/bhead") ipnt scl scl "0")

  (setq ly (- hy  (/ lh 2.0)))                      ;line y좌표

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "7")
  (command "LINE" (list ix iy) (list (+ ix (* 200 s)) iy) "")   ;윗line그리기
  (setvar "CECOLOR" "1")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")   ;아래line그리기
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (*   0 s)) iy) (list (+ ix (*   0 s)) (- iy lh)) "")
  (setvar "CECOLOR" "1")
  (command "LINE" (list (+ ix (*  20 s)) iy) (list (+ ix (*  20 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  40 s)) iy) (list (+ ix (*  40 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  70 s)) iy) (list (+ ix (*  70 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (*  90 s)) iy) (list (+ ix (*  90 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 120 s)) iy) (list (+ ix (* 120 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 140 s)) iy) (list (+ ix (* 140 S)) (- iy lh)) "")
  (command "LINE" (list (+ ix (* 170 s)) iy) (list (+ ix (* 170 S)) (- iy lh)) "")
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (* 200 s)) iy) (list (+ ix (* 200 S)) (- iy lh)) "")
  (setvar "CECOLOR" oldc)


  (setq iy (- iy (* 1.5 lh)))                       ;새로운 insert point
  (setq ndia (length tlist))                        ;직경 갯수

  (setq cl 0                                        ;현재 line번호
        gtw 0.0                                     ;Grand total weight
        gtwa 0.0)

  (setq dcount 0)                                   ;DIA count

  (repeat ndia                                      ;DIA갯수만큼 반복
    (setq dlist (sort_list (nth dcount tlist) 0))   ;DIA별 묶음
    (setq cdia (nth 1 (nth 0 dlist)))               ;현재 직경
    (setq sbn (length dlist))                       ;묶음내 갯수
    (setq stl 0.0)                                  ;Sub total length
    (setq vy1 cl)

    (setq sbcount 0)                                ;Sub bar count
    (repeat sbn                                     ;철근 갯수만큼반복
      (setq bn (nth 0 (nth sbcount dlist))          ;철근번호
            bd (nth 1 (nth sbcount dlist))          ;철근직경
            bl (nth 2 (nth sbcount dlist))          ;철근길이
            nb (nth 3 (nth sbcount dlist)))         ;철근갯수
      (if (/= sbcount 0) (setq bd (chr 34)))        ;처음것이 아니면 같음 표시
      (if (< bl 1.0)
        (setq blt (rtos (* bl 1000.0) 2 0))
        (setq blt (rtos bl 2 3))
      ) ;of if
      (setq tl  (* bl nb)                           ;길이*갯수
            stl (+ stl tl))                         ;소계길이

      (setq y    (- iy (* cl lh)))                  ;현재 line y위치

      (command "TEXT" "M" (list bnx y) th "0.0" bn)              ;번호
      (command "TEXT" "M" (list bdx y) th "0.0" bd)              ;직경
      (command "TEXT" "MR" (list blx y) th "0.0" blt)            ;길이
;      (command "TEXT" "MR" (list nbx y) th "0.0" (itoa nb))        ;정수갯수
;      (command "TEXT" "MR" (list nbx y) th "0.0" (rtos nb 2 3))    ;실수갯수

      (command "TEXT" "MR" (list nbx y) th "0.0"        ;정수갯수,실수갯수판단
        (if (> (- nb (fix nb)) 0)                       ;소수점 이하갯수면
          (rtos nb 2 3)                                 ;실수갯수
          (rtos nb 2 0)
        );if
      );command

      (command "TEXT" "MR" (list tlx y) th "0.0" (rtos tl 2 3))  ;총길이

      (setq ly (- y (/ lh 2.0)))                                    ;line y좌표
      (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "1")
      (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line그리기
      (setvar "CECOLOR" oldc)

      (setq sbcount (1+ sbcount))                                   ;다음철근
      (setq cl (+ cl 1))                                            ;현재 line
    ) ;of repeat

    (cond
      ((= cdia "D10") (setq uw 0.560))
      ((= cdia "D13") (setq uw 0.995))
      ((= cdia "D16") (setq uw 1.560))
      ((= cdia "D19") (setq uw 2.250))
      ((= cdia "D22") (setq uw 3.040))
      ((= cdia "D25") (setq uw 3.980))
      ((= cdia "D29") (setq uw 5.040))
      ((= cdia "D32") (setq uw 6.230))
      ((= cdia "H10") (setq uw 0.560))
      ((= cdia "H13") (setq uw 0.995))
      ((= cdia "H16") (setq uw 1.560))
      ((= cdia "H19") (setq uw 2.250))
      ((= cdia "H22") (setq uw 3.040))
      ((= cdia "H25") (setq uw 3.980))
      ((= cdia "H29") (setq uw 5.040))
      ((= cdia "H32") (setq uw 6.230))
    ) ;of cond

    (setq tw (* stl uw 0.001)                                   ;총중량
          twt (rtos tw 2 3)
          tw (atof twt)
;          twa (* tw 1.03)                                       ;총중량(가산)
          twa (* tw add_factor)                                       ;총중량(가산)
          twat (rtos twa 2 3)
          twa (atof twat)
          gtw (+ gtw tw)
          gtwa (+ gtwa twa))                                    ;전체중량

    (setq    y  (- iy (* lh cl)))                               ;현재 line y좌표

    (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "소      계")
;    (command "INSERT" (strcat (prefix) "BLOCKS/BSUBT")         ;소계
;                      (list (/ (+ ix nbx) 2.0) y)
;                      scl scl "0")
    (command "TEXT" "MR" (list tlx y) th "0.0" (rtos stl 2 3))  ;총길이 소계
    (command "TEXT" "MR" (list uwx y) th "0.0" (rtos uw 2 3))   ;단위중량
    (command "TEXT" "MR" (list twx y) th "0.0" twt)   ;총중량소계
    (command "TEXT" "MR" (list twax y) th "0.0" twat) ;총중량소계(가산)

    (setq ly (- y (/ lh 2.0)))                                    ;line y좌표

    (setq vy1 (- iy (* vy1 lh) (/ lh -2.0))
          vy2 (- iy (* cl lh) (/ lh -2.0)))

    (setq oldc (getvar "CECOLOR"))  (setvar "CECOLOR" "1")
    (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line그리기
    (command "LINE" (list (+ ix (*  20 s)) vy1) (list (+ ix (*  20 S)) vy2) "")
    (command "LINE" (list (+ ix (*  40 s)) vy1) (list (+ ix (*  40 S)) vy2) "")
    (command "LINE" (list (+ ix (*  70 s)) vy1) (list (+ ix (*  70 S)) vy2) "")
    (setvar "CECOLOR" oldc)

    (setq cl (1+ cl))                                           ;현재 line번호
    (setq dcount (1+ dcount))                                   ;다음 직경으로
  ) ;of repeat

  (setq y (- iy (* lh cl)))                                 ;y좌표

  (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "총      계")
;  (command "INSERT" (strcat (prefix) "BLOCKS/BGRDT")            ;총계
;                    (list (/ (+ ix nbx) 2.0) y)
;                    scl scl "0")
  (setq txttotal (rtos gtw 2 3))    ;전체중량 text
  (setq txttotala (rtos gtwa 2 3))    ;전체중량(할증) text
  
  (command "TEXT" "MR" (list twx y) th "0.0" txttotal)    ;천체중량
  (command "TEXT" "MR" (list twax y) th "0.0" txttotala)  ;천체중량(가산)

  (setq ldec1 (- (djdg_lengthtxt (substr txttotal (- (strlen txttotal) 3) 4))
	         (* 0.5 (djdg_lengthtxt "."))))                                                 ;소수점이하 길이
  (setq ldec2 (- (djdg_lengthtxt (substr txttotala (- (strlen txttotala) 3) 4))
                 (* 0.5 (djdg_lengthtxt "."))))
;  (command "TEXT" "M" (list (- twx (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
;  (command "TEXT" "M" (list (- twax (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twx ldec1) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twax ldec2) (+ y (/ th 2.0))) th "0.0" "T")

  (setq ly (- y (/ lh 2.0)))                                    ;line y좌표

  (setq vy1 (+ iy (/ lh 2.0))
        vy2 (- iy (* (1+ cl) lh) (/ lh -2.0)))

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "7")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")
  (command "LINE" (list ix vy1) (list ix vy2) "")
  (setvar "CECOLOR" "1")
  (command "LINE" (list (+ ix (* 90 s)) vy1) (list (+ ix (* 90 s)) vy2) "")
  (command "LINE" (list (+ ix (* 120 s)) vy1) (list (+ ix (* 120 s)) vy2) "")
  (command "LINE" (list (+ ix (* 140 s)) vy1) (list (+ ix (* 140 s)) vy2) "")
  (command "LINE" (list (+ ix (* 170 s)) vy1) (list (+ ix (* 170 s)) vy2) "")
  (setvar "CECOLOR" "7")
  (command "LINE" (list (+ ix (* 200 s)) vy1) (list (+ ix (* 200 s)) vy2) "")
  (setvar "CECOLOR" oldc)
  
  ;(command "circle" (list (+ ix (* 200 s)) vy2) 100)
  (setq #YBL vy2)  ;y of bottom line
  (list (list (car ipnt) vy2) (list (+ ix (* 200 s)) (cadr ipnt)))	;좌측하단-우측상단좌표
) ;of defun



;*************************************************
; Function : WRITE_DATA
;            WRITE DATA file
;            Yi Suk-Jong
;            96/9/6
;*************************************************
; list로 구성된 data list를 받아서 파일로 만들어준다.
; 넘어오는 값
;     LLIST : 출력하고싶은 list
;        FN : File Name
;*************************************************

(defun WRITE_DATA( LLIST FN / nl count opf nl line nb)

  (setq nl (length LLIST)                               ;list의 갯수
        count           0)                              ;첫 list부터

  (setq opf (open fn "w"))                              ;file열기

  (repeat nl                                            ;list갯수만큼 반복
    (setq nb (nth 3 (nth count llist)))
    (setq line (strcat (nth 0 (nth count llist)) ","
                       (nth 1 (nth count llist)) ","
                       (rtos (nth 2 (nth count llist)) 2 3) ","
;                       (itoa (nth 3 (nth count llist)))
                       (if (> (- nb (fix nb)) 0)        ;소수점 이하갯수면
                         (rtos nb 2 3)                  ;실수갯수
                         (rtos nb 2 0)                  ;정수갯수
                       );if
               );strcat
    );setq
    (write-line line opf)                               ;파일에 쓰기
    (setq count (1+ count))                             ;다음 list로
  ) ;of repeat

  (close opf)                                           ;파일 닫기
  (princ "\nDATA FILE ")
  (princ FN)
  (princ " is made")

) ;of defun

;************************************************
; Function : LT
;           Less Then
;           Yi Suk-Jong
;           1996/2/27
;************************************************
; 인수-1이 인수-2보다 작은가를 판단해준다.
; 넘어오는 값:
;       ARG1 : 인수-1
;       ARG2 : 인수-2
; 넘어가는 값:
;       T    : 인수-1이 인수-2보다 작을 때
;       nil  : 인수-1이 인수-2보다 작지 않을때
;************************************************

(defun LT(arg1 arg2)
  (setq hpos1 (str_position arg1 "-")
        hpos2 (str_position arg2 "-"))
  (if (and (= hpos1 nil) (= hpos2 nil))
    (lt1 arg1 arg2)
    (progn
      (if hpos1
        (setq arg1f (substr arg1 1 (1- hpos1)))
        (setq arg1f arg1)
      );if
      (if hpos2
        (setq arg2f (substr arg2 1 (1- hpos2)))
        (setq arg2f arg2)
      );if      
      (if (= arg1f arg2f)
        (lt1 arg1 arg2)
        (lt1 arg1f arg2f)
      ) ;of if
    );progn
  );if
;  (if (and (is-num arg1) (is-num arg2))
;    (< (atof arg1) (atof arg2))
;    (< arg1 arg2)
;  ) ;of if
) ;of defun

;----------------------------------
; function : str_position
;            Yi Suk Jong
;            00/7/15
;----------------------------------
; str1 : long string
; str2 : short string
;----------------------------------
(defun str_position(str1 str2 / str1 str2 len1 len2 count )

  (setq len1 (strlen str1)
        len2 (strlen str2))
  (setq count 1)
  (while (and (/= (substr str1 count len2) str2) (<= count (- len1 len2 -1)))
    (setq count (1+ count))
  ); repeat  
 (if (> count (- len1 len2 -1)) nil count)
);defun str_position

;-----------------------------------
; function: split_blist
;           Yi Suk Jong
;           01/11/30
;-----------------------------------
(defun split_blist( blist idia /
                    )
;  (setq blist (reverse (sort_list blist 1)))    ;철근 직경을 기준으로 sort
  (setq bbarlist nil
        sbarlist nil)
  (setq nbar (length blist))
  (setq ibar 0)
  (repeat nbar
    (setq diaofbar (atoi (substr (nth 1 (nth ibar blist)) 2 2)))
    (if (>= diaofbar idia)
      (setq bbarlist (append bbarlist (list (nth ibar blist))))
      (setq sbarlist (append sbarlist (list (nth ibar blist))))
    )
    (setq ibar (1+ ibar))
  );repeat
  (list bbarlist sbarlist)
);defun

;------------------------------
; function : bardlg (bar dialog)
;	Yi Suk Jong
;------------------------------
(defun bardlg(/  
                  dcl_id )  
  
  (setq dcl_id (load_dialog "DJDG"))                 ;ddscl.dcl load  
  (if (not (new_dialog "bar" dcl_id)) (exit))         ;ddscl.dcl 안의 scl빨몹  

;  (if (= #tr34 "1") (set_tile "tr34" "1"))
  (cond
    ((= #roundup 4) (set_tile "round4" "1"))
    ((= #roundup 5) (set_tile "round5" "1"))
    ((= #roundup 0) (set_tile "roundno" "1"))
    ((= #roundup nil)
      (set_tile "roundno" "1")
      (setq #roundup 0))
  );cond
  (if (/= #addratio nil) (set_tile "addratio" (rtos #addratio 2 0)) (set_tile "addratio" "3"))
  (if (/= #adds nil) (set_tile "adds" (rtos #adds 2 0)) (set_tile "adds" "3"))
  (if (/= #addb nil) (set_tile "addb" (rtos #addb 2 0)) (set_tile "addb" "6"))
  (if (= #addg "1") (set_tile "addg" "1")(set_tile "addg" "0"))
  (cond							;output type초기화
    ((= #BAR_outtype "DRAW") (set_tile "drawtbl" "1"))
    ((= #BAR_outtype "FILE") (set_tile "fileout" "1"))
    ((= #BAR_outtype nil) (set_tile "drawtbl" "1")(setq #BAR_outtype "DRAW"))
  );cond  
  (set_addg)
  ;  (alert #addg)
;;;  (set_tile "div" (rtos 10 2 0))
;;;  (set_tile "prop" (strcat "Y = aX^2 = " (rtos #a 1 6) " X^2"))
;;;  (set_tile "cap" #cap)  
;;;  (set_tile "both" #both)
;;;  (set_tile "length"  (strcat "L= " (rtos h 2 3)))
;;;  (set_tile "height"  (strcat "H= " (rtos l2 2 3)))


;  (action_tile "tr34" "(set_tr34)")     ;user 럼력 box
  (action_tile "round4" "(setq #roundup 4)")
  (action_tile "round5" "(setq #roundup 5)")
  (action_tile "roundno" "(setq #roundup 0)")  
  (action_tile "addg" "(set_addg)")               ;user 럼력 box
  (action_tile "addratio" "(set_val $key)")               ;user 럼력 box
  (action_tile "adds" "(set_val $key)")               ;user 럼력 box
  (action_tile "addb" "(set_val $key)")               ;user 럼력 box
  (action_tile "drawtbl" "(setq #BAR_outtype \"DRAW\")")
  (action_tile "fileout" "(setq #BAR_outtype \"FILE\")")
  (action_tile "cancel" "(do_cancel)")                ;CALCEL button
  (action_tile "accept" "(do_accept)")                ;CALCEL button
  (start_dialog)  
  (unload_dialog dcl_id)  
  (princ)  
) ;of defun para-DIALOG

(defun set_tr34 ()
  (setq #tr34 (get_tile "tr34"))
);defun

    
(defun set_addg ()
  (setq #addg (get_tile "addg"))
  (if (= #addg "0")
    (progn
      (mode_tile "adds" 1)
      (mode_tile "addb" 1)
      (mode_tile "addratio" 0)
    );progn
    (progn
      (mode_tile "adds" 0)
      (mode_tile "addb" 0)
      (mode_tile "addratio" 1)      
    );progn  
  );if
);defun

(defun set_val(key )
  (setq val (atof (get_tile key)))
  (if (<= val 0.0)
    (progn
      (set_tile "error" "Invalid Input")
      (mode_tile key 2)
      (mode_tile key 3)
    );progn
    (progn
      (cond
        ((= key "addratio") (setq #addratio val))
        ((= key "adds") (setq #adds val))
        ((= key "addb") (setq #addb val))
      );cond
      (set_tile "error" "")
    );progn
  );if
);defun

(defun do_cancel()
  (done_dialog)
  (exit)
);defun

(defun do_accept()           ;dialog box를 끝내기 전에 모든 입력 데이타 확인
  (if (and (set_val "addratio")
           (set_val "adds") 
           (set_val "addb"))
    (done_dialog)
  ) ;of IF
) ;of defun

;----------------------------
; function : djdg_lengthtxt
;		Length of text
;	Yi Suk Jong
;----------------------------
(defun djdg_lengthtxt(txt)
  (setq tb (textbox (list (cons 1 txt)))) 	;textbox
  (- (car (nth 1 tb)) (car (nth 0 tb)))         ;length of text
);defun


(defun c:test()
   (setq ssc (ssget '((-4 . "<OR")           ;solid나 solid hatch만 선택
                         (0 . "CIRCLE")
                         (-4 . "<AND")
                           (0 . "INSERT")
                           (2 . "djdg_ratt")
                         (-4 . "AND>")
                       (-4 . "OR>"))))
  (djdg_mtredraw ssc 3)
);


;**************************************************
; Program : BARV : 변수가 적용된 철근재료표 만들기
;           Bar material list with Variable
;           Yi Suk-Jong
;           1996/5/7
;**************************************************
; data file을 읽어 철근 재료표를 만들어준다
; 실수 갯수를 인식하도록 고침 98/7/11
; 할증률을 사용자가 입력하도록 고침 99/1/27
; Pier번호를 행으로, 변수명을 컬럼으로 하도록 수정 06/08/10
;**************************************************

(defun C:BARV(
/       scl     ilist    llist   fn      opf     ilnum
        num_marking      num_var l_count count   num_table
        tname   var_list ch      inline  ibar_list
        tcount  total_list       str0    str1    str2
        str3    lst      bcount
        llist   ipnt
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                     ;내장에러루틴 가동

  (push-env)                                            ;환경변수 대피

  (setq scl (getvar "DIMSCALE"))                        ;스케일값 구함


  (setq ilist nil                                       ;빈 input line list만듬
        llist nil)                                      ;빈 data line-list 만듬


  (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name입력
  (setq opf (open fn "r"))                          ;file open

  (while (setq ch (read-line opf))                  ;한줄을 읽는다
    (if (/= (substr ch 1 1) ";")                    ; ";"입력시 무시
      (setq ilist (append ilist (list ch)))         ;ilist에 추가
    );if
  );while

  (setq ilnum (length ilist))                       ;input line수
  (setq num_marking 0                               ;marking 수
        num_var     0)                              ;variable 수

  (setq count 0)
  (repeat ilnum
    (if (= (strcase (substr (nth count ilist) 1 3)) "VAR")  ;var가 발견뻍을 때
      (setq num_marking count                               ;marking갯수
;            num_var (- ilnum count 1))                      ;variable갯수-->table갯수
	    num_table (- ilnum count 1))			;table갯수(p1,p2,p3...)
    );if
    (setq count (1+ count))
  );repeat


  (if (= num_table 0)
    (setq num_marking   ilnum                           ;variable이 없는 경우
          num_table     1    )                          ;마킹갯수=line갯수, 테이블갯수=1
    (progn                                            ;bariable이 있는 경우
;      (setq tnames (data-in (nth num_marking ilist))) ;table이름
      (setq vnames (data-in (nth num_marking ilist))) ;variable이름      
;      (setq num_table (1- (length tnames)))           ;table갯수
      (setq num_var (1- (length vnames)))           ;Variable갯수      

      (setq count (1+ num_marking)             
            tbl_list nil)

      (repeat num_table                          ;table 리스트 만들기(("P1" 1.2 3.4) ("P2" 1.2 4.5))
        (setq tbl_list (append tbl_list (list (data-in (nth count ilist)))))
        (setq count (1+ count))
      );repeat
    );progn
  );if

  (setq count 0                             ;(( "1" "d29" "1.4" "45") ...)
        ibar_list nil)
  (repeat num_marking                       ;marking입력줄들 모음
    (setq  ibar_list (append ibar_list (list (data-in (nth count ilist)))))
    (setq count (1+ count))
  );repeat

  (if (= num_var 0) (setq num_var 1))
  (setq tcount 0)                                       ;table갯수만큼 반복
  (setq total_list nil)                                 ;총 table리스트
  (repeat num_table
    (setq bcount 0)
    (setq llist nil)
    (repeat num_marking                                 ;marking갯수만큼 반복
      (setq inline (nth bcount ibar_list))
      (setq str0 (strcase (car (sp-trunc (nth 0 inline)))))       ;천근번호
      (setq str1 (strcase (car (sp-trunc (nth 1 inline)))))       ;철근직경
      (setq str2 (car (sp-trunc (nth 2 inline))))       ;철근길이
      (setq str3 (car (sp-trunc (nth 3 inline))))       ;철근갯수(실수)
      (if (= (substr str1 1 1) "$")                     ;철근직경이 변수일 때
        (setq str1 (findvar tbl_list vnames str1 tcount))
      );if
      (if (= (substr str2 1 1) "$")                     ;철근길이가 변수일때
        (setq str2 (atof (findvar tbl_list vnames str2 tcount)))
        (setq str2 (atof str2))
      );if
      (if (= (substr str3 1 1) "$")                     ;철근갯수가 변수일 때
        (setq str3 (atof (findvar tbl_list vnames str3 tcount)))
        (setq str3 (atof str3))
      );if
      (setq lst (list str0 str1 str2 str3))                 ;철근1개에 대한 data
      (setq llist (append llist (list lst)))                ;llist에 추가
      (setq bcount (1+ bcount))
    );repeat

    (setq total_list (append total_list (list llist)))      ;총 철근데이타


    (setq tcount (1+ tcount))
  );repeat


  (princ "\n") (princ num_marking) (princ " Marking found")
  (princ "\n") (princ num_table)   (princ " Table found")
  (princ "\n") (princ num_var) (princ " Variable found")

  (close opf)                                       ;file close


  (setq dsth (* (getvar "dimscale") (getvar "dimtxt")))		;dimscale이 적용된 text높이
  (setq gap (* dsth 5))						; 테이블 간격 5글자만큼  
  (setq br (getpoint "\nInsert point: "))               	;삽입점 입력받음
  (setq count 0)  (repeat num_table                                         	;TABLE갯수만큼 반복
    (setq tname (strcat "[" (car (nth count tbl_list)) "]"))			;table name
    (setq ip (list (+ (car br) (* (if (> count 0) 1 0) gap)) (cadr br)))	;삽입점
    (command "text" (list (car ip) (+ (cadr ip) (* dsth 0.5)))
			  dsth  "0" tname) 			;table제목 쓰기    
    (setq bound (bar_list (nth count total_list) 3 ip scl))              ;bar list그리기 bound return받음
    (setq br (nth 1 bound))
    (setq count (1+ count))
  );repeat

  (pop-env)                                             ;환경변수 복귀

  (princ "\nNormal terminated")                         ;정상적으로 끝남 표시

;  (setq *error* oer seterr nil)

  (princ)

) ;of defun


;-----------------------------------------------------------------------
;function : findvar
;           해당변수의 값을 찻아낸다.
;           99/4/5
;           Yi Suk Jong
;-----------------------------------------------------------------------
; (findvar var_list var tindex)
;>>> 넘어오는 값
; tbl_list : table list    예: (("P1" "3.5" "2.4" "7.0") ("P2" "5" "7" "5"))
; varlist : 변수 list	   예: ("var" "$A1L" "$A1N")
; varname : 찻고자하는 변수명  예: $AL
;  tindex : table의 index 예: var, P1, P2, P3 일때  P1에 대하여는 0
;>>> 사용예
; (findvar var_list $AL 0) : P1에 대하여 $AL에 해당하는 값(string)
;-----------------------------------------------------------------------
(defun findvar(tbl_list vlist varname tindex
/ vpos found
;var_list var tindex
)
  (setq found nil)
  (setq vpos (vl-position varname vlist))                 ;찾고자하는 변수명의 idnex
  (setq found (nth vpos (nth tindex tbl_list)))		
  (if (/= found nil)
    (setq found found)
    (setq found nil)
  );if
);defun
