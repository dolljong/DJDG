;**************************************************
; Program : HBLIST
;           Hangul Bar LIST
;           Yi Suk-Jong
;           1996/5/7
;**************************************************
; data file을 읽어 철근 재료표를 만들어준다
; 실수 갯수를 인식하도록 고침 98/7/11
; 할증률을 사용자가 입력하도록 고침 99/1/27
;**************************************************

(defun C:HBLIST(
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

  (setq count 0)
  (repeat num_table                                         ;TABLE갯수만큼 반복
    (setq ipnt (getpoint "\nInsert point: "))               ;삽입점 입력받음

    (bar_list (nth count total_list) ipnt scl)              ;bar list그리기

    (setq count (1+ count))
  );repeat

  (pop-env)                                             ;환경변수 복귀

  (princ "\nNormal terminated")                         ;정상적으로 끝남 표시

;  (setq *error* oer seterr nil)

  (princ)

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
/       alist       nl       rlist       slist        count      min
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq min (nth aidx (nth 0 rlist)))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (nth count1 rlist)))    ;현재 값
      (if (lt c_val min)                             ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq min c_val)                          ;최소값을 현재 값으로
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

(defun LT(arg1 arg2)
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

(defun BAR_LIST(blist ipnt s
/      blist   ipnt    s
       bd      bdx     bl      blist    blt     blx     bn      bnx     cdia
       cl      count   dcount  fdia     gtw     gtwa    hy      ix      iy
       lh      ln      ly      nb       nbx     ndia    oldc    sbcount sbn
       slist   stl     th      tl       tlist   tlx     tw      twa     twat
       twax    twt     twx     uwx      vy1     vy1     vy2     y       add_factor
)


  (setq th (getvar "DIMTXT"))                                 ;글자 크기
  (setq lh 7)
                                     ;한 line의 높이
  (setq add (getreal "\n할증률<3%>: "))
  (if (= add nil)                               ;엔터입력시 할증률은 3%
    (setq add_factor 1.03)
    (setq add_factor (+ 1 (/ add 100)))
  );if

;  (setq add_factor 1.03)                        ;할증률

  (setq th (* th s))                            ;글자크기 scale대로
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
;  (command "TEXT" "M" (list (+ ix (* 100.0 s)) (+ iy (* th 5)))
;                  (* 6.0 s) "0.0" "B A R  L I S T")
;  (command "TEXT" "M" (list (+ ix (*  10.0 S)) hy) th "0.0" "MARK")
;  (command "TEXT" "M" (list (+ ix (*  30.0 S)) hy) th "0.0" "DIA")
;  (command "TEXT" "M" (list (+ ix (*  55.0 S)) hy) th "0.0" "LENGTH")
;  (command "TEXT" "M" (list (+ ix (*  80.0 S)) hy) th "0.0" "NUM")
;  (command "TEXT" "M" (list (+ ix (* 105.0 S)) hy) th "0.0" "TOTAL L.")
;  (command "TEXT" "M" (list (+ ix (* 130.0 S)) hy) th "0.0" "UNIT W.")
;  (command "TEXT" "M" (list (+ ix (* 155.0 S)) hy) th "0.0" "TOTAL W.")
;  (command "TEXT" "M" (list (+ ix (* 185.0 S)) hy) th "0.0" "REMARKS")
  (command "TEXT" "M" (list (+ ix (* 185.0 s)) (- hy lh)) th "0.0"
           (strcat "ADD " (rtos (* (- add_factor 1.0) 100) 2 0) "%"))

  (command "INSERT" (strcat (prefix) "blocks/bhead") ipnt scl scl "0")

  (setq ly (- hy  (/ lh 2.0)))                                  ;line y좌표
;  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list ix iy) (list (+ ix (* 200 s)) iy) "")   ;윗line그리기
;  (setvar "CECOLOR" "RED")
;  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")   ;아래line그리기
;  (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list (+ ix (*   0 s)) iy) (list (+ ix (*   0 s)) (- iy lh)) "")
;  (setvar "CECOLOR" "RED")
;  (command "LINE" (list (+ ix (*  20 s)) iy) (list (+ ix (*  20 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  40 s)) iy) (list (+ ix (*  40 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  70 s)) iy) (list (+ ix (*  70 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (*  90 s)) iy) (list (+ ix (*  90 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 120 s)) iy) (list (+ ix (* 120 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 140 s)) iy) (list (+ ix (* 140 S)) (- iy lh)) "")
;  (command "LINE" (list (+ ix (* 170 s)) iy) (list (+ ix (* 170 S)) (- iy lh)) "")
;  (setvar "CECOLOR" "WHITE")
;  (command "LINE" (list (+ ix (* 200 s)) iy) (list (+ ix (* 200 S)) (- iy lh)) "")
;  (setvar "CECOLOR" oldc)

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
;      (command "TEXT" "MR" (list nbx y) th "0.0" (itoa nb))  ;정수갯수

      (command "TEXT" "MR" (list nbx y) th "0.0"
        (if (> (- nb (fix nb)) 0)                         ;소수점 이하갯수면
          (rtos nb 2 3)                                    ;실수갯수
          (rtos nb 2 0)
        );if
      );command

      (command "TEXT" "MR" (list tlx y) th "0.0" (rtos tl 2 3))  ;총길이

      (setq ly (- y (/ lh 2.0)))                                    ;line y좌표
      (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "RED")
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
          twa (* tw add_factor)                                       ;총중량(가산)
          twat (rtos twa 2 3)
          twa (atof twat)
          gtw (+ gtw tw)
          gtwa (+ gtwa twa))                                    ;전체중량

    (setq    y  (- iy (* lh cl)))                               ;현재 line y좌표

;    (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "S U B  T O T A L")
    (command "INSERT" (strcat (prefix) "BLOCKS/BSUBT")         ;소계
                      (list (/ (+ ix nbx) 2.0) y)
                      scl scl "0")
    (command "TEXT" "MR" (list tlx y) th "0.0" (rtos stl 2 3))  ;총길이 소계
    (command "TEXT" "MR" (list uwx y) th "0.0" (rtos uw 2 3))   ;단위중량
    (command "TEXT" "MR" (list twx y) th "0.0" twt)   ;총중량소계
    (command "TEXT" "MR" (list twax y) th "0.0" twat) ;총중량소계(가산)

    (setq ly (- y (/ lh 2.0)))                                    ;line y좌표

    (setq vy1 (- iy (* vy1 lh) (/ lh -2.0))
          vy2 (- iy (* cl lh) (/ lh -2.0)))

    (setq oldc (getvar "CECOLOR"))  (setvar "CECOLOR" "RED")
    (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line그리기
    (command "LINE" (list (+ ix (*  20 s)) vy1) (list (+ ix (*  20 S)) vy2) "")
    (command "LINE" (list (+ ix (*  40 s)) vy1) (list (+ ix (*  40 S)) vy2) "")
    (command "LINE" (list (+ ix (*  70 s)) vy1) (list (+ ix (*  70 S)) vy2) "")
    (setvar "CECOLOR" oldc)

    (setq cl (1+ cl))                                           ;현재 line번호
    (setq dcount (1+ dcount))                                   ;다음 직경으로
  ) ;of repeat

  (setq y (- iy (* lh cl)))                                 ;y좌표

;  (command "TEXT" "M" (list (/ (+ ix nbx) 2.0) y) th "0.0" "G R A N D  T O T A L")
  (command "INSERT" (strcat (prefix) "BLOCKS/BGRDT")            ;총계
                    (list (/ (+ ix nbx) 2.0) y)
                    scl scl "0")
  (command "TEXT" "MR" (list twx y) th "0.0" (rtos gtw 2 3))    ;천체중량
  (command "TEXT" "MR" (list twax y) th "0.0" (rtos gtwa 2 3))  ;천체중량(가산)
  (command "TEXT" "M" (list (- twx (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")
  (command "TEXT" "M" (list (- twax (* th 3)) (+ y (/ th 2.0))) th "0.0" "T")

  (setq ly (- y (/ lh 2.0)))                                    ;line y좌표

  (setq vy1 (+ iy (/ lh 2.0))
        vy2 (- iy (* (1+ cl) lh) (/ lh -2.0)))

  (setq oldc (getvar "CECOLOR")) (setvar "CECOLOR" "WHITE")
  (command "LINE" (list ix ly) (list (+ ix (* 200 s)) ly) "")      ;line그리기
  (command "LINE" (list ix vy1) (list ix vy2) "")
  (setvar "CECOLOR" "RED")
  (command "LINE" (list (+ ix (* 90 s)) vy1) (list (+ ix (* 90 s)) vy2) "")
  (command "LINE" (list (+ ix (* 120 s)) vy1) (list (+ ix (* 120 s)) vy2) "")
  (command "LINE" (list (+ ix (* 140 s)) vy1) (list (+ ix (* 140 s)) vy2) "")
  (command "LINE" (list (+ ix (* 170 s)) vy1) (list (+ ix (* 170 s)) vy2) "")
  (setvar "CECOLOR" "WHITE")
  (command "LINE" (list (+ ix (* 200 s)) vy1) (list (+ ix (* 200 s)) vy2) "")
  (setvar "CECOLOR" oldc)

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

(defun c:testfindvar()

  (princ (findvar (list (list"P1" "1.0" "4.0" "6.0")
                    (list "P2" "5"   "6"   "7")
                    (list "P3" "4.5" "6.5" "8.4")) '("var" "$A1L" "$A1N" "$A2L")  "$A1L" 0))

);defun
