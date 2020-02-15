;*******************************
; Program : MTXT
;           Modify TeXT
;           Yi Suk-Jong
;           01/12/7
;*******************************
; 순차적으로 text를 수정해준다.(수평,수직방향 선택가능)
;*******************************

(defun C:MTXT(/
                txtformat          startnum       stepnum      ans       num         
                numtxt        msg        esel       irepeat    txtent
                startpoint           
                count      ent       index       ipnt      ntxt  
                numss   sslst      tlst             txt       txtim
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수저장
 
  (setq txtformat (getstringold oldtxtformat "\nEnter text format"))  ;text format입력
  (setq oldtxtformat txtformat)
 
  (setq startnum (getintold oldstartnum "\nEnter Start Number")) ;시작번호 입력
  (setq oldstartnum startnum)

  (setq stepnum (getintold oldstepnum "\nEnter Step Number")) ;증분입력
  (setq oldstepnum stepnum)
  
  (initget "Individual Selected")                           ;한개씩 혹은 한꺼번에 선택
  (setq ans (getkword "\nIndividual/Selected <I>: "))
  (if (= ans nil) (setq ans "Individual"))
  (cond
    ((= ans "Individual")				;한개씩 수정할 때
      (setq num startnum)
      (setq numtxt (str_subst (itoa num) "#" txtformat))
      (setq msg (strcat "\n[" numtxt "]Pick Text or [Format(" txtformat
			")/Start(" (itoa startnum)
			")/sTep(" (itoa stepnum)
			")]: "))
      (initget "Format Start sTep")
      (setq esel (entsel msg))
      (setq irepeat 1)
      (while (/= esel nil)
        (cond
	  ((= esel "Format")
            (setq txtformat (getstringold oldtxtformat "\nEnter text format"))
            (setq oldtxtformat txtformat)
	  );subcond
	  ((= esel "Start")
            (setq startnum (getintold oldstartnum "\nEnter Start Number"))
            (setq oldstartnum startnum)
	    (setq irepeat 0)
	  );subcond
	  ((= esel "sTep")
            (setq stepnum (getintold oldstepnum "\nEnter Step Number"))
            (setq oldstepnum stepnum)
	    (setq startnum num
	          irepeat 0)
  	  );subcond
	  (T
	    (setq txtent (entget (car esel)))
            (entmod (subst (cons 1 numtxt) (assoc 1 txtent) txtent))
	  );subcond
        );cond
        (setq num (+ startnum (* irepeat stepnum)))
	(setq irepeat (1+ irepeat))
        (setq numtxt (str_subst (itoa num) "#" txtformat))
        (setq msg (strcat "\n[" numtxt "]Pick Text or [Format(" txtformat
  			  ")/Start(" (itoa startnum)
			  ")/sTep(" (itoa stepnum)
			  ")]: "))
        (initget "Format Start sTep")     
        (setq esel (entsel msg))
      );endwhile
    );subcond
    ((= ans "Selected")						;한꺼번에서택하는경우
      (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity만 선택
      (setq startpoint (getpoint "\nPick Start point: "))
      (setq numss (sslength sslst))                         ;ss-list의 갯수
      (princ numss)
      (princ " text found\n")                               ;선택된 text갯수

      (setq count 0                                         ;첫 text부터
            tlst nil)

      (repeat numss                                             ;text갯수만큼
        (setq ipnt (assoc 10 (entget (ssname sslst count))))    ;삽입점
        (setq txtim (cons (ssname sslst count) (list (distance startpoint (cdr ipnt)))))           ;엔티티이름과 기준점에서의 거리로 list만듬
        (setq tlst (append tlst (list txtim)))                  ;text list에 추가
        (setq count (1+ count))                                 ;다음 text로
      ) ;of repeat

      (setq tlst (sort_list tlst 1)) ;가까운 순으로 정렬
     
      (setq irepeat 0)
      (repeat numss						;Text수정
        (setq num (+ startnum (* irepeat stepnum)))
;	(setq irepeat (1+ irepeat))
        (setq numtxt (str_subst (itoa num) "#" txtformat))
	
        (setq txtent (entget (car (nth irepeat  tlst))))
	
        (entmod (subst (cons 1 numtxt) (assoc 1 txtent) txtent))
	(setq irepeat (1+ irepeat))
      );repeat
    );subcond 
  );cond
  (pop-env)                                             ;환경변수 복귀

  (setq *error* oer seterr nil)
  (princ)  
);defun



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
/       alist       nl       rlist       slist        count      minv
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq minv (nth aidx (nth 0 rlist)))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (nth count1 rlist)))    ;현재 값
      (if (< c_val minv)                             ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq minv c_val)                          ;최소값을 현재 값으로
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


  