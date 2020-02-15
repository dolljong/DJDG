;**************************************************
; Program : ALIGNBAR
;           Align BAR detail
;           Yi Suk-Jong(dolljong@dreamwiz.com)
;           01/01/26
;**************************************************
; bar detail 정렬
; 
;**************************************************

(defun C:alignBAR( 
  / ssc ipnt ygap ix iy nc clist cent cc cx cy rc p1 p2 ss  llist lent lpnt1 lpnt2 ssbox ipnt2 )

  (defun SETERR(s)					;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)			;내장에러루틴 기동

  (push-env)                                ;환경변수 대피


  (setq ssc (ssget '((0 . "CIRCLE"))))      ;circle들을 입력
  (setq ipnt (getpoint  "\nSpecify insert point: "))  ;pick insert point
  (setq ygap (getdist ipnt "\nEnter Y-gap: "))
  (setq ix (car ipnt)       ;x
	iy (cadr ipnt))     ;y
	
  (setq nc (sslength ssc))                  ;circle의 갯수
  (setq clist (reverse (sort_byassoc ssc 10 2)))      ;make circle list that is sorted by y-coord
;  (setq llist nil)                          ;빈 line-list 만듬

  (setq count 0)
  (repeat nc                                            ;circle갯수만큼 반복
    (setq cent (entget (nth count clist)))             ;circle정보
    (setq cc (cdr (assoc 10 cent))
	  cx (car cc)
	  cy (cadr cc))                     ;center 좌표
    (setq rc (cdr (assoc 40 cent)))                     ;radius (름)
    (setq p1 (list (- cx rc) (+ cy rc) 0.0)
	  p2 (list (+ cx rc) (- cy (* 2 rc)) 0.0)
          ss (ssget "C" p1 p2 '((0 . "LINE"))))                ; MARKING/LINE 잡아내기

    (setq lent (entget (ssname ss 0))
          lpnt1 (cdr (assoc 10 lent))
	  lpnt2 (cdr (assoc 11 lent)))
    (if (< (car lpnt1) (car lpnt2))
      (setq p2 (list (car lpnt2) (- cy (* 2 rc)) 0.0))
      (setq p2 (list (car lpnt1) (- cy (* 2 rc)) 0.0))
    );if  
    (setq ssbox (ssget "c" p1 p2))
    (setq ipnt2 (list ix (- iy (* count ygap)) 0.0))
    (command "copy" ssbox "" cc ipnt2)
    (setq count (1+ count))                             ;다음 circle로
  ) ;of repeat


  (pop-env)                                             ;환경변수 복귀

  (princ "\nNormal terminated")                         ;정상적으로 끝남 표시
  (setq *error* oer seterr nil)
  (princ)

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


;******************************************************
; Function : SORT_byassoc
;           SORT by assoc
;           Yi Suk-Jong(dolljong@dreamwiz.com
;           2001/01/26
;******************************************************
; list를 sort해준다.
; 넘어오는 값
;     ALIST : SORT되어야할 LIST, (ssget)상태로 넘어옴
;      ass  : 기준이 될 assoc index
;      AIDX : 기준이 되는 sub list (ex: (10 1.0 2.0 3.0) 2.0-->2 
; 넘겨지는 값
;             SORT된 entity LIST
;******************************************************
; ex: (sort_byassoc (ssget) 10 2) sort by y-coord
(defun SORT_BYASSOC(ss ass aidx
  /       ssn  count alist nl slist rlist minv min_th c_list c_val count1 )
  
  (setq ssn (sslength ss))
  (setq count 0
	alist nil)
  (repeat ssn
    (setq alist (append alist (list (ssname ss count)))) 
    (setq count (1+ count))
  );repeat

  
  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq minv (nth aidx (assoc ass (entget (nth 0 rlist)))))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (assoc ass (entget c_list))))    ;현재 값
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