;*******************************
; Program : TXTGAP
;           change TeXT GAP
;           Yi Suk-Jong
;           98/10/1
;*******************************
; text간격을 수정해준다.
;*******************************

(defun C:TXTGAP(/
                count ent index sslst numss ip tgap
;                tlst ipnt txtim oldxy newxy newent
)

  (defun SETERR(s)                                          ;내장에러루틴 정의
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

  (setq oer *error* *error* seterr)                         ;내장에러루틴 가동

  (push-env)                                                ;환경변수 대피

  (setq index 3)                                        ;수직방향으로 정렬

  (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity만 선택
  (setq numss (sslength sslst))                         ;ss-list의 갯수
  (princ numss)
  (princ " text found\n")                               ;선택된 text갯수

  (setq ip (getpoint "\nPick insert point: "))           ;text삽입점
  (setq tgap (getdist "\nText Gap: "))                   ;text간격 입력

  (setq count 0                                         ;첫 text부터
        tlst nil)

  (repeat numss                                             ;text갯수만큼
    (setq ipnt (assoc 10 (entget (ssname sslst count))))    ;삽입점
;    (setq ipnt (assoc 11 (entget (ssname sslst count))))    ;삽입점
    (setq txtim (cons (ssname sslst count) ipnt))           ;엔티티이름과 삽입점으로 list만듬
    (setq tlst (append tlst (list txtim)))                  ;text list에 추가
    (setq count (1+ count))                                 ;다음 text로
  ) ;of repeat

  (setq tlst (reverse (sort_list tlst index)))            ;수평방향일 경우 x값을 기준으로

  (setq count 0)
  (repeat numss
    (setq ent (entget (nth 0 (nth count tlst))))        ;text entity정보
    (setq old10 (assoc 10 ent))                         ;구좌표
    (setq old11 (assoc 11 ent))                         ;구좌표
    (setq new10 (cons 10                                ;새좌표
                      (list (cadr old10)                    ; x
                            (- (cadr ip) (* count tgap)))))     ; y
    (setq new11 (cons 11                                ;새좌표
                      (list (cadr old11)                    ; x
                            (- (cadr ip) (* count tgap)))))     ; y
    (setq ent (entmod (subst new10 old10 ent)))         ;삽입점(10) 갱신
    (setq ent (entmod (subst new11 old11 ent)))         ;삽입점(11) 갱신
    (setq count (1+ count))                             ;다음 text로
  ) ;of repeat

  (pop-env)                                             ;환경변수 복귀

  (setq *error* oer seterr nil)

  (princ)
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
      (if (< c_val min)                             ;현재 값이 min보다 작을때
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

