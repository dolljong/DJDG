;******************************************************************
; Program : BCROSS
;           BREAK CROSS
;           Yi Suk-Jong
;           1996/3/22
;*****************************************************************
; 선택된 엔티티들의 접선으로 이루어진 격자를 구성해준다.
; 제한사항 : - 직선과 직선 또는 직선과 호로 이루어진 교점만 인식
;            - poly line인식불가
;*****************************************************************

(defun C:BCROSS(/
cen     count   count1  crs_dst crs_num crs_pnt crs_pnt2    dx      dy
ent     ent1    entype  entype1 ep      ep1     ep2         ipnt1   ipnt2
pnt1    pnt_lst r       snum    snum    sp      sp1         sp2     ssent
ssnum                                                   ;지역변수 정의
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  
;  (setq oer *error* *error* seterr)

  (push-env)                                            ;환경변수 대피
  (setq ssent (ssget))                                  ;entity선택
  (setq snum (sslength ssent))                          ;entity의 갯수

  (setq ipnt1 (getpoint "\nPick base point: "))         ;기준점 입력
  (setq ipnt2 (getpoint ipnt1 "\nPick destination point: "))    ;목표점 입력
  (setq dx (- (car ipnt2) (car ipnt1)))                         ;x편차
  (setq dy (- (cadr ipnt2) (cadr ipnt1)))                       ;y편차

  (setq count 0)                                        ;첫번째 entity부터
  (repeat snum                                          ;entity갯수만큼 반복
    (setq ent (entget (ssname ssent count)))            ;기준선정보
    (setq entype (cdr (assoc 0 ent)))                   ;entity type
    (cond
      ((= entype "LINE")                                ;기준선이 line인 경우
        (setq sp (cdr (assoc 10 ent))                   ;기준선의 시작점
              ep (cdr (assoc 11 ent)))                  ;기준선의 끝점
      )
      ((= entype "ARC")                                 ;호인 경우
        (progn
          (setq cen (cdr (assoc 10 ent)))               ;호의 중심
          (setq r   (cdr (assoc 40 ent)))               ;호의 반경
          (setq sp (polar cen (cdr (assoc 50 ent)) r))  ;호의 시작점
          (setq ep (polar cen (cdr (assoc 51 ent)) r))  ;호의 끝점
        ) ;of progn
      ) ;of entype=arc
    ) ;of cond

    (setq sp2 (list (+ (car sp) dx) (+ (cadr sp) dy) 0.0))  ;좌표변환

    (setq pnt_lst (list (append sp2 (list 0.0))))           ;교점 list

    (setq count1 0)                                         ;나머지 검색
    (repeat snum                                            ;entity갯수만큼 반복
      (if (/= count count1)                     ;기준선과 같은 entity일 땐 skip
        (progn
          (setq ent1 (entget (ssname ssent count1)))    ;entity정보
          (setq entype1 (cdr (assoc 0 ent1)))           ;entity type

          (cond
            ((and (= entype "LINE") (= entype1 "LINE")) ;직선과 직선
              (setq sp1 (cdr (assoc 10 ent1))           ;시작점
                    ep1 (cdr (assoc 11 ent1)))          ;끝점
              (setq crs_pnt (inters sp ep sp1 ep1))     ;두선의 교점
            ) ;of entype=LINE entype1=LINE

            ((and (= entype "ARC") (= entype1 "LINE"))  ;호와 직선
              (setq sp1 (cdr (assoc 10 ent1))           ;시작점
                    ep1 (cdr (assoc 11 ent1)))          ;끝점
              (setq crs_pnt (cross ent sp1 ep1))        ;entity가 ARC일때
            ) ;of entype=ARC entype1=LINE

            ((and (= entype "LINE") (= entype1 "ARC"))
              (setq crs_pnt (cross ent1 sp ep))
            ) ;of entype=LINE entype1=ARC

          ) ;of cond

          (if (/= crs_pnt nil)                          ;교점이 존재할때만
            (progn
              (setq crs_dst (distance sp crs_pnt))      ;시작점에서 교점까지 거리
              (setq crs_pnt2 (list (+ (car crs_pnt) dx) ;좌표변환
                                   (+ (cadr crs_pnt) dy) 0.0))
              (setq pnt_lst (append pnt_lst             ;교점list,거리 list 만들기
                                    (list (append crs_pnt2 (list crs_dst)))))
            ) ;of PROGN
          ) ;of IF
        ) ;of PROGN
      ) ;of IF
      (setq count1 (1+ count1))                             ;다음 entity로
    ) ;of REPEAT
    (setq ep2 (list (+ (car ep) dx) (+ (cadr ep) dy) 0.0))  ;좌표변환
    (setq pnt_lst (append pnt_lst                           ;마지막점 추가
                          (list (append ep2 (list (distance sp ep))))))
    (setq pnt_lst (sort_list pnt_lst 3))                ;순서대로 sort
    (setq crs_num (length pnt_lst))                     ;교점의 갯수
    (setq pnt1 (del_atom (nth 0 pnt_lst) 3))            ;거리 atom 지우고
    (command "LINE" pnt1)                               ;line그리기 시작(첫점)
    (setq count1 1)
    (repeat (1- crs_num)                                ;두번째부터 마지막까지
      (setq pnt1 (del_atom (nth count1 pnt_lst) 3))     ;point구하기
      (command pnt1)                                    ;line그리기
      (setq count1 (1+ count1))                         ;다음 점으로
    ) ;of repeat
    (command "")
    (setq count (1+ count))                             ;다음 entity로
  ) ;of REPEAT                                          ;마지막 entity까지

  (pop-env)                                             ;환경변수 복귀
  (setq *error* oer seterr nil)
  (princ)

) ;of defun CRSL


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
/       alist       nl       rlist       slist        count      mini
        min_th      count1   c_list      c_val        aidx
)

  (setq nl (length alist))                  ;list의 갯수

  (setq slist nil)                          ;빈 sort된 list만듬
  (setq rlist alist)                        ;최대값을 축출한 나머지 list

  (setq count nl)                           ;list 갯수부터 한개씩 빼면서 반복

  (repeat nl                                        ;list갯수만큼
    (setq mini (nth aidx (nth 0 rlist)))             ;첫번째 list를 작은 값으로
    (setq min_th 0)                                 ;최소값의 위치를 처음으로
    (setq count1 1)                                 ;두번째 list부터
    (repeat (1- count)
      (setq c_list (nth count1 rlist))              ;현재 list
      (setq c_val (nth aidx (nth count1 rlist)))    ;현재 값
      (if (< c_val mini)                             ;현재 값이 min보다 작을때
        (progn
          (setq min_th count1)                      ;최소값위치를 현재 위치로
          (setq mini c_val)                          ;최소값을 현재 값으로
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


