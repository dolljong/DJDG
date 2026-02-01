;************************************************
; Program : DDMCHT
;           Dialog Multi CHange Text
;           Yi Suk-Jong
;           96/5/8
;************************************************
; 순차적으로 text를 수정해준다. (Dialog Box이용)
;************************************************

(defun C:DDMCHT(/                                  ;지역변수 정의
                ans      count     index      ipnt       numss
                sslst    tlst      txt        txtim
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                                ;환경변수 대피

  (initget "Horizontal Vertical")                           ;수정방향설정
  (setq ans (getkword "\nHorizontal/Vertical <V>: "))
  (if (= ans nil) (setq ans "Vertical"))                    ;default=수직방향
  (if (= ans "Vertical")
    (setq index 3)
    (setq index 2)
  ) ;of IF

  (setq sslst (ssget '((0 . "TEXT"))))                  ;text entity만 선택
  (setq numss (sslength sslst))                         ;ss-list의 갯수
  (princ numss)
  (princ " text found\n")                               ;선택된 text갯수

  (setq count 0                                         ;첫 text부터
        tlst nil)

  (repeat numss                                             ;text갯수만큼
    (setq ipnt (assoc 10 (entget (ssname sslst count))))    ;삽입점
    (setq txtim (cons (ssname sslst count) ipnt))           ;엔티티이름과 삽입점으로 list만듬
    (setq tlst (append tlst (list txtim)))                  ;text list에 추가
    (setq count (1+ count))                                 ;다음 text로
  ) ;of repeat

  (if (= ans "Vertical")                                    ;수직방향일 경우 y값을 기준으로
    (setq tlst (reverse (sort_list tlst index)))            ;수평방향일 경우 x값을 기준으로
    (setq tlst (sort_list tlst index))
  ) ;of IF

  (setq count 0)
  (repeat numss
    (setq ent (entget (nth 0 (nth count tlst))))        ;text entity정보
    (setq txt (cdr (assoc 1 ent)))                      ;text내용
    (redraw (nth 0 (nth count tlst)) 3)                 ;text강조
    (NT_DIALOG txt)                                     ; Dialog Box통하여 text입력
    (if (/= new_txt "")                                    ;새text 내용이 있을때만
      (entmod (subst (cons 1 new_txt) (assoc 1 ent) ent))) ;text갱신
    (redraw (nth 0 (nth count tlst)) 4)                 ;강조된 entity복구
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

;;;
;;; new text입력 루틴
;;;
(defun NT_DIALOG ( old_txt / dcl_id old_txt)

  (setq dcl_id (load_dialog "DJDG"))
  (if (not (new_dialog "txtedit" dcl_id)) (exit))

  (set_tile "text_edit" old_txt)

  (action_tile "text_edit" "(set_txt)")
  (action_tile "accept" "(done_dialog)")
  (action_tile "cancel" "(set_txt)")
  (mode_tile "text_edit" 2)
  (start_dialog)
  (unload_dialog dcl_id)

) ;of defun NT_DIALOG

(defun SET_TXT( / in )
  (setq in (get_tile "text_edit"))
  (setq new_txt in)
) ; of defun SET_TXT

