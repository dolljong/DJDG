;**********************************
; Program : APLOT
;           Auto PLOT
;           By Suk-Jong Yi
;           1995/7/21, 7/24
;***********************************
; 선택된 border 를 출력
;***********************************

(defun C:APLOT( /
                name_lst
)

  (setq bdr_B 815                                   ;border의 폭
        bdr_H 570                                   ;border의 높이
        bn    "BORDER"                             ;블럭 이름
        xg    -15                                    ;x gap
        yg    -5)                                   ;y gap

  (setq ds (getvar "DIMSCALE"))                     ;scale 값구함

  (setq ss_lst (ssget))                             ;entity 선택
  (setq ss_num (sslength ss_lst))                   ;선택된 entity갯수

  (setq index 0)
  (setq bdr_num 0)

  (repeat ss_num                                    ;entity갯수만큼 반복
    (setq ent (entget (ssname ss_lst index)))         ;entity 정보
    (if (and (= (cdr (assoc 0 ent)) "INSERT")         ;border인 경우
             (= (substr (cdr (assoc 2 ent)) 1 (strlen bn)) bn))
      (progn
        (redraw (ssname ss_lst index) 3)              ;border반전
        (setq plt_name (getstring "\nPLT filename(No extension): ")) ;plt name 입력(확장자 없음)
        (setq plt_name (strcat plt_name ".PLT"))                     ;입력 name에다 확장자 추가
        (while (or (/= (findfile (strcat (getvar "DWGPREFIX") plt_name)) nil)
                   (is_member plt_name name_lst))     ;디렉토리와 plt name list에 동일 이름 존재 확인
          (princ "File ") (princ plt_name) (princ " is already exist ")
          (setq plt_name (getstring "PLT filename(No extension): "))
          (setq plt_name (strcat plt_name ".PLT"))
        ) ;of WHILE                                   ;동일이름 존재시 계속 반복
        (if (= bdr_num 0)
          (progn                                      ;첫 border인 경우
            (setq index_lst (list index))               ;몇번째 entity인가를 기억
            (setq name_lst (list plt_name))             ;plt 이름은 뭔가를 기억
          ) ;of THEN
          (progn                                      ;첫 border가 아닌경우
            (setq index_lst (append index_lst (list index)))
            (setq name_lst (append name_lst (list plt_name)))
          ) ;of THEN
        ) ;of IF
        (setq bdr_num (1+ bdr_num))
        (redraw (ssname ss_lst index) 4)              ;반전된 border를 정상으로
      ) ;of progn
    ) ;of IF
    (setq index (1+ index))                           ;다음 entity로
  ) ;of repeat
  (princ bdr_num) (princ " Border(s) found")        ;몇개의 border가 선택됬는지 표시

  (setq index 0)
  (repeat (length index_lst)                        ;선택된 border 갯수만큼 반복
    (setq bdr_ent (entget (ssname ss_lst (nth index index_lst))))  ;border entity정보
    (setq ipnt (cdr (assoc 10 bdr_ent)))            ;border의 insert point
    (setq i_scale (cdr (assoc 41 bdr_ent)))         ;border의 scale factor
    (setq low_left (list (+ (car ipnt) (* xg i_scale))        ;border의 좌측 아래
                         (+ (cadr ipnt) (* yg i_scale))))
    (setq up_right (list (+ (car ipnt) (* bdr_B i_scale))    ;border의 우측 위
                         (+ (cadr ipnt) (* bdr_H i_scale))))
    (setvar "CMDDIA" 0)                                     ;command echo OFF
    (command "PLOT" "W" low_left up_right "N" "N" (nth index name_lst))  ;plot명령
    (setvar "CMDDIA" 1)                                     ;command echo ON
    (princ (nth index name_lst)) (princ " is Plotted") (terpri)
    (setq index (1+ index))                                 ;다음 border로
  ) ;of repeat

  (princ)
) ;of defun



;;
;; atom이 list의 원소인가?
;;
(defun is_member(a lst / count)
  (setq count 0)
  (if (/= (length lst) 0)
    (progn
      (while (and (/= a (nth count lst)) (< count (length lst)))
        (setq count (1+ count))
      ) ;of WHILE
      (if (= count (length lst))
       nil
       count
      ) ;of IF
    ) ;of THEN
    nil
  ) ;of IF
) ;of defun
