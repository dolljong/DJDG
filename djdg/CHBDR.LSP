;*******************************************
; Program : CHBDR
;           CHange BorDeR
;           By Suk-Jong Yi
;           1995/7/21
;*******************************************
; 도각을 바꿔준다 ( Border0 <--> Border )
;*******************************************

(defun C:CHBDR( /
                ent_lst     ssnum       index       ent         blk_name
                new_name    ipnt        x_scl       insert_type
)

  (setq ent_lst (ssget))                        ;entity선택
  (setq ssnum (sslength ent_lst))               ;선택된 entity 갯수

  (setq index 0)
  (repeat ssnum                                 ;선택된 entity갯수만큼 반복
    (setq ent (entget (ssname ent_lst index)))  ;entity 정보
    (if (= (cdr (assoc 0 ent)) "INSERT")        ;entity가 블럭인경우
      (progn
        (setq blk_name (cdr (assoc 2 ent)))     ;블럭이름
        (setq insert_type (cdr (assoc 70 (tblsearch "BLOCK" blk_name)))) ;insert type(insert/xref)
        (cond
          ((= blk_name "BORDER")                                ;border인경우
            (setq new_name (strcat (prefix) "blocks/border0"))  ;새로운 이름을 border0로
            (setq ipnt (cdr (assoc 10 ent)))                    ;insert point
            (setq x_scl (cdr (assoc 41 ent)))                   ;scale factor
            (command "ERASE" (ssname ent_lst index) "")         ;원래 border지우기
            (if (or (= insert_type 4) (= insert_type 100))      ;insert/xref형식에 따라
              (command "XREF" "" new_name ipnt x_scl "" "")       ;xref 명령수행
              (command "INSERT" new_name ipnt x_scl "" "")        ;insert명령수행
            ) ;of IF
          )
          ((= blk_name "BORDER0")                               ;border0인 경우
            (setq new_name (strcat (prefix) "blocks/border"))   ;새로운 이름을 border로
            (setq ipnt (cdr (assoc 10 ent)))                    ;insert point
            (setq x_scl (cdr (assoc 41 ent)))                   ;scale factor
            (command "ERASE" (ssname ent_lst index) "")         ;원래 border지우기
            (if (or (= insert_type 4) (= insert_type 100))      ;insert/xref형식에 따라
              (command "XREF" "" new_name ipnt x_scl "" "")       ;xref 명령 수행
              (command "INSERT" new_name ipnt x_scl "" "")        ;insert 명령 수행
            ) ;of IF
          )
        ) ;of COND
      ) ;of THEN
    ) ;of IF
    (setq index (1+ index))                                     ;다음 엔티티로
  ) ;of REPEAT

  (princ)
) ;of defun CHBDR

