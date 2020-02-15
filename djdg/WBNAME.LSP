;-------------------
; Program : WBNAME (WBlock File NAME)
;           YI Suk Jong
;           99/9/9
;-------------------
(defun c:wbname()
  (setq ss (ssget '((0 . "TEXT"))))                 ;text entity입력
  (setq ns (sslength ss))                           ;selection set의 entity갯수
;  (setq fname (strcat (getvar "dwgprefix") "/"))    ;filename을 방이름으로
  (setq fname (getvar "dwgprefix"))    ;filename을 방이름으로
  (setq index 0)
  (repeat ns
    (setq txt (cdr (assoc 1 (entget (ssname ss index))))) ;text값
    (if (> index 0)
      (setq fname (strcat fname "-" txt))   ;두번째 text부터는 text앞에 "-"추가
      (setq fname (strcat fname txt))       ;첫번째 text인 경우엔 방이름에 붙임
    );if
    (setq index (1+ index))
  );repeat

  (setq ipnt (getpoint "\nInsertion base point:")) ;삽입점 선택
  (setq sse (ssget))                        ;wblock할 entity선택

  (setvar "FILEDIA" 0)                      ;명령진행시 dialog box뜨지 않도록

  (command "WBLOCK"          ;wblock명령 실행
           fname             ;filename
           ""                ;block name
           ipnt              ;insert point
           sse               ;선택된 entity
           "")               ;end selection
           
  (command "oops")
  
  (setvar "FILEDIA" 1)                      ;

);defun


