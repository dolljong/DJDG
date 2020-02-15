;----------------
; command : m3dy 
; Multi 3d CYlinder 
;----------------
(defun c:m3cy()
 (setq d (getreal "Diameter: "))

 (setq ssc (ssget '((0 . "LINE"))))           ;line들과 block이름이 djdg_ratt인 insert 선택
 (setq nc (sslength ssc))                  ;line 및 att의 갯수

  (setq count 0)
  (repeat nc                                            ;line갯수만큼 반복
    (setq lent (entget (ssname ssc count)))             ;line정보
    (3cy lent d)                     ;center 좌표
    (setq count (1+ count))                             ;next line
  ) ;of repeat
                         
                       

);defun



(defun 3cy(e d / )
  ;(setq en (car (entsel "Select Line: ")))
;  (setq e (entget en))
  (setq p1 (cdr (assoc 10 e)  ))
  (setq p2 (cdr (assoc 11 e)))
  (command "cylinder" p1 "d" d "a" p2 )
  
);


