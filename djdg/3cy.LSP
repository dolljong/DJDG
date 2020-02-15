;----------------
; command : m3dy 
; Multi 3d CYlinder 
;----------------
(defun c:m3cy()
 (setq d (getreal "Diameter: "))

 (setq ssc (ssget '((0 . "LINE"))))           ;line��� block�̸��� djdg_ratt�� insert ����
 (setq nc (sslength ssc))                  ;line �� att�� ����

  (setq count 0)
  (repeat nc                                            ;line������ŭ �ݺ�
    (setq lent (entget (ssname ssc count)))             ;line����
    (3cy lent d)                     ;center ��ǥ
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

