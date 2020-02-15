;-------------------
; Program : WBl (WBlock using File NAME list)
;           YI Suk Jong
;           04/11/10
;-------------------
(defun c:wbl()
  (setq ss (ssget '((0 . "TEXT"))))                 ;text entity���b
  (setq ns (sslength ss))                           ;selection set�� entity����
;  (setq fname (strcat (getvar "dwgprefix") "/"))    ;filename�i �w���q�a��
  (setq fname (getvar "dwgprefix"))    ;filename�i �w���q�a��
  (setq index 0)
  (repeat ns
    (setq txt (cdr (assoc 1 (entget (ssname ss index))))) ;text�t
    (if (> index 0)
      (setq fname (strcat fname "-" txt))   ;���弁 text����e text�|�A "-"�a
      (setq fname (strcat fname txt))       ;���弁 text�� �w���E �w���q�A ����
    );if
    (setq index (1+ index))
  );repeat

  (setq ipnt (getpoint "\nInsertion base point:")) ;�s���� ��Ȃ
  (setq sse (ssget))                        ;wblock�i entity��Ȃ

  (setvar "FILEDIA" 0)                      ;�w�w��З�� dialog box�a�� �g����

  (command "WBLOCK"          ;wblock�w�w ��З
           fname             ;filename
           ""                ;block name
           ipnt              ;insert point
           sse               ;��Ȃ�E entity
           "")               ;end selection

  (setvar "FILEDIA" 1)                      ;

);defun


(defun get_fnlist()
  (setq fn (strcat (prefix) "djdg/wbl.dat"))      ;file name�Է�
  (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file�� ���� ���
        (progn
           (setq count 1)
	   (setq llist nil)
           (while (setq ch (read-line opf))             ;������ �д´�
              (princ (chr 13))                          ;�Է��� �޼��� ���
              (princ count)
              (princ " Line Processing...")
              (setq inline (data-in ch))
              (setq lst (cons                           ;���� data�� ���� data��
			  (delsp (strcase (nth 1 inline)))   ;subject
			  (delsp (strcase (nth 0 inline))))) ;filename (number)
              (setq llist (append llist (list lst)))                   ;llist�� �߰�
              (setq count (1+ count))                   ;line��ȣ ����
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
      (close opf)                                           ;file close
);defun


(defun delsp(str / )
  (setq return str)
  (while (vl-string-position (ascii " ") return)
    (setq return (vl-string-subst "" " " return))
  );while
  return
);defun  
  