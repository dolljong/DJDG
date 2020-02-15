;****************************************
;*    trsch
;*              Translation character
;*              By Suk-Jong Yi
;*              99/2/9
;****************************************

(defun C:trsch(
              / llist txtlst txtlst1 umlst chlst)     ;������������

  (setq umlst (getum))

;  (setq umlst (getum1))
  
  (setq sstxt (ssget "X" '((0 . "TEXT"))))   ;��� text ��
  
  (setq nss (sslength sstxt))			;ss����
  (setq index 0)

  (repeat nss					;text������ŭ �ݺ�
    (setq ent (entget (ssname sstxt index)))	;entity���� ����
    (setq oldass1 (assoc 1 ent))   		;text��������
    (setq oldtxt (cdr oldass1)
	  newtxt oldtxt)
    (setq i 0)
    (setq num (length umlst))                   ;���� ����
    (repeat num					;text list��ŭ �ݺ�
      (setq ums (nth i umlst)
	    nhanja (1- (length ums)))           ;���� ����
      (setq um (nth 0 ums))                     ;�ѱ�
      (setq j 1)
      (repeat nhanja                             ;���ڰ�����ŭ �ݺ�
        (setq hanja (nth j ums))                 ;get one ���� 
        (setq newtxt (str-rep newtxt hanja um)) 
        (setq j (1+ j))
      );repeat
      (setq i (1+ i))
    ); repeat
    (setq newass1 (cons 1 newtxt))	;
    (setq entl1 (subst newass1 oldass1 ent))         ;���ο� entity�����ۼ�
    (entmod entl1)                            ;���ο� text�� ������Ʈ
    (setq index (1+ index))			;����text��ƼƼ��
  );repeat
);defun  


;****************************************
;*    function : DELSPC
;*              DELete SPaCe
;*              By Suk-Jong Yi
;*              03/10/30
;****************************************

(defun delspc( txt / txt1 result lstr i ch)
  (setq txt1 txt)
  (setq result "")
  (setq lstr (strlen txt1))
  (setq i 1)
  (repeat lstr
    (setq ch (substr txt1 i 1))
    (if (/= ch " ")
     (setq result (strcat result ch))
    );if		  
    (setq i (1+ i))
  );repeat
  result
);defun


(defun readhan( )
;      (setq fn (getfiled "INPUT DATA" "" "DAT" 0))      ;file name�Է�
      (setq fn (getstring "\nEnter file name: "))      ;file name�Է�
      (setq opf (open fn "r"))                          ;file open
      (if opf                                           ;file�� ���� ���
        (progn
           (setq count 1)
           (while (setq ch (read-line opf))             ;������ �д´�
              (princ (chr 13))                          ;�Է��� �޼��� ���
              (princ count)
              (princ " Line Processing...")
              (setq llist (append llist (list ch)))                   ;llist�� �߰�
              (setq count (1+ count))                                  ;line��ȣ ����
           ) ;of while
        ) ;of progn
        (princ "\nFile not found")                          ;file�� ���� ���
      ) ;of if
      (close opf)                                           ;file close
    
  (setq nline (length llist))   ;�Էµ����� ���� ��

  (setq i 0)
  (repeat nline
    
    (setq line (nth i llist))
    (setq lenstr (strlen line))  ;string��
    (setq j 0
	  chlst nil)
    (repeat (1- (fix (/ lenstr 2)))
      (setq chlst (append chlst (list (substr line (1+ (* j 2)) 2))))
      (setq j (1+ j))
    );repeat
    (setq umlst (append umlst  (list chlst)))          			;0, 2, 4
    (setq i (1+ i))
  );repeat  
);defun

(defun str-rep (str pat rep)
  (while (vl-string-search pat str)
    (setq str (vl-string-subst rep pat str))
  )
  str
)



(defun getum( )
  '(
( "��" "��" "ʡ" "ʢ" "ʣ" "ʤ" "ʥ" "ʦ" "ʧ" "ʨ" "ʩ" "ʪ" "ʫ" "ʬ" "ʮ" "ʭ" "ʯ" "ʰ" "ʱ" "ʴ" "ʵ" "ʼ" "ʲ" "ʳ" "ʶ" "ʷ" "ʸ" "ʹ" "ʺ" "ʻ" "ʽ" )
( "��" "ʾ" "л" "ʿ" "��" "��" "��" "��" "̫" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "ˡ" "ˢ" "ˤ" "ˣ" "˥" "˦" )
( "��" "˧" "˨" "˩" "˪" "˫" "ˬ" "˭" "ˮ" "˯" "��" "˰" "˽" "˷" "˹" "˱" "˲" "˺" "˳" "˴" "˵" "˶" "˸" "˻" "˼" "˾" )
( "��" "˿" "��" "��" "��" "��" "��" "��" "��" "��" "ο" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "̡" "̢" "̣" )
( "��" "̤" "̥" "̦" )
( "��" "̧" "̩" "̨" )
( "��" "ʣ" "̭" "̰" "̪" "̮" "̫" "̬" "̯" )
( "��" "̱" "̷" "̲" "̳" "̺" "̴" "̵" "̶" "̸" "̹" "��" "̻" )
( "��" "��" "̼" "̽" "̾" "̿" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "͢" "ͭ" "��" "��" "��" "͡" "ͣ" "ͤ" "ͥ" "ͦ" "ͧ" "̿" "ͨ" "ͩ" "ͪ" "ͫ" "ͬ" "ͮ" )
( "��" "ͯ" "Ͱ" "ͱ" "Ͳ" "ͳ" "ʹ" "͵" "ʫ" "Ͷ" "ͷ" "͸" "Ϳ" "͹" "ͺ" "ͻ" "ͼ" "��" "��" "ͽ" ";" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ʹ" "��" "��" "��" "��" "��" "��" "��" )
( "��" "ͱ" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "΢" "Φ" "��" "Ρ" "��" "Σ" "Τ" "Υ" "Χ" "Ψ" )
( "��" "Ω" "ά" "Ϊ" "Ϋ" )
( "��" "��" "ή" "ί" "ΰ" "��" "α" "δ" "β" "��" "γ" "ε" "ζ" "ι" "��" "η" "θ" "κ" "λ" "μ" "ν" )
( "��" "ξ" "��" "ο" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "Ь" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "ϡ" "Ϣ" "ϣ" "Ϥ" "ϥ" "Ϧ" "ϧ" "Ϩ" "ϩ" "Ϫ" "ϫ" "Ϭ" "ϵ" "Ϸ" "��" "ϭ" "Ϯ" "ϯ" "ϰ" "ϱ" "ϲ" "ϳ" "ϴ" "϶" "ϸ" "Ϲ" "Ͼ" "��" "��" "Ϻ" "ϻ" "ϼ" "Ͻ" "Ͽ" "��" "��" "��" "˻" "��" "��" "��" "��" "��" "��" "��" "��" "��" "Т" )
( "��" "ͱ" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "ϣ" "��" "��" "��" "С" "Т" )
( "��" "У" "Ф" "Х" "Ц" "Ч" "Ш" "Э" "а" "Щ" "Ъ" "Ы" "Ь" "��" "Ю" "Я" "б" "��" )
( "��" "в" "г" "ж" "д" "е" "з" "Т" )
( "��" "й" )
( "��" "к" "л" "м" "ʦ" "��" "н" "о" "п" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "̢" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "̣" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "Ѫ" "ѩ" "ѫ" "��" "��" "��" "��" "��" "Ѣ" "ѡ" "ѿ" "ѣ" "�" "ѥ" "Ѥ" "Ѧ" "ѧ" "Ѩ" "Ѭ" "ѭ" "Ѯ" "ѯ" "Ѱ" "��" "ѱ" "Ѳ" "ѳ" "Ѵ" "��" "ѵ" "Ѷ" "ѷ" "Ѹ" "Ѽ" "ѹ" "Ѻ" "ѻ" "ѽ" "Ѿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "ү" "��" "��" "��" "��" "��" "��" "Ұ" "ա" "դ" "��" "��" "գ" "բ" )
( "��" "զ" "��" "է" "ը" "ժ" "��" "լ" "խ" )
( "��" "կ" "հ" "��" "ձ" "��" "մ" "��" "��" "ն" )
( "��" "��" "��" )
( "��" "��" "չ" "��" "ս" "��" "��" "��" "��" "��" )
( "��" "Ү" "��" "��" "Ҥ" "ҡ" "��" )
( "��" "ҥ" "Ҧ" "��" "��" "��" "ҫ" "��" )
( "��" "Ҭ" "��" "Ү" "ү" "��" "��" "Ұ" "ұ" )
( "��" "��" )
( "��" "ҳ" )
( "��" "Ҵ" "ҵ" "Ҷ" "�" )
( "��" "Ҹ" "ҹ" "Һ" "ҷ" )
( "��" "��" )
( "��" "Ҽ" "һ" )
( "��" "ҽ" "��" "ҿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "פ" "צ" "ר" "ש" "װ" "ק" "׫" "׬" )
( "��" "��" )
( "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" )
( "��" "ӡ" "Ӣ" "ӣ" "Ӥ" "ӥ" "Ӧ" "ӧ" "Ӭ" "ӳ" "��" "Ө" "ө" "Ӫ" "ӫ" "ӭ" "Ӯ" "Ӳ" "ӯ" "Ӱ" "ӱ" "Ӵ" )
( "��" "��" "ӵ" "Ӷ" "ӷ" "ӹ" "Ӹ" )
( "��" "Ӻ" "ӻ" "Ӽ" "��" "ӽ" "ӿ" "��" "��" "��" "Ӿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ԣ" "Ԥ" "ԥ" "Ԧ" "Դ" "Զ" "��" "��" "ԡ" "Ԣ" "ԧ" "Ԭ" "Ա" "Բ" "Գ" "Ԩ" "ԩ" "Ԫ" "ԫ" "��" "ԭ" "Ԯ" "ԯ" "԰" "Ե" "Է" )
( "��" "Թ" "Լ" "Ը" "Ժ" "Ի" "Խ" "Ծ" "��" "Կ" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "Թ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "ա" "դ" "��" "��" "գ" "׫" "բ" )
( "��" "զ" "ե" "է" "ը" "ժ" "թ" "��" "��" "լ" "խ" )
( "��" "ӡ" "կ" "հ" "ճ" "ձ" "ղ" "մ" "յ" "ն" )
( "��" "շ" "ո" )
( "��" "չ" "ջ" "ս" "պ" "ռ" "��" "��" "վ" "տ" "��" )
( "��" "��" "��" "��" )
( "��" "Ҧ" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "֡" "֢" "֥" "֧" "֣" "֤" "֦" "֨" )
( "��" "֩" "֪" "֫" "֬" "֭" "֮" "��" "֯" "ֲ" "ְ" "ֱ" "ֳ" )
( "��" "ִ" )
( "��" "ֵ" "ֶ" "ַ" "һ" "ֹ" "ֺ" "ֻ" "ּ" "��" "ֽ" "־" "ֿ" "��" "��" "��" "��" "��" "��" )
( "��" "��" "֩" "��" "��" "��" "��" )
( "��" "ҽ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "ף" "��" )
( "��" "��" "פ" "��" "��" "��" "��" "��" "��" "��" "׾" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "ס" "��" "��" "��" "��" "ע" )
( "��" "ף" )
( "��" "פ" "ץ" "צ" "ר" "ש" "װ" "ק" "׭" "ת" "׮" "׫" "׬" "ׯ" )
( "��" "ױ" "׵" "��" "׶" "׷" "ײ" "׳" "״" "��" "׸" "׹" "׺" "׻" "׼" "׽" "׾" )
( "��" "׿" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "פ" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "ء" "آ" "أ" )
( "��" "ؤ" "إ" "ئ" "ا" "ب" "ڼ" "ة" "ت" "ث" )
( "��" "ج" "ح" "خ" "د" "ذ" "ر" "��" )
( "��" "ز" "س" "ش" "ص" "ض" "ط" "ظ" "ؼ" "ػ" "ؽ" "ع" "غ" "ؿ" "��" "ؾ" "��" "λ" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ף" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "٪" "��" "��" "��" "��" "��" )
( "��" "��" "١" )
( "��" "٢" "٣" "٤" "٨" "٥" "٦" "٧" "٫" "٬" "٩" "٪" "٭" "ٮ" "ٯ" "ٰ" )
( "��" "ٱ" )
( "��" "ٲ" "ٳ" "��" "ٴ" "ٵ" "ٶ" "ٸ" "ٷ" "ٹ" "ٺ" "د" "ٻ" "ټ" "��" "ٽ" "پ" "ٿ" "��" "��" "��" "��" "ذ" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "ڨ" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "ٵ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "ٳ" "��" "��" )
( "��" "��" "��" "��" "��" "��" "ش" "ڡ" "��" "��" "ؾ" "ڣ" "ڢ" "ڤ" "ڥ" "ڦ" "ڧ" )
( "��" "ڨ" "ک" "ڪ" )
( "��" "ګ" "ڬ" "ڭ" "ڮ" "گ" "ڰ" "ڴ" "ڵ" "ڱ" "ڲ" "ڳ" "ڹ" "ڻ" "ڶ" "ڷ" "ڸ" "ں" "ڼ" "ڽ" )
( "��" "ھ" "��" "��" "ڿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ݭ" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "ܧ" "ޯ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "ۡ" "ۢ" "ۣ" "ۤ" "ۥ" "ۦ" )
( "��" "ۧ" "ۨ" "۩" "۪" "۫" "۬" "��" "ۭ" "۵" "��" "��" "ۮ" "ۯ" "۰" "۱" "۲" "۸" "۹" "۳" "۴" "ۻ" "ۼ" "۶" "۷" "ۺ" "۽" "۾" "ۿ" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "ܢ" "��" "��" "ܡ" "ܣ" "ܤ" )
( "��" "��" "ܦ" "ܧ" "ܫ" "��" "ܨ" "ܩ" "ܪ" )
( "��" "ܬ" "��" "ܭ" "ܮ" "ܯ" )
( "��" "ܰ" "ܱ" "ܲ" "ܳ" "ܴ" "޳" "ܵ" "ܶ" "۳" "ܷ" "ܸ" "ܹ" "ܺ" "ܻ" "ܼ" "ܽ" "ܾ" "ܿ" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ݫ" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ݡ" "ݢ" "ݣ" "ݤ" "��" "ݦ" "ݩ" "ݪ" "ݻ" "ݾ" "ݧ" "ݨ" "ݰ" "ݱ" "ݫ" "ݳ" "ݴ" "ݬ" "ݭ" "ݮ" "ݯ" "ݲ" "��" "��" "ݵ" "ݶ" "ݷ" "ݸ" "ݹ" "ݺ" "��" "ݼ" "ݽ" "ݿ" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ީ" "��" "��" "��" "��" "��" "ޡ" "ޢ" "��" "��" "��" "��" "��" "��" "��" "ޣ" "��" "��" "��" "��" "��" "ޥ" "��" "��" "��" "ޤ" "ަ" "ާ" "ި" "��" "ު" "ޫ" "ެ" )
( "��" "ޭ" "ޮ" "ޯ" "޳" "޴" "޵" "ް" "ޱ" "޲" "޶" "޷" "޸" "޹" "��" "޺" )
( "��" "޻" "޼" "޽" "��" "޾" )
( "��" "޿" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "�" "��" "��" "��" )
( "��" "ߡ" "ߢ" "ߣ" "ߥ" "ߤ" "ߦ" "ߪ" "ߧ" "ߨ" "ߩ" "߫" "߬" )
( "��" "߭" "߮" "߯" "߰" "��" "߱" )
( "��" "߲" "��" "߶" "ߴ" "ߵ" "߷" "߸" "߹" )
( "��" "ߺ" "߻" "߼" "߽" )
( "��" "߾" "߿" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "�" "�" "��" "��" "��" "��" "�" "�" "�" "�" "�" "�" )
( "��" "�" "�" "��" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" )
( "��" "��" "�" "�" "�" "Ӥ" "�" "�" "�" "��" "�" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ߪ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "�" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" )
( "��" "��" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "��" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "�" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "�" "�" "�" "��" "��" "��" "��" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "��" "��" "�" "�" "��" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "�" )
( "��" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "�" "�" "��" "�" "��" "�" "�" )
( "��" "�" "�" "�" )
( "��" "�" )
( "��" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" )
( "��" "�" "�" "��" "��" "��" "��" "��" "��" "��" "ե" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "�" "��" "��" "��" "ˣ" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "�" "��" "��" "��" "��" "��" "��" )
( "��" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "��" "�" "��" "�" "��" "�" "�" "�" "�" "�" )
( "��" "�" "��" "�" "�" "�" "�" "��" "�" "�" )
( "��" "��" "�" "��" "��" "�" "�" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "ܢ" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "ҳ" "��" "��" "�" "��" "��" "�" "��" "�" "�" "�" "�" "��" "�" "�" "�" "�" "��" "�" "��" "��" "��" )
( "��" "�" "��" "�" "��" "�" "�" "��" "��" "�" "�" "�" "�" "��" "�" )
( "��" "��" "�" "��" "��" "��" "��" "Ҵ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "֡" "��" "��" "��" "��" "֢" "֥" "֧" "��" "��" "��" "��" "֣" "��" "��" "��" "֤" "��" "��" "֦" "��" "֨" "��" "��" )
( "��" "֪" "֫" "��" "��" "��" "֭" "��" "�" "֮" "��" "��" )
( "��" "��" "��" "֯" "Һ" "ҷ" "��" "ֱ" "��" "��" "��" "��" "ֳ" "��" "��" "��" "�" "�" "�" )
( "��" "ִ" "�" "�" "�" )
( "��" "ֵ" "ַ" "�" "һ" "�" "ֺ" "�" "ֻ" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "ּ" "��" "��" "��" "��" "��" "��" "��" "��" "־" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "�" "��" "�" "�" "�" "��" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" )
( "��" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" )
( "��" "��" "��" "�" "�" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "Զ" "��" "��" "�" "ե" "��" "��" "��" "��" "��" "�" "�" "�" "��" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "��" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "��" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ף" )
( "��" "��" "��" "��" "��" "ϡ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "�" "��" "�" "��" "��" "��" "��" "��" "��" "��" "�" "��" "�" "�" )
( "��" "�" "��" "�" "�" "�" )
( "��" "�" "�" )
( "��" "��" "�" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "��" "�" "�" "�" "�" "�" "�" "��" "�" "�" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "ױ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "׵" "��" "׶" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "׸" "�" "�" "�" "�" "�" "�" "׺" "�" "׼" "�" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "׾" )
( "��" "׿" "�" "��" "��" "��" "�" "��" "�" )
( "��" "��" "��" "в" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "Ѫ" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "�" "�" "��" )
( "��" "�" "�" "�" "��" "��" "��" "�" "��" "�" "��" "�" "�" "�" "�" "��" "�" "�" "��" "��" "�" "�" "��" "�" "�" "��" "�" "�" "��" "�" "�" "��" "�" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "ء" "آ" "أ" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" )
( "��" "�" "��" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "��" "��" "��" "��" "��" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "��" "�" "�" "�" "�" )
( "��" "�" "�" "�" "��" "�" "��" "�" "�" "�" "�" )
( "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "�" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" )
( "��" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "��" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "Ѷ" "�" "�" "ѽ" "�" "�" "�" "�" "��" "�" "�" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "̴" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "�" "��" "��" "��" "��" "��" "��" "�" "�" )
( "¡" "�" "�" "�" )
( "��" "�" "�" "�" "�" "�" "�" "�" "�" "�" "��" "�" "�" "�" "�" "��" "��" )
( "��" "��" "�" "��" "�" "��" "�" "��" "�" "�" "�" "�" )
( "��" "��" "�" "�" "�" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "â" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ä" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "å" "��" "��" "��" "��" "��" )
( "ó" "��" "��" "��" "��" "��" )
( "ô" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "õ" "��" "Ӣ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ö" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "÷" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ø" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" )
( "û" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ü" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ף" )
( "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "ġ" "��" "��" "��" "��" "�" "��" "�" "�" "��" "��" "�" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ģ" "��" "��" "��" )
( "ģ" "��" )
( "ĥ" "��" "��" "��" )
( "ħ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ĩ" "��" )
( "Ī" "��" "��" )
( "��" "��" )
( "Ÿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ź" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ź" "ӣ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ż" "��" "��" "�" "��" )
( "Ž" "��" "��" "��" "��" )
( "ž" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "�" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ư" "��" "��" "��" )
( "ƴ" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "ܩ" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "۱" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "ܩ" "ܪ" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "ܪ" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ܢ" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ݦ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "ǥ" "��" "��" "��" "��" "��" "��" "��" "ݴ" "��" "��" "��" "��" "��" "��" "��" )
( "Ǭ" "��" )
( "ǰ" "��" "��" )
( "ǳ" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "ʣ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "ս" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "˽" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" )
( "��" "�" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "̸" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ȣ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ͼ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ȥ" "��" "��" "��" )
( "ȥ" "��" "��" "��" "��" "��" "��" "��" )
( "Ȧ" "��" "��" "��" )
( "ȫ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ȭ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ȯ" "��" "��" "��" "��" "��" "��" "��" )
( "ȯ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "Ȱ" "��" "��" "��" "��" "��" "��" "��" )
( "Ȳ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "ȸ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "ȹ" "��" "��" "��" )
( "Ⱦ" "��" "��" "��" "��" )
( "ȿ" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" "��" "ѩ" "��" "��" )
( "��" "��" "��" "��" )
( "��" "��" "��" "��" "��" )
( "��" "��" )
( "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" "��" )
( "��" "��" )
    ); 
);defun

(defun getum1()
  '(
("��" "��" "ʡ" "ʢ" "ʣ" "ʤ" "ʥ" "ʦ" "ʧ" "ʨ" "ʩ" "ʪ" "ʫ" "ʬ" "ʮ" "ʭ" "ʯ" "ʰ" "ʱ" "ʴ" "ʵ" "ʼ" "ʲ" "ʳ" "ʶ" "ʷ" "ʸ" "ʹ" "ʺ" "ʻ" "ʽ" )
    );
);defun  