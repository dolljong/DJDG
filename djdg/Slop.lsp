;*******************************
; Program : SLOP
;           SLOP mark
;           Suk-Jong Yi
;           1995. 3. 20
;*******************************
;����� slop�� ǥ�����ش�. (%)
;*******************************

(defun C:SLOP(/
        ds      mp      sp      sgnx    sl      ent     spent
        epent   dx      sgn     tmp     dy      ang     angl
        sgnt    sltxt   getxt   lsttxt  txtlen  txtl    blsp
        blep    ap1ang  ap1     ap2ang  ap2     txtp    txtang  clr
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)                                        ;ȯ�溯�� ����

  (setq ds (getvar "DIMSCALE"))
  (setq th (getvar "DIMTXT")) 

  (setq mp (getpoint "\nPick mid-point: "))
  (setq sp (getpoint mp "\nPick side: "))
  (setq sgnx (/ (abs (- (car sp) (car mp))) (- (car sp) (car mp))))
                                                  ;�߰����� ���̵� ����Ʈ�� x��ȣ
  (initget "Referance 2Points")
  (setq sl (getreal "\nReferance/2Points <Slope>: "))     ;���� ���⸦ �Է��� ���ΰ�?
  (cond
    ((= sl "Referance")                          ;�ƴϸ� ��ƼƼ�� ������ ���ΰ�?
      (setq ent (entget (car (entsel))))
      (setq spent (cdr (assoc 10 ent)))
      (setq epent (cdr (assoc 11 ent)))
      (setq dx (- (car epent) (car spent)))
      (setq sgn (/ (abs dx) dx))
      (if (/= sgn sgnx)                           ;������ ������ ����ڰ� ������
        (progn                                    ;����� �ݴ� ������ ��� ��������
          (setq tmp spent)                        ;������ ������ �ٲ۴�
          (setq spent epent)
          (setq epent tmp)
        ) ;of progn
      ) ;of if
      (setq dx (- (car epent) (car spent)))      ;Delta X�� ����
      (setq dy (- (cadr epent) (cadr spent)))    ;Delta Y�� ����
      (setq sl (* (/ dy dx 0.01) sgnx))
    );subcond
    ((= sl "2Points")
     (setq pnt1 (getpoint "\nPick first point: ")
           pnt2 (getpoint pnt1 "\nPick second  point: "))
     (setq dx (- (car pnt2) (car pnt1))
	   dy (- (cadr pnt2) (cadr pnt1)))
     (setq sl (* (/ dy dx 0.01) sgnx))
    );subcond
    
  ) ;of if

  (setq ang (atan (/ sl 100.0)))
  (cond                                    ;ȭ��ǥ�� �������� ���°� ����������
    ((= sgnx -1) (setq angl (- pi ang)))   ;���°��� ���� ������ ������ ���
    ((= sgnx 1) (setq angl (+ 0.0 ang)))   ;�������� �� ��� 180+ang
  ) ;of cond                               ;���������� �� ��� 0+ang


  ;������ ��ȣ�� ��Ʈ������ �����
  (if (> sl 0.0)
    (setq sgnt "+")
    (setq sgnt "")
  )               ;���� slop�� ��� +��ȣ �߰�

  (setq sltxt (strcat "S=" sgnt (rtos (float sl) 2 3) "%"))   ;��ȣ + slop + "%"
  (princ "\nEnter text:<")             ;���α׷��� ���� Text�� �׷��� �����
  (princ sltxt)                        ;������ �ƴϸ� ����ڰ� ���Ƿ� �ٰ��� ����
  (princ ">: ")                        ;default�� ���Ͽ��� slop
  (setq getxt (getstring))             ;����� text�Է�
  (if (= getxt "")                     ;����� text�� nil�� ��� default��
    (setq lsttxt sltxt)
    (setq lsttxt getxt)
  ) ;of if
  (setq txtlen (* (strlen lsttxt) th ds))                    ;��ü text�� ����
  (setq txtl (+ txtlen (* ds 2 th)))  ;text���̿��� 2���� ���ڸ� �߰��� ����

  ;ȭ��ǥ ���̽������� �������� ������ ���Ѵ�
  (setq blsp (polar mp (- angl pi) (/ txtl 2.0)))
  (setq blep (polar mp    angl     (+ (/ txtl 2.0) (* ds th)))) ;ȭ��ǥ���� ���
  (setq ap1ang (+ angl pi (* (dtor 15.0) sgnx -1)))
  (setq ap1 (polar blep ap1ang (* ds 2.0)))     ;ȭ��ǥ ����̴� 2.0mm
  (setq ap2ang (+ ap1ang (* (dtor 105.0) sgnx)))
  (setq ap2 (polar ap1 ap2ang (* (sin (dtor 15.0)) (* ds 2.0))))


  (setq txtp (polar mp (* (+ angl (/ pi 2.0)) sgnx) (* ds th)))  ;�ؽ�Ʈ�� insert point
  (if (< sgnx 0)
    (setq txtang (- angl pi))          ;���ʹ����� ��� text�� ����
    (setq txtang angl)                 ;������ ������ ��� ���̽����� ���� ����
  )
  (setq clr (getvar "CECOLOR"))
  (setvar "CECOLOR" "RED")
  (command "PLINE" blsp blep ap1 ap2 "")                      ;ȭ��ǥ �׸���
  (setvar "CECOLOR" clr)
  (command "TEXT" "J" "M" txtp (* ds th) (rtod txtang) lsttxt) ;�ؽ�Ʈ ����

  (pop-env)                           ;�����ص� ȯ�溯�� ����

  (setq *error* oer seterr nil)       ;���忡����ƾ ����

  (princ)

) ;of defun
