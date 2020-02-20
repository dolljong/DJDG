;********************************************
; Program : PSCBEAM
;           PSC BEAM
;           Suk-Jong Yi
;           96/5/10
;********************************************
; PSC BEAM �ܸ� �Ϲݵ��� �׷��ش�.
;********************************************

(defun C:PSCBEAM()

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

  (push-env)
  (setq ds (getvar "DIMSCALE"))                                 ;�����ϰ�
  (setq b (* 1000.0 (getreal "\nB(m): ")))                      ;������
  (setq bn (getint "\nNumber of beam: "))                       ;beam����
  (setq pb (* 1000.0 (getreal "\nPich of beam(m): ")))          ;beam����
  (setq ls (* 0.01 (getreal "\nLeft slop(%): ")))               ;���� slop
  (setq rs (* 0.01 (getreal "\nRight slop(%): ")))              ;���� slop
  (setq ta (getreal "\nThickness of asphalt(mm): "))            ;����β�
  (setq ts (getreal "\nThickness of slab(mm): "))               ;slab�β�
  (setq p (getpoint "\nPick Insert point: "))                   ;������ (�߾ӻ��)

  (setq b2 (/ b 2.0))                                           ;�������� ��
  (setq bb (* pb (- bn 1)))                                     ;beam���� ����

  (setq pl (list (- (car p) (- b2 456))
                 (+ (cadr p) (* (- b2 456) ls)) 0.0))           ;�������
  (setq pr (list (+ (car p) (- b2 456))
                 (+ (cadr p) (* (- b2 456) rs)) 0.0))           ;���������

  (setq sct (list (car p) (- (cadr p) ta))                      ;������߾ӻ��
        scb (list (car p) (- (cadr sct) ts)))                   ;������߾��ϴ�

  (setq slte (list (- (car p) b2) (+ (cadr sct) (* ls b2)) 0.0))    ;�������ʻ�ܳ�
  (setq srte (list (+ (car p) b2) (+ (cadr sct) (* rs b2)) 0.0))    ;���������ʻ�ܳ�

  (setq slbe (list (car slte) (- (cadr slte) ts) 0.0))              ;���������ϴܳ�
  (setq srbe (list (car srte) (- (cadr srte) ts) 0.0))              ;�����������ϴܳ�

  (command "LINE" sct slte slbe scb srbe srte sct "")               ;�����׸���

  (barrier 0 slte (* ls 100.0))                                               ;���ʹ�ȣ��
  (barrier 1 srte (* rs 100.0))                                               ;�����ʹ�ȣ��

  (command "LINE" pl p pr "")                                       ;���弱 �׸���

  (if (= (rem bn 2) 1)
    (progn                                                          ;�� ������ Ȧ�� �϶�
      (setq ly (+ (cadr scb) (* 350 ls))                            ;�߾Ӻ� ���� y
            ry (+ (cadr scb) (* 350 rs)))                           ;�߾Ӻ� ���� y
      (if (<= ly ry)
        (setq iy ly)
        (setq iy ry))                                           ;���� ���� insert y��
      (draw_beam (list (car p) iy))                             ;�߾Ӻ� �׸���
      (setq bn2 (/ (- bn 1) 2))                                 ;���� �� ����
      (setq cnt 1)                                              ;����ù��° ��
      (repeat bn2
        (setq btp (list (- (car p) (* cnt pb))                  ;�� �����
                        (- (+ (cadr scb) (* cnt pb ls)) (* (abs ls) 350.0))
                        0.0))
        (draw_beam btp)                                         ;���ʺ� �׸���
        (setq cnt (1+ cnt))                                     ;���� ���� ��
      ) ;of repeat
      (setq cnt 1)                                              ;������ù��° ��
      (repeat bn2
        (setq btp (list (+ (car p) (* cnt pb))                  ;�� �����
                        (- (+ (cadr scb) (* cnt pb rs)) (* (abs rs) 350.0))
                        0.0))
        (draw_beam btp)                                         ;���ʺ� �׸���
        (setq cnt (1+ cnt))                                     ;���� ���� ��
      ) ;of repeat
    ) ;of progn
    (progn                                                      ;�� ������ ¦�� �϶�
      (setq bn2 (/ bn 2))                                       ;���� ���� ����
      (setq cnt 1)                                              ;ù ���ʺ�����
      (repeat bn2
        (if (= cnt 1)
          (setq dx (* pb 0.5))                                  ;�߾ӿ��� ���߾ӱ��� �Ÿ�
          (setq dx (+ (* pb 0.5) (* (1- cnt) pb)))
        ) ;of IF
        (setq btp (list (- (car p) dx)                          ;�� �����
                        (- (+ (cadr scb) (* dx ls)) (* (abs ls) 350.0))
                        0.0))
        (draw_beam btp)                                         ;�� �׸���
        (setq cnt (1+ cnt))                                     ;���� ���� ��
      ) ;of repeat
      (setq cnt 1)                                              ;ù �����ʺ�����
      (repeat bn2
        (if (= cnt 1)
          (setq dx (* pb 0.5))                                  ;�߾ӿ��� ���߾ӱ��� �Ÿ�
          (setq dx (+ (* pb 0.5) (* (1- cnt) pb)))
        ) ;of IF
        (setq btp (list (+ (car p) dx)                          ;�� �����
                        (- (+ (cadr scb) (* dx rs)) (* (abs rs) 350.0))
                        0.0))
        (draw_beam btp)                                         ;�� �׸���
        (setq cnt (1+ cnt))                                     ;���� ���� ��
      ) ;of repeat
    ) ;of progn
  ) ;of IF

;******  ġ���� ���� (�Ʒ�)
  (setq canl (/ (- b (* (1- bn) pb)) 2.0))                 ;ĵƿ���� ����
  (setq dpy1 (- (cadr slbe) 2000.0))                       ;ġ����y��
  (setq dp1 (list (car slbe) dpy1 0.0))                    ;ġ���� ������
  (setq dpy2 (- dpy1 (* ds 20)))                           ;ġ����y��
  (setq dp11 (list (car slbe) dpy2 0.0))                   ;ġ������ġ
  (setq pp (f_dh dp1 canl 1 -1 nil))                       ;���� ĵƿ���� ġ��
  (setq pp (f_dh pp pb (1- bn) -1 nil))                    ;beam����ġ��
  (setq pp (f_dh pp canl 1 -1 nil))                        ;������ ĵƿ���� ġ��
  (setq pp (f_dh dp1 b 1 -2 nil))                          ;���� ġ��

;****** ġ���� ���� (��)
  (setq pp1 (list (- (car p) (/ b 2)) (+ (cadr p) (* ls b 0.5) 1000)))   ;���ʹ�ȣ�� �����
  (setq pp (f_dh pp1 450 1 1 nil))                          ;���� ���� ġ��
  (setq pp (f_dh pp (- b 900) 1 1 nil))                     ;����� ġ��
  (setq pp (f_dh pp 450 1 1 nil))                           ;������ ���� ġ��
  (setq pp (f_dh pp1 b 1 2 nil))                            ;��ü �� ġ��

;****** slop ǥ��
  (setq pnt (list (- (car p) (/ b 4)) (+ (cadr p) (* (/ b 4) ls) ds)))
  (f_slop pnt -1 (* ls 100))
  (setq pnt (list (+ (car p) (/ b 4)) (+ (cadr p) (* (/ b 4) rs) ds)))
  (f_slop pnt 1 (* rs 100))


;****** ������

  (pop-env)
  (setq *error* oer seterr nil)
  (princ)

) ;of DEFUN


;*******************************************
; Function : DRAW_BEAM
;            DRAW psc BEAM
;            Suk-Jong Yi
;            96/5/11
;*******************************************
; PSC BEAM(�ܸ�)�� �׷��ش�
;*******************************************

(defun DRAW_BEAM( ipnt / ipnt fp)
  (setq fp (list (- (car ipnt) 350) (cadr ipnt) 0.0))           ;������
  (command "LINE" fp "@700,0" "@0,-180" "@-250,-100" "@0,-1270"
                     "@230,-220" "@0,-230" "@-660,0" "@0,230"
                     "@230,220" "@0,1270" "@-250,100" "C")
) ;of defun

;********************************************
; Function : BARRIER
;            DRAW BARRIER
;            Suk-Jong Yi
;            96/7/3
;********************************************
;��ȣå�� �׷��ش�.
; �Ѿ���� ��
;     LR : Left / Right
;     ip : Insert Point
;     SL : SLOP (%)
;********************************************
(defun BARRIER( LR ip SL / LR ip SL)
  (if (= LR 0)                                      ;���ʿ���
    (progn
      (setq inpo (list (+ (car ip) 30)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (+ (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" inpo "@0,1080" "@230,0" "@70,-700"
                           "@120,-175" inpi "")                             ;�����׸���
    ) ;of PROGN
    (progn
      (setq inpo (list (- (car ip) 30.0)
                       (+ (cadr ip) (* sl -30.0 0.01)) 0.0))     ;�ٱ��ʾƷ�
      (setq inpi (list (- (car ip) 450.0)
                       (+ (cadr ip) (* sl -450.0 0.01)) 0.0))   ;���ʾƷ�
      (command "PLINE" inpo "@0,1080" "@-230,0" "@-70,-700"
                           "@-120,-175" inpi "")                            ;�����׸���
    ) ;of progn
  ) ;of IF
) ;of defun

;***********************************
; Program : F_SLOP
;           Function SLOP mark
;           Suk-Jong Yi
;           96/7/3
;***********************************
;����� slop�� ǥ�����ش�. (%)
; �Ѿ���� ��
;     MP : Mid Point ȭ��ǥ�� �߰���
;   SGNX : (ȭ��ǥ�� �������)
;     SL : SLop�� (%)
;***********************************

(defun F_SLOP(MP SGNX SL
/
        ds      mp      sp      sgnx    sl      ent     spent
        epent   dx      sgn     tmp     dy      ang     angl
        sgnt    sltxt   getxt   lsttxt  txtlen  txtl    blsp
        blep    ap1ang  ap1     ap2ang  ap2     txtp    txtang  clr
)

  (setq ds (getvar "DIMSCALE")
        th (getvar "DIMTXT"))

  (setq ang (atan (/ sl 100.0)))
  (cond                                    ;ȭ��ǥ�� �������� ���°� ����������
    ((= sgnx -1) (setq angl (- pi ang)))   ;���°��� ���� ������ ������ ���
    ((= sgnx 1) (setq angl (+ 0.0 ang)))   ;�������� �� ��� 180+ang
  ) ;of cond                               ;���������� �� ��� 0+ang


;������ ��ȣ�� ��Ʈ������ �����
  (if (> sl 0.0)
    (setq sgnt "S=+")
    (setq sgnt "S=")
  )               ;���� slop�� ��� +��ȣ �߰�

  (if (> (abs (rem sl 1)) 0)
    (setq slt (rtos sl 2 3))
    (setq slt (rtos (float sl) 2 1))
  ) ;of if
  (setq sltxt (strcat sgnt slt "%"))   ;��ȣ + slop + "%"
  (setq lsttxt sltxt)
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
  (setvar "CECOLOR" "1")
  (command "PLINE" blsp blep ap1 ap2 "")                      ;ȭ��ǥ �׸���
  (setvar "CECOLOR" clr)
  (command "TEXT" "J" "M" txtp (* ds th) (rtod txtang) lsttxt) ;�ؽ�Ʈ ����

) ;of defun

;*******************************************
; Program : F_DO
;           Function Dimension Oblique
;           Jong-Suk Yi
;           96/7/3
;*******************************************
; Vertical DIM�� OBLIQUE�����ش�.
; �������� - ���� DIM���� �ش�ȴ�.
;          - OBLIQUE���� 30���� ������ �ִ�.
;*******************************************

(defun F_DO(SP DST N LR TXT1
/
divl    divn    dp      ds      dsel    dtx     dtxt1   dtxt1p  dtxt2
dtxt2p  dx      dxy     dy      ep      fst     lstdim  next    pnt1
pnt2    ppnt    sent    sgn     sp      th      txt     txt1    txtlen
)

  (setq th (getvar "DIMTXT")                          ;textũ�� =dimtxt
        dim_gap 10.0)                                 ;ġ���� ����

  (setq ds (getvar "DIMSCALE"))                       ;scale factor

  (if (> lr 0)                                              ;���� ������ ����
    (setq sgn 1)
    (setq sgn -1)
  ) ;of if

  (setq dx (* ds (+ 15 (* dim_gap (- (abs lr) 1)))))        ;�������� ġ�����ǰŸ�

  (setq next (* dst n))

  (setq ep (list (car sp) (+ (cadr sp) next)))        ;ep ��ġ���

  (setq dxy (list (+ (car ep) (* dx sgn)) (car ep) 0.0))  ;ġ������ ���� ��ġ

  (setq dy (distance sp ep))                          ;�� ���� �Ÿ�

  (if (< dy 1000.0)
    (setq txt (rtos dy 2 0))                          ;1000�̸��� ��
    (setq txt (rtos (* dy 0.001) 2 3))                ;1000�̻��� ��
  ) ;of if(dy < 1000)

  (if (> n 1)
    (progn
      (setq divl dst)                                 ;������ ���� �Է�
      (setq divn (rtos n 2 0))              ;���� �������
      (if (< divl 1000.)
        (setq divl (rtos divl 2 0))                   ;������ ���̰� 1000�̸���
        (setq divl (rtos (* divl 0.001) 2 3))) ;of if  ������ ���̰� 1000�̻��
        (setq txtlen (* (+ (strlen txt) (strlen divn) (strlen divl) 2) th ds
                     (cdr (assoc 41 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))))
        (if (>= txtlen dy)
          (progn                                  ;text�� ������ ���� �ȵ���
            (setq dtxt1 (strcat divn "@" divl))   ;���ٷ� ����
            (setq dtxt2 (strcat "=" txt))
            (setq dtxt1p (mapcar '+ (mid-point sp ep)
                                    (list (- (* dx sgn) (* ds th))  ;x��ġ
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))
                                          0.0)))                     ;z��ġ
            (setq dtxt2p (mapcar '+ (mid-point sp ep)
                                    (list (+ (* dx sgn) (* ds th))  ;x��ġ
                                          (* dx (/ (sin (/ pi 6)) (cos (/ pi 6))))
                                          0.0)))                     ;z��ġ
            (command "TEXT" "M" dtxt1p (* th ds) "90" dtxt1)
            (command "TEXT" "M" dtxt2p (* th ds) "90" dtxt2)
            (command "DIM1" "VER" sp ep dxy " ")              ;DIM��� ����
          ) ;of progn THEN
          (progn                                  ;text�� ������ ���� ����
            (setq dtxt1 (strcat divn "@" divl "=" txt))
            (command "DIM1" "VER" sp ep dxy dtxt1)            ;DIM��� ����
          ) ;of progn ELSE
        ) ;of IF
      ) ;of progn THEN
      (progn
        (if (= txt1 "") (setq txt1 txt))                      ;�����Է½� �� text�� ��
        (command "DIM1" "VER" sp ep dxy txt1)             ;DIM��� ����
      ) ;of progn ELSE
    ) ;of if(txt1=@)

    (setq lstdim (entlast))                               ;��� ������� dim����
    (setq oldexo (getvar "DIMEXO"))
    (setvar "DIMEXO" 3)
    (command "DIM1" "OBL" lstdim "" (* sgn 30))           ;30����ŭ ������
    (command "DIM1" "UPDATE" lstdim "")
    (setvar "DIMEXO" oldexo)

  ep                                                        ;������ ����������.

) ;defun
