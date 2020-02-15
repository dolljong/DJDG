;*****************************************
;       CAMBER
;           draw CAMBER
;           Jong-Suk Yi
;           1996. 2. 12
;*****************************************
; 이 풀그림은 처짐도(CAMBER)를 그려준다.
; DATA 입력사항은 (절점번호, X-좌표, Y-좌표) 이다.

(defun C:CAMBER(
/               gap     bdrx    bdry   yfac    rh     fcw     trg    th
                fn      opf     nlist  ch      nnum   ncamb   xmax   xmin
                xscale  yscale  cw     ccount  camby  firstx  lastx  count
                node    x       y      pnt     lpnt   bpnt    oldc   ap
                b1      b2      cp     txtpnt  ntbl   tblh    maxn   tcount
                node1   node2   tbly   tblpx   tblpy
)

  (defun SETERR(s)
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR
  (setq oer *error* *error* seterr)

;*** 각종 초기값 지정 (Camber도)
  (setq gap (getint "\nLEFT MARGIN (mm)<60>: "))    ;도각과 camber도와의 간격(여백)
  (if (= gap nil) (setq gap 60))                    ;return입력시 gap=60mm
  (setq bdrx 780)                                   ;border의 x크기
  (setq bdry 557)                                   ;border의 y크기
  (setq yfac 100)                                   ;y-factor

;*** 각종 초기값 지정 (Camber table)
    (setq rh 7)                                     ;row height (table 줄높이)
    (setq fcw 20)                                   ;first column width
    (setq trg 2)                                    ;text right gap


    (setvar "CMDECHO" 0)                            ;command echo off
    (setvar "BLIPMODE" 0)                           ;blip mode off
    (setq th (getvar "DIMTXT"))                     ;text size 지정

    (setq fn (getfiled "INPUT DATA" "" "DAT" 0))    ;file name입력
    (setq opf (open fn "r"))                        ;file open

    (setq nlist nil)                                ;빈 node-list 만듬

;*** data file로부터 DATA입력

    (if opf                                         ;file이 없는 경우
      (progn
         (while (setq ch (read-line opf))           ;한줄을 읽는다
            (setq nlist (append nlist (list (strloc ch)))) ;nlist에 추가
         ) ;of while
      ) ;of progn
      (princ "\nFile not found")                    ;file이 없는 경우
    ) ;of if
    (close opf)                                     ;file close


    (setq nnum (length nlist))                         ;입력 line의 갯수
    (princ nnum) (princ "LINE FOUND")
    (setq ncamb (- (length (nth 1 nlist)) 2))          ;camber의 수

    (setq xmax (atof (nth 1 (nth (1- nnum) nlist))))  ;맨 끝점의 x값
    (setq xmin (atof (nth 1 (nth 0 nlist))))           ;첫점 X값

    (setq xscale (/ (- bdrx (* 2 gap)) xmax))          ;도각과의 x scale(계산)
    (setq yscale (getint "\nY-Scale: "))               ;y방향 scale(입력값)
    (setq cw (getint "\nColumn width (mm)<20>: "))     ;camber table 칸 넓이
    (if (< cw 15) (setq cw 15))

    (setq ccount 2)

    (repeat ncamb                                      ;camber 갯수만큼 반복

      (setq camby (- 1 (* (/ 1 (+ ncamb 1.5)) (- ccount 1))))      ;camber도 y위치

      (setq firstx (list (+ (* xscale xmin) gap) (* bdry camby)))  ;첫 x점
      (setq lastx (list (+ (* xscale xmax) gap) (* bdry camby)))   ;마지막 x점
      (command "LINE" firstx lastx "")                 ;base line그리기

      (setq count 0)                                   ;첫점부터..

      (repeat nnum                               ;마지막 점까지..
        (setq node (nth 0 (nth count nlist)))
        (setq X (atof (nth 1 (nth count nlist))))
        (setq Y (atof (nth ccount (nth count nlist))))

        (setq pnt (list (+ (* x xscale) gap)           ;camber 점
                        (+ (* y yscale -1 xscale) (* bdry camby))))

        (if (= count 0)
          (setq lpnt pnt)                       ;첫점이면 선 안그리고 넘어감
          (command "LINE" lpnt pnt "")          ;전점에서 현재점으로 선그림
        )

        (setq bpnt (list (+ (* x xscale) gap) (* bdry camby)))  ;base point
        (setq oldc (getvar "CECOLOR"))                          ;현재색 대피
        (setvar "CECOLOR" "GREEN")                              ;수직선색=초록
        (command "LINE" pnt bpnt "")                            ;수직선그리기
        (setvar "CECOLOR" oldc)                                 ;옛색 돌리기

        (if (= (substr node 1 1) "+")               ;node에 fix단표시 있을때
           (progn
             (command "LINE" bpnt "@3<-60" "@3<180" "C")   ;fix단 표시(삼각형)
             (setq node (substr node 2))                   ;fix단 표시(+)제거
             (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier번호 읽음
           )
           (if (= (substr node 1 1) "-")            ;node에 move단표시 있을때
             (progn
               (command "LINE" bpnt "@3<-60" "@3<180" "C") ;move단 표시(삼각형)
               (setq b1 (list (- (+ (* x xscale) gap) 1.5) ;밑줄 시작점
                              (- (* bdry camby) 3)))
               (setq b2 (list (+ (+ (* x xscale) gap) 1.5) ;밑줄 끝점
                              (- (* bdry camby) 3)))
               (command "LINE" b1 b2 "")                   ;밑줄 그림
               (setq node (substr node 2))                 ;move단 표시(-)제거
               (setq ap (nth 0 (reverse (nth count nlist)))) ;Abut/Pier번호 읽음
             ) ;of progn
           );of if
        ) ;of if

        (if (/= ap nil)                                     ;Abut/Pier일 경우
          (progn
            (setq cp (list (car bpnt) (- (cadr bpnt) 12)))  ;원의 중심
            (command "CIRCLE" cp "4.0")                     ;원그리기
            (command "TEXT" "M" cp 3.0 0.0 ap)              ;text쓰기
            (setq ap nil)
          ) ;of progn
        ) ;of if

        (setq node (atoi node))                     ;node를 정수로
        (setq node (itoa node))                     ;node를 문자열로(공백제거)
        (setq txtpnt (list (+ (* x xscale) gap) (- (* bdry camby) 5))) ;text pnt
        (command "TEXT" "M" txtpnt th "0" node)     ;node 번호적기

        (setq lpnt pnt)                                 ;현재점을 전점으로

        (setq count (1+ count))                         ;다음 data
      ) ;of repeat                                      ;다음 node로

      (setq ccount (1+ ccount))
    ) ;of repeat                                        ;다음 camber로

  (setq ntbl (+ (/ (* (1+ nnum) cw) (- bdrx (* gap 2))) 1))  ;table갯수
  (setq tblh (* rh (1+ ncamb)))                         ;table 높이

  (setq maxn (/ (- bdrx (* gap 2)) cw))         ;한줄 최대 node수

  (setq tcount 0)                               ;table count

  (repeat ntbl
    (setq node1 (1+ (* tcount maxn)))                ;시작 node
    (setq node2 (* (1+ tcount) maxn))           ;끝 node
    (if (> node2 nnum)                    ;끝 node가 마지막 node보다 크면
      (setq node2 nnum)
    )

    (setq tbly (/ camby 2.0))                              ;camber table y위치
    (setq tblpx (- gap fcw)
          tblpy (- (* bdry tbly) (* tcount (+ tblh cw))))   ;table의 삽입위치

    (camb_table tblpx tblpy node1 node2 nlist)

    (setq tcount (1+ tcount))
  ) ;of repeat

  (setq *error* oer seterr nil)
  (princ)
) ;;of defun RDL



;*******************************************************************
;     Function : STRLOC
;                get STRing LOCation
;                Jong-Suk Yi
;                1995. 2. 8
;******************************************************************
; 이 함수는 ,로 불리된 data를 나누어 한개의 list에 묶어준다.
; 이때 형변환 없이 모든 data는 문자열로 return된다.
;******************************************************************
(defun STRLOC(arg1
/             srt    strl    count    num    strt    nchr    subs
              lst    rslt
)
;(setq oer *error* *error* seterr)   ;Store AutoLISP error routine
   (setq str arg1)                              ;넘어온 문자열
   (setq strl (strlen arg1))                    ;넘어온 문자열의 길이
   (setq count 1)
   (setq num 1)
   (setq strt 1)                                ;추출시작 위치
   (setq nchr 1)                                ;추출문자 갯수
   (repeat (+ strl 1)
      (setq subs (substr str count 1))          ;문자 한개
      (if (or (= subs ",") (= subs ""))         ;현재 문자가 ,이거나 끝일때
         (progn
            (setq lst (substr str strt (- nchr 1)))    ;시작위치부터
            (if (= rslt nil)
               (setq rslt (list lst))                  ;돌림값이 비었을때
               (setq rslt (append rslt (list lst)))    ;돌림값에다 추가
            ) ;of if
            (setq nchr 0)                       ;추출갯수 다시 0으로
            (setq strt (1+ count))              ;다음 추출시작을 다음문자로
         ) ;of progn
         nil
      ) ;of if
      (setq count (1+ count))                   ;다음 문자로
      (setq num (1+ num))                       ;
      (setq nchr (1+ nchr))                     ;문자 갯수 한개 증가
   ) ;of repeat
   (setq arg1 rslt)                             ;돌림값 돌림
;(setq *error* oer seterr nil)                  ; Restore previous error handler
) ;of defun STRLOC

;****************************************************
; Function : CAMB_TABLE
;            CAMBer TABLE
;            Yi Suk-Jong
;            1996/2/14
;****************************************************
; 이 함수는 주어진 data로 솟음도 Table만들어준다
;  받는 값은
;      ax : table의 좌측상단의 x위치
;      ay : table의 좌측상단의 y위치
;  anode1 : 시작 node번호
;  anode2 : 끝 node번호
;  anlist : 입력된 data list
;****************************************************

(defun CAMB_TABLE(ax ay anode1 anode2 anlist
/                 ipnt     noden     xl      oldc     hl       y        st_pnt
                  end_pnt  ey        f_sp    f_ep     vl       nx       sp
                  ep       nn        cc      nx       ny       nxy      nd
                  ncx      nc        ncy     ncoord   nctxt    ncxy
)

  (setq noden (+ 1 (- anode2 anode1)))              ;node 갯수
  (setq xl (+ fcw (* noden cw)))                    ;x길이

  (setq oldc (getvar "CECOLOR"))                    ;현재 색 대피
  (setvar "CECOLOR" "RED")                          ;table의 색을 빨간색으로

  (setq hl 0)                                       ;수평선 count

  (repeat (+ ncamb 2)                               ;수평선그리기
    (setq y (- ay (* hl rh)))                       ;수평선 y값
    (setq st_pnt (list ax y))                       ;수평선 시작점
    (setq end_pnt (list (+ ax xl) y))               ;수평선 끝점
    (command "LINE" st_pnt end_pnt "")              ;수평선 그리기
    (setq hl (1+ hl))                               ;다음 수평선
  ) ;of repeat

  (setq ey (- ay (* (+ ncamb 1) 7)))                ;수직선의 끝점 y값
  (setq f_sp (list ax ay))                          ;첫번째 start point
  (setq f_ep (list ax ey))                          ;첫번째 end point
  (command "LINE" f_sp f_ep "")                     ;첫번째 수직선 그리기

  (setq vl 0)                                       ;수직선 count

  (repeat (1+ noden)                                ;node갯수 만큼 반복
    (setq nx (+ ax fcw (* vl cw)))                  ;n번째의 X좌표
    (setq sp (list nx ay)                           ;시작점
          ep (list nx ey))                          ;끝점
    (command "LINE" sp ep "")                       ;수직선 그리기
    (setq vl (1+ vl))                               ;다음 수직선으로
  ) ;of repeat

  (setvar "CECOLOR" oldc)                           ;옛색 되돌리기

  (setq nn anode1)                                  ;node count
  (setq cc 1)                                       ;column count

  (repeat noden                                     ;node갯수 만큼 반복
    (setq nx (+ ax fcw (/ cw 2.0) (* (1- cc) cw)))  ;n번째 node의 x좌표
    (setq ny (- ay (/ rh 2.0)))                     ;node의 y좌표
    (setq nxy (list nx ny))                         ;node의 xy좌표
    (setq nd (nth 0 (nth (1- nn) nlist)))                ;node번호 입력값
    (if (or (= (substr nd 1 1) "-") (= (substr nd 1 1) "+"))  ;Abut/Pier인경우
      (setq nd (itoa (atoi (substr nd 2))))         ;첫문자(+/-)와 공백 제거
      (setq nd (itoa (atoi nd)))                    ;공백 제거
    ) ;of if
    (command "TEXT" "M" nxy th 0.0 nd)             ;node번호 적기

    (setq ncx (+ ax fcw cw (- 0.0 trg) (* (1- cc) cw)))   ;n번째 좌표text의 y좌표

    (setq nc 1)                                           ;첫번째 camber부터

    (repeat ncamb                                         ;camber수만큼 반복
      (setq ncy (- ny (* rh nc)))                         ;camber text의 y좌표
      (setq ncoord (atof (nth (+ nc 1) (nth (1- nn) anlist)))) ;camber량(입력값,실수)
      (setq nctxt (rtos (* yfac ncoord -1) 2 3))          ;camber량 text
      (setq ncxy (list ncx ncy))                          ;text의 insert point
      (command "TEXT" "MR" ncxy th 0.0 nctxt)            ;text쓰기
      (setq nc (1+ nc))                                   ;다음 camber로
    ) ;of repeat

    (setq nn (1+ nn))                                     ;다음 node로
    (setq cc (1+ cc))                                     ;다음 column으로
  ) ;of repeat
) ;of defun

(princ)
