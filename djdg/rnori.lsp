;  Program : RNORI ;Round NORI
; function : 3PARC ; 중심과, 시작각, 전체각크기, 반지름을 이용하여 호를 그려줌
; function : Polar_line ; 중심과, 각도, 시작반지름, 끝 반지름을 이용하여 polar위의 직선을 그려줌

;**********************************
;  Program : RNORI
;            Round NORI
;            By Jong-Suk Yi
;            98/8/27
;*********************************
; 05/08/15 : elevation을 입력하여 기리도록 수정

(defun c:RNORI(
	       / cen div_ang sodan danh sp ep rlist topel botel deltael num_r
	         deltael remh dh sang enag total_angle ra total_r count index r2 r1 
		)

  (defun SETERR(s)                                  ;내장에러루틴
    (if (/= s "Function cancelled")
        (princ (strcat "\nError: " s))
    ); of If
    (setq *error* oer seterr nil)
    (princ)
  ); of SETERR

;  (setq oer *error* *error* seterr)                 ;내장에러루틴 가동

  (push-env)                                        ;환경변수 대피

  (setq cen (getpoint "\nPick Center point: "))     ;중심좌표

  (setq div_ang 6)                                  ;나눌수
  (setq sodan 1000)                                 ;소단폭
  (setq danh 6)					;단높이 6.0m

  (setq sp (getpoint cen "\nPick start point: "))   ;array 시작점
  (setq ep (getpoint cen "\nPick End point: "))     ;array 끝점

  (setq rlist nil)

;노리 길이를 직접 입력받음  
;  (setq r (getdist cen "\n첫단 노리길이: "))        ;첫 노리길이 입력
;
;  (setq count 2)
;  (while r
;    (setq rlist (append rlist (list r)))            ;노리길이 리스트에 추가
;    (princ "\n") (princ count)
;    (setq r (getdist " 단 노리길이: "))            ;2단 이후 노리길이 입력
;    (setq count (1+ count))
;  );while
;  (setq num_r (length rlist))                       ;노리단수
  
  ;elevation으로 입력받음.
  (setq topel (getreal "\n노리 시작점의 Elevation(단위:m): "))
  (setq botel (getreal "\n노리 끝점의 Elevation(단위:m): "))
  (setq deltael (- topel botel))  ;elevation차

  (setq num_r (fix (/ deltael danh)))		; 단 수 
  (if (> (rem deltael danh) 0)			;단 높이로 나눠서 나머지가 있으면 단 수 하나 더하기 
    (setq num_r (1+ num_r)))
  
  (setq remh deltael)
;  (setq i 0)
  (while (> remh 0);나머지 높이가 남은 경우 반복
    (if (= rlist nil) (setq slop 1.5) (setq slop 1.8))  ;첫번째 면 소단 기울기 1.5, 아니면 1.8
    (if (>= remh 6.0)  	;나머지가 6보다 크면
      (setq dh 6.0)  	;높이차 6.0ㅡ
      (setq dh remh)	;6.0안되면 현재값
    );if
    (setq remh (- remh dh))	;나머지 높이 재계산
    (setq rlist (append rlist (list (* dh slop 1000)))) ; 단의 평면길이 더하기
;    (setq 
  );while

	 
  (setq sang (angle cen sp))                        ;시작점 절대각
  (setq eang (angle cen ep))                        ;끝점 절대각

  (setq total_angle (dang sang eang))               ;전체 각차
  (setq delta_angle (/ total_angle div_ang))        ;delta angle


  ;--------------------- ARC 그리기

  (setq ra (nth 0 rlist))                           ;첫단 노리길이

  (3parc cen sang total_angle ra)

  (setq total_r ra)

  (setq count 1)
  (repeat (1- num_r)
    (setq total_r (+ total_r sodan))
    (3parc cen sang total_angle total_r)            ;소단끝선 그리기
    (setq total_r (+ total_r (nth count rlist)))
    (3parc cen sang total_angle total_r)            ;노리끝선 그리기
    (setq count (1+ count))
  );repeat

  ;--------------------- 긴 노리선그리기
  (setq index 1)

  (repeat (1- div_ang)
    (polar_line cen (+ sang (* index delta_angle)) 0 ra)

    (setq count 1
          r2    ra)
    (repeat (1- num_r)
      (setq r1 (+ r2 sodan))
      (setq r2 (+ r1 (nth count rlist)))
      (polar_line cen (+ sang (* index delta_angle)) r1 r2)
      (setq count (1+ count))
    );repeat
    (setq index (1+ index))                         ;다음 각으로
  ) ;repeat

  ;--------------------- 짧은 노리선그리기
  (setq index 0)

  (repeat  div_ang
    (polar_line cen (+ sang (/ delta_angle 2.0) (* index delta_angle)) ;첫단노리
                0 (/ ra 2.0))
    (setq count 1
          r2    ra)
    (repeat (1- num_r)                                      ;두번재단 이상 노리
      (setq r1 (+ r2 sodan))
      (setq r2 (+ r1 (/ (nth count rlist) 2)))
      (polar_line cen (+ sang (/ delta_angle 2.0) (* index delta_angle))
                  r1 r2)
      (setq r2 (+ r1 (nth count rlist)))
      (setq count (1+ count))
    );repeat

    (setq index (1+ index))                         ;다음 각으로
  ) ;repeat

  (pop-env)                                         ;환경변수복귀

  (setq *error* oer seterr nil)                     ;내장에러루틴

  (princ)

) ;;; End of program


;----------------------------
; function : 3PARC :
;            3 Pint ARC
;            By Yi Suk Jong
;            99/4/1
;----------------------------
; 중심과, 시작각, 전체각크기, 반지름을 이용하여 호를 그려줌
; 넘어오는 값
;      center_point : 중심점
;      s_angle      : 시작가
;      total_angle  : 전체각
;      radius       : 반지름
;----------------------------

(defun 3PARC(center_point s_angle total_angle radius
/ center_point s_angle total_angle radius)
  (command "ARC" (polar center_point s_angle                       radius)
                 (polar center_point (+ s_angle (/ total_angle 2)) radius)
                 (polar center_point (+ s_angle total_angle)       radius))
);defun

;----------------------------
; function : Polar_line :
;            draw Polar line
;            By Yi Suk Jong
;            99/4/1
;----------------------------
; 중심과, 각도, 시작반지름, 끝 반지름을 이용하여 polar위의 직선을 그려줌
; 넘어오는 값
;      center_point : 중심점
;      angle        : 각도
;      radius1      : 지작반지름
;      radius2      : 끝반지름
;----------------------------

(defun Polar_line(center_point angle radius1 radius2
/ center_point angle radius1 radius2 )

  (command "LINE" (polar center_point angle radius1)
                  (polar center_point angle radius2)
                  "")

);defun

