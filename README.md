# [DJDG]
[다정다감] AutoCAD AutoLisp 3'rd party program for Civil Engineers 
# Manual
Site : <a href=http://www.gumifo.org/djdg/manual target=_blank>http://www.gumifo.org/djdg/manual</a>
# Download
Click [Clone or Down] button on DJDG repository. Click [Download ZIP]  
You can get DJDG-master.zip
# Install
1. Unzip DJDG-master.zip
2. Copy DJDG folder under autocad folder where is ACAD.EXE
3. Copy BLOCKS folder  under autocad folder where is ACAD.EXE
4. Copy SUPPORT folder to under autocad folder where is ACAD.EXE
5. Run AutoCAD
6. Command menuload
7. Select djdg.cuix at support folder 
8. You can find the DJDG menu on menubar. if you can't find menubar then command menubar and select 1.
# Command list
| Command | 설명(Kor) | Description(Eng) |
|---------|---------|----------|
|2PD		|[두 점간의 길이 기입하기](http://gumifo.org/djdg/manual/8/2pd.htm)	|Write distance between two points|
|ABUT		|교대단면그리기리			|Draw section of abutment|
|ADDTEXT	|숫자 text에 일정값 더하기	|Plus number on number text|
|ALL-ULT	|설계방법표시하기			|Insert Design Method(Allowable/Ultimate)|
|ALLPLOT	|모든 도곽출력			|Plot all borders|
|APLOT		|선택된 도곽 출력			|Plot selected borders|
|ARD		|구간 Array	           	|Array linear between seclected two point|
|ARP		|호를 따라 Array			|Array polar between seclected two point|
|ARSLOP		|기울어진 면을 따라 Array		|Array linear referencing inclined line|
|ASLAB		|콘크리트포장용 접속슬래브 그리기	|Draw approach slab (Concrete pavement type)|
|B1			|철근마킹-1(밑줄있음)			|Rebar marking type-1(with under line)|
|B3			|철근마킹-3(밑줄있음)			|Rebar marking type-2(with under line)|
|B4			|철근마킹-4(밑줄있음)			|Rebar marking type-3(with under line)|
|B5			|철근마킹-5(밑줄있음)			|Rebar marking type-4(with under line)|
|B6			|철근마킹-6(밑줄있음)			|Rebar marking type-5(with under line)|
|BAR		|철근상세도로 철근재료표 만들기	|Make rebar table using rebar details|
|BASEC		|Base Con'c 그리기			|Draw base concrete|
|BB	ENTITY 	|한점에서 자르기				|Break a entity at a point|
|BC			|철근마킹 바꾸기(밑줄없음→밑줄있음)	|Change rebar marking type|
|BCN		|차례대로 숫자 쓰기			|Write number |
|BCROSS		|서로만나는점에서 자르기			|Write numbers in sequence|
|BDASH		|선의 일부를 숨은선으로 고치기	|Change a part of line to hidden line|
|BM1		|철근마킹-1(밑줄없음)			|Rebar marking type-1(Only circle)|
|BM2		|철근마킹 보조선				|Rebar marking type-2(Only circle)|
|BM3		|철근마킹-3(밑줄없음)			|Rebar marking type-3(Only circle)|
|BM4		|철근마킹-4(밑줄없음)			|Rebar marking type-4(Only circle)|
|BM5		|철근마킹-5(밑줄없음)			|Rebar marking type-5(Only circle)|
|BM6		|철근마킹-6(밑줄없음)			|Rebar marking type-6(Only circle)|
|BMD		|모메트선도(BMD) 그리기		|Draw bending moment diagram|
|BOR		|시추주상도그리기				|Draw boring log|
|BORM		|보링위치표시하기				|Draw position marking of boring|
|BRACKET	|브라켓 그리기				|Draw concrete bracket|
|BRR		|방호벽(h=1.080m) 그리기		|Draw concrete barrier (h=1.080m)|
|BRR1		|방호벽(h=1.350m) 그리기		|Draw concrete barrier (h=1.350m)|
|BSE		|교량시종점표시하기			|Start point or End point of Bridge|
|CALT		|사칙연산하여 도면에 기입하기		|Apply arithmetic to number text|
|CAMBER		|솟음도(Camber) 그리기		|Draw camber |
|CDD		|Dounut크기 한꺼번에 바꾸기	|Change diameter of donut at once|
|CDS		|치수선 Scale고치기			|Change Scale of Dimensions|
|CEH		|Hatch경계 Entity만 hatch layer로 복사하기	|Copy Entities to Hatch layer|
|CHCOLOR	|색바꾸기					|CHange CoLoR|
|CHLTYPE	|Line type바꾸기			|Change line type |
|CONC		|Con'c표시하기				|Insert concrete marking block|
|COORD2		|CAD상의 좌표기입			|Write coordinate|
|CRC		|원크기 한꺼번에 바꾸기			|Change diameter of Circle at once|
|CTH		|Text크기 바꾸기			|Change Text Height|
|CTEXT		|Text내용 복사하기			|Copy TeXT Content|
|CUTPIPE	|물결모양 절단 및 생략표시하기	|Draw wave shape cutting mark|
|DA			|기울어진 치수선그리기			|Dimension Aligned|
|DC			|호치수선그리기				|Dimension Circle or Arc|
|DELSHORT	|일정길이 이하의 line지우기		|Delete all lines that is shorter then specified length|
|DH			|수평치수선그리기				|Dimension Horizontal|
|DIR		|방향표시(행선지)하기			|DIRection MaRKing|
|DIVA		|호의 일부분 Divide하기		|Divide a part of Arc|
|DIVL		|선의 일부분 Divide하기		|Divide a part of Line|
|DJDGFONT	|토목구조설계용 font(djdg.shx) 보기	|View font code table for Civil engineering|
|DO			|Oblique된 수직치수선그리기			|Dimension Oblique|
|DSK		|기존치수선에 skew길이값 ()안에 추가	|Add skewed distance on existing dimension|
|DSS		|치수선합 치수선그리기				|Draw Dimension that sum existing dimensions|
|DUD		|치수 위/아래로 옮기기				|Up and Down dimension text|
|DV			|수직치수선그리기					|Dimension Vertical|
|EARTH		|터파기 단면 그리기				|Draw section of excavation |
|ELM		|Elevation 표시하기			|Insert Elevation marking and Write Elevation|
|FF2		|중심선기준 양쪽으로 offset하기		|Offset 2 side at once|
|FFP		|여러번 offset하기				|Offset several times|
|GRNDMRK	|지표면 표시하기					|Insert ground surface marking|
|GSECTION	|Gsection data파일 만들기		|Make data file for G-Section|
|GUSSET		|Gusset plate 그리기			|Draw gusset plate|
|HBLIST		|데이터 파일로 철근재료표 만들기		|Make rebar table using data file|
|HOOK		|갈고리철근 그리기				|Draw hook rebar|
|JEON		|전면,배면,상면,하면,내측,외측 표시	|Insert front marking|
|JUMP		|교대부 점검용계단(평면) 그리기		|Draw stairs for abutment inspection  (plan)|
|JUNG		|분리형중분대(h=890mm) 그리기		|Draw center barrier(h=890mm, half type)|
|JUNG1		|일체형중분대(h=890mm) 그리기		|Insert center barrier-1(h=890mm, Intergrated type)|
|JUNG2		|일체형중분대(h=1.350m) 그리기		|Insert center barrier-2(h=1350mm, Intergrated type)|
|LANEMARK	|차선표시그리기					|Draw lane marking|
|LARC		|Arc위의 두점사이의 호길이 알아내기	|Measure distance betwee two points on the arc|
|LEAD		|지시선 만들기					|Draw leader|
|LINEXT		|Line 늘리기					|Exend line by distance|
|LSLOP		|경사선표시하고 경사(%)표시하기		|Draw Line by slop and slop marking|
|MCHT		|Text를 순서대로 수정하기			|Edit selected texts in order|
|MEASA		|호의 일부분 Measure하기			|Measure command on Arc|
|MEASL		|선의 일부분 Measure하기			|Measure command on Line|
|MKBDR		|자동 Plot을 위한 도곽만들기		|Make border for auto plot|
|MLEAD		|여러개 화살표가진 지시전(2)			|Draw leader that has several arrow head(straight type)|
|MTLEAD		|여러개 화살표가진 지시선(1)			|Draw leader that has several arrow head|
|NORIPF		|노리측면도 그리기				|Draw section line of embankment|
|NORTH		|방위표시하기					|Insert north marking|
|NOTEBOX	|Note Box그리기				|Draw note box|
|NT			|치수 고치기					|Edit text with Dialog box In succession|
|NUMCOOR	|좌표list만들기					|Make coordinate list of selected points|
|OB			|기준선에 맞춰 Oblique하기		|Convert vertical dimension to Oblique dim|
|PARA		|포물선 그리기				|Draw Parabola|
|PARC		|Arc위의 임의 점 잡아내기		|Find the point that is far from your point on the arc |
|PIER		|교각측면도 그리기			|Draw pier side view using dialog box|
|PILES		|말뚝들 그리기				|Draw piles using dialog box|
|PLM		|면의 명칭 표시하기			|Draw name of surface|
|PLOTBDR	|자동 Plot용 도곽 Plot하기	|Plot borders automatically|
|PLOTEPS	|eps파일 만들기				|Plot to eps file|
|PNORI		|평면노리(직선) 그리기			|Draw plan view of embankment|
|PSCBEAM	|PSC beam 단면도 그리기		|Draw section of PSC beam bridge|
|QTABLE		|쿼트로 table의 테이블그리기	|Draw table using data file that made of "+", "-", "|"|
|RBAR		|절곡 표시하기				|Draw rounded rebar|
|RBRF		|원형기둥 주철근 정면도			|Draw front view of main rebar of circular column |
|RBRP		|원형기둥 주철근 평면도그리기		|Draw plan view of main rebar of circular column |
|RCD		|R.C.D 파일 그리기			|Draw front view of R.C.D pile|
|RDL		|좌표값 data file 읽거 line 그리기	|Read coordinates data file and Draw line|
|RMARK1		|원이나 호의 R표시하기			|Radius MARK|
|RNORI		|회전(원뿔)노리그리기			|Draw plan view of rounded embankment|
|ROUND		|Round 표현하기			|Draw rounded surfece|
|RREF		|Rotate referance		|Rotate referance|
|RTEXT		|글자 둥글게 배열하기			|Write text on circle or arc|
|RWALL		|옹벽단면 그리기				|Draw retaining wall section using Dialog box|
|SARW		|단면표시 하기				|Insert Section arrow |
|SB			|Split철근 그리기			|Draw rebar that resists splitting forces|
|SBLOCK		|교량받침블럭 보강철근 그리기		|Draw bearing block|
|SHATCH		|간단하게 Solid Hatch하기	|Solid hatch|
|SHOE		|Bearing 표시				|Insert bearing type marking|
|SLOP		|노면 기울기(slop,%) 표시하기	|Draw slop mark(%)|
|SLOPL		|경사표시하기				|Draw slop lines that express inclined surface|
|SOLIDBK	|Solid Background로 보내기	|Put all solid back|
|SPILE1		|강관파일그리기(내면)			|Draw steel pile (section)|
|SPILE2		|강관파일 그리기(외면)			|Draw steel pile (front view)|
|SPLICE		|Splice 그리기				|Draw splice plate using dialog box|
|STRP1		|스터럽철근 그리기(인장측)		|Stirrup rebar (tension side)|
|STRP1		|스터럽철근 그리기(압축)		|Stirrup rebar (compression side)|
|SUNGTO		|성토표시하기				|Insert embankment marking|
|TABLE		|Table그리기				|Draw table|
|TAH		|Text를 수평으로 정렬하기		|Align texts to horizontal line|
|TAV		|Text를 수직으로 정렬하기		|Align texts to vertical line|
|TC			|강재두께 변화 표시			|Draw taper of steel plate thickness(1:5)|
|THUN		|번개모양 생략표시하기			|Draw break line(thunder shape)|
|TM			|기존 Text에 원이나 Box 테두리 씌우기	|Draw circle or box on existing text|
|TRI		|경사면 표시하기				|Draw slope ratio|
|TRIMB1		|강재선 겹치는 부분 정리하기(1)	|trim between lines|
|TRIMB2		|강재선 겹치는 부분 정리하기(2)	|trim between lines(make gap both side)|
|TRIMB3		|강재선 겹치는 부분 정리하기(3)	|trim between lines(make gap one side)|
|TXTFIL		|외곽선 폰트 Solid Hatch 하기		|Do Solid hatch double line text|
|TXTR		|Text를 line이나 원의 맞게 기울이기		|Rotate text referancing slop of line or circle|
|WATERM		|수면표시하기						|Insert water level marking|
|WAVE		|물결모양 생략표시하기					|Draw break line (Wave shape)|
|WBNAME		|Text를 file명으로 Wblock하기		|Do Wblock saving file name is selected text|
|WDWGN		|파일명 기입하기				|Write current file name on drawing|
|WELD		|용접기호 표시				|Draw welding symbol|
|WFLOW		|물흐르는방향표시하기			|Insert water flow direction|
|WLEN		|Line의 길이 기입하기			|Write length of line|
