## 071818

### 도메인에 따라 고민해야할 데이터 특성
- 기상청에서 관측하고 있는 기상 요소들은 서로가 높은 상관관계를 갖고 있는 경우가 다반사이다. (ex_강수량과 상대습도) <br/>
이를 잘 파악 후 feature engineering을 진행하며 모델링을 하는 것이 중요.

- 변수 데이터의 분포가 기상 요소의 경우 특별하다. 양 극값에 빈도가 몰려있는 경우, 0이 절대적 비중을 차지하고 있는 경우 등. <br/>
로그 변환, 이항 처리 등 특성을 고려한 변환을 고민해야 한다.

### 항상 어려운 질문: 그래서 네 분석이 어떤 비지니스 가치를 갖고 있는데?
- 데이터가 정갈히 주어지는 교실 안 통계와는 달리, 이번 이벤트처럼 주제 설정을 분석가가 하는 경우 가장 어렵게 다가오는 물음. <br/>
고민 또 고민..
------
## 072018

### 결측치의 imputaion
- 어떤 변수에 대해 결측치를 채워 넣는 작업을 진행할 경우, 논리적인 접근이 필요하다. <br/>
왜 결측치를 제거하였으며(효율성 대비 or 당사의 선행연구), 대체할 경우 어떤 기법을 왜 사용했는지 논리적으로 뒷받침 되어야한다.<br/>
(kNN 기법의 경우 어떤 형태, 특성을 가진 변수에 대응할 수 있으므로...) <br/>

------
## 072218
- 연 / 월 / 일 /시가 별개의 열로 구성된 테이블도 dplyr %>% group_by(month, day)로 가능하다.
- 또한 lubridate 패키지로 Date 형태의 value를 month, day 등의 함수를 통해 자유롭게 뽑아낼 수 있다.
----
## 072318
- 랜덤포레스트 기법이 선형회귀보다 성능이 안좋게 나오는 경우는 무슨 이유일까?
----
## 072618
- 분석의 활용 어필은 결국 보고서. 전처리만큼 중요한 작업이 보고서 작성
- 누구에게 분석 내용을 설명하는가? 설명은 어디서부터 어디까지 진행할 것인가?
- 팀원들에게 자유롭게 묻고 자유롭게 구사해본 분석
- 활용 가능하고 사용 가치가 있는 서비스 구현의 
