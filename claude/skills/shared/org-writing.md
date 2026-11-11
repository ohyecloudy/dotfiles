# org 작성 (정본)

org 문서를 쓰는 스킬(`code-tour`, `walkthrough`, `warm-start`, `to-spec`, `to-tickets`, `domain-modeling`, `improve-codebase-architecture`, `poc`)이 공유하는 본문 작성 규칙. 한 bullet에 여러 사실을 몰아 쓰면 훑어 읽을 수 없어서 둔다. 스킬 고유 규칙(식별자 verbatim, 다이어그램 등)은 각 스킬에 남기고, 여기와 겹치는 규칙은 여기 하나만 둔다.

## 짧은 문장 plain list

- 한 bullet = 사실 하나 = 절 하나.
  - "조건 하나 + 결과 하나"(`X하면 Y`)는 한 bullet로 허용.
  - 다른 bullet을 받치는 전제, 이유, 출처는 하위 bullet로 내린다.
- 쪼갤 신호
  - 쉼표나 ` - `로 이은 절
  - 접속어: 다만, 그리고, 이어서, 하지만
  - 문장이 될 만큼 긴 괄호
- 길이 숫자 상한은 두지 않는다. 의미 단위로 판정한다.

## 라벨 bullet

- 값이 짧으면 `라벨: 값`. 값은 단어나 짧은 구만(`예`, `done`, `3개`).
- 값이 문장이거나 여러 개면 콜론 없이 `라벨`만 쓰고 하위 bullet로 펼친다.
- description list(`- 라벨 :: 값`)는 쓰지 않는다.

## 깊이와 기호

- 홀수 깊이 `-`, 짝수 깊이 `+`. plain text로 볼 때 깊이가 눈에 들어오게 한다.
- 최대 깊이 3. 더 필요하면 상위 항목을 heading으로 올린다.

## 순서

- 번호 목록(`1.`)은 독자가 그 순서대로 실행할 절차에만 쓴다. 하위는 기호 교대 규칙을 따른다.
- 관측된 순서(로그 흐름, 호출 체인)는 번호 없는 형제 bullet을 시간순으로 둔다.
- 한 bullet 안 `->`는 두 항목 연결(원인 -> 결과, 명령 -> 출력)까지. 세 단계 이상이면 형제 bullet로 분해한다.

## 출처

- 커밋 hash, 코드 위치, 실측/분석 표기는 그것이 증명하는 단위에 붙인다.
- 묶음 전체의 출처면 마지막 하위 bullet로 `커밋: <hash>`를 따로 둔다. 라벨 값으로 올리지 않는다.
- 하위 bullet이 없는 단일 bullet은 끝에 ` - <hash>`를 붙여도 된다.

## 2단 `항목 - 보충`

- 왼쪽이 식별자(링크, CL, 이름)이고 오른쪽이 짧은 구면 허용. 목차처럼 훑는 줄이다.
- 오른쪽이 문장이거나 ` - `가 2개 이상이면 오른쪽을 하위 bullet로 내린다.

## 예시

Before - 한 bullet에 결과, 조건, 전제, 근거가 섞임.

```org
  - 답: 예 - 설치본 5.8.0 DLL을 참조해 빌드한 HordeServer.GoogleChat.dll(dll+pdb 2개)을 설치 디렉터리에 넣고 server.json으로 켜면, 서버 재빌드 없이 로드되고 config 갱신 시 Google Chat에 메시지가 도착함
  - 핵심 근거: 실서버 로그 Loading ...GoogleChat.dll -> Configuration updated (success) -> webhook POST HTTP 200 + message name, 약 1초 - b470a1ac31
```

After

```org
  - 답: 예
    + config 갱신 시 Google Chat에 메시지가 도착함
    + HordeServer.GoogleChat.dll(dll+pdb 2개)을 설치 디렉터리에 넣고 server.json으로 켜면 서버 재빌드 없이 로드
      - 설치본 5.8.0 DLL을 참조해 빌드
  - 핵심 근거
    + 실서버 로그 Loading ...GoogleChat.dll
    + Configuration updated (success)
    + webhook POST HTTP 200 + message name
    + 약 1초
    + 커밋: b470a1ac31
```

## 공통 표기

- bullet은 물리적으로 한 줄. 문장 중간 하드 줄바꿈 금지(표시는 emacs soft-wrap).
- org verbatim(`=...=`) 안에 `=` 문자를 넣지 않는다(구문이 깨진다).
- 한글 조사가 바로 뒤에 붙는 자리에는 마크업을 쓰지 않는다. `~Workout~에`는 org가 마크업으로 인식하지 못해 export에서 깨진다. `Workout 모델에`처럼 조사가 붙지 않게 쓰고, 부득이하면 `~Workout~ 에`로 한 칸 띄운다.
- 헤딩 아래 본문은 별 개수 + 1칸 들여쓴다(`*` -> 2칸, `**` -> 3칸). [org-formatting.md](org-formatting.md) 정렬이 맞추므로 초안에서 손으로 맞출 필요는 없다.
- org 테이블은 표시 폭 100 컬럼 안팎. 긴 셀은 아래 행으로 이어 쓰고 항목 사이에 구분선 `|---+---|`. 테이블은 위 리스트 규칙의 대상이 아니다.
