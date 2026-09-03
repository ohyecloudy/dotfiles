---
name: to-tickets
description: 계획·spec·현재 대화를 트레이서 불릿(tracer bullet) 티켓 묶음으로 분해해 로컬 파일로 발행. 각 티켓은 티켓당 한 파일에 자신의 블로킹 엣지(blocking edge)를 텍스트로 선언.
disable-model-invocation: true
---

# To Tickets

계획·spec·대화를 **티켓** 묶음으로 분해: 트레이서 불릿(tracer bullet) 수직 슬라이스(vertical slice), 각 티켓은 자신을 **막는(block)** 티켓들을 선언한다.

티켓은 **로컬 파일**로 발행한다 — 티켓당 한 파일, 상태는 `ready-for-agent`. 트리아지 라벨(triage label)이 대화에서 이미 정해졌으면 그걸 쓴다.

## 프로세스

### 1. 컨텍스트 수집

대화 컨텍스트에 이미 있는 것으로 작업한다. 사용자가 인자로 레퍼런스(spec 경로, 이슈 번호나 URL)를 넘기면 가져와 본문과 코멘트 전체를 읽는다.

- 레퍼런스도 없고 대화에도 spec이 없으면 `docs/specs/*/spec.org`(특히 `#+status: ready-for-agent`)를 훑어 후보를 제시하고, 어느 spec을 분해할지 사용자에게 확인한다. silent auto-pick 금지.
- 소스가 spec이면 그 폴더 slug가 곧 티켓의 `<feature-slug>` — 티켓은 같은 feature 폴더 `docs/specs/<slug>/tickets/`에 co-locate.

### 2. 코드베이스 탐색 (선택)

아직 코드베이스를 탐색하지 않았다면, 현재 상태를 파악하기 위해 탐색한다.

- `CONTEXT.org`(다중 컨텍스트면 `CONTEXT-MAP.org` → 각 `CONTEXT.org`)가 있으면 읽어 티켓 제목·설명에 그 용어를 쓴다.
- 건드리는 영역에 `docs/adr/` ADR이 있으면 존중한다.
- 둘 다 없으면 강제하지 않는다. to-tickets는 glossary를 *읽기만* 한다 — 새 용어 확정·갱신은 `domain-modeling` 몫.

구현을 쉽게 만들 사전 리팩터링(prefactor) 기회를 찾는다. "변경을 쉽게 만든 다음, 그 쉬운 변경을 하라(Make the change easy, then make the easy change)."

### 3. 수직 슬라이스 초안

작업을 **트레이서 불릿(tracer bullet)** 티켓으로 분해한다.

<vertical-slice-rules>

- 각 슬라이스는 모든 레이어(스키마, API, UI, 테스트)를 관통하는 좁지만 **완전한** 경로를 자른다: 한 레이어만 자르는 수평 슬라이스(horizontal slice)가 **아니라** 수직.
- 완성된 슬라이스는 그 자체로 데모 가능하거나 검증 가능하다.
- 각 슬라이스는 신선한 컨텍스트 윈도우 하나에 들어갈 크기.
- 사전 리팩터링(prefactor)은 먼저 한다.

</vertical-slice-rules>

각 티켓에 **블로킹 엣지(blocking edge)**를 준다: 시작 전에 완료돼야 하는 다른 티켓들. 블로커가 없는 티켓은 즉시 시작 가능.

**넓은 리팩터링(wide refactor)은 수직 슬라이싱의 예외다.** **넓은 리팩터링**은 하나의 기계적 변경(컬럼 이름 변경, 공유 심볼 재타이핑)인데 그 **블래스트 반경(blast radius)**이 코드베이스 전체로 퍼져, 한 번의 편집이 수천 개 호출부를 동시에 깨뜨려 어떤 수직 슬라이스도 green으로 안착할 수 없는 경우다. 이를 트레이서 불릿에 억지로 밀어넣지 말고 **확장-축소(expand–contract)**로 순서를 잡아라. 먼저 확장(expand): 아무것도 깨지지 않게 새 형태를 옛 형태 옆에 추가한다. 그다음 호출부를 블래스트 반경 크기(패키지별, 디렉토리별)로 나눠 배치(batch)로 마이그레이션한다. 각 배치는 확장에 블록된 별도 티켓이며, 옛 형태가 아직 존재하므로 배치마다 CI가 green을 유지한다. 마지막으로 축소(contract): 호출자가 하나도 안 남으면 옛 형태를 삭제한다 — 모든 마이그레이션 배치에 블록된 티켓으로. 배치조차 단독으로 green을 유지할 수 없으면, 순서는 유지하되 모든 배치가 최종 통합-검증 티켓을 블록하는 통합 브랜치를 공유하게 하라. green은 거기서만 약속된다.

### 4. 사용자에게 확인

제안한 분해를 번호 목록으로 제시한다. 각 티켓마다 보여줄 것:

- **제목(Title)**: 짧은 서술적 이름
- **블로킹(Blocked by)**: 먼저 완료돼야 하는 다른 티켓(있으면)
- **무엇을 전달하나(What it delivers)**: 이 티켓이 동작하게 만드는 end-to-end 동작

사용자에게 묻는다:

- 세분화 정도가 적절한가? (너무 굵다 / 너무 잘다)
- 블로킹 엣지가 맞나: 각 티켓이 정말로 자신을 gate하는 티켓에만 의존하나?
- 합치거나 더 쪼갤 티켓이 있나?

사용자가 분해를 승인할 때까지 반복한다.

### 5. 티켓 발행

승인된 티켓을 티켓당 한 파일로 `docs/specs/<feature-slug>/tickets/<NN>-<slug>.org`에 쓴다(소스 spec과 같은 feature 폴더에 co-locate; spec이 없는 소스면 `docs/tickets/<feature-slug>/`로 폴백). 의존성 순서(블로커 먼저)로 `01`부터 번호를 매긴다. 각 파일의 "Blocked by"에 의존하는 번호/제목을 나열한다. 아래 티켓당 파일 템플릿을 쓴다: 티켓 하나당 파일 하나, 절대 하나로 합친 파일 금지.

**프론티어(frontier)**를 표시한다: 블로커 없는 티켓 = 다운스트림 에이전트가 먼저 집을 티켓. 순수 선형 체인이면 위에서 아래로. (to-tickets는 여기서 멈춘다 — 구현은 픽업 에이전트 몫)

발행 후 to-tickets는 멈춘다 — 구현은 시작하지 않는다. 파일은 커밋·스테이징하지 않고 생성만 하며(커밋은 사용자 몫), 발행한 경로 목록을 사용자에게 보고한다.

<local-ticket-template>
#+title: <NN>: <티켓 제목>
#+status: ready-for-agent

- Spec: [[file:../spec.org][docs/specs/<slug>/spec.org]] (소스가 spec일 때; 아니면 계획/대화 출처 명시)
- 블로킹(Blocked by): 이 티켓을 gate하는 티켓의 번호/제목, 또는 "None (can start immediately)"

* 무엇을 만드나(What to build)

  이 티켓이 동작하게 만드는 end-to-end 동작 — 레이어별 구현 목록이 아니라 사용자 관점.

* 인수 기준(acceptance criteria)

  - [ ] 인수 기준 1
  - [ ] 인수 기준 2
</local-ticket-template>

구체적 파일 경로나 코드 스니펫은 피하라 — 금방 낡는다. 예외: 프로토타입이 산문보다 결정을 더 정확히 담는 스니펫(상태 기계, reducer, 스키마, 타입 모양)을 냈다면 인라인하고 프로토타입 출처임을 짧게 명시하라. 동작 데모가 아니라 결정이 담긴 핵심 부분만, 중요한 것만 남겨라. 스니펫은 `#+begin_src <lang>` 블록에 넣는다(언어가 불명확하면 `#+begin_example`).

## org 작성 규칙

- 한 항목 = 한 bullet, 물리적으로 한 줄(문장 중간 하드 줄바꿈 금지 — 길어도 한 줄, 표시는 emacs soft-wrap).
- org verbatim(`=...=`) 안에 `=` 문자를 넣지 않는다(구문이 깨진다).
- 한글 조사가 바로 뒤에 붙는 자리에는 마크업을 쓰지 않는다 — `~Workout~에`는 org가 마크업으로 인식하지 못해 export에서 깨진다. `Workout 모델에`처럼 조사가 붙지 않게 문장을 쓰고, 부득이하면 `~Workout~ 에`로 한 칸 띄운다.
- 헤딩 아래 본문은 별 개수 + 1칸 들여쓴다(`*` → 2칸, `**` → 3칸, `***` → 4칸). emacs `org-adapt-indentation` 기본 동작과 같다.
- 완료한 인수 기준은 **대문자** `- [X]`로 표시한다. 소문자 `- [x]`는 org가 체크박스로 인식하지 않아 진행률 집계에서 통째로 빠진다.
