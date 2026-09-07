---
name: to-spec
description: 현재 대화를 spec(PRD)으로 합성해 repo `docs/specs/`에 발행. 인터뷰 없이 이미 논의된 내용만 종합 - grill 세션 이후 사용.
disable-model-invocation: true
---

이 스킬은 현재 대화 맥락과 코드베이스 이해를 spec(PRD로 알고 있을 수도 있는 문서)으로 합성한다.

- 사용자를 **인터뷰하지 마라** - 이미 아는 것만 종합한다.
- 확인 질문 없이 끝까지 **비인터랙티브**로 진행한다. 남은 애매함은 사용자를 붙잡지 말고 spec의 "추가 노트"에 적는다.
- **전제**: 이 스킬은 `grill`/`grill-with-docs`로 계획을 다듬은 *다음* 실행한다. 설계가 덜 여물었으면 to-spec 대신 먼저 grilling하라.

## 저장 위치 판정

먼저 저장소 루트를 보고 in-repo인지 외부(`~`)인지 결정한다. 전체 규칙은 [project-docs-storage.md](../shared/project-docs-storage.md) 참고. 요약: 루트에 `CONTEXT(-MAP).org` 또는 마커 `.project-docs`가 있으면 **in-repo**, 없으면 **외부** - 아래 모든 경로를 `~/project-docs/<정규화된 저장소 절대경로>/` 아래에 그대로 복제(서브트리 미러). 읽기·쓰기 모두 이 규칙을 따른다.

## 프로세스

1. **탐색.** 아직 안 했다면 repo를 탐색해 현재 상태를 파악한다.
   - `CONTEXT.org`(다중 컨텍스트면 `CONTEXT-MAP.org` → 각 `CONTEXT.org`)가 있으면 읽어 그 용어를 spec 전반에 사용한다. 외부 모드면 미러 트리(`~/project-docs/<정규화 경로>/`)에서 읽는다.
   - 건드리는 영역에 `docs/adr/` ADR이 있으면 존중한다.
   - 둘 다 없으면 그냥 넘어간다(강제하지 않음). to-spec은 glossary를 *읽기*만 한다 - 갱신은 `domain-modeling` 몫.

2. **작성.** 아래 템플릿으로 spec을 쓴다. 도메인 용어는 `한글(영문)` 병기.

3. **저장.** `docs/specs/<slug>/spec.org`에 저장한다(`<slug>` = 제목 kebab; feature 폴더 하나에 spec + 티켓 co-locate). 외부 모드면 `~/project-docs/<정규화 경로>/docs/specs/<slug>/spec.org`. 커밋·스테이징은 하지 않는다 - 저장 경로만 출력한다.

4. **포매팅.** 저장한 뒤 emacs로 org 들여쓰기를 정렬한다.

```bash
emacs -Q --batch --eval '(progn (require (quote org)) (setq org-adapt-indentation t) (let ((dir default-directory)) (dolist (f command-line-args-left) (find-file (expand-file-name f dir)) (org-mode) (org-indent-region (point-min) (point-max)) (save-buffer))))' <생성한 .org 파일들>
```

emacs에서 파일을 열었다 저장한 것과 같은 상태로 만든다 - 헤딩 아래 본문이 별 개수 + 1칸으로 정렬된다. 여러 파일을 한 번에 넘길 수 있다. 외부 모드 파일은 `~/project-docs/...` 절대경로로 넘긴다(`default-directory` 상대 처리와 무관하게 동작). emacs가 없거나 실패하면 파일은 그대로 두고 사용자에게 그 사실만 알린다.

<spec-template>
#+title: <spec 제목>
#+status: ready-for-agent
#+created: <YYYY-MM-DD>

* 문제 정의

  사용자 관점에서, 사용자가 겪는 문제.

* 해결책

  사용자 관점에서, 그 문제의 해결책.

* 사용자 스토리

  번호 매긴 목록. 각 항목 형식:

  1. <액터>로서, <기능>을 원한다, 그래야 <이득>.

  예) 모바일 뱅킹 고객으로서, 계좌 잔액을 보고 싶다, 그래야 지출 결정을 더 잘 내린다.

  기능의 모든 *진짜* 측면을 덮되 padding 금지 - 필요한 만큼만. 작은 기능이면 짧아도 된다.

* 구현 결정

  내려진 구현 결정 목록. 예:

  - 만들거나 수정할 모듈
  - 그 모듈의 수정될 인터페이스
  - 개발자의 기술적 명확화
  - 아키텍처 결정 / 스키마 변경 / API 계약 / 구체적 상호작용

  구체적 파일 경로나 코드 스니펫은 넣지 마라 - 금방 낡는다.
  예외: 프로토타입이 산문보다 정확히 결정을 담는 스니펫(상태 기계, reducer, 스키마, 타입 모양)을 냈다면 해당 결정에 인라인하고 프로토타입 출처임을 짧게 명시. 결정 핵심만 남기고 동작 데모는 버려라. 스니펫은 `#+begin_src <lang>` 블록에 넣는다(언어가 불명확하면 `#+begin_example`).

* 테스트 결정

  - 좋은 테스트의 정의: 구현 세부가 아니라 *외부 동작*만 테스트. 가능한 *가장 높은 진입점*에서 테스트.
  - 테스트할 모듈.
  - 코드베이스의 유사 테스트(prior art).

* 범위 밖

  이 spec에서 범위 밖인 것들.

* 추가 노트

  기타 노트. spec 작성 중 남은 애매함·확인 필요 사항도 여기 기록.
</spec-template>

## org 작성 규칙

- 한 항목 = 한 bullet, 물리적으로 한 줄(문장 중간 하드 줄바꿈 금지 - 길어도 한 줄, 표시는 emacs soft-wrap).
- org verbatim(`=...=`) 안에 `=` 문자를 넣지 않는다(구문이 깨진다).
- 한글 조사가 바로 뒤에 붙는 자리에는 마크업을 쓰지 않는다 - `~Workout~에`는 org가 마크업으로 인식하지 못해 export에서 깨진다. `Workout 모델에`처럼 조사가 붙지 않게 문장을 쓰고, 부득이하면 `~Workout~ 에`로 한 칸 띄운다.
- 헤딩 아래 본문은 별 개수 + 1칸 들여쓴다(`*` → 2칸, `**` → 3칸, `***` → 4칸). 포매팅 단계가 자동으로 맞추므로 초안에서 손으로 맞출 필요는 없다.
