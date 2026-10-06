---
name: improve-codebase-architecture
description: 코드베이스에서 deepening(얕은 모듈을 깊게 만드는) 기회를 찾아 mermaid 다이어그램을 담은 org 리포트로 제시하고, 사용자가 고른 후보를 grilling으로 파고든다.
disable-model-invocation: true
---

# Improve Codebase Architecture

아키텍처 마찰(friction)을 드러내고 **deepening 기회**를 제안한다: 얕은 모듈(shallow module)을 깊은 모듈(deep module)로 바꾸는 리팩토링. 목표는 테스트 가능성(testability)과 AI 탐색 용이성(AI-navigability)이다.

이 커맨드는 프로젝트의 도메인 모델에서 *정보를 얻고(informed by)*, 공용 설계 어휘 위에 세워진다:

- Skill 도구로 `codebase-design`을 호출해 아키텍처 어휘(**module**, **interface**, **depth**, **seam**, **adapter**, **leverage**, **locality**)와 그 원칙(삭제 테스트(deletion test), "인터페이스가 곧 테스트 표면", "어댑터가 하나면 가상의 심, 둘이면 진짜 심")을 가져온다. 모든 제안에서 이 용어를 정확히 쓰고, "컴포넌트(component)", "서비스(service)", "API", "경계(boundary)"로 흐르지 말 것.
- `CONTEXT.org`의 도메인 언어가 좋은 심(seam)에 이름을 준다. `docs/adr/`의 ADR은 이 커맨드가 다시 들추지 말아야 할 결정을 기록한다.

## 저장 위치 판정

`CONTEXT.org`와 ADR을 읽기 전에, 먼저 저장소 루트를 보고 in-repo인지 외부(`~`)인지 결정한다. 전체 규칙은 [project-docs-storage.md](../shared/project-docs-storage.md) 참고. 외부 모드면 도메인 문서는 `~/project-docs/<정규화된 저장소 절대경로>/` 아래에 있다. 읽기·쓰기 모두 이 규칙을 따른다.

## Process

### 1. 탐색(Explore)

**스캔하기 전에 범위부터: YAGNI.** 모듈을 깊게 만드는 건 이후 그 모듈에 대한 변경을 쉽게 만들어 되갚으므로, 최근 바뀐 부분에 가중치를 더 둔다. 보기 전에 *어디를* 볼지 먼저 정한다:

- 사용자가 방향(모듈, 서브시스템, 통증 지점)을 지목했다면 그걸 취하고 아래 추론은 건너뛴다.
- 아니면 커밋 히스토리(`git log --oneline`)를 충분히 거슬러 올라가 코드베이스의 핫스팟(hot spot) - 반복해서 등장하는 파일과 영역 - 을 찾고, 그 경로들이 먼저 눈길을 끌게 둔다. 변경이 흩어져 뚜렷한 핫스팟이 없으면 그물을 넓힌다.

손대려는 영역의 도메인 용어집(`CONTEXT.org`)과 ADR을 먼저 읽는다.

그다음 서브 에이전트를 띄워 코드베이스를 훑는다. 경직된 휴리스틱을 따르지 말고 유기적으로 탐색하며 마찰(friction)을 느끼는 지점을 기록한다:

- 한 개념을 이해하는 데 작은 모듈 여러 개를 오가야 하는 곳은?
- 인터페이스가 구현만큼 복잡한 **얕은(shallow)** 모듈은?
- 순수 함수가 테스트 가능성만을 위해 추출됐지만, 진짜 버그는 그것이 어떻게 호출되는지에 숨어 있는(지역성(locality) 없음) 곳은?
- 강하게 결합된 모듈이 심(seam)을 넘어 새는(leak) 곳은?
- 테스트되지 않았거나 현재 인터페이스로는 테스트하기 어려운 부분은?

얕다고 의심되는 것에는 **삭제 테스트(deletion test)**를 적용한다: 삭제하면 복잡성이 한곳에 모이는가, 아니면 그냥 옮겨가는가? "그렇다, 모인다"가 원하는 신호다.

### 2. 후보를 org 리포트로 제시

저장소에 아무것도 남지 않도록 저장소 밖 `~/architecture-review/YYYY-MM-DD-HHMM-<repo>.org`에 쓴다(`<repo>` = 저장소 디렉터리 이름). 디렉터리가 없으면 만든다. 파일을 자동으로 열지 않고 절대경로만 알려준다(emacs `ffap`로 점프).

각 후보에 **before/after 다이어그램**을 org mermaid 블록(`#+begin_src mermaid :file ... :width "2048"`)으로 넣는다. 후보에 맞는 패턴(콜 플로우, 왕복 시퀀스, 단면도, 질량 다이어그램, 콜 그래프 붕괴)을 골라 섞는다. 시각적으로 만들 것.

각 후보는 `**` 헤딩 하나로 렌더한다:

- **Tags**: recommendation strength(`strong` / `worth_exploring` / `speculative`)와 의존성 분류를 헤딩 태그로
- **Files**: 관련된 파일/모듈
- **Problem**: 현재 아키텍처가 왜 마찰을 일으키는가
- **Solution**: 무엇이 바뀔지 평이한 설명
- **Before / After**: `*** Before` / `*** After` 하위 헤딩에 같은 패턴으로 연속 배치해 얕음과 깊어짐을 보여준다
- **Wins**: 지역성(locality)과 레버리지(leverage) 관점에서, 그리고 테스트가 어떻게 나아지는지로 설명

리포트 맨 위에 **Top recommendation** 섹션을 둔다: 어느 후보를 먼저 다룰지와 이유, 그 후보 헤딩으로 가는 내부 링크.

**도메인은 `CONTEXT.org` 어휘로, 아키텍처는 `codebase-design` 어휘로 말한다.** `CONTEXT.org`가 "Order"를 정의했다면 "the Order intake module"이라 부르고, "the FooBarHandler"나 "the Order service"라 부르지 않는다.

**ADR 충돌**: 후보가 기존 ADR과 모순되면, 마찰이 ADR을 재검토할 만큼 진짜일 때만 표면화한다. 후보의 `*** ADR` 하위 헤딩에 한 줄로 분명히 표시한다(예: _"ADR-0007과 모순되지만, ...때문에 재검토할 가치가 있음"_). ADR이 금하는 이론적 리팩토링을 전부 나열하지 말 것.

전체 org 스캐폴드, 다이어그램 패턴, 톤은 [ORG-REPORT.md](ORG-REPORT.md) 참고.

쓴 뒤 emacs로 org 들여쓰기를 정렬한다(emacs에서 열었다 저장한 것과 같은 상태). line ending도 실행 OS에 맞춘다. emacs가 없거나 실패하면 파일은 그대로 두고 사용자에게 그 사실만 알린다.

```bash
emacs -Q --batch --eval '(progn (require (quote org)) (setq org-adapt-indentation t) (let ((dir default-directory)) (dolist (f command-line-args-left) (find-file (expand-file-name f dir)) (org-mode) (org-indent-region (point-min) (point-max)) (set-buffer-file-coding-system (if (memq system-type (quote (windows-nt ms-dos))) (quote utf-8-dos) (quote utf-8-unix))) (save-buffer))))' <생성한 .org 파일>
```

아직 인터페이스를 제안하지 말 것. 파일을 쓴 뒤 사용자에게 묻는다: "이 중 어느 것을 탐색하고 싶으세요?"

### 3. Grilling 루프

사용자가 후보를 고르면 Skill 도구로 `grilling`을 호출해 결정 트리를 함께 따라간다: 제약(constraint), 의존성, 깊어진 모듈의 모양, 심(seam) 뒤에 무엇이 앉는지, 어떤 테스트가 살아남는지.

파고드는 동안 Skill 도구로 `domain-modeling`을 호출해 도메인 모델을 최신으로 유지한다 - 용어집 대조, 모호한 용어 정제, `CONTEXT.org` 갱신, ADR 제안은 모두 `domain-modeling`의 규칙을 따른다(여기서 다시 서술하지 않음). 이 `grilling` + `domain-modeling` 조합이 곧 `grill-with-docs`이며, 이 스킬은 그 위에 아키텍처 맥락만 얹는다:

- ADR을 제안할 때 프레이밍: _"미래의 아키텍처 리뷰가 이걸 다시 제안하지 않도록 ADR로 기록할까요?"_
- 깊어진 모듈의 대안 인터페이스를 탐색하려면 Skill 도구로 `codebase-design`을 호출해 design-it-twice 병렬 서브 에이전트 패턴을 쓴다.
