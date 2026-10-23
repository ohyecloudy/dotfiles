# 프로젝트 문서 저장 위치 판정 (정본)

`domain-modeling`, `to-spec`, `to-tickets`가 공유하는 저장 위치 규칙. 세 스킬이 만드는 산출물(`CONTEXT.org`/`CONTEXT-MAP.org`, `docs/adr/`, `docs/specs/`, `docs/tickets/`)이 서로 링크로 엮이므로, 판정은 **하나로 통일**돼야 상대 링크와 glossary 읽기가 같은 트리 안에서 유효하다. "저장소 텍스트의 문서 참조" 절은 `tdd`·`implement`·`commit-message`도 따른다.

공용 저장소를 오염시키지 않도록 **기본값은 외부(`~`)**.

## 판정 규칙 (저장소 루트 기준)

모델·spec·ticket을 만들거나 읽기 전에, 먼저 저장소 루트를 보고 in-repo인지 외부인지 결정한다:

1. 루트에 `CONTEXT.org` 또는 `CONTEXT-MAP.org`가 있으면 → **in-repo** (기존 프로젝트 자동 승계)
2. 루트에 마커 파일 `.project-docs`가 있으면 → **in-repo** (아직 문서가 없는 새 개인 프로젝트용. 빈 파일이며 존재 여부만 판정)
3. 둘 다 없으면 → **외부** (기본값, 공용 프로젝트 안전)

- `docs/specs/`·`docs/adr/`의 존재는 승계 신호로 **쓰지 않는다**. 공용 저장소가 원래 가질 수 있어 in-repo로 오판하면 오염된다. `docs/adr/`는 "읽어서 존중하는" 대상일 뿐이다.

## 외부 모드 매핑

외부일 때는 in-repo에서 쓸 경로를 `~/project-docs/<정규화된 저장소 절대경로>/` 아래에 **그대로** 복제한다(서브트리 미러). 즉 in-repo 경로 `X`는 외부에서 `~/project-docs/<정규화 경로>/X`가 된다. 세 스킬의 산출물(CONTEXT/spec/ticket/생성 ADR)은 읽기·쓰기 모두 이 외부 경로를 대상으로 한다.

절대경로 정규화: 드라이브 콜론 제거 + 슬래시 통일.

- `D:\repo` → `~/project-docs/D/repo/`
- `/home/user/proj` → `~/project-docs/home/user/proj/`

예(`E:\repo`, 외부 모드):

- `CONTEXT.org` → `~/project-docs/E/repo/CONTEXT.org`
- `src/ordering/CONTEXT.org` → `~/project-docs/E/repo/src/ordering/CONTEXT.org`
- `docs/adr/0001-x.org` → `~/project-docs/E/repo/docs/adr/0001-x.org`
- `docs/specs/<slug>/spec.org` → `~/project-docs/E/repo/docs/specs/<slug>/spec.org`
- `docs/specs/<slug>/tickets/01-x.org` → `~/project-docs/E/repo/docs/specs/<slug>/tickets/01-x.org`

`CONTEXT-MAP.org`의 `file:./src/...`나 티켓의 `[[file:../spec.org]]` 같은 상대 링크는 미러된 외부 트리 안에서 그대로 유효하므로 재작성하지 않는다.

## 저장소 텍스트의 문서 참조

저장소에 커밋되는 텍스트(코드 주석, 식별자·테스트 이름, 커밋 메시지, in-repo 문서)는 **저장소 밖 문서를 참조하지 않는다**. 저장소만 받은 사람은 그 문서를 찾을 수 없다.

- 기준은 모드가 아니라 **참조 대상 문서의 위치**. 외부 모드여도 저장소 안 upstream 문서(`docs/adr/` 등)는 인용해도 된다. 저장소 안 문서 인용은 허용이지 의무가 아니다.
- 금지: 저장소 밖 문서의 ADR 번호(`(ADR 0003)`, `ADR-0007 참고`), 경로·링크(`~/project-docs/...`), 정의 참조 지시(`CONTEXT.org 정의 참고`)
- 허용: 용어집 용어 자체를 쓰는 것. 문서 없이도 읽힌다.
- 이유가 필요하면 결론을 한 구절로 직접 적는다. 예) `config 해석을 CLI에 두기 위해(ADR 0003)` → `config 해석을 CLI에 두기 위해`

## 주의

- 저장소를 이해하기 위한 upstream in-repo 문서 읽기는 모드와 무관하게 항상 허용된다. 외부 모드가 제한하는 것은 Claude 산출물의 **쓰기** 위치다.
- emacs 정렬 등 파일을 인자로 넘길 때, 외부 모드 파일은 `~/project-docs/...` 절대경로로 넘긴다(`default-directory` 상대 처리와 무관하게 동작).
