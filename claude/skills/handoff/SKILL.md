---
name: handoff
description: 현재 대화를 다른 에이전트가 이어받을 수 있도록 핸드오프 문서로 압축.
argument-hint: "다음 세션은 어디에 쓰일 예정인가?"
disable-model-invocation: true
---

새 에이전트가 작업을 이어갈 수 있도록 현재 대화를 요약한 handoff 문서를 작성한다. 저장소에 아무것도 남지 않도록 저장소 밖 `~/handoff/YYYY-MM-DD-HHMM-<slug>.md`에 쓴다(`<slug>` = 다음 세션 주제 kebab). 디렉터리가 없으면 만든다. 문서가 저장소 밖에 있으므로 파일 참조는 전부 절대 경로로 쓰고, 작성 후 절대 경로만 알려준다(emacs evil `gF` 로 점프).

문서에 "suggested skills" 섹션을 포함해, 에이전트가 호출해야 할 스킬을 제안한다.

이미 다른 산출물(spec `docs/specs/`, 계획, ADR, 이슈, 커밋, diff)에 담긴 내용은 중복하지 않는다. 대신 경로나 URL로 참조한다.

API 키, 비밀번호, 개인 식별 정보 같은 민감한 정보는 마스킹한다.

사용자가 인자를 넘겼다면, 다음 세션이 집중할 내용에 대한 설명으로 간주하고 그에 맞게 문서를 조정한다.
