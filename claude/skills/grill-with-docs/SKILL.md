---
name: grill-with-docs
description: 계획이나 설계를 다듬기 위한 집요한 인터뷰. 진행하면서 문서(ADR, 용어집)도 함께 생성.
disable-model-invocation: true
---

`/domain-modeling` 스킬을 사용하면서 `/grilling` 세션을 진행한다.

## 산출물 경계

이 스킬이 만드는 문서는 `domain-modeling`이 지시하는 둘뿐이다.

- `CONTEXT.org` - 용어집
- `docs/adr/` - ADR

**세션 도중에 spec·티켓을 쓰지 마라.** 설계가 아무리 굳었어도, 산출물 목록으로 제안해 승인을 받았어도 마찬가지다. spec·티켓을 만드는 길은 아래 "세션 종료"의 동의 질문 하나뿐이고, 그때도 각 스킬(`to-spec`, `to-tickets`)의 절차를 그대로 따른다.

## 세션 종료

확정된 결정을 대화에 정리하고 멈춘다. `/to-spec`은 대화 맥락을 그대로 읽어 합성하므로 별도 인계 문서를 만들 필요가 없다.

정리 뒤:

- 아직 열린 갈래가 남았으면 -> 무엇이 미해결인지 밝히고 멈춘다. 묻지 않는다
- 설계가 여물었으면(문제·해결책·주요 구현 결정·범위 밖이 모두 확정) -> 아래 번호 목록으로 묻고, 이번 설계에 맞는 추천 번호와 이유를 붙인다
  1. spec
  2. spec 후 tickets
  3. 아니오

답에 따라:

- `spec` -> [to-spec/SKILL.md](../to-spec/SKILL.md)를 Read해 그 절차를 **이 대화에서 인라인으로** 수행한다
- `spec 후 tickets` -> 위와 같이 spec을 발행한 뒤, [to-tickets/SKILL.md](../to-tickets/SKILL.md)를 Read해 방금 발행한 spec을 소스로 인라인 수행한다. to-tickets 자체의 분해 승인 단계는 그대로 거친다
- `아니오` -> 멈춘다. 다시 제안하지 않는다

두 스킬은 `disable-model-invocation`이라 Skill 도구로 부를 수 없다. 이 질문에 대한 답이 사용자 동의다. 서브에이전트에 맡기지 않는다 - grilling 맥락이 넘어가지 않는다.
