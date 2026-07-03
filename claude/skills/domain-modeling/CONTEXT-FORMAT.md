# CONTEXT.md 형식

## 구조

```md
# {컨텍스트 이름}

{이 컨텍스트가 무엇이고 왜 존재하는지 한두 문장 설명.}

## Language

**Order**:
{용어를 설명하는 한두 문장}
_Avoid_: Purchase, transaction

**Invoice**:
배송 후 고객에게 보내는 결제 요청.
_Avoid_: Bill, payment request

**Customer**:
주문하는 개인 또는 조직.
_Avoid_: Client, buyer, account
```

## 규칙

- **의견을 분명히 할 것.** 같은 개념에 여러 단어가 있으면 최선을 하나 고르고 나머지는 `_Avoid_`에 나열.
- **정의는 짧게.** 최대 한두 문장. 무엇을 *하는지*가 아니라 무엇*인지*를 정의.
- **이 프로젝트 컨텍스트에 고유한 용어만 포함.** 일반 프로그래밍 개념(타임아웃, 에러 타입, 유틸리티 패턴)은 프로젝트가 많이 쓰더라도 해당 없음. 용어 추가 전 질문: 이 컨텍스트에 고유한 개념인가, 일반 프로그래밍 개념인가? 전자만 포함.
- **자연스러운 묶음이 생기면 소제목으로 그룹화.** 모든 용어가 하나의 응집된 영역에 속하면 평면 목록도 무방.

## 단일 vs 다중 컨텍스트 저장소

**단일 컨텍스트(대부분의 저장소):** 저장소 루트에 `CONTEXT.md` 하나.

**다중 컨텍스트:** 저장소 루트의 `CONTEXT-MAP.md`가 컨텍스트 목록, 위치, 상호 관계를 나열:

```md
# Context Map

## Contexts

- [Ordering](./src/ordering/CONTEXT.md) — 고객 주문을 접수하고 추적
- [Billing](./src/billing/CONTEXT.md) — 인보이스를 생성하고 결제를 처리
- [Fulfillment](./src/fulfillment/CONTEXT.md) — 창고 피킹과 배송을 관리

## Relationships

- **Ordering → Fulfillment**: Ordering이 `OrderPlaced` 이벤트를 발행하고, Fulfillment가 이를 소비해 피킹 시작
- **Fulfillment → Billing**: Fulfillment가 `ShipmentDispatched` 이벤트를 발행하고, Billing이 이를 소비해 인보이스 생성
- **Ordering ↔ Billing**: `CustomerId`와 `Money` 타입 공유
```

스킬은 어떤 구조인지 추론:

- `CONTEXT-MAP.md`가 있으면 읽어서 컨텍스트를 찾음
- 루트에 `CONTEXT.md`만 있으면 단일 컨텍스트
- 둘 다 없으면 첫 번째 용어가 확정될 때 루트 `CONTEXT.md`를 생성

- 다중 컨텍스트가 존재하면 현재 주제가 어느 것과 관련되는지 추론. 불분명하면 질문.
