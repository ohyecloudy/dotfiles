---
name: codebase-design
description: 깊은 모듈(deep module)을 설계하기 위한 공용 어휘. 사용자가 모듈의 인터페이스를 설계·개선하거나, 깊게 만들(deepening) 기회를 찾거나, 심(seam)을 어디에 둘지 정하거나, 코드를 더 테스트 가능하게 혹은 AI가 탐색하기 쉽게 만들려 할 때, 또는 다른 스킬이 이 deep-module 어휘를 필요로 할 때 사용.
---

# Codebase Design

**깊은 모듈(deep module)**을 설계한다: 작은 인터페이스 뒤에 많은 동작을 두고, 깔끔한 심(seam)에 배치하며, 그 인터페이스를 통해 테스트한다. 코드를 설계하거나 재구조화하는 모든 곳에서 이 언어와 원칙을 쓴다. 목표는 호출자를 위한 레버리지(leverage), 유지보수자를 위한 지역성(locality), 그리고 모두를 위한 테스트 가능성(testability)이다.

## 용어집(Glossary)

이 용어를 정확히 쓴다: "컴포넌트(component)", "서비스(service)", "API", "경계(boundary)"로 바꿔 부르지 말 것. 일관된 언어가 핵심 전부다.

**모듈(Module)**: 인터페이스와 구현을 가진 모든 것. 의도적으로 규모 불문(scale-agnostic) — 함수, 클래스, 패키지, 여러 계층을 관통하는 슬라이스 모두 해당. _Avoid_: 유닛(unit), 컴포넌트, 서비스.

**인터페이스(Interface)**: 호출자가 모듈을 올바르게 쓰기 위해 알아야 하는 모든 것 — 타입 시그니처뿐 아니라 불변식(invariant), 순서 제약(ordering constraint), 에러 모드(error mode), 필수 설정, 성능 특성까지. _Avoid_: API, 시그니처(signature) (너무 좁다 — 타입 수준 표면만 가리킴).

**구현(Implementation)**: 모듈 내부, 즉 코드 본체. **어댑터(Adapter)**와 구별된다: 작은 어댑터가 큰 구현을 가질 수도 있고(Postgres 리포지토리), 큰 어댑터가 작은 구현을 가질 수도 있다(in-memory fake). 심(seam)이 화제일 때 "어댑터"를, 그 외엔 "구현"을 쓴다.

**깊이(Depth)**: 인터페이스에서의 레버리지. 호출자(또는 테스트)가 배워야 하는 인터페이스 단위당 행사할 수 있는 동작의 양. 작은 인터페이스 뒤에 많은 동작이 앉아 있으면 **깊은(deep)** 모듈, 인터페이스가 구현만큼 복잡하면 **얕은(shallow)** 모듈.

**심(Seam)** _(Michael Feathers)_: 그 자리를 편집하지 않고도 동작을 바꿀 수 있는 지점 — 모듈의 인터페이스가 사는 *위치*. 심을 어디에 둘지는 그 뒤에 무엇을 둘지와는 별개의 설계 결정이다. _Avoid_: 경계(boundary) (DDD의 bounded context와 의미가 겹침).

**어댑터(Adapter)**: 심에서 인터페이스를 만족시키는 구체적인 것. 실체(내부에 무엇이 있는가)가 아니라 *역할*(어떤 슬롯을 채우는가)을 가리킨다.

**레버리지(Leverage)**: 호출자가 깊이에서 얻는 것. 배우는 인터페이스 단위당 더 많은 능력. 구현 하나가 N개의 호출부와 M개의 테스트에 걸쳐 되갚는다.

**지역성(Locality)**: 유지보수자가 깊이에서 얻는 것. 변경·버그·지식·검증이 호출자들에 흩어지지 않고 한 곳에 모인다. 한 번 고치면 모든 곳이 고쳐진다.

## 깊음 vs 얕음(Deep vs shallow)

**깊은 모듈** = 작은 인터페이스 + 많은 구현:

```
┌─────────────────────┐
│   Small Interface   │  ← Few methods, simple params
├─────────────────────┤
│                     │
│  Deep Implementation│  ← Complex logic hidden
│                     │
└─────────────────────┘
```

**얕은 모듈** = 큰 인터페이스 + 적은 구현(피할 것):

```
┌─────────────────────────────────┐
│       Large Interface           │  ← Many methods, complex params
├─────────────────────────────────┤
│  Thin Implementation            │  ← Just passes through
└─────────────────────────────────┘
```

인터페이스를 설계할 때 물어라:

- 메서드 개수를 줄일 수 있나?
- 파라미터를 단순화할 수 있나?
- 복잡성을 더 안으로 숨길 수 있나?

## 원칙(Principles)

- **깊이는 구현이 아니라 인터페이스의 속성이다.** 깊은 모듈은 내부적으로 작고, mock 가능하고, 교체 가능한 부품들로 구성될 수 있다 — 그것들이 인터페이스의 일부가 아닐 뿐이다. 모듈은 인터페이스에 있는 **외부 심(external seam)**뿐 아니라 **내부 심(internal seam)**(구현에 private하며 자체 테스트가 사용)도 가질 수 있다.
- **삭제 테스트(deletion test).** 모듈을 삭제한다고 상상하라. 복잡성이 사라지면 그건 pass-through였다. 복잡성이 N개의 호출자에 걸쳐 되살아나면 그건 제 몫을 하고 있던 것이다.
- **인터페이스가 곧 테스트 표면(test surface)이다.** 호출자와 테스트는 같은 심을 넘는다. 인터페이스 *너머*를 테스트하고 싶어진다면, 모듈의 모양이 잘못됐을 가능성이 크다.
- **어댑터가 하나면 가상의 심, 둘이면 진짜 심이다.** 심을 사이에 두고 실제로 무언가 달라지지 않는 한 심을 도입하지 마라.

## 테스트 가능성을 위한 설계(Designing for testability)

좋은 인터페이스는 테스트를 자연스럽게 만든다:

1. **의존성을 만들지 말고 받아라(Accept dependencies, don't create them).**

   ```typescript
   // Testable
   function processOrder(order, paymentGateway) {}

   // Hard to test
   function processOrder(order) {
     const gateway = new StripeGateway();
   }
   ```

2. **부수 효과를 내지 말고 결과를 반환하라(Return results, don't produce side effects).**

   ```typescript
   // Testable
   function calculateDiscount(cart): Discount {}

   // Hard to test
   function applyDiscount(cart): void {
     cart.total -= discount;
   }
   ```

3. **작은 표면적(Small surface area).** 메서드가 적을수록 필요한 테스트가 적다. 파라미터가 적을수록 테스트 셋업이 단순하다.

## 관계(Relationships)

- **모듈(Module)**은 정확히 하나의 **인터페이스(Interface)**를 가진다(호출자와 테스트에 제시하는 표면).
- **깊이(Depth)**는 **모듈**의 속성이며, 그 **인터페이스**를 기준으로 측정된다.
- **심(Seam)**은 **모듈**의 **인터페이스**가 사는 곳이다.
- **어댑터(Adapter)**는 **심**에 앉아 **인터페이스**를 만족시킨다.
- **깊이**는 호출자에게 **레버리지(Leverage)**를, 유지보수자에게 **지역성(Locality)**을 만들어 낸다.

## 기각된 관점(Rejected framings)

- **깊이 = 구현 라인 수 대 인터페이스 라인 수의 비율**(Ousterhout): 구현을 부풀리는 걸 보상한다. 우리는 대신 깊이=레버리지를 쓴다.
- **"인터페이스" = TypeScript `interface` 키워드나 클래스의 public 메서드**: 너무 좁다 — 여기서 인터페이스는 호출자가 알아야 하는 모든 사실을 포함한다.
- **"경계(Boundary)"**: DDD의 bounded context와 의미가 겹친다. **심(seam)** 또는 **인터페이스**라고 말하라.

## 더 깊이(Going deeper)

- **의존성을 고려해 클러스터를 깊게 만들기**는 [DEEPENING.md](DEEPENING.md) 참고: 의존성 분류, 심 규율(seam discipline), 그리고 겹치지 말고 교체하는(replace-don't-layer) 테스트.
- **대안 인터페이스 탐색**은 [DESIGN-IT-TWICE.md](DESIGN-IT-TWICE.md) 참고: 병렬 서브 에이전트를 띄워 인터페이스를 근본적으로 다른 여러 방식으로 설계한 뒤, 깊이·지역성·심 배치로 비교한다.
