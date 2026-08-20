# 언제 모킹하나

**시스템 경계(system boundary)**에서만 모킹한다:

- 외부 API(결제, 이메일 등)
- 데이터베이스(때때로 — 테스트 DB 선호)
- 시간/무작위성(time/randomness)
- 파일 시스템(때때로)

모킹하지 말 것:

- 자신의 클래스/모듈
- 내부 협력자(internal collaborator)
- 자신이 통제하는 모든 것

## 모킹 가능하게 설계하기

시스템 경계에서는 모킹하기 쉬운 인터페이스를 설계한다:

**1. 의존성 주입(dependency injection)을 사용하라**

외부 의존성을 내부에서 생성하지 말고 주입받아라:

```typescript
// Easy to mock
function processPayment(order, paymentClient) {
  return paymentClient.charge(order.total);
}

// Hard to mock
function processPayment(order) {
  const client = new StripeClient(process.env.STRIPE_KEY);
  return client.charge(order.total);
}
```

**2. 범용 페처(generic fetcher)보다 SDK 스타일 인터페이스를 선호하라**

조건 분기가 들어간 하나의 범용 함수 대신, 각 외부 작업마다 구체적인 함수를 만들어라:

```typescript
// GOOD: Each function is independently mockable
const api = {
  getUser: (id) => fetch(`/users/${id}`),
  getOrders: (userId) => fetch(`/users/${userId}/orders`),
  createOrder: (data) => fetch('/orders', { method: 'POST', body: data }),
};

// BAD: Mocking requires conditional logic inside the mock
const api = {
  fetch: (endpoint, options) => fetch(endpoint, options),
};
```

SDK 접근의 이점:
- 각 목(mock)이 하나의 구체적인 형태만 반환
- 테스트 셋업에 조건 분기 없음
- 테스트가 어떤 엔드포인트를 사용하는지 보기 쉬움
- 엔드포인트별 타입 안전성
