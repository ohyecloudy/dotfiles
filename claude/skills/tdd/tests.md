# 좋은 테스트와 나쁜 테스트

## 좋은 테스트

**통합 스타일(integration-style)**: 내부 부품의 목(mock)이 아니라 실제 인터페이스를 통해 테스트한다.

```typescript
// GOOD: Tests observable behavior
test("user can checkout with valid cart", async () => {
  const cart = createCart();
  cart.add(product);
  const result = await checkout(cart, paymentMethod);
  expect(result.status).toBe("confirmed");
});
```

특징:

- 사용자/호출자가 신경 쓰는 동작을 테스트
- 공개 API만 사용
- 내부 리팩터링을 견뎌냄
- HOW가 아니라 WHAT을 서술
- 테스트당 하나의 논리적 단언(assertion)

## 나쁜 테스트

**구현 세부사항 테스트(implementation-detail test)**: 내부 구조에 결합됨.

```typescript
// BAD: Tests implementation details
test("checkout calls paymentService.process", async () => {
  const mockPayment = jest.mock(paymentService);
  await checkout(cart, payment);
  expect(mockPayment.process).toHaveBeenCalledWith(cart.total);
});
```

경고 신호(red flag):

- 내부 협력자(internal collaborator)를 모킹
- private 메서드를 테스트
- 호출 횟수/순서를 단언
- 동작 변경 없이 리팩터링하면 테스트가 깨짐
- 테스트 이름이 WHAT이 아니라 HOW를 서술
- 인터페이스 대신 외부 수단으로 검증

```typescript
// BAD: Bypasses interface to verify
test("createUser saves to database", async () => {
  await createUser({ name: "Alice" });
  const row = await db.query("SELECT * FROM users WHERE name = ?", ["Alice"]);
  expect(row).toBeDefined();
});

// GOOD: Verifies through interface
test("createUser makes user retrievable", async () => {
  const user = await createUser({ name: "Alice" });
  const retrieved = await getUser(user.id);
  expect(retrieved.name).toBe("Alice");
});
```

**동어반복 테스트(tautological test)**: 기대값이 구현을 되풀이해서, 테스트가 구성상 통과한다.

```typescript
// BAD: Expected value is recomputed the way the code computes it
test("calculateTotal sums line items", () => {
  const items = [{ price: 10 }, { price: 5 }];
  const expected = items.reduce((sum, i) => sum + i.price, 0);
  expect(calculateTotal(items)).toBe(expected);
});

// GOOD: Expected value is an independent, known literal
test("calculateTotal sums line items", () => {
  expect(calculateTotal([{ price: 10 }, { price: 5 }])).toBe(15);
});
```
