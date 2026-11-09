# 통합테스트(integration test)

통합테스트는 부품들이 프로덕션과 같은 방식으로 **이어졌는지**를 검증한다. 각 부품이 올바른지는 단위 테스트 몫이다.

프로젝트 `CLAUDE.md`에 통합테스트 정의(진입점, fake 목록, 파일 위치)가 있으면 그것이 우선한다. 없으면 아래 일반 정의를 기본값으로 쓴다.

## 판별 기준

아래를 **모두** 만족해야 통합테스트다. 하나라도 빠지면 단위 테스트다.

1. **진입점과 같은 연결**: 프로덕션 진입점(main, app init, 핸들러 등록)과 같은 순서로 실제 협력자 2개 이상을 잇는다.
2. **경계에만 fake**: fake는 프로세스/OS 경계(네트워크, IPC, 디스크, 시간, OS 서비스)에만 둔다. 앱 내부 심(seam)을 fake로 바꾸면 단위 테스트다.
3. **실제 직렬화**: 경계를 넘는 데이터는 실제 직렬화(serialization)를 거쳐 수신 진입점을 지난다.
4. **연결과 순서만 검증**: 경로가 이어졌는지와 부품 사이 순서(도착 순서, 유실, 재연결)만 본다. 각 부품의 세부 분기는 단위 테스트 몫이다.

경계 fake(loopback, 메모리 저장소 등)가 아직 없으면 그 fake 작성도 첫 슬라이스에 포함한다. 테스트를 쉽게 하려고 내부 심을 fake로 바꾸는 우회는 금지 - 그건 단위 테스트를 통합테스트로 부르는 것일 뿐이다.

## 예시

```typescript
// GOOD: Wires real collaborators like main(), fakes only the network
test("order placed on web reaches the warehouse queue", async () => {
  const network = new LoopbackNetwork();
  const warehouse = startWarehouse({ network });
  const web = startWeb({ network });

  await web.placeOrder({ sku: "A-1", qty: 2 });
  await network.deliverAll();

  expect(warehouse.pendingOrders()).toEqual([{ sku: "A-1", qty: 2 }]);
});

// BAD: Internal seam replaced with a fake - this is a unit test
test("order placed on web reaches the warehouse queue", async () => {
  const fakeWarehouse = { enqueue: jest.fn() };
  const web = startWeb({ warehouse: fakeWarehouse });
  await web.placeOrder({ sku: "A-1", qty: 2 });
  expect(fakeWarehouse.enqueue).toHaveBeenCalled();
});
```
