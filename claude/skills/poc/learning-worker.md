# PoC 학습 worker

`/poc` 메인이 `구현` 대안 하나의 학습 섹션 작성을 맡기는 서브에이전트 절차. `explain-change`의 `evidence: full` JSON과 다이어그램 소스가 메인 컨텍스트에 실리지 않게 분리했다. 렌더 결과를 poc.org에 직접 쓰고 메인에는 짧게만 돌려준다.

## 입력 (메인 프롬프트)

- poc.org 절대 경로
- 맡은 대안명 - 합의의 짧은 이름. poc.org에 쓸 때 이 이름으로만 부르고 `A`/`B` 같은 문자 라벨을 쓰지 않는다
- 대안 커밋 범위 `<첫 커밋>^..<마지막 커밋>`
- 기존 흐름 작성 여부 - 첫 `구현` 대안만 `예`
- 저장소 위치
- 낯선 영역 (없으면 생략)

**사용자에게 묻지 않는다.** 독자 프로필 판정은 메인이 끝내고 넘긴다.

## 절차

1. Skill 도구로 `explain-change`를 호출한다. 넘기는 것: 대안 커밋 범위, 저장소 위치, 낯선 영역(있으면), `evidence: full`, 다이어그램 `auto`. 기존 흐름 작성이 `예`면 `before: [sequence, state, er]`도 넘긴다 - 첫 대안 범위의 "전"은 시작 커밋이라 다른 대안 코드가 섞이지 않는다. 뒤 대안은 앞 대안 커밋 위에 있어 "전"이 오염되므로 요청하지 않는다.
2. 결과로 poc.org `* 핵심 코드 워크스루` 아래 `** <대안명>` 서브트리를 쓴다. 형식은 [SKILL.md](SKILL.md)의 템플릿.
   - `*** 그림 - <종류 한글명>, <title>`: `diagrams`의 후 소스만, 장마다 heading 하나. 배치 순서·표기는 [reading-doc.md](../shared/reading-doc.md)의 "그림 섹션". `:file`은 `./<대안 slug>-<kind>[-<시점 slug>].png`. `diagrams`가 비면 `*** 그림` 한 개에 "없음" 한 줄.
   - 논리 단위: `units[].summary` + `evidence`. 매핑은 [reading-doc.md](../shared/reading-doc.md)의 "explain-change 결과 매핑".
3. 기존 흐름 작성이 `예`면 `* 핵심 코드 워크스루` 맨 앞 `** 기존 흐름`을 쓴다.
   - `before`가 있는 그림마다 `*** <종류 한글명> - <title>` + caption + `before` 블록(`:file ./base-<kind>[-<시점 slug>].png`).
   - `before: ""`만 있거나 `before`가 하나도 없으면 "없음 - 신규 흐름" 한 줄.
4. `* 사전 지식`은 `prerequisites`로 채운다 - reason별 필드는 [reading-doc.md](../shared/reading-doc.md)의 "explain-change 결과 매핑". 앞 대안이 같은 개념명을 이미 썼으면 새 heading 대신 그 항목 `등장 위치`에 이 대안 위치를 덧붙인다. 낯선 영역이 없고 `no-precedent` 항목도 없으면 섹션이 비어 있는 채로 두고 메인이 지운다.
5. "이 코드가 trade-off의 어느 쪽을 택한 것인지"는 poc.org `* 합의 상세 - 서브에이전트 읽기용`과 `* 시도 로그 - 서브에이전트 기록`를 읽어 논리 단위마다 보강한다. 시도 로그에 근거가 없으면 지어내지 말고 그 줄을 생략한다.
6. 코어 결과의 맨 경로는 org 링크로 바꾼다([code-locations.md](../shared/code-locations.md)).

`** <대안명>`, `** 기존 흐름`, `* 사전 지식` 밖은 건드리지 않는다 - 다른 대안 worker가 순차로 같은 파일에 쓴다.

## 반환

```
학습 섹션: 완료 | 실패(<원인 한 줄>)
대안: <대안명>
사전 지식: <추가한 개념 수>
워크스루: <논리 단위 수>
그림: <장 수>, 기존 흐름: <장 수 | 신규 흐름 | 해당 없음>
```
