---
name: implement
description: spec이나 티켓 묶음에 기술된 작업을 구현. 가능한 곳에서 /tdd로 진행하고, 완료 후 /code-review로 검토한 뒤 현재 브랜치에 커밋.
disable-model-invocation: true
---

# Implement

사용자가 지정한 spec 또는 티켓에 기술된 작업을 구현한다.

가능한 곳에서는 사전 합의된 심(seam)에서 `/tdd`로 진행한다.

타입체크(typecheck)를 자주, 개별 테스트 파일을 자주 돌리고, 전체 테스트 스위트(test suite)는 마지막에 한 번 돌린다.

완료되면 `/code-review`로 작업을 검토한다.

작업을 현재 브랜치에 커밋한다.
