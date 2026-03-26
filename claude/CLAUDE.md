# Global Preferences

## Communication

- 답변은 org-mode 마크업 사용
- 대화는 한국어, 기술 용어는 한글(영문), 예) 클로저(closure)
- 커밋 메시지/코드 주석/변수명은 영어
- 설명은 간결하게 — 변경 이유와 핵심만
- 긴 코드 블록 대신 diff 형태로 변경점만 보여줄 것
- 불확실한 부분은 추측하지 말고 먼저 물어볼 것

## Git

- Conventional Commits: feat:, fix:, refactor:, docs:, chore:
- 커밋은 작은 논리 단위로 분리
- main 직접 push 금지
- IMPORTANT: 확인 없이 git push, git reset --hard, force push 하지 말 것

## Coding

- 타입 안전성 우선 — any, unknown 남용 금지
- 매직 넘버 대신 이름 있는 상수
- 에러 핸들링 생략 금지 — 실패 경로를 항상 고려
- 기존 코드 스타일과 패턴을 따를 것 — 새 패턴 도입 전에 확인

## Workflow

- 변경 전 기존 테스트 먼저 실행해서 현재 상태 확인
- 전체 테스트 스위트 대신 관련 파일만 실행
- 큰 변경은 작은 단계로 나눠서 각 단계가 동작하는 상태를 유지
- 동작하는 코드를 확인 없이 리팩토링하지 말 것
- IMPORTANT: 파일 삭제, 대규모 rename은 반드시 사전 확인

## Local Configuration

@~/.claude/CLAUDE.local.md 

## Gotchas
