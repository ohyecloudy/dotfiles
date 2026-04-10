---
name: commit-message
description: 커밋 메시지 생성. "커밋 메시지 작성해줘", "commit message 생성", "커밋 메시지 만들어줘" 등의 요청에 트리거.
---

# commit-message skill

## 동작 절차

1. `git diff --cached` 실행 → staged 변경 사항 확인
2. staged가 없으면 `git diff` 실행 → unstaged 변경 사항 확인
3. 변경 내용 분석 후 적절한 type, scope 결정
4. Angular 컨벤션에 맞는 커밋 메시지 생성
5. 생성된 메시지 출력 (코드 블록으로 감싸서 복사하기 쉽게)

## Commit Convention 형식

```
<type>(<scope>): <subject>

<body>

<footer>
```

## Type 목록

| type | 용도 |
|------|------|
| feat | 새 기능 추가 |
| fix | 버그 수정 |
| docs | 문서만 변경 |
| style | 포매팅, 세미콜론 누락 등 (로직 변경 없음) |
| refactor | 버그 수정도 기능 추가도 아닌 코드 변경 |
| test | 테스트 추가 또는 수정 |
| chore | 빌드 프로세스, 보조 도구 변경 (유지보수) |

## Scope 결정 방법

- 변경이 발생한 모듈, 컴포넌트, 파일명 등을 사용
- 예: `auth`, `user`, `api`, `config`, `deps`
- 적합한 scope가 없거나 범위가 너무 넓으면 생략

## Subject 규칙

- **영어**로 작성
- 명령형, 현재 시제 사용 ("change" not "changed" nor "changes")
- 첫 글자 소문자
- 끝에 마침표(.) 없음
- 50자 이내 권장

## Body 규칙

- **한글**로 작성
- 명령형, 현재 시제
- 변경 이유 및 이전 동작과의 차이 설명
- 72자 줄바꿈 권장
- body가 자명한 경우 생략 가능

## 출력 형식

생성된 커밋 메시지를 아래 형식으로 출력:

```
feat(scope): add new feature

변경 이유와 핵심 내용을 한글로 설명.
이전 동작과의 차이가 있으면 여기에 기술.
```

변경 사항이 여러 논리 단위로 나뉘는 경우, 각각의 커밋 메시지를 별도로 제안하고 분리 커밋을 권장.
