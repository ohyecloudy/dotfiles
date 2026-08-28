# doom.d

## config.org

- 새 설정·함수는 heading으로 구분 — topic 단위는 `*`, 세부 항목(패키지·함수)은 `**`
  - 한 heading = 하나의 관심사
- 코드는 `#+begin_src elisp` 블록, 비활성/미적용 블록은 `:tangle no`
- 각 블록 근처에 한국어 산문으로 "왜/무엇" 설명 첨부
- 커스텀 함수·명령은 `my/` prefix 사용
- 재사용 elisp 패키지는 `lisp/`에 두고 `* my packages` 섹션에서 `(require 'my-...)`

## lisp/ 테스트

- `lisp/`에 elisp 코드(함수·패키지)를 추가·수정하면 ERT 테스트를 같이 작성 — 테스트 없는 로직 변경 금지
- 테스트 파일은 같은 디렉토리에 `<패키지>-test.el`, 테스트 이름은 `my/<패키지>-test/...` prefix
- 파일 상단 `;; -*- lexical-binding: t; -*-` + `(require 'ert)` + 대상 `(require 'my-...)`
- 실행(관련 파일만): `emacs -Q --batch -L lisp -l lisp/<패키지>-test.el -f ert-run-tests-batch-and-exit`
- 변경 전 기존 테스트 먼저 실행해 통과 확인, 변경 후 다시 실행
