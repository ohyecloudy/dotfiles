# doom.d

## config.org

- 새 설정·함수는 heading으로 구분 — topic 단위는 `*`, 세부 항목(패키지·함수)은 `**`
  - 한 heading = 하나의 관심사
- 코드는 `#+begin_src elisp` 블록, 비활성/미적용 블록은 `:tangle no`
- 각 블록 근처에 한국어 산문으로 "왜/무엇" 설명 첨부
- 커스텀 함수·명령은 `my/` prefix 사용
- 재사용 elisp 패키지는 `lisp/`에 두고 `* my packages` 섹션에서 `(require 'my-...)`
