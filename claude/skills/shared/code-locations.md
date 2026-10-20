# 코드 위치 표기 (정본)

`code-tour`, `walkthrough`, `improve-codebase-architecture`가 공유하는 코드 위치 규칙. 출력 매체에 따라 형식이 갈린다.

- **org 문서 안**: org 링크. 사용자는 `]l`/`[l`(`org-next-link`/`org-previous-link`)로 위치 사이를 이동하고 `RET`으로 연다(`C-c C-RET` 다른 창, `C-o` 복귀). org 링크가 아닌 위치는 `]l`이 건너뛰므로, 하나라도 맨 경로면 탐색 흐름이 끊긴다.
- **그 밖(채팅, `explain-change` JSON 등)**: 맨 경로 `/abs/path/foo.ts:42`. emacs evil `gF`나 compilation-mode로 연다. org 문서를 쓰는 스킬이 이 경로를 받아 링크로 바꾼다.

## org 문서 안

- **코드 위치는 `[[file:/abs/path/foo.ts::42][foo.ts:42]]`**. target은 절대 경로 + `::` + 단일 라인, description은 `파일명:라인`. 심볼은 링크 뒤에 따로 `=심볼=`.
- **디렉터리는 `[[file:/abs/path/dir/][dir/]]`**. dired로 열린다.
- **외부 문서(ADR·spec·CONTEXT) 인용도 라인 링크.** heading 이름은 링크 뒤 평문 `"<heading>"`. `::*heading` 검색 링크는 쓰지 않는다 - heading이 바뀌면 링크가 깨진다.
- **target 경로 형식**
  - Windows는 드라이브 형식 `D:/...`. MSYS 형식 `/d/...`은 org가 열지 못한다.
  - target에 `[`, `]`가 있으면 `\`로 escape.
- **같은 파일이 반복돼도 매번 전체 target.** description만 짧다.
- **링크를 verbatim으로 감싸지 않는다.** `=...=` 안에서는 링크가 아니게 된다.
- **라인 없는 파일 링크 금지.** 호출 스킬이 명시한 예외만 허용(예: code-tour 반사실 질문 단서 - `[[file:/abs/path/FooTests.swift][FooTests.swift]]`).
- 산문에 인라인해도 된다.
- 링크가 아닌 것: babel 헤더 인자(mermaid `:file` 등).

### 금지 예시

| 금지                                         | 이유                         | 올바른 형태                                         |
|----------------------------------------------+------------------------------+-----------------------------------------------------|
| `/abs/path/Foo.swift:42`                     | 맨 경로 - `]l`이 건너뜀      | `[[file:/abs/path/Foo.swift::42][Foo.swift:42]]`    |
| `Foo.swift:127`                              | 파일명만 - 경로를 모름       | `[[file:/abs/path/Foo.swift::127][Foo.swift:127]]`  |
| `=:86= =receive=`                            | 라인만 축약 - 파일을 모름    | `[[file:/abs/path/Recorder.swift::86][Recorder.swift:86]] =receive=` |
| `[[file:/abs/path/Foo.swift:42]]`            | 단일 콜론 - 파일명 일부가 됨 | `[[file:/abs/path/Foo.swift::42][Foo.swift:42]]`    |
| `[[file:/d/work/Foo.swift::42]]`             | MSYS 경로 - 열리지 않음      | `[[file:D:/work/Foo.swift::42][Foo.swift:42]]`      |
| `[[file:src/Foo.swift::42]]`                 | 상대 경로 - 문서 위치 기준   | `[[file:/abs/path/src/Foo.swift::42][Foo.swift:42]]` |
| `=[[file:/abs/path/Foo.swift::42]]=`         | verbatim - 링크가 아님       | `[[file:/abs/path/Foo.swift::42][Foo.swift:42]]`    |
| `[[file:/abs/path/Foo.swift::42-50]]`        | 라인 범위 - 점프 대상이 아님 | `[[file:/abs/path/Foo.swift::42][Foo.swift:42]]`    |
| `[[file:/abs/path/Foo.swift]]`               | 라인 없음(예외 아니면)       | `[[file:/abs/path/Foo.swift::12][Foo.swift:12]]` (주 선언 라인) |

### 같은 파일의 여러 위치

한 줄에 몰아 축약하지 않는다. 하위 bullet로 펼친다 - 한 bullet = 한 위치 = 물리적으로 한 줄.

```org
   3. [[file:/abs/path/Recorder.swift::17][Recorder.swift:17]] =Recorder= - 상태 필드 주석부터 전부 읽기
      - [[file:/abs/path/Recorder.swift::86][Recorder.swift:86]] =receive=
      - [[file:/abs/path/Recorder.swift::70][Recorder.swift:70]] =handle= - 원격 실행
```

### 검사

쓴 뒤 아래를 실행한다. 출력이 있으면 위반 - 고치고 출력이 없을 때까지 다시 실행한다.

```bash
for f in <파일들>; do
  sed -E 's/\[\[[^]]*\](\[[^]]*\])?\]//g' "$f" | grep -nE '=:[0-9]+=|=([A-Za-z]:)?/[^=[:space:]]*/[^=]*=|(^|[^[:alnum:]_.-])([A-Za-z]:|~)?/[^[:space:]]*:[0-9]+|(^|[^/[:alnum:]_.-])[[:alnum:]_.-]+\.[[:alpha:]]+:[0-9]+' | sed "s|^|$f:|"
  grep -nE '=\[\[|\[\[file:[^]]*[^:]:[0-9]+\]|\[\[file:[^]]*::[0-9]+-|\[\[file:/[A-Za-z]/|\[\[file:([^/A-Za-z~]|[A-Za-z][^:])' "$f" | sed "s|^|$f:|"
done
```

- 첫 줄: 링크를 지운 뒤 남은 위치 - 라인만 축약, verbatim 경로, 맨 경로, 파일명만. `sed`는 라인 단위라 라인 번호가 보존된다.
- 둘째 줄: verbatim 링크, 링크 target - 단일 콜론 라인, 라인 범위, MSYS 경로, 상대 경로.
- verbatim 경로 패턴은 슬래시 2개 이상을 요구한다 - `=/code-tour=` 같은 slash command 오탐 회피.
- 라인 없는 링크는 잡지 않는다(예외 단서·디렉터리와 구분이 어렵다). 위 규칙으로 직접 지킨다.

## 그 밖(채팅, JSON)

- **`/abs/path/foo.ts:42`**(절대 경로 + 단일 라인). 디렉터리는 `/abs/path/dir/`.
- 같은 파일이 반복돼도 매번 전체 경로. 라인 범위·파일명만·상대 경로 금지.
