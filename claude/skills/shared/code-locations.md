# 코드 위치 표기 (정본)

`code-tour`, `walkthrough`가 공유하는 코드 위치 규칙. 사용자는 문서의 위치를 emacs evil `gF`(`evil-find-file-at-point-with-line`)나 compilation-mode로 바로 연다. 둘 다 `/abs/path:line` 형식만 라인까지 점프하므로, 형식이 하나라도 어긋나면 그 위치는 손으로 찾아야 한다.

## 규칙

- **코드 위치는 어디에 쓰든 `/abs/path/foo.ts:42`**(절대 경로 + 단일 라인). 디렉터리는 `/abs/path/dir/`.
- **같은 파일이 반복돼도 매번 전체 경로.** 앞 항목과 같은 파일이라고 라인만 남기지 않는다.
- **절대 경로는 예외 없이 verbatim으로 감싸지 않는다.** `=...=` 안에서는 점프가 막힌다. 심볼은 경로 뒤에 따로 `=심볼=`.
- **라인 없는 파일 경로 금지.** 호출 스킬이 명시한 예외만 허용(예: code-tour 반사실 질문 단서).
- 산문에 인라인해도 된다 - 형식만 맞으면 문장 중간에서도 점프된다.

## 금지 예시

| 금지                        | 이유                         | 올바른 형태                               |
|-----------------------------+------------------------------+-------------------------------------------|
| `=:86= =receive=`           | 라인만 축약 - 파일을 모름    | `/abs/path/Recorder.swift:86 =receive=`   |
| `Foo.swift:127`             | 파일명만 - 경로를 모름       | `/abs/path/Foo.swift:127`                 |
| `=/abs/path/Foo.swift:42=`  | verbatim - 점프가 막힘       | `/abs/path/Foo.swift:42`                  |
| `=/abs/path/dir/=`          | verbatim - 점프가 막힘       | `/abs/path/dir/`                          |
| `/abs/path/Foo.swift:42-50` | 라인 범위 - 점프 대상이 아님 | `/abs/path/Foo.swift:42`                  |
| `/abs/path/Foo.swift`       | 라인 없음(예외 아니면)       | `/abs/path/Foo.swift:12` (주 선언 라인)   |

## 같은 파일의 여러 위치

전체 경로 반복이 길다고 한 줄에 몰아 축약하지 않는다. 하위 bullet로 펼친다 - 한 bullet = 한 위치 = 물리적으로 한 줄.

```org
   3. /abs/path/Recorder.swift:17 =Recorder= - 상태 필드 주석부터 전부 읽기
      - /abs/path/Recorder.swift:86 =receive=
      - /abs/path/Recorder.swift:70 =handle= - 원격 실행
```

## 검사

쓴 뒤 아래를 실행한다. 출력이 있으면 위반 - 고치고 출력이 없을 때까지 다시 실행한다.

```bash
grep -nE '=:[0-9]+=|=/[^=[:space:]]*/[^=]*=|(^|[^/[:alnum:]_.-])[[:alnum:]_.-]+\.[[:alpha:]]+:[0-9]+' <파일들>
```

- 차례로 라인만 축약, verbatim 경로, 파일명만을 잡는다.
- verbatim 경로 패턴은 슬래시 2개 이상을 요구한다 - `=/code-tour=` 같은 slash command 오탐 회피.
- 라인 없는 경로와 라인 범위는 잡지 않는다(예외 단서·디렉터리와 구분이 어려워 오탐이 많다). 위 규칙으로 직접 지킨다.
