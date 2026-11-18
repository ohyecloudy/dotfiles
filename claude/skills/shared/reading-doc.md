# 코드 따라 읽기 문서 (정본)

`code-tour`, `walkthrough`가 공유하는 문서 작성 규칙. 두 스킬 모두 사람이 코드를 따라 읽으며 이해하는 org 문서를 만들고, 같은 `explain-change` 결과를 같은 모양의 섹션으로 옮긴다. 규칙이 스킬마다 복사돼 있으면 한 곳만 고쳐져 갈라지므로 여기 하나만 둔다. 스킬 고유 규칙(질문 종류, 힌트 개수, 답 골격, `:file` 경로 앞부분 등)은 각 스킬에 남긴다.

## 코드 발췌

문서는 기준 커밋 시점에 쓰이고 읽기는 나중이다 - 그사이 링크 위치 코드가 바뀌거나 없어질 수 있다. 링크만 있으면 문서가 의미를 잃으므로, 링크 없이 문서만으로 읽을 수 있게 발췌한다.

- **위치 링크가 있는 bullet 바로 아래에 그 위치의 코드 블록.** 배치는 [org-writing.md](org-writing.md)의 "코드 블록" 규칙.
- **발췌한 코드가 정본, 링크는 지금 파일로 가는 길.** 발췌는 문서의 기준 커밋 시점 코드에서 뽑는다(어느 커밋이 기준인지는 스킬이 정한다). 기준 커밋이 working tree와 다르면 `git show <sha>:<path>`로 읽는다.
- **예외 (발췌 없음)**
  - 정답을 먼저 보여 주면 안 되는 위치 - 힌트, 다시 볼 곳 안내 등. 어느 자리인지는 스킬이 정한다. 이 위치는 "링크 + `=심볼=`"로 써서 코드가 바뀌어도 심볼로 찾을 수 있게 한다.
  - 디렉터리 링크, 외부 문서(ADR, spec, CONTEXT) 링크.
- **최소 라인만.** hunk, 함수 통째 복붙 금지. 설명의 근거가 되는 줄만, 원본 들여쓰기 유지. 큰 범위를 가리키는 위치는 대표 라인만 싣고 "전체는 링크로" 한 줄을 붙인다.
- **분량 상한** - 블록당
  - 답, 근거, 선택 코드: ~10줄
  - 설계 포인트, 언어/프레임워크 포인트처럼 결정이나 문법 하나를 보여 주는 곳: ~5줄
  - 호출 흐름의 한 단계(시나리오 화살표 하나 등): ~3줄 - 호출 라인 하나가 근거
  - 논리 단위(워크스루 단위, `바뀐 곳` 항목) 합계: 20줄
- **기존 코드를 고친 곳은 diff로 전/후.** `#+begin_src diff`(`-`/`+` + 원본 들여쓰기). 순수 신규면 `#+begin_src <lang>`. diff도 최소 대비 라인만 - git 원본 hunk를 그대로 붙이지 않는다. 전/후 합산으로 분량 상한을 센다.
- 언어가 불명확하면 `#+begin_example`.
- 블록 본문 들여쓰기와 escape는 [org-formatting.md](org-formatting.md)의 "블록 본문".

## 그림 섹션

`explain-change`의 `diagrams`를 org mermaid 블록으로 싣는 규칙. 차트 선택과 표기는 [diagrams.md](diagrams.md).

- **순서**: `tree` -> `overview` -> `sequence`(시점 순) -> `state`/`er`/`logic`/`class`. 넓은 것(어디에) -> 시점 간 연결 -> 시점 안 순서 -> 세부.
- **각 장** = heading `<종류 한글명> - <title>`(종류 한글명 = 위치 / 개요 / 시퀀스 / 상태 / 데이터 / 분기 / 계약) + caption 한 줄 + 블록. heading 하나에 블록 하나.
- **전/후 대비**(`sequence`, `state`, `er`): 그 항목에 `before`가 있으면 하위 heading `전` / `후` 둘로 나눠 싣는다.
  - `before`가 소스: `전`에 `before`, `후`에 `mermaid`. `:file`에 `-before` / `-after`를 붙인다.
  - `before: ""`(전에 없던 대상): `전` 본문에 "없음 - 신규 흐름" 한 줄, 블록 없음.
  - `before` 없음(전/후 동일): 하위 heading 없이 한 장.
  - 차이 표시(sequence `rect`, state/er `changed`)는 코어가 넣은 그대로 싣는다.
- mermaid 소스는 편집, 재작성 금지.
- `:file`은 스킬 출력 디렉터리 아래 `<slug>-<위치>.png` - 장마다 고유하게. 같은 kind `sequence`가 여러 장이면 시점 slug를 붙인다. 크기 옵션은 블록에 쓰지 않는다(ob-mermaid 기본값).
- 렌더는 온디맨드 - 사용자가 `C-c C-c`/export로 PNG를 만든다(pre-render하지 않는다).

```org
** 시퀀스 - <title>

   - <caption 한 줄>

*** 전

    #+begin_src mermaid :file ~/<출력 디렉터리>/<slug>-<위치>-before.png
      <before 소스>
    #+end_src

*** 후

    #+begin_src mermaid :file ~/<출력 디렉터리>/<slug>-<위치>-after.png
      <mermaid 소스>
    #+end_src
```

## 확인 질문 공통 틀

중요한 부분을 요약 읽기로 끝내지 않고 코드로 확인하게 하는 질문. 질문 종류, 힌트 개수, 답 골격은 스킬별.

- **질문 = `TODO` heading 하나.** 스킬은 `TODO`로만 만든다. `DONE` 전환은 사용자 몫.
- **힌트, 답은 질문 아래 서브heading**(상태 키워드 없음). 서브heading이라 문서를 열면 접혀 있다 - 힌트부터 순서대로 펼친다.
- **heading 하나에 물음 하나, 한 문장.** "A는 무엇이고, B는 어디이며, 왜 C인가"처럼 묶으면 heading이 문단이 된다 - 나눠서 별도 질문으로.
- **확정 가능한 것만.** 출제 시점에 정답을 스스로 확정한다 - 확정 못 하면 그 질문은 버린다. 의견, 취향을 묻지 않는다.
- **힌트에 정답 문장 금지.** 어디를 볼지, 무엇을 찾을지(심볼, 조건)만.
- **답에는 근거 코드 발췌**(§코드 발췌). 위치를 말하는 항목마다 그 항목 아래 링크 + 블록 - 끝에 몰아 붙이면 어느 코드가 어느 근거인지 안 보인다.
- **진행 표시**: 접힌 상태에서 남은 질문이 보이게 통계 쿠키를 단다.
  - 질문 묶음 heading 끝에 `[/]` - 직속 `TODO` 질문을 센다.
  - 묶음이 여러 층이면 최상위 heading에 `[/]` + `:COOKIE_DATA: todo recursive` drawer - 상태 키워드 없는 중간 heading을 건너 질문 전체를 센다.
  - 스킬은 빈 `[/]`만 쓴다. 개수는 [org-formatting.md](org-formatting.md) 정렬이 채우고, 이후엔 emacs가 상태 전환마다 갱신한다.
  - heading 링크 매칭에서 org는 쿠키를 무시한다 - 링크 제목에 쿠키를 넣지 않는다.

## explain-change 결과 매핑

- **경로**: 코어 결과는 맨 경로 - 문서에 옮길 때 org 링크로 바꾼다([code-locations.md](code-locations.md)).
- **`reading_order[].code`**(`evidence: full`): 항목 하나 = heading 하나 = 코드 블록 하나.
  - `diff` -> `#+begin_src diff`, `new` -> `#+begin_src <code.lang>`(org babel 언어명으로). `code.text`를 그대로 넣는다.
  - `deleted` -> heading `삭제: =<symbol>=`, 링크 bullet 대신 "삭제 전: <파일 경로>" 평문 한 줄(지금 파일에 없어 링크가 열리지 않는다), 블록은 `diff`.
  - `code`가 없는 항목(기계적 반복의 나머지)은 링크 bullet만.
- **`units[].evidence`**: `before`가 있으면 diff, 없으면 `<lang>` 블록(§코드 발췌).
- **`diagrams`**: §그림 섹션.
- **`prerequisites`**: reason별 필드. 모든 항목에 `등장 위치` 링크 + 발췌(~5줄).
  - `no-precedent`: `무엇` / 스킬이 아는 만큼의 `왜 여기 필요했나`, `대안` / `주의점`
  - `reader-unfamiliar`: `무엇` / `독자가 아는 것으로는`(주력 언어, 버전에서 같은 일을 하던 방식 | 해당 없음) / `주의점`
- **`omitted`**: `다루지 않은 변경` 섹션 - 생략분과 이유를 한 줄씩. 선별 요약은 "이게 전부인가?"에 답하지 못하면 신뢰받지 못한다.
- **`units[].architecture` + `impact`**: §아키텍처 델타.

## 아키텍처 델타

**요약이지 나열이 아니다.** 읽는 사람이 3초 안에 구조 변화를 잡는 게 목적 - `units[].architecture`를 그대로 펼치면 실패한다.

- **요지 1줄 + 델타 5줄 상한.** 넘으면 뺀다. 세부는 코드 블록과 함께 본문에 나오므로 잃는 정보가 없다.
- **`요지`는 델타 전체를 한 줄로.** 나머지를 안 읽어도 구조 변화를 잡을 수 있게 쓴다.
- **모든 줄이 `<축>: <전> -> <후>`.** 신설, 삭제도 `없음 -> =AssetValidator=` / `=LegacyParser= -> 없음`. 전후 대비가 안 잡히면 델타가 아니므로 본문으로 내린다.
- **축당 1줄.** 한 축에 여러 건이면 대표 하나만 쓰고 `외 N곳`으로 묶는다. 예: =WatchWorkoutDisplay= -> =WatchWorkoutSelection= , 시그니처 4곳 전파.
- **축은 이번 변경에서 실제 움직인 것만.** 모듈, 인터페이스, 의존성, 소유권, 흐름은 흔한 예일 뿐 고정 목록이 아니다. 안 움직인 축은 줄째로 뺀다. 축이 전부 비면 "없음" 한 줄.
- **경로는 다음 줄에 단독으로.** 축당 대표 위치 1개만. 줄 끝에 붙이면 줄이 길어져 정작 스캔할 앞부분이 밀린다. 델타 줄은 요약이라 발췌 대상이 아니다 - 근거 코드는 본문에 있다.
- **ADR은 하위 섹션으로 분리한다.** 델타 줄과 섞으면 `전 -> 후` 스캔이 끊긴다.

```org
* 아키텍처 델타

  - 요지: <이 변경의 구조적 정체 한 줄>

  - <축>: <전> -> <후>
    [[file:/abs/path/foo.ts::40][foo.ts:40]]
```

## org 표기

- **코드 위치는 [code-locations.md](code-locations.md)의 org 규칙** - 어디에 쓰든 `[[file:/abs/path/foo.ts::42][foo.ts:42]]`, 같은 파일 반복도 전체 target, 링크는 verbatim 금지. 채팅 보고의 경로는 맨 경로.
- **본문은 [org-writing.md](org-writing.md)** - 짧은 문장 plain list, 깊이별 기호 교대, 출처 위치, 코드 블록 배치, 공통 표기.
- **식별자는 verbatim `=...=`로 감싼다**(예: =FLHitWindowRunner=). 안에 `=` 문자를 넣지 않는다(구문이 깨진다) - 대입식은 평문으로.
- **빈 섹션은 "없음" 한 줄.** 통째로 지우면 "해당 없음"과 "확인 안 함"이 구분되지 않는다. 섹션째 생략해도 되는 예외는 스킬이 정한다.
- **문서 안 참조는 순번 금지**("3번째 항목" X). `[[*<heading 제목>][<라벨>]]` 링크를 건다. 제목은 TODO 키워드, 태그, 쿠키를 뺀 heading 텍스트와 완전 일치.

## 포매팅

생성, 수정한 .org 파일에 순서대로 실행한다.

1. [code-locations.md](code-locations.md)의 검사 - 출력이 없을 때까지 위반을 고친다. 원문 보존 블록(스킬이 지정)의 출력은 위반이 아니다.
2. heading 링크 검사 - 출력이 있으면 대상 없는 링크. 제목을 맞추고 다시 실행한다.

   ```bash
   f=<생성한 .org 파일>
   awk 'FNR==NR { if (match($0, /^\*+ /)) { h = substr($0, RLENGTH + 1); sub(/^(TODO|KEEP|REVERT|DONE) /, "", h); gsub(/ *\[[0-9]*(%|\/[0-9]*)\]/, "", h); sub(/[ \t]+(:[^ \t]+:)?[ \t]*$/, "", h); H[h] = 1 } next } { s = $0; while (match(s, /\[\[\*[^]]+\]/)) { t = substr(s, RSTART + 3, RLENGTH - 4); if (!(t in H)) print FILENAME ":" FNR ": " t; s = substr(s, RSTART + RLENGTH) } }' "$f" "$f"
   ```

3. [org-formatting.md](org-formatting.md)의 정렬.
