# org 정렬 (정본)

org 문서를 쓰는 스킬(`code-tour`, `walkthrough`, `warm-start`, `to-spec`, `to-tickets`, `domain-modeling`, `improve-codebase-architecture`, `poc`)이 공유하는 마무리 단계. 생성·수정한 .org를 emacs에서 열었다 저장한 것과 같은 상태로 만든다 - 사용자가 emacs로 열어 저장할 때 diff가 생기지 않게 하려고 둔다. 명령이 스킬마다 복사돼 있으면 한 곳만 고쳐져 갈라지므로 여기 하나만 둔다.

## 정렬

생성·수정한 .org 파일을 넘겨 실행한다. 여러 파일을 한 번에 넘길 수 있고, 상대 경로는 현재 디렉터리 기준, 절대 경로(외부 모드의 `~/project-docs/...` 등)도 그대로 된다.

Windows (PowerShell):

```powershell
emacs -Q --batch -l "$env:USERPROFILE\.claude\skills\shared\org-format.el" <생성·수정한 .org 파일들>
```

macOS / Linux:

```bash
emacs -Q --batch -l ~/.claude/skills/shared/org-format.el <생성·수정한 .org 파일들>
```

[org-format.el](org-format.el)이 하는 일:

- `org-adapt-indentation t`: 헤딩 아래 본문을 별 개수 + 1칸으로 들여쓴다.
- `org-src-tab-acts-natively nil`: src 블록 내용을 언어 모드로 재들여쓰기하지 않는다. 켜져 있으면 diff 문맥 줄의 접두 공백이 사라지고 원본 코드 들여쓰기가 바뀐다.
- src/example 블록 본문을 `#+begin` 줄 + 2칸으로 옮긴다(`org-edit-src-content-indentation` 기본값 - `C-c '` 편집 후 저장해도 diff가 안 생긴다).
  - 가장 덜 들여쓴 줄을 기준으로 통째 이동해 상대 들여쓰기·diff 접두(`-`/`+`/공백)는 그대로다.
- src/example 블록 앞뒤에 빈 줄 하나를 넣는다.
  - 앞: 블록 위 `#+name:`/`#+caption:` 등 affiliated keyword가 있으면 그 위에.
  - 뒤: `#+RESULTS`가 붙어 있거나 파일 끝이면 넣지 않는다.
- line ending을 실행 OS에 맞춘다(Windows CRLF `utf-8-dos`, 그 외 LF `utf-8-unix`). Bash heredoc 등 앞 단계가 LF로 썼어도 여기서 맞춰진다.
- `make-backup-files nil`: `~` 백업 파일을 남기지 않는다.
- 여러 번 실행해도 결과가 같다.
- emacs가 없거나 실패하면 파일은 그대로 두고 사용자에게 그 사실만 알린다.

## 블록 본문

정렬은 블록 본문을 통째 옮길 뿐 줄 내용은 고치지 않는다. 그래서 쓸 때 맞춰 둔다.

- 블록 본문은 `#+begin` 줄 + 2칸에서 시작하고, 원본의 상대 들여쓰기·diff 접두(`-`/`+`/공백)는 그대로 둔다. 기준 열이 어긋나도 정렬이 맞추지만 상대 들여쓰기는 복원하지 못한다.
- 첫 비공백 문자가 `*` 또는 `#+`인 줄은 그 앞에 `,`를 넣는다(org 표준 escape - 앞 공백은 유지되고 `C-c '`·export에서 제거된다). 안 하면 ` #+end_src` 같은 줄이 블록을 일찍 닫는다.
