# org 정렬 (정본)

org 문서를 쓰는 스킬(`code-tour`, `walkthrough`, `warm-start`, `to-spec`, `to-tickets`, `domain-modeling`, `improve-codebase-architecture`)이 공유하는 마무리 단계. 생성·수정한 .org를 emacs에서 열었다 저장한 것과 같은 상태로 만든다 - 사용자가 emacs로 열어 저장할 때 diff가 생기지 않게 하려고 둔다. 명령이 스킬마다 복사돼 있으면 한 곳만 고쳐져 갈라지므로 여기 하나만 둔다.

## 정렬

생성·수정한 .org 파일을 넘겨 실행한다. 여러 파일을 한 번에 넘길 수 있고, 상대 경로는 현재 디렉터리 기준, 절대 경로(외부 모드의 `~/project-docs/...` 등)도 그대로 된다.

```bash
emacs -Q --batch --eval '(progn (require (quote org)) (setq make-backup-files nil) (setq org-adapt-indentation t) (setq org-src-tab-acts-natively nil) (let ((dir default-directory)) (dolist (f command-line-args-left) (find-file (expand-file-name f dir)) (org-mode) (org-indent-region (point-min) (point-max)) (set-buffer-file-coding-system (if (memq system-type (quote (windows-nt ms-dos))) (quote utf-8-dos) (quote utf-8-unix))) (save-buffer))))' <생성·수정한 .org 파일들>
```

- `org-adapt-indentation t`: 헤딩 아래 본문을 별 개수 + 1칸으로 들여쓴다.
- `org-src-tab-acts-natively nil`: src 블록 내용을 건드리지 않는다. 켜져 있으면 블록마다 언어 모드로 재들여쓰기해 diff 문맥 줄의 접두 공백이 사라지고 원본 코드 들여쓰기가 바뀐다. `#+begin`/`#+end` 줄만 정렬된다.
- line ending을 실행 OS에 맞춘다(Windows CRLF `utf-8-dos`, 그 외 LF `utf-8-unix`). Bash heredoc 등 앞 단계가 LF로 썼어도 여기서 맞춰진다.
- `make-backup-files nil`: `~` 백업 파일을 남기지 않는다.
- emacs가 없거나 실패하면 파일은 그대로 두고 사용자에게 그 사실만 알린다.

## 블록 본문

정렬이 블록 내용을 건드리지 않으므로 쓴 그대로 남는다. 그래서 쓸 때 맞춰 둔다.

- 블록 본문은 `#+begin` 줄과 같은 들여쓰기에서 시작하고, 원본의 상대 들여쓰기·diff 접두(`-`/`+`/공백)는 그대로 둔다.
- 첫 비공백 문자가 `*` 또는 `#+`인 줄은 그 앞에 `,`를 넣는다(org 표준 escape - 앞 공백은 유지되고 `C-c '`·export에서 제거된다). 안 하면 ` #+end_src` 같은 줄이 블록을 일찍 닫는다.
