---
name: blog-feedback
description: 블로그 3곳(lifelog, pnotes, emacsian)에서 랜덤 글을 뽑아 글마다 원문 인용 + 코멘트 형식의 짧은 피드백을 텔레그램으로 전송. 좋은 점, 고칠 점, 표현 제안, 건조한 유머, 확장 주제 제안. macOS·Windows 공용.
argument-hint: "[블로그당 개수 | URL...]"
disable-model-invocation: true
---

# blog-feedback

ohyecloudy.com 블로그 글에 대한 일회성 피드백. 수시로 실행해서 묵은 글을 하나씩 퇴고하는 용도. 피드백은 글마다 텔레그램 메시지 하나로 받는다. 보낸 글의 URL은 `~/blog-feedback/reviewed.txt`에 쌓여 다음 랜덤 추출에서 빠진다(기기별 기록).

macOS·Windows 공용. 1단계의 `python3`는 Windows에서 `python`으로 바꿔 실행한다(있는 쪽을 쓴다). 3단계는 `uv`가 필요하다. Bash 도구 기준.

## 대상

- lifelog: `https://ohyecloudy.com/lifelog/sitemap.xml` 중 `/lifelog/archives/` 이하만
- pnotes: `https://ohyecloudy.com/pnotes/sitemap.xml` 중 `/pnotes/archives/` 이하만
- emacsian: `https://ohyecloudy.com/emacsian/sitemap.xml` 중 날짜 경로(`/YYYY/MM/DD/`) 글 전체

## 인자

- 없음 → 블로그마다 1개씩, 총 3개
- 정수 N → 블로그마다 N개씩
- URL 1개 이상 → 그 글만 (이미 리뷰한 글이어도 다시 리뷰)

## 절차

### 1. 글 수집

아래 명령을 Bash로 그대로 실행하고 stdout을 `~/blog-feedback/posts.json`에 저장한다. `$ARGUMENTS`가 비어 있으면 인자 없이 실행. **WebFetch 금지** - 작은 모델이 요약해서 돌려주므로 문장 단위 피드백이 불가능하다. 반드시 이 명령으로 원문을 받는다.

```bash
mkdir -p ~/blog-feedback && python3 - $ARGUMENTS > ~/blog-feedback/posts.json <<'PY'
# Picks posts (random per blog, or given URLs) and prints their raw text as JSON.
import html, json, random, re, sys, urllib.error, urllib.request
from html.parser import HTMLParser
from pathlib import Path

BLOGS = {
    "lifelog": ("https://ohyecloudy.com/lifelog/sitemap.xml", "/lifelog/archives/"),
    "pnotes": ("https://ohyecloudy.com/pnotes/sitemap.xml", "/pnotes/archives/"),
    "emacsian": ("https://ohyecloudy.com/emacsian/sitemap.xml", r"/emacsian/\d{4}/\d{2}/\d{2}/"),
}
REVIEWED_FILE = Path.home() / "blog-feedback" / "reviewed.txt"
DEFAULT_PER_BLOG = 1
MAX_FAILURES_PER_BLOG = 3
HTTP_TIMEOUT_SEC = 15
BLOCK = {"p", "div", "li", "br", "tr", "blockquote", "table", "ul", "ol", "dd", "dt"}
HEADING = {"h1", "h2", "h3", "h4", "h5", "h6"}
SKIP = {"script", "style", "nav", "aside"}
FETCH_ERRORS = (urllib.error.URLError, TimeoutError, ConnectionError)

sys.stdout.reconfigure(encoding="utf-8"); sys.stderr.reconfigure(encoding="utf-8")

def log(m): print(m, file=sys.stderr)

def fetch(url):
    req = urllib.request.Request(url, headers={"User-Agent": "blog-feedback/1.0"})
    with urllib.request.urlopen(req, timeout=HTTP_TIMEOUT_SEC) as r:
        return r.read().decode("utf-8", errors="replace")

class Content(HTMLParser):
    """Text inside <section class="page__content">, keeping headings and code blocks."""
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.parts, self.depth, self.skip, self.pre, self.found = [], 0, 0, False, False
    def handle_starttag(self, tag, attrs):
        if self.depth == 0:
            if tag == "section" and "page__content" in (dict(attrs).get("class") or "").split():
                self.depth, self.found = 1, True
            return
        if tag == "section": self.depth += 1
        if tag in SKIP: self.skip += 1
        elif tag == "pre": self.pre = True; self.parts.append("\n```\n")
        elif tag in HEADING: self.parts.append("\n\n" + "#" * int(tag[1]) + " ")
        elif tag in BLOCK: self.parts.append("\n")
    def handle_endtag(self, tag):
        if self.depth == 0: return
        if tag == "section": self.depth -= 1; return
        if tag in SKIP: self.skip = max(0, self.skip - 1)
        elif tag == "pre": self.pre = False; self.parts.append("\n```\n")
        elif tag in HEADING or tag in BLOCK: self.parts.append("\n")
    def handle_data(self, d):
        if self.depth and not self.skip:
            self.parts.append(d if self.pre else re.sub(r"\s+", " ", d))
    def text(self):
        t = re.sub(r"[ \t]+\n", "\n", "".join(self.parts))
        return re.sub(r"\n{3,}", "\n\n", t).strip()

def meta(page, prop):
    m = re.search(rf'<meta property="{re.escape(prop)}" content="([^"]*)"', page)
    return html.unescape(m.group(1)) if m else None

def load(url):
    try:
        page = fetch(url)
    except FETCH_ERRORS as e:
        log(f"warn: fetch failed {url}: {e}"); return None
    c = Content(); c.feed(page); body = c.text()
    if not c.found or not body:
        log(f"warn: no content section {url}"); return None
    slug = url.rstrip("/").rsplit("/", 1)[-1]
    date = meta(page, "article:published_time")
    if not date:
        m = re.search(r"/(\d{4})/(\d{2})/(\d{2})/", url)
        date = "-".join(m.groups()) if m else None
    return {"blog": next((b for b in BLOGS if f"/{b}/" in url), None), "url": url, "slug": slug,
            "title": meta(page, "og:title") or slug, "date": date[:10] if date else None, "body": body}

def reviewed():
    try:
        return set(REVIEWED_FILE.read_text(encoding="utf-8").split())
    except FileNotFoundError:
        return set()
    except (OSError, UnicodeDecodeError) as e:
        log(f"warn: cannot read {REVIEWED_FILE}: {e}"); return set()

def pick(per_blog):
    seen, posts = reviewed(), []
    log(f"already reviewed: {len(seen)}")
    for name, (sitemap, path_filter) in BLOGS.items():
        try:
            urls = [html.unescape(u) for u in re.findall(r"<loc>([^<]+)</loc>", fetch(sitemap))]
        except FETCH_ERRORS as e:
            log(f"warn: sitemap failed {name}: {e}"); continue
        cands = [u for u in urls if re.search(path_filter, u) and u not in seen]
        random.shuffle(cands)
        log(f"{name}: {len(cands)} candidates")
        got = fails = 0
        while got < per_blog and cands and fails <= MAX_FAILURES_PER_BLOG:
            p = load(cands.pop())
            if p: posts.append(p); got += 1
            else: fails += 1
        if got < per_blog: log(f"warn: {name}: picked {got}/{per_blog}")
    return posts

args = sys.argv[1:]
if args and all(a.startswith("http") for a in args):
    posts = [p for p in map(load, args) if p]
elif not args:
    posts = pick(DEFAULT_PER_BLOG)
elif len(args) == 1 and args[0].isdigit() and int(args[0]) > 0:
    posts = pick(int(args[0]))
else:
    log("usage: [N | URL...]"); sys.exit(2)
print(json.dumps({"posts": posts}, ensure_ascii=False, indent=2))
sys.exit(0 if posts else 1)
PY
```

- stdout(`posts.json`): `{posts: [{blog, url, slug, title, date, body}]}`. stderr: 진행 상황과 경고
- `body`는 본문 텍스트. 헤딩은 `#`, 코드는 ``` 펜스로 보존
- exit 1(글 0개)이면 stderr 경고를 사용자에게 그대로 전하고 종료

### 2. 글마다 서브에이전트 병렬 리뷰

글 하나당 general-purpose 에이전트 하나를 **한 메시지에서 병렬로** 띄운다. 글끼리 컨텍스트를 섞지 않는 게 목적이므로 메인이 직접 리뷰하지 않는다. 프롬프트에 넣을 것:

- 리뷰할 글: `~/blog-feedback/posts.json`의 `posts[i]` (절대 경로와 인덱스로 전달, 서브에이전트가 Read로 읽음)
- 규칙: 이 SKILL.md 절대 경로. `## 메시지 규칙`, `## 표현 기준`, `## 유머 기준`, `## 메시지 템플릿` 섹션을 따르라고 지시
- 감정 단어 팔레트: 이 SKILL.md 옆 `EMOTION-WORDS.md` 절대 경로. `[감정]` 제안 때 Read로 참고하라고 지시
- 출력 경로: `~/blog-feedback/outbox/<blog>-<slug>.html` (절대 경로로 풀어서 전달, 디렉토리 없으면 생성)
- 지시: 메시지 파일을 Write로 저장한 뒤 `<제목> - <먼저 고칠 것 한 줄>` 한 줄만 반환. 글 본문 외 정보는 추측하지 말 것 (WebFetch로 다른 글 조회 금지)

### 3. 텔레그램 전송

서브에이전트가 모두 끝나면 아래를 실행한다. outbox의 파일마다 메시지 하나를 보내고, 성공한 글만 `reviewed.txt`에 URL을 추가하고 파일을 지운다. 실패한 파일은 outbox에 남으므로 원인을 고친 뒤 이 단계만 다시 실행하면 된다.

```bash
uv run --quiet --no-project --with keyring python - <<'PY'
# Delivers outbox messages to Telegram and records delivered posts as reviewed.
import json, sys, urllib.error, urllib.parse, urllib.request
from pathlib import Path

ROOT = Path.home() / "blog-feedback"
OUTBOX = ROOT / "outbox"
REVIEWED_FILE = ROOT / "reviewed.txt"
KEYRING_SERVICE = "blog-feedback"
TELEGRAM_MAX_CHARS = 4096
HTTP_TIMEOUT_SEC = 15
SETUP_HINT = """setup (once per machine, values are prompted):
  uvx keyring set blog-feedback telegram-token
  uvx keyring set blog-feedback telegram-chat-id"""

sys.stdout.reconfigure(encoding="utf-8"); sys.stderr.reconfigure(encoding="utf-8")

def die(m): print(m, file=sys.stderr); sys.exit(1)

try:
    import keyring
except ImportError:
    die("error: keyring not installed\n" + SETUP_HINT)
token = keyring.get_password(KEYRING_SERVICE, "telegram-token")
chat_id = keyring.get_password(KEYRING_SERVICE, "telegram-chat-id")
if not token or not chat_id:
    die("error: telegram-token or telegram-chat-id missing in keyring\n" + SETUP_HINT)

files = sorted(OUTBOX.glob("*.html")) if OUTBOX.is_dir() else []
if not files:
    die("error: outbox is empty")
failed = 0
for f in files:
    url, _, text = f.read_text(encoding="utf-8").partition("\n")
    text = text.strip()
    if not url.startswith("http") or not text:
        print(f"fail {f.name}: first line must be the post URL, then the message", file=sys.stderr); failed += 1; continue
    if len(text) > TELEGRAM_MAX_CHARS:
        print(f"fail {f.name}: {len(text)} chars > {TELEGRAM_MAX_CHARS}", file=sys.stderr); failed += 1; continue
    err = "telegram returned ok=false"
    data = urllib.parse.urlencode({"chat_id": chat_id, "text": text, "parse_mode": "HTML",
                                   "link_preview_options": json.dumps({"is_disabled": True})}).encode()
    try:
        with urllib.request.urlopen(f"https://api.telegram.org/bot{token}/sendMessage", data, timeout=HTTP_TIMEOUT_SEC) as r:
            ok = json.load(r).get("ok")
    except urllib.error.HTTPError as e:
        ok, err = False, e.read().decode("utf-8", errors="replace")
    except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
        ok, err = False, str(e)
    if not ok:
        print(f"fail {f.name}: {err}", file=sys.stderr); failed += 1; continue
    with REVIEWED_FILE.open("a", encoding="utf-8") as out:
        out.write(url.strip() + "\n")
    f.unlink()
    print(f"sent {f.name}")
sys.exit(1 if failed else 0)
PY
```

- `keyring`은 OS 기본 비밀 저장소를 쓴다(macOS 키체인, Windows 자격 증명 관리자). 토큰을 파일·환경 변수·dotfiles에 두지 않는다
- `uv`로 실행해 `keyring`을 전역 설치 없이 격리 환경에서 쓴다(Homebrew Python의 PEP 668 회피, Windows 동일). `uv`가 없으면 설치를 안내하고 멈춘다
- 키가 없거나 `keyring` 미설치면 stderr의 setup 안내를 사용자에게 그대로 전하고 멈춘다. 값을 대신 입력받거나 다른 곳에 저장하지 않는다
- HTML 파싱 오류(400 `can't parse entities`)로 실패하면 해당 서브에이전트 출력의 이스케이프 문제다. 그 파일만 고쳐 다시 실행

### 4. 보고

글마다 한 줄: `[<blog>] <서브에이전트 반환 한 줄>` + 전송 결과(sent/fail). 메시지 본문은 대화창에 다시 쓰지 않는다.

## 메시지 규칙

짧게. 글 하나에 인용 블록 6~8개가 상한이다. 모든 항목은 **원문 인용 → 그 아래 코멘트** 구조.

1. **좋은 점** 1개 - 반드시 가장 먼저. 빈말("잘 썼다") 금지, 왜 좋은지 구체적으로
2. **고칠 점** 2~3개 - 글쓰기(문장·리듬·오탈자), 구조(흐름·제목과 본문 일치), 논리(주장과 근거의 연결), 정확성·시의성, 독자 가치 중 이 글에서 가장 중요한 것만 고른다. 코멘트 앞에 축 이름을 짧게 붙이고, 가능하면 `→ 수정안`. 수정안은 글쓴이의 짧은 평서문 문체를 유지하면서 더 부드럽게 읽히는 연결과 리듬으로
   - 시의성 지적은 작성일(`date`) 기준으로는 타당했는지 맥락을 같이 적는다. 오래된 글이라는 이유만으로 깎지 않는다
   - 논리는 근거 없이 결론으로 건너뜀, 한두 경험으로 일반화, 인과가 아닌 것을 인과로 엮음, 본문에서 나오지 않는 결론을 본다. 배치·흐름은 구조 축 몫. `→ 수정안` 대신 `→ 빠진 연결고리`(넣으면 되는 근거·전제 한 문장)
   - 주장이 없는 일기·감상 글에서는 감정 흐름의 건너뜀을 논리 비약으로 지적하지 않는다. 억지로 찾지 말 것
3. **표현** 1~2개 - 최소 1개. `## 표현 기준`을 따른다
4. **유머** 1개 - 원문 인용 뒤에 붙일 문장. `## 유머 기준`을 따른다
5. **확장 주제** 1~2개 - 이 글에서 출발해 새 글로 써볼 만한 주제. 출발점이 된 원문을 인용하고 주제 한 줄 + 왜 지금 쓸 만한지 한 줄
6. **먼저 고칠 것** - 한 줄

코멘트는 한 항목당 1~2문장. 전체 4096자 이하(넉넉히 3500자 안팎 목표).

## 표현 기준

뭉툭한 표현을 더 정밀한 단어로 바꾸는 제안. 고칠 점이 틀린 것을 고친다면, 표현은 맞지만 흐릿한 것을 선명하게 한다. 아래 두 종류 중 이 글에 맞는 것만 고른다.

- `[감정]` - "좋았다", "힘들었다", "짜증 났다"처럼 감정을 뭉뚱그린 곳. 같은 감정군의 후보 단어 2~3개를 `/`로 나열하고, 그중 하나로 다시 쓴 문장을 `→`로 붙인다
  - 후보는 `EMOTION-WORDS.md`에서 고른다. 참고 팔레트일 뿐이므로 목록 밖 단어도 된다
  - 문맥에 맞는 감정군을 먼저 고르고(예: 일이 끝나서 좋았다면 기쁨보다 안도·홀가분), 그 안에서 고른다
- `[형용사]` - "좋은", "괜찮은", "대단한"처럼 무엇이 어떤지 말하지 않는 형용사. 색·소리·온도·질감·크기처럼 감각으로 떠오르는 형용사로 바꾼 문장을 `→`로 붙인다
  - 반대로 부사+형용사가 겹친 과잉 수식("정말 너무 좋은")이 눈에 띄면 덜어내는 쪽으로 제안해도 된다

공통:

- 글쓴이의 짧은 평서문 문체를 유지한다. 화려하거나 문어적인 단어로 바꾸지 말고 정밀도만 올린다
- 원문이 일부러 건조하게 쓴 곳(유머, 절제된 마무리)은 건드리지 않는다
- 비유 제안은 하지 않는다. 비유는 유머 몫

## 유머 기준

글쓴이가 선호하는 유머는 무미건조하지만 곱씹으면 재미있는 쪽(dry humor)이다.

원칙:

- 짧은 평서문. 감탄사, 이모티콘, "ㅋㅋ", 느낌표 없음
- 웃기려는 티를 내지 않고 사실을 적듯 씀. 펀치라인 설명이나 강조 없음
- 사물이나 대상에 의도를 붙임
- 핑계나 자기합리화를 정색하고 말함
- 예상과 반대로 뒤집음
- 개발 용어로 일상을 비유함
- 결과를 원인에 엮어 슬쩍 비꼼

글쓴이 본인의 문장 예시:

- "술 마시고 글을 쓰고 맑은 정신으로 퇴고하라는 말을 들은 적이 있다. 술이란 배경 마음으로 바로 교체할 수 있는 마법의 물약이라는 것인가?"
- "무의식까지 활용하게 하는 건 초과 근무이려나?"
- "허위 활동이라니. 한 시간에 하나. 출처도 표기. 착한 봇인데 말이다. 재수 없게 AI 검출 오류 오차범위에 들어간 것일까? 자동화된 이메일 답장에 삭막함을 느꼈다."
- "요리하고 상을 차리다 손이 미끄러져서 맥주도 같이 차렸다."
- "홍피망이 있는지 없는지에 비주얼이 결정된다. 청피망보다 1000원 더 비싸더라. 지들도 아는 거지."
- "체중 1~2kg 오차를 감수하고 있다. "사람의 기분까지 측정해주는 저울이다." 이렇게 좋게 생각하면서 사용하고 있다."
- "유명 인사답게 동영상도 많이 찍었다. 자신의 범죄 다큐멘터리에서 넉넉하게 사용할 정도의 양이다."
- "미래의 나를 믿으며 오늘의 할 일을 미루듯이 무작정 미루면 안 된다. 무의식 worker thread가 job을 잘 가져가서 처리할 수 있도록 의식이 데이터를 가공해놔야 한다."

예시는 톤 참고용. 예시 문장이나 소재를 재사용하지 말고, 리뷰하는 글의 내용에서 소재를 찾는다. 설명조("~하는 드문 경우다")로 끝내지 말 것 - 해석은 독자 몫으로 남긴다.

## 메시지 템플릿

파일 형식: **첫 줄은 글 URL 그대로**(전송 스크립트가 떼어내 `reviewed.txt`에 기록), 둘째 줄부터 텔레그램 HTML 메시지.

```html
https://ohyecloudy.com/...
<b>[<blog>] <title></b> (<date>)
<a href="<url>">원문</a>

<b>좋은 점</b>
<blockquote>원문 인용</blockquote>
코멘트

<b>고칠 점</b>
<blockquote>원문 인용</blockquote>
[글쓰기] 코멘트 → 수정안

<blockquote>원문 인용</blockquote>
[구조] 코멘트

<blockquote>원문 인용</blockquote>
[논리] 코멘트 → 빠진 연결고리

<b>표현</b>
<blockquote>원문 인용</blockquote>
[감정] 코멘트. 후보1 / 후보2 / 후보3 → 다시 쓴 문장

<b>유머</b>
<blockquote>원문 인용</blockquote>
뒤에 붙일 문장

<b>확장 주제</b>
<blockquote>원문 인용</blockquote>
주제 - 왜 지금 쓸 만한지

<b>먼저 고칠 것</b>
한 줄
```

렌더 규칙:

- 텔레그램 HTML이 지원하는 태그만: `<b>` `<i>` `<a>` `<code>` `<blockquote>`. 줄바꿈은 실제 개행(`<br>` 금지)
- 인용·코멘트 안의 `<` `>` `&`는 반드시 `&lt;` `&gt;` `&amp;`로 이스케이프 (안 하면 전송이 400으로 실패)
- 인용은 원문 그대로. 인용 안에서 글을 고치지 않는다. 긴 문장은 앞뒤를 `…`로 잘라도 된다
- em dash(U+2014) 금지, 하이픈(-) 사용. 이모지 없음
