# HTML 리포트 형식

아키텍처 리뷰는 OS 임시 디렉터리에 자체 완결형(self-contained) HTML 파일 하나로 렌더한다. Tailwind와 Mermaid는 둘 다 CDN에서 온다. Mermaid는 그래프 모양 다이어그램을 안정적으로 처리하고, 손으로 만든 div와 인라인 SVG는 더 편집(editorial)적인 비주얼(질량 다이어그램, 단면도)을 처리한다. 둘을 섞는다: 모든 것을 Mermaid에 기대면 이내 뻔해 보인다.

## 스캐폴드(Scaffold)

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <title>Architecture review for {{repo name}}</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script type="module">
      import mermaid from "https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs";
      mermaid.initialize({ startOnLoad: true, theme: "neutral", securityLevel: "loose" });
    </script>
    <style>
      /* small custom layer for things Tailwind doesn't cover cleanly:
         dashed seam lines, hand-drawn-feeling arrow heads, etc. */
      .seam { stroke-dasharray: 4 4; }
      .leak { stroke: #dc2626; }
      .deep { background: linear-gradient(135deg, #0f172a, #1e293b); }
    </style>
  </head>
  <body class="bg-stone-50 text-slate-900 font-sans">
    <main class="max-w-5xl mx-auto px-6 py-12 space-y-12">
      <header>...</header>
      <section id="candidates" class="space-y-10">...</section>
      <section id="top-recommendation">...</section>
    </main>
  </body>
</html>
```

## 헤더(Header)

저장소 이름, 날짜, 그리고 압축된 범례(legend): 실선 상자 = module, 점선 = seam, 빨간 화살표 = leakage, 두꺼운 어두운 상자 = deep module. 도입 문단은 없다. 곧바로 후보로 들어간다.

## 후보 카드(Candidate card)

다이어그램이 무게를 진다. 산문은 드물고, 평이하며, (`codebase-design` 스킬의) glossary 용어를 격식 없이 쓴다.

각 후보는 하나의 `<article>`:

- **Title**: 짧게, deepening을 이름 짓는다(예: "Collapse the Order intake pipeline").
- **Badge row**: recommendation strength(`Strong` = emerald, `Worth exploring` = amber, `Speculative` = slate), 그리고 의존성 분류 태그(`in-process`, `local-substitutable`, `ports & adapters`, `mock`).
- **Files**: 모노스페이스 목록, `font-mono text-sm`.
- **Before / After diagram**: 중심축. 두 컬럼, 나란히. 아래 패턴 참고.
- **Problem**: 한 문장. 무엇이 아픈가.
- **Solution**: 한 문장. 무엇이 바뀌는가.
- **Wins**: bullet, 각 6단어 이하. 예: "Tests hit one interface", "Pricing logic stops leaking", "Delete 4 shallow wrappers".
- **ADR callout**(해당 시): amber 색조 상자에 한 줄.

설명 문단은 없다. 다이어그램을 이해하는 데 문단이 필요하다면, 다이어그램을 다시 그려라.

## 다이어그램 패턴(Diagram patterns)

후보에 맞는 패턴을 고른다. 섞는다. 모든 다이어그램이 똑같아 보이게 하지 마라. 다양성이 핵심의 일부다.

### Mermaid 그래프 (의존성 / 콜 플로우의 주력)

요점이 "X가 Y를 부르고 Y가 Z를 부르는데, 이 난장판을 봐라"일 때 Mermaid `flowchart`나 `graph`를 쓴다. 낙하산처럼 튀지 않게 Tailwind 스타일 카드로 감싼다. `classDef`로 leakage 엣지를 빨강, deep module을 어둡게 스타일링한다. "before: 6번 왕복; after: 1번"에는 시퀀스 다이어그램이 잘 맞는다.

```html
<div class="rounded-lg border border-slate-200 bg-white p-4">
  <pre class="mermaid">
    flowchart LR
      A[OrderHandler] --> B[OrderValidator]
      B --> C[OrderRepo]
      C -.leak.-> D[PricingClient]
      classDef leak stroke:#dc2626,stroke-width:2px;
      class C,D leak
  </pre>
</div>
```

### 손으로 만든 boxes-and-arrows (Mermaid 레이아웃이 말을 안 들을 때)

module을 테두리와 라벨을 가진 `<div>`로. 화살표는 relative 컨테이너 위에 absolute로 배치한 인라인 SVG `<line>`이나 `<path>`로. "after" 다이어그램을 내부가 회색 처리된 하나의 두꺼운 테두리 deep module처럼 보이게 하고 싶을 때 이걸 꺼내라. Mermaid는 그 무게감으로 렌더하지 못한다.

### 단면도(Cross-section) (계층적 얕음에 좋음)

호출이 통과하는 계층을 보여주려 수평 띠(`h-12 border-l-4`)를 쌓는다. Before: 각자 아무것도 안 하는 6개의 얇은 계층. After: 통합된 책임으로 라벨된 두꺼운 띠 1개.

### 질량 다이어그램(Mass diagram) ("인터페이스가 구현만큼 넓음"에 좋음)

module당 두 사각형: 하나는 인터페이스 표면적, 하나는 구현. Before: 인터페이스 사각형이 구현 사각형만큼 높다(shallow). After: 인터페이스 사각형은 짧고 구현 사각형은 높다(deep).

### 콜 그래프 붕괴(Call-graph collapse)

Before: 중첩된 상자로 렌더된 함수 호출 트리. After: 같은 트리가 하나의 상자로 붕괴되고, 이제 내부가 된 호출들은 그 안에 흐리게 표시된다.

## 스타일 가이드(Style guidance)

- 코퍼레이트 대시보드가 아니라 편집(editorial)적으로. 여백을 넉넉히. 헤딩에 세리프 선택 가능(`font-serif`는 stone/slate와 잘 어울린다).
- 색은 아끼며: 액센트 하나(emerald 또는 indigo) + leakage용 빨강 + 경고용 amber.
- before/after가 스크롤 없이 나란히 편안하게 앉도록 다이어그램은 ~320px 높이로 유지한다.
- 다이어그램 안 module 라벨은 `text-xs uppercase tracking-wider`로, UI가 아니라 도식(schematic)처럼 읽히게.
- 스크립트는 Tailwind CDN과 Mermaid ESM import뿐이다. 그 외에는 정적: 앱 코드 없음, Mermaid 자체 렌더링 외 인터랙티비티 없음.

## Top recommendation 섹션

더 큰 카드 하나. 후보 이름, 이유 한 문장, 해당 카드로 가는 앵커 링크. 그게 전부다.

## 톤(Tone)

평이한 문장, 간결하게. 단, 아키텍처 명사와 동사는 `codebase-design` 스킬에서 곧장 온다. 간결함이 표류의 핑계는 아니다.

**정확히 이것만 쓴다:** module, interface, implementation, depth, deep, shallow, seam, adapter, leverage, locality.

**절대 바꿔 쓰지 않는다:** component, service, unit (module 대신) · API, signature (interface 대신) · boundary (seam 대신) · layer, wrapper (module을 뜻할 때).

**이 스타일에 맞는 표현:**

- "Order intake module is shallow: interface nearly matches the implementation."
- "Pricing leaks across the seam."
- "Deepen: one interface, one place to test."
- "Two adapters justify the seam: HTTP in prod, in-memory in tests."

**Wins bullet**은 이득을 glossary 용어로 이름 짓는다: *"locality: bugs concentrate in one module"*, *"leverage: one interface, N call sites"*, *"interface shrinks; implementation absorbs the wrappers"*. *"easier to maintain"*이나 *"cleaner code"*라고 쓰지 마라. glossary에 없는 말이고 제 몫을 하지 못한다.

얼버무림 없이, 목청 가다듬기 없이, "it's worth noting that…" 없이. 문장이 bullet이 될 수 있으면 bullet으로 만들어라. bullet을 잘라낼 수 있으면 잘라내라. 어떤 용어가 `codebase-design` glossary에 없다면, 새 용어를 지어내기 전에 있는 용어로 손을 뻗어라.
