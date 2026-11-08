# LeanCcg

[![Lean Action CI](https://github.com/rikitoro/LeanCcg/actions/workflows/lean_action_ci.yml/badge.svg?branch=main)](https://github.com/rikitoro/LeanCcg/actions/workflows/lean_action_ci.yml)

Lean 4で実装した組合せ範疇文法（Combinatory Categorial Grammar; CCG）の構文解析器である。語と範疇の対応を辞書として与え、CYK型のチャート解析により導出木を列挙する。

基本範疇・関数型範疇・範疇変数を扱い、前向き関数適用では単一化によって範疇変数を具体化する。得られた置換は導出木全体に反映する。

## 環境とビルド

Lean 4.35.0を使用する。バージョンは `lean-toolchain` で指定している。

`elan` を導入した環境で、以下を実行する。

```bash
git clone https://github.com/rikitoro/LeanCcg.git
cd LeanCcg
lake build LeanCcg LeanCcg.Examples
```

ライブラリと実行例をビルドし、各ファイルの `#guard` を評価する。GitHub Actionsでも同じ対象をビルドする。

## 使い方

プロジェクト直下に `Example.lean` を作成し、次のコードを記述する。

```lean
import LeanCcg

def exampleLexicon : Lexicon := {
  ("Keats",  [.NP]),
  ("eats",   [.S \> .NP /> .NP]),
  ("apples", [.NP])
}

-- 文全体の範疇が S となる導出木を表示する。
#eval (parse exampleLexicon "Keats eats apples").filter
  (fun t => t.cat == .S)

#guard (parse exampleLexicon "Keats eats apples").any
  (fun t => t.cat == .S)
```

VS Code上で評価するか、以下を実行する。

```bash
lake env lean Example.lean
```

`parse` は空白区切りの文字列を受け取り、入力全体を覆う導出木を `List Tree` として返す。返す木の根を `S` に限定しないため、必要に応じて `Tree.cat` で絞り込む。解析結果がなければ空リストを返す。

### 範疇の記法

| 範疇 | Leanでの表現 |
| --- | --- |
| 文 `S` | `.S` |
| 名詞句 `NP` | `.NP` |
| 名詞 `N` | `.N` |
| 範疇変数 `X0` | `.Var 0` |
| 右側の `Y` を引数とし、`X` を返す範疇 `X/Y` | `x /> y` |
| 左側の `Y` を引数とし、`X` を返す範疇 `X\Y` | `x \> y` |

`/>` と `\>` は同じ優先順位の左結合である。例えば `.S \> .NP /> .NP` は `(.S \> .NP) /> .NP` を表す。

## 範疇変数と単一化

等位接続詞 `and` に範疇 `(X\X)/X` を与えることで、名詞句と動詞句に同じ辞書項目を使える。

```lean
import LeanCcg

def coordLexicon : Lexicon := {
  ("and",    [(.Var 0 \> .Var 0) /> .Var 0]),
  ("apples", [.NP]),
  ("pears",  [.NP]),
  ("runs",   [.S \> .NP]),
  ("sleeps", [.S \> .NP])
}

#eval (parse coordLexicon "apples and pears").filter
  (fun t => t.cat == .NP)

#eval (parse coordLexicon "runs and sleeps").filter
  (fun t => t.cat == (.S \> .NP))
```

`and pears` では `X` が `NP` に、`and sleeps` では `S\NP` に具体化される。この置換は結果の範疇だけでなく、`and` の葉を含む導出木全体に反映される。

変数は各語の出現ごとに独立に扱う。木を結合する際に右側の変数IDを振り直し、左側との衝突を避ける。単一化には occurs check を含み、`X = X/NP` のような循環する制約を拒否する。

## 組合せ規則

| 規則 | 表示 | 内容 |
| --- | --- | --- |
| `Fa` | `[>]` | 前向き関数適用 |
| `Ba` | `[<]` | 後向き関数適用 |
| `Fc` | `[>B]` | 一般化前向き関数合成 |
| `Bc` | `[<B]` | 一般化後向き関数合成 |
| `Fx` | `[>Sx]` | 前向き交差置換 |
| `Bx` | `[<Sx]` | 後向き交差置換 |
| `Ft` | `[>T]` | 前向き型繰り上げ |
| `Bt` | `[<T]` | 後向き型繰り上げ |

現在、パーサーで単一化を行う規則は `Fa` のみである。他の二項規則は、対応する範疇の完全一致を用いる。型繰り上げは `NP` から `S/(S\NP)` または `S\(S/NP)` への変換に限定している。

## ファイル構成

| ファイル | 内容 |
| --- | --- |
| `LeanCcg.lean` | ライブラリの入口 |
| `LeanCcg/Category.lean` | 範疇・変数・記法・範疇の表示 |
| `LeanCcg/Subst.lean` | 有限置換の適用と合成 |
| `LeanCcg/Unify.lean` | 範疇の単一化と循環検出 |
| `LeanCcg/Rules.lean` | 組合せ規則と規則適用時の置換 |
| `LeanCcg/Tree.lean` | 導出木・木全体への置換・変数IDの振り直し |
| `LeanCcg/Applying.lean` | 規則による木の結合と型繰り上げ |
| `LeanCcg/Parsing.lean` | 辞書・チャート解析・`parse` |
| `LeanCcg/Utility.lean` | トークン分割などの補助処理 |
| `LeanCcg/Examples.lean` | 解析例と回帰テスト |

## テスト

`#guard` により、置換の適用・合成、単一化の成功・失敗、循環検出、変数スコープの分離、導出木全体への置換の反映を確認する。

```bash
lake build LeanCcg LeanCcg.Examples
```

`lake build` のみでは `Examples.lean` は対象にならないため、回帰テストを含める場合は上記のように明示する。

## 現在の範囲

- 手動で定義した範疇辞書を使い、構文的な導出木を生成する。意味表現の構築や意味合成は含まない。
- 入力は半角スペースで分割する。句読点の分離や形態素解析は行わない。
- 複数の導出木を列挙し、同値な導出の正規化・重複除去は行わない。
- 動作確認は `#guard` によるテストであり、解析器の健全性・完全性の形式証明は含まない。
