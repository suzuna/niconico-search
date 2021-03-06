## 使い方

### 特徴

公式のニコニコ動画では行えない、以下の検索が可能です。

- 投稿日時、再生数、コメント数、マイリスト数、いいね数、再生時間をフィルタ条件に指定した検索
- 検索結果を、コメント率、マイリスト率、いいね率、マイリスト数/コメント数の大小順で表示

ただし、二点目の特徴を実現するために、全ての検索結果を一度に取得する必要があります。そのため、

- 検索結果の件数が多いと表示に時間がかかります。
  - 検索結果1000件につき5秒程度かかります。
- ニコニコ動画に負荷をかけすぎないようにするため、検索結果を表示できる上限を10000件としています。
  - 10000件を超える場合は、10000件を超えないようにフィルタ条件を設定してください。

### 使い方

左側のパネルに条件を入力し、表示順を選択して検索ボタンを押してください。使わない条件は空欄でOKです。

検索結果が表示された後に表示順を変えたい場合は、表示順を選択し直して検索ボタンを押してください。

### その他仕様

マイリスト数/コメント数の大小を表示順に設定した場合、コメント数が0の動画は検索結果から除外されます。（0で割れないため）

そのため、検索結果にコメント数が0の動画がある場合、結果の上部に表示される検索結果の件数より、実際に表示される動画の件数が少なくなりますが、仕様です。

### 仕組み

ニコニコ動画公式の[スナップショット検索API v2](https://site.nicovideo.jp/search-api-docs/snapshot)を使用しています。

APIの仕様上、データの更新は1日1回です。
