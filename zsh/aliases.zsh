alias reload!='. ~/.zshrc'

function history-all { history -Ei 1 } # 全履歴の一覧を出力する
function histall  { history -Ei 1 } # 全履歴の一覧を出力する
alias hist='history -Ei'
alias histgrep='history -Ei 1 | grep'
