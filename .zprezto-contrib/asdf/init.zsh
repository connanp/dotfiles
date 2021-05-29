# load this before compinit is called, so before completions
if [[ -s "$HOME/.asdf/asdf.sh" ]]; then
  . $HOME/.asdf/asdf.sh
  fpath=(${ASDF_DIR}/completions $fpath)
fi

if (( ! $+commands[asdf] )); then
  return 1
fi
