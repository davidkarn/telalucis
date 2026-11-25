#!/usr/bin/env bash
# Distill a local model for footnote extraction from the relabelled training chats (see
# relabel-footnote-training): train a LoRA adapter for a small model with mlx-lm on a share of
# the chats, serve it, and measure it on the eval set (eval-footnotes), as a run in
# output/distill/<run>/. Apple silicon only (MLX).
#
#   scripts/distill_footnotes.sh [all|train|eval]        (from src/; all by default)
#
# train: pick the chats (those that fit in MAX_SEQ tokens), then train. eval: serve the run's
# adapter and measure it on the hand-checked batches. Settings, by environment variable:
#   RUN          the run's name (distill-<student>-<chats>)
#   STUDENT      the model to train, an MLX model on Hugging Face (mlx-community/Qwen2.5-7B-Instruct-4bit)
#   TEACHER      whose answers to learn (anthropic/claude-haiku-5.5)
#   TRAIN_CHATS  training chats to use, the first so many (1500; 0 for all)
#   VALID_CHATS  validation chats (100)
#   ITERS        training steps, a chat each (as many as the training chats: one pass)
#   MAX_SEQ      the longest chat trained on, in tokens; longer ones are left out (8192)
#   NUM_LAYERS   layers given adapters (16; -1 for all)
#   LR           learning rate (1e-5)
#   PORT         the server's port while measuring (the first free one from 8081; 8080 is the
#                default local model's)
#   EVAL_ARGS    more arguments for eval-footnotes ("--checked-only")
#   TEMPLATE_ARGS chat template arguments for the server, as JSON (for Qwen3: '{"enable_thinking": false}')
set -euo pipefail

cd "$(dirname "$0")/.."

STEP="${1:-all}"
STUDENT="${STUDENT:-mlx-community/Qwen2.5-7B-Instruct-4bit}"
TEACHER="${TEACHER:-anthropic/claude-haiku-5.5}"
TRAIN_CHATS="${TRAIN_CHATS:-1500}"
VALID_CHATS="${VALID_CHATS:-100}"
MAX_SEQ="${MAX_SEQ:-8192}"
NUM_LAYERS="${NUM_LAYERS:-16}"
LR="${LR:-1e-5}"
PORT="${PORT:-}"
EVAL_ARGS="${EVAL_ARGS:---checked-only}"
TEMPLATE_ARGS="${TEMPLATE_ARGS:-}"

slug() { echo "$1" | sed 's/[^A-Za-z0-9.-]/_/g'; }

RUN="${RUN:-distill-$(slug "${STUDENT##*/}")-${TRAIN_CHATS}}"
CHATS="output/footnote_training/relabel/$(slug "$TEACHER")"
OUT="output/distill/$RUN"
VENV="output/distill/venv"
PY="$VENV/bin/python"

if [[ ! -f "$CHATS/train.jsonl" ]]; then
  echo "no chats in $CHATS/train.jsonl: run relabel-footnote-training --model $TEACHER first" >&2
  exit 1
fi

# mlx-lm in a venv of its own, made once
if [[ ! -x "$VENV/bin/mlx_lm.lora" ]]; then
  echo "making $VENV with mlx-lm"
  python3.14 -m venv "$VENV" 2>/dev/null || python3 -m venv "$VENV"
  "$VENV/bin/pip" install --quiet --upgrade pip mlx-lm
fi

mkdir -p "$OUT/data"

# The first TRAIN_CHATS training chats (written in an order spread over the books) and
# VALID_CHATS validation ones, as mlx-lm reads them; those longer than MAX_SEQ tokens are left
# out, as mlx-lm would cut them short, teaching unfinished answers
pick_chats() {
  "$PY" - "$STUDENT" "$CHATS" "$OUT/data" "$TRAIN_CHATS" "$VALID_CHATS" "$MAX_SEQ" <<'PYTHON'
import json, sys
from transformers import AutoTokenizer

student, chats, out, train_n, valid_n, max_seq = sys.argv[1:7]
tokenizer = AutoTokenizer.from_pretrained(student)

for split, n in (('train', int(train_n)), ('valid', int(valid_n))):
    kept, long = [], 0
    with open(f'{chats}/{split}.jsonl') as f:
        for line in f:
            if n > 0 and len(kept) >= n:
                break
            messages = json.loads(line)['messages']
            text     = tokenizer.apply_chat_template(messages, tokenize=False)
            tokens   = len(tokenizer.encode(text, add_special_tokens=False))
            if tokens > int(max_seq):
                long += 1
            else:
                kept.append(json.dumps({'messages': messages}, ensure_ascii=False))
    with open(f'{out}/{split}.jsonl', 'w') as f:
        f.write('\n'.join(kept) + '\n')
    print(f'{split}: {len(kept)} chats, {long} longer than {max_seq} tokens left out')
PYTHON
}

train() {
  pick_chats
  local chats
  chats=$(wc -l < "$OUT/data/train.jsonl" | tr -d ' ')
  echo "training $STUDENT on $chats chats ($OUT)"
  "$VENV/bin/mlx_lm.lora" \
    --model "$STUDENT" --train --data "$OUT/data" --adapter-path "$OUT/adapters" \
    --fine-tune-type lora --mask-prompt --batch-size 1 --iters "${ITERS:-$chats}" \
    --max-seq-length "$MAX_SEQ" --num-layers "$NUM_LAYERS" --learning-rate "$LR" \
    --grad-checkpoint --steps-per-report 25 --steps-per-eval 250 --val-batches 25 \
    --save-every 250 --seed 0 \
    2>&1 | tee "$OUT/train.log"
}

# Serve the student with its adapter until measured, then stop it. The server names the model
# it was started with "default_model", so the eval asks for local/default_model.
evaluate() {
  if [[ ! -f "$OUT/adapters/adapters.safetensors" ]]; then
    echo "no adapter in $OUT/adapters: train first" >&2
    exit 1
  fi

  # another server on the port would answer in the student's place
  in_use() { lsof -nP -iTCP:"$1" -sTCP:LISTEN > /dev/null 2>&1; }
  if [[ -z "$PORT" ]]; then
    PORT=8081
    while in_use "$PORT"; do PORT=$((PORT + 1)); done
  elif in_use "$PORT"; then
    echo "port $PORT is in use (by $(lsof -nP -iTCP:"$PORT" -sTCP:LISTEN | awk 'NR == 2 { print $1 }')): give another PORT" >&2
    exit 1
  fi

  local args=(--model "$STUDENT" --adapter-path "$OUT/adapters" --port "$PORT" --max-tokens 8192)
  if [[ -n "$TEMPLATE_ARGS" ]]; then
    args+=(--chat-template-args "$TEMPLATE_ARGS")
  fi

  echo "serving $STUDENT with $OUT/adapters on port $PORT"
  "$VENV/bin/mlx_lm.server" "${args[@]}" > "$OUT/server.log" 2>&1 &
  local server=$!
  trap 'kill "$server" 2>/dev/null || true' EXIT

  local ready=false
  for _ in $(seq 1 300); do
    if ! kill -0 "$server" 2>/dev/null; then
      echo "the server stopped: see $OUT/server.log" >&2
      exit 1
    elif curl -s "http://127.0.0.1:$PORT/v1/models" > /dev/null; then
      ready=true
      break
    fi
    sleep 2
  done
  if [[ "$ready" != true ]]; then
    echo "the server didn't start in 10 minutes: see $OUT/server.log" >&2
    exit 1
  fi

  # shellcheck disable=SC2086 # EVAL_ARGS is several arguments
  LOCAL_LLM_URL="http://127.0.0.1:$PORT/v1" npx tsx --env-file=.env cli/main.ts eval-footnotes \
    --model local/default_model $EVAL_ARGS --out "$OUT/eval.json" \
    2>&1 | tee "$OUT/eval.log"
}

case "$STEP" in
  all)   train && evaluate ;;
  train) train ;;
  eval)  evaluate ;;
  *)     echo "usage: $0 [all|train|eval]" >&2; exit 1 ;;
esac
