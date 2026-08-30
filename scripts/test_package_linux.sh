#!/usr/bin/env bash
# Cross-platform check: run the replication package's verification inside
# a Linux container, from the deposit zip alone.
#
# Requires docker (on macOS: `brew install colima docker && colima start`).
#
# Usage:  bash scripts/test_package_linux.sh [zip] [--amd64]
#   zip      path to the deposit (default: package-build/shadow-replication.zip)
#   --amd64  force x86-64 emulation (slower; different CPU architecture)
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
ZIP="${1:-$ROOT/package-build/shadow-replication.zip}"
PLATFORM="linux/$(docker version --format '{{.Server.Arch}}')"
[ "${2:-}" = "--amd64" ] && PLATFORM="linux/amd64"

echo "=== Linux container test: $PLATFORM, $(basename "$ZIP") ==="
docker run --rm --platform "$PLATFORM" \
  -v "$ZIP":/deposit.zip:ro \
  python:3.14-slim bash -eu -c '
    apt-get update -qq && apt-get install -y -qq unzip diffutils > /dev/null
    cd /tmp && unzip -q /deposit.zip && cd shadow-replication
    echo "--- container: $(uname -sm), $(python -V)"
    echo "--- installing frozen requirements (this downloads Linux wheels)"
    pip install -q -r requirements-freeze.txt
    echo "--- running the verification"
    PYTHON=python bash scripts/run_replication.sh
  '
