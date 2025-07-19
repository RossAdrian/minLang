# Justfile for mincc compiler + Venus test suite

# Download Venus if missing
download-venus:
  mkdir -p build
  if [ ! -f build/venus.jar ]; then \
  echo "Downloading Venus..."; \
    curl -L "https://github.com/ghishadow/venus-1/releases/latest/download/venus.jar" -o build/venus.jar; \
    echo "✅ Venus downloaded"; \
  fi

# Compile OCaml compiler
build:
  ocamlc -o mincc mincc.ml
  echo "✅ mincc compiler built"

# Run all tests
test: build
  sh ./scripts/run_x64_tests.sh

# Default target
default: test
