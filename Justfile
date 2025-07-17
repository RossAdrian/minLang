# Justfile for mincc compiler + Venus test suite

# Download Venus if missing
download-venus:
  mkdir -p build
  if [ ! -f build/venus.jar ]; then \
  echo "Downloading Venus..."; \
    curl -L "https://github.com/ThaumicMekanism/venus/releases/latest/download/venus.jar" -o build/venus.jar; \
    echo "✅ Venus downloaded"; \
  fi

# Compile OCaml compiler
build:
  ocamlc -o mincc mincc.ml
  echo "✅ mincc compiler built"

# Run all tests
test: build download-venus
  mkdir -p build
  for src in tests/*.my_lang; do \
    base=$(basename $src .my_lang); \
    asm="build/$base.s"; \
    out="build/$base.out"; \
    echo "🛠️ Compiling $src..."; \
    ./mincc -c $src -o $asm; \
    echo "🚀 Running $asm..."; \
    java -jar build/venus.jar $asm > $out; \
    if grep -q "a0 = 0" $out; then \
      echo "✅ $base passed"; \
    else \
      echo "❌ $base failed"; \
    fi; \
  done

# Default target
default: test
