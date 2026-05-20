#!/bin/bash
# ===========================================================================
# build_demos.sh — Compila todos los demos de MakerAI Suite (FPC Port)
# ===========================================================================
# Uso:
#   ./build_demos.sh                    — Linux nativo
#   ./build_demos.sh --win64            — cross-compile a Windows x64
#   FPC=/ruta/al/fpc ./build_demos.sh   — FPC específico
#   ./build_demos.sh clean              — elimina bin/ bin_win/ y lib/
# ===========================================================================

set -euo pipefail

FPC="${FPC:-fpc}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# --- clean ---
if [ "${1:-}" = "clean" ]; then
    echo "🧹 Limpiando bin/ bin_win/ lib/ ..."
    rm -rf bin bin_win lib
    echo "✅ Listo. Ejecuta sin argumentos para recompilar."
    exit 0
fi

# --- win64 cross-compile ---
WIN64=false
if [ "${1:-}" = "--win64" ]; then
    WIN64=true
    # Buscar ppcrossx64 (cross-compiler Win64)
    FPC_DIR="$(dirname "$FPC")"
    if [ -x "$FPC_DIR/ppcrossx64" ]; then
        FPC="$FPC_DIR/ppcrossx64"
    elif command -v ppcrossx64 &>/dev/null; then
        FPC=ppcrossx64
    else
        echo "❌ ppcrossx64 no encontrado. Instálalo o especifica FPC=..."
        exit 1
    fi
    BIN_DIR="bin_win"
    EXTRA_FLAGS=(-Twin64)
    # RTL de Win64 (junto al cross-compiler o en fpc.cfg)
    RTL_WIN64="${FPC_DIR}/../units/x86_64-win64/rtl"
    if [ -d "$RTL_WIN64" ]; then
        EXTRA_FLAGS+=("-Fu$RTL_WIN64")
    fi
    PLATFORM="Windows x64"
else
    BIN_DIR="bin"
    EXTRA_FLAGS=()
    PLATFORM="Linux ($(uname -m))"
fi

# --- preparar directorios ---
mkdir -p "$BIN_DIR" lib

# --- compilar ---
OK=0
FAIL=0
TOTAL=0
FAIL_LIST=""

echo "📦 Compilando para $PLATFORM"
echo "   FPC: $FPC"
echo "   Dir: $SCRIPT_DIR"
echo "═══════════════════════════════════════════════════════════════"

for f in *.pas; do
    TOTAL=$((TOTAL + 1))
    printf "  [%2d] %-35s " "$TOTAL" "$f"

    OUT=$(mktemp /tmp/makerai_build_XXXXXX)
    # shellcheck disable=SC2086
    if $FPC -FE"$BIN_DIR" -FUlib "${EXTRA_FLAGS[@]}" "$f" >"$OUT" 2>&1; then
        echo "✅"
        OK=$((OK + 1))
    else
        echo "❌ FAIL"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $f"
        grep -E 'Fatal:|Error:' "$OUT" | head -3 | while read -r line; do
            echo "       $line"
        done
    fi
    rm -f "$OUT"
done

# --- reporte ---
echo "═══════════════════════════════════════════════════════════════"
echo "  OK: $OK  |  FAIL: $FAIL  |  Total: $TOTAL"

if [ "$FAIL" -gt 0 ]; then
    echo ""
    echo "  Fallaron:"
    for f in $FAIL_LIST; do
        echo "    ❌ $f"
    done
    if $WIN64; then
        echo ""
        echo "  💡 Los errores pueden deberse a units no compiladas para Win64."
        echo "     Verifica que las dependencias tengan RTL para x86_64-win64."
    fi
    exit 1
else
    echo "  ✅ Todos compilados correctamente."
    if $WIN64; then
        echo ""
        echo "  📁 Ejecutables en: $BIN_DIR/"
        echo "  📋 Copiar a Windows: cp $BIN_DIR/*.exe ~/Escritorio/Compartidos_MV_IA/MakerAI_Demos_Win/"
    fi
    exit 0
fi
