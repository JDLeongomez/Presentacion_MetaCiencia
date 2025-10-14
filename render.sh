#!/bin/bash
# Renderiza el sitio y copia los GIFs al output

## PARA CORRER: ./render.sh

quarto render
cp img/*.gif docs/img/
echo "Render completado y GIFs copiados ✅"
