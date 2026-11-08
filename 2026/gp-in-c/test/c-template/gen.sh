#!/bin/bash
awk -f ui/preprocess.awk "$1" \
    | awk -f ui/template-to-m4.awk \
    | awk -f ui/trace-scopes.awk \
    | awk -f ui/postprocess.awk \
    | m4 c-template.m4 -
