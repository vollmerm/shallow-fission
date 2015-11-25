#!/bin/bash

# Script used to validate locally or from CI.

stack --no-system-ghc --install-ghc test
